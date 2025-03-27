library(data.table)
library(magrittr)
library(ggplot2)

# Loading the cleaned data
d_deaths <- readxl::read_excel("data.xlsx")
setDT(d_deaths)
d <- melt.data.table(
  d_deaths, 
  id.vars = "age",
  variable.factor = FALSE,
  value.name = "deaths_n"
  )
d[, year := as.numeric(variable)]
d[, variable := NULL]
d[, age_numeric := stringr::str_extract(age, "\\d+") %>% as.numeric()]
d[, age_cat := fcase(
  age_numeric == 0, "000_000",
  age_numeric %in% 1:19, "001_019",
  age_numeric %in% 20:39, "020_039",
  age_numeric %in% 40:64, "040_064",
  age_numeric %in% 65:79, "065_079",
  age_numeric %in% 80:200, "080p"
)]
d <- d[, .(
  deaths_n = sum(deaths_n)
), keyby = .(year, age_cat)]


p <- csdata::nor_population_by_age_cats(cats = list(
  "000_000" = 0,
  "001_019" = 1:19,
  "020_039" = 20:39,
  "040_064" = 40:64,
  "065_079" = 65:79,
  "080p" = 80:200
))[location_code=="nation_nor"]

d[p, on = c("year==calyear", "age_cat==age"), pop_jan1_n := pop_jan1_n]
d[,deaths_vs_pop_pr100000 := 100000*deaths_n/pop_jan1_n]

d[, year_frozen_at_2023 := year]
d[year >= 2023, year_frozen_at_2023 := 2023]

p <- plnr::Plan$new()
p$add_data(name="data", direct = d)

p$add_argset(age = "000_000")
p$add_argset(age = "001_019")
p$add_argset(age = "020_039")
p$add_argset(age = "040_064")
p$add_argset(age = "065_079")
p$add_argset(age = "080p")

analysis_1 <- function(argset, data){
  if(plnr::is_run_directly()){
    data <- p$get_data()
    argset <- p$get_argset(2)
  }
  pd <- data$data[age_cat==argset$age & year>=2010]
  fit_data <- pd[year<=2019]
  if(argset$age=="001_019") fit_data <- fit_data[year!=2011]

  set.seed(4)
  fit <- rstanarm::stan_glm(
    deaths_n ~ year_frozen_at_2023, 
    data = fit_data, 
    offset=log(pop_jan1_n),
    family = poisson,
    refresh = 0,
    iter = 50000,
    chains = 10
  )

  pred_baseline <- rstanarm::posterior_predict(fit, pd, offset=log(pd$pop_jan1_n))
  pred_obs_minus_baseline <- pred_baseline
  pred_obs_over_baseline_pr1 <- pred_baseline
  for(i in 1:ncol(pred_baseline)){
    pred_obs_minus_baseline[,i] <-  pd$deaths_n[i] - pred_baseline[,i]
    pred_obs_over_baseline_pr1[,i] <- pd$deaths_n[i] / pred_baseline[,i]
  }
  retval1 <- apply(pred_baseline, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
    t() %>%
    as.data.frame()
  names(retval1) <- c("baseline_p025", "baseline_p50", "baseline_p975")
  
  retval2 <- apply(pred_obs_minus_baseline, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
    t() %>%
    as.data.frame()
  names(retval2) <- c("absoluteexcess_p025", "absoluteexcess_p50", "absoluteexcess_p975")
  
  retval3 <- apply(pred_obs_over_baseline_pr1, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
    t() %>%
    as.data.frame()
  names(retval3) <- c("relativeexcess_p025", "relativeexcess_p50", "relativeexcess_p975")

  return(cbind(pd, retval1, retval2, retval3))
}

p$apply_action_fn_to_all_argsets(fn = analysis_1)

raw <- p$run_all()
r <- rbindlist(raw)

r[, age_pretty := fcase(
  age_cat=="000_000", "0 years old",
  age_cat=="001_019", "1-19 years old",
  age_cat=="020_039", "20-39 years old",
  age_cat=="040_064", "40-64 years old",
  age_cat=="065_079", "65-79 years old",
  age_cat=="080p", "80+ years old"
)]

q <- ggplot(r[year<=2024], aes(x = year))
q <- q + geom_ribbon(aes(ymin=100000*baseline_p025/pop_jan1_n, ymax=100000*baseline_p975/pop_jan1_n), fill = "black", alpha = 0.5)
q <- q + geom_line(aes(y = 100000*baseline_p50/pop_jan1_n))
q <- q + geom_point(aes(y = 100000*deaths_n/pop_jan1_n))
q <- q + geom_vline(xintercept = 2019.5, lty = 2, color = "red")
q <- q + facet_wrap(~age_pretty, scales = "free")
q <- q + scale_y_continuous("Deaths per 100 000 population")
q <- q + scale_x_continuous(NULL, breaks = seq(2010, 2024, 2), minor_breaks = NULL)
q <- q + csstyle::set_x_axis_vertical()
print(q)

tab <- r[year==2024,.(
  age_pretty,
  deaths_n,
  baseline_p50,
  baseline_ci = paste0(baseline_p025, " to ",baseline_p975),
  absoluteexcess_p50,
  absoluteexcess_ci = paste0(absoluteexcess_p025, " to ", absoluteexcess_p975),
  relavtiveexcess_p50 = round(100*(relativeexcess_p50-1),1),
  relativeexcess_ci = paste0(round(100*(relativeexcess_p025-1),1), " to ", round(100*(relativeexcess_p975-1),1))
)]
tab
