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

d <- d[age_numeric %in% 1:39]

d <- d[, .(
  deaths_n = sum(deaths_n)
), keyby = .(year)]


p <- csdata::nor_population_by_age_cats(cats = list(
  "001_039" = 1:39
), include_total = FALSE)[
  location_code=="nation_nor" & 
    age=="001_039" & 
    calyear %in% 2010:2024]

d[p, on = c("year==calyear"), pop_jan1_n := pop_jan1_n]
d[,deaths_vs_pop_pr100000 := 100000*deaths_n/pop_jan1_n]

d[, year_frozen_at_2023 := year]
d[year >= 2023, year_frozen_at_2023 := 2023]

set.seed(4)
fit <- rstanarm::stan_glm(
  deaths_n ~ year_frozen_at_2023, 
  data = d[year %in% c(2010, 2012:2019)], 
  offset=log(pop_jan1_n),
  family = poisson,
  refresh = 0,
  iter = 50000,
  chains = 10
)
  
pred_baseline <- rstanarm::posterior_predict(fit, d, offset=log(d$pop_jan1_n))
pred_obs_minus_baseline <- pred_baseline
pred_obs_over_baseline_pr1 <- pred_baseline
for(i in 1:ncol(pred_baseline)){
  pred_obs_minus_baseline[,i] <-  d$deaths_n[i] - pred_baseline[,i]
  pred_obs_over_baseline_pr1[,i] <- d$deaths_n[i] / pred_baseline[,i]
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

raw <- cbind(d, retval1, retval2, retval3)

r <- copy(raw)

q <- r %>%
  dplyr::mutate(label = dplyr::case_when(
    year < 2020 ~ "Dødsfall før 2020",
    deaths_n > baseline_p975 & year >= 2020 ~ "Dødsfall etter 2020 som er høyere enn forventet",
    deaths_n <= baseline_p975 & year >= 2020 ~ "Dødsfall etter 2020 som ikke er høyere enn forventet"
  )) %>%
  dplyr::filter(year <= 2024) %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = 100000 * baseline_p025 / pop_jan1_n,
                  ymax = 100000 * baseline_p975 / pop_jan1_n),
              alpha = 0.5, fill = "lightgrey") +
  geom_line(aes(y = 100000 * baseline_p50 / pop_jan1_n), color = "darkgrey", linewidth = 1) +
  geom_point(aes(y = 100000 * deaths_n / pop_jan1_n, fill = label),
             size = 4, shape = 21, color = "black", alpha = 0.9) +
  geom_vline(xintercept = 2019.5, lty = 2, color = "red", linewidth = 1) +
  scale_y_continuous("Dødsfall per 100 000 innbyggere") +
  scale_x_continuous(NULL, breaks = seq(2010, 2024, 1), minor_breaks = NULL) +
  scale_fill_manual(NULL, values = c(
    "Dødsfall før 2020" = "#374E55FF", 
    "Dødsfall etter 2020 som ikke er høyere enn forventet" = "#79AF97FF",
    "Dødsfall etter 2020 som er høyere enn forventet" = "#B24745FF"
  ),
  guide = guide_legend(reverse = TRUE)) +
  ggthemes::theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(face = "bold"))
q
csstyle::save_a4(
  q,
  "figure_1.png"
)

tab <- r[year==2024,.(
  deaths_n,
  baseline_p50,
  baseline_ci = paste0(baseline_p025, " to ",baseline_p975),
  absoluteexcess_p50,
  absoluteexcess_ci = paste0(absoluteexcess_p025, " to ", absoluteexcess_p975),
  relavtiveexcess_p50 = round(100*(relativeexcess_p50-1),1),
  relativeexcess_ci = paste0(round(100*(relativeexcess_p025-1),1), " to ", round(100*(relativeexcess_p975-1),1))
)]
tab
