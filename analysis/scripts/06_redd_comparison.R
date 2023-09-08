# Author: Kevin See
# Purpose: Compare various estimates to redd-count based estimates
# Created: 5/5/23
# Last Modified: 5/5/23
# Notes:

#-----------------------------------------------------------------
# load packages
library(tidyverse)
library(here)
library(magrittr)
library(janitor)
library(lubridate)
library(readxl)
library(mgcv)

theme_set(theme_bw())

#-----------------------------------------------------------------
# load data
# time-series models
load(here("analysis/data/derived_data",
          "time_series.rda"))

# GAM models
time_step = c("30 min",
              "1 hour")[1]
load(here("analysis/data/derived_data",
          paste0("gam_",
                 str_replace(time_step, " ", "_"),
                 ".rda")))

day_draws <- read_rds(here("analysis/data/derived_data",
                           paste0("mcmc_draws_day_",
                                  str_replace(time_step, " ", "_"),
                                  ".rds"))) |>
  pivot_wider(names_from = direction,
              values_from = value) |>
  filter(draw != 359) |>
  mutate(net = up - down) |>
  pivot_longer(cols = c(down,
                        up,
                        net),
               names_to = "direction",
               values_to = "value") |>
  mutate(across(direction,
                ~ factor(.,
                         levels = c("up", "down", "net")))) |>
  mutate(year = year(date)) |>
  relocate(year, .before = 0)

down_date <- "May 15"

yr_draws_net <- day_draws |>
  filter(direction == "net",
         date < ymd(paste(year, down_date))) |>
  group_by(year,
           draw,
           direction) |>
  summarize(across(value,
                   sum),
            .groups = "drop")

yr_draws_up <- day_draws |>
  filter(direction == "up",
         date >= ymd(paste(year, down_date))) |>
  group_by(year,
           draw,
           direction) |>
  summarize(across(value,
                   sum),
            .groups = "drop")

yr_draws <- yr_draws_net |>
  bind_rows(yr_draws_up) |>
  pivot_wider(names_from = direction,
              values_from = value) |>
  mutate(total = net + up)

# summary statistics
yr_est <- yr_draws |>
  pivot_longer(c(net, up, total),
               names_to = "group",
               values_to = "value") |>
  group_by(year,
           group) |>
  summarize(
    across(
      value,
      list(mean = ~ mean(.),
           median = ~ median(.),
           se = ~ sd(.)),
      .names = "{.fn}"),
    .groups = "drop") |>
  left_join(yr_draws |>
              pivot_longer(c(net, up, total),
                           names_to = "group",
                           values_to = "value") |>
              group_by(year,
                       group) |>
              reframe(across(value,
                             ~ quantile(., c(0.025, 0.975)))) %>%
              add_column(quantile = rep(c("2.5%", "97.5%"), nrow(.) / 2)) |>
              pivot_wider(names_from = quantile,
                          values_from = value)) |>
  mutate(across(group,
                ~ recode(.,
                         "net" = "early",
                         "up" = "late")),
         across(group,
                ~ factor(.,
                         levels = c("early",
                                    "late",
                                    "total")))) |>
  arrange(year, group)

#-----------------------------------------------------------------
# read in redd-based estimates
redd_est <- read_excel(here("analysis/supplementary-materials/redd_data",
                            "Historic Dungeness Steelhead Escapement 1988-2022.xlsx"),
                       1,
                       skip = 2) |>
  select(c(1,3)) |>
  rlang::set_names(c("year",
                     "redd_est")) |>
  # filter(year >= 2019) |>
  mutate(across(year,
                ~ as.factor(as.character(.))))

#-----------------------------------------------------------------
# put together all the various estimates, including redd counts
comp_est <-
  yr_est |>
  filter(group == "total") |>
  select(-group) |>
  left_join(ts_df |>
              select(year, direction, ts_data,
                     ends_with("preds")) |>
              unnest(c(ts_data, ends_with("preds"))) |>
              pivot_longer(cols = c(n_fish,
                                    ends_with("pred")),
                           names_to = "type",
                           values_to = "value") |>
              pivot_wider(names_from = direction,
                          values_from = value) |>
              mutate(net = up - down) |>
              mutate(total = if_else(date_time < ymd(paste(year, down_date)),
                                     net,
                                     up)) |>
              filter(month(date_time) >= 2,
                     (month(date_time) < 6 |
                        (month(date_time) == 6 & day(date_time) <= 15))) |>
              group_by(year, type) |>
              summarize(
                across(
                  total,
                  ~ sum(., na.rm = T)
                ),
                .groups = "drop") |>
              pivot_wider(names_from = type,
                          values_from = total) |>
              relocate(n_fish,
                       .after = year)) |>
  mutate(across(year,
                as_factor)) |>
  left_join(redd_est)


#-----------------------------------------------------------------
save(redd_est,
     yr_est,
     yr_draws,
     comp_est,
     file = here("analysis/data/derived_data",
                 "redd_comp.rda"))

#-----------------------------------------------------------------
comp_est |>
  filter(!is.na(redd_est)) |>
  select(-c(se:`97.5%`)) |>
  select(-ends_with("pred")) |>
  relocate(n_fish,
           .after = "year") |>
  mutate(across(-year,
                ~ (. / redd_est)))


pd = 0.2
comp_est |>
  ggplot(aes(x = year)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`,
                    color = "GAM"),
                width = 0.1) +
  geom_point(aes(y = median,
                 color = "GAM"),
             size = 4) +
  geom_point(aes(y = kalman_pred,
                 color = "Kalman"),
             size = 4,
             position = position_jitter(width = pd)) +
  geom_point(aes(y = lin_pred,
                 color = "Linear\nInterpolation"),
             size = 4,
             position = position_jitter(width = pd)) +
  geom_point(aes(y = ma_pred,
                 color = "Moving\nAverage"),
             size = 4,
             position = position_jitter(width = pd)) +
  geom_point(aes(y = redd_est,
                 color = "Redds"),
             size = 4,
             position = position_jitter(width = pd)) +
  scale_color_brewer(palette = "Set1",
                     name = "Estimate") +
  labs(x = "Year",
       y = "Net Upstream Escapement")



pd = 0.2
comp_est |>
  ggplot(aes(x = year)) +
  geom_errorbar(aes(ymin = `2.5%`,
                    ymax = `97.5%`,
                    color = "GAM"),
                width = 0.1) +
  geom_point(aes(y = median,
                 color = "GAM"),
             size = 4) +
  geom_point(aes(y = n_fish,
                 color = "n obs\nSthd."),
             size = 4,
             position = position_jitter(width = pd)) +
  geom_point(aes(y = redd_est,
                 color = "Redds"),
             size = 4,
             position = position_jitter(width = pd)) +
  scale_color_brewer(palette = "Set1",
                     name = "Estimate") +
  labs(x = "Year",
       y = "Net Upstream Escapement")

#-----------------------------------------------------------------
# make a plot
yr_draws |>
  mutate(across(year,
                as_factor)) |>
  ggplot(aes(x = year,
             y = total,
             fill = year)) +
  geom_boxplot() +
  # geom_violin(draw_quantiles = c(0.5)) +
  geom_point(data = redd_est,
             aes(y = redd_est),
             color = "blue",
             size = 6) +
  # scale_y_continuous(limits = c(NA, 1500)) +
  labs(x = "Year",
       y = "Net Upstream Escapement") +
  theme(legend.position = "none",
        text = element_text(size = 21)) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))


ggsave(here("analysis/figures",
            "gam_redd_year_est.png"),
       width = 8,
       height = 7)

