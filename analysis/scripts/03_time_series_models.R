# Author: Kevin See
# Purpose: Fit time-series models
# Created: 4/24/23
# Last Modified: 7/3/23
# Notes:

#-----------------------------------------------------------------
# load packages
library(tidyverse)
library(here)
library(magrittr)
library(janitor)
library(lubridate)
library(ggfortify)
library(forecast)
library(imputeTS)

theme_set(theme_bw())

#-----------------------------------------------------------------
# load data
load(here("analysis/data/derived_data",
          "spp_comp_pred.rda"))

load(here("analysis/data/derived_data",
          "ops_data.rda"))



#-----------------------------------------------------------------
# create time series
half_hr_op = ops_df |>
  filter(time_scale == "Half Hour") |>
  pull(ops) |>
  extract2(1)


half_hr_periods <- half_hr_periods |>
  left_join(half_hr_op |>
              select(date_time, op_perc)) |>
  filter(year %in% unique(sonar_sthd$year),
         month(date) <= max(month(sonar_sthd$date))) |>
  mutate(across(op_perc,
                ~ replace_na(., 0))) |>
  mutate(half_hr_int = 1:n(),
         half_hr_interval = interval(start = date_time,
                                     end = date_time + minutes(29) + seconds(59)))

sthd_cnts <- sonar_sthd |>
  mutate(across(time,
                ~ if_else(is.na(.),
                          hour,
                          .)),
         dt = date + time,
         across(dt,
                ~ ymd_hms(as.character(.), tz = "America/Los_Angeles"))) |>
  filter(!is.na(direction),
         direction %in% c("downstream",
                          "upstream")) |>
  mutate(half_hr_int = NA_real_)

# which interval is each observation in?
for(i in 1:nrow(sthd_cnts)) {
  sthd_cnts$half_hr_int[i] = which(sthd_cnts$dt[i] %within% half_hr_periods$half_hr_interval)
}
rm(i)


ts_half_hr <- half_hr_periods |>
  crossing(direction = c("up", "down")) |>
  left_join(sthd_cnts %>%
              group_by(half_hr_int) %>%
              count(direction,
                    name = "n_fish") %>%
              ungroup() %>%
              filter(direction %in% c("upstream",
                                      "downstream")) %>%
              mutate(across(direction,
                            ~ recode(.,
                                     "upstream" = "up",
                                     "downstream" = "down")))) |>
  mutate(across(n_fish,
                ~ if_else(reviewed & is.na(.),
                          0, .)))

ts_half_hr |>
  group_by(year) |>
  summarize(across(n_fish,
                   ~ sum(., na.rm = T)))

#-----------------------------------------------------------------
# fit some time series models

# model data
mod_df <- ts_half_hr |>
  # group_by(year) |>
  # mutate(min_dt = min(date_time[operational]),
  #        max_dt = max(date_time[operational])) |>
  # filter(between(date_time,
  #                min_dt,
  #                max_dt)) |>
  # ungroup() |>
  select(year,
         direction,
         date_time,
         op_perc,
         n_fish)

# fit some ARIMA models, and make a few different flavors of predictions
ts_df <- mod_df |>
  nest(ts_data = -c(year, direction)) |>
  mutate(ts = map(ts_data,
                  .f = function(x) {
                    ts(x$n_fish)
                  })) |>
  # fit some ARIMA models
  mutate(auto_arima = map(ts,
                          .f = auto.arima,
                          seasonal = F,
                          allowdrift = F,
                          stepwise = F,
                          ic = "aicc"),
         order = map_chr(auto_arima,
                         .f = function(x) {
                           arimaorder(x) %>%
                             as.vector() %>%
                             paste(collapse = " ")
                         }),
         sigma2 = map_dbl(auto_arima,
                          "sigma2"),
         se = sqrt(sigma2)) |>
  # make predictions based on best ARIMA model
  mutate(kalman_preds = map(ts,
                            .f = function(x) {
                              na_kalman(x,
                                        model = "auto.arima",
                                        smooth = T) %>%
                                as_tibble() %>%
                                rename(kalman_pred = x)
                            }),
         # predict based on linear interpolation
         lin_preds = map(ts,
                         .f = function(x) {
                           na_interpolation(x,
                                            option = "linear") %>%
                             as_tibble() %>%
                             rename(lin_pred = x)
                         }),
         # predict based on moving average
         ma_preds = map(ts,
                        .f = function(x) {
                          na_ma(x,
                                k = 4,
                          ) %>%
                            as_tibble() %>%
                            rename(ma_pred = x)
                        }))

#-----------------------------------------------------------------
# save some objects
save(ts_half_hr,
     mod_df,
     ts_df,
     file = here("analysis/data/derived_data",
                 "time_series.rda"))
#-----------------------------------------------------------------


#-----------------------------------------------------------------

ts_df |>
  select(year, direction, ts_data,
         ends_with("preds")) |>
  unnest(c(ts_data, ends_with("preds"))) #|>
  # mutate(across(ends_with("pred"),
  #               ~ round(., 1))) |>
  # filter(date_time >= ymd(20190321)) |>
  # filter(floor_date(date_time, unit = "days") == ymd(20190321)) |>
  # as.data.frame()


ts_df |>
  select(year, direction, ts_data,
         ends_with("preds")) |>
  unnest(c(ts_data, ends_with("preds"))) |>
  ggplot(aes(x = date_time,
             y = kalman_pred)) +
  geom_line() +
  geom_point(aes(y = n_fish),
             color = "red") +
  facet_wrap(~ direction + year,
             scales = "free")

i = 1
ggplot_na_imputations(ts_df$ts[[i]],
                      ts_df$kalman_preds[[i]])


ts_df |>
  select(year, direction, ts_data,
         ends_with("preds")) |>
  unnest(c(ts_data, ends_with("preds"))) |>
  group_by(year, direction) |>
  summarize(
    across(
      c(n_fish,
        ends_with("pred")),
      ~ sum(., na.rm = T)
    ),
    .groups = "drop"
  ) |>
  left_join(full_est_ts |>
              filter(direction != "net") |>
              group_by(year, direction) |>
              summarize(across(total,
                               sum),
                        .groups = "drop"))
