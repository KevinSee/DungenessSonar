# Author: Kevin See
# Purpose: Fit time-series models
# Created: 4/24/23
# Last Modified: 2/14/25
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
sthd_cnts <-
  sonar_sthd |>
  mutate(across(time,
                ~ if_else(is.na(.),
                          hour,
                          .)),
         dt = date + time) |>
  filter(!is.na(direction),
         direction %in% c("downstream",
                          "upstream"))
# tz(sthd_cnts$dt) = "America/Los_Angeles"

#-----------------------------------------------------------------
# half hour time-step
half_hr_op <-
  ops_df |>
  filter(time_scale == "Half Hour") |>
  pull(ops) |>
  extract2(1)


half_hr_periods <-
  half_hr_periods |>
  left_join(half_hr_op |>
              select(date_time, op_perc)) |>
  filter(year %in% unique(sonar_sthd$year),
         month(date) <= max(month(sonar_sthd$date))) |>
  mutate(across(op_perc,
                ~ replace_na(., 0))) |>
  mutate(time_int = 1:n(),
         time_interval = interval(start = date_time,
                                     end = date_time + minutes(29) + seconds(59)))

# which interval is each observation in?
sthd_cnts_half <-
  sthd_cnts |>
  left_join(half_hr_periods |>
              select(time_int,
                     min_dt = date_time,
                     max_dt),
            by = join_by(between(dt,
                                 min_dt,
                                 max_dt)))


ts_half_hr <-
  half_hr_periods |>
  crossing(direction = c("up", "down")) |>
  left_join(sthd_cnts_half %>%
              count(time_int,
                    direction,
                    name = "n_fish") %>%
              filter(direction %in% c("upstream",
                                      "downstream")) %>%
              mutate(across(direction,
                            ~ recode(.,
                                     "upstream" = "up",
                                     "downstream" = "down")))) |>
  mutate(across(n_fish,
                ~ case_when(reviewed &
                              is.na(.) ~ 0,
                            !reviewed ~ NA_real_,
                            .default = .)))

ts_half_hr |>
  group_by(year,
           direction) |>
  summarize(across(n_fish,
                   ~ sum(., na.rm = T)),
            .groups = "drop") |>
  pivot_wider(names_from = direction,
              values_from = n_fish) |>
  mutate(net = up - down)

ts_half_hr |>
  filter(!is.na(n_fish),
         !reviewed) |>
  as.data.frame()



#-----------------------------------------------------------------
# create time series
# 15 minute time-step
quarter_hr_op = ops_df |>
  filter(time_scale == "15 min") |>
  pull(ops) |>
  extract2(1)


quarter_hr_periods <-
  quarter_hr_periods |>
  left_join(quarter_hr_op |>
              select(date_time, op_perc)) |>
  filter(year %in% unique(sonar_sthd$year),
         month(date) <= max(month(sonar_sthd$date))) |>
  mutate(across(op_perc,
                ~ replace_na(., 0))) |>
  mutate(time_int = 1:n(),
         time_interval = interval(start = date_time,
                                  end = max_dt))

# which interval is each observation in?
sthd_cnts_quarter <-
  sthd_cnts |>
  left_join(quarter_hr_periods |>
              select(time_int,
                     min_dt = date_time,
                     max_dt),
            by = join_by(between(dt,
                                 min_dt,
                                 max_dt)))

ts_quarter_hr <-
  quarter_hr_periods |>
  crossing(direction = c("up", "down")) |>
  left_join(sthd_cnts_quarter %>%
              count(time_int,
                    direction,
                    name = "n_fish") %>%
              filter(direction %in% c("upstream",
                                      "downstream")) %>%
              mutate(across(direction,
                            ~ case_match(.,
                                         "upstream" ~ "up",
                                         "downstream" ~ "down",
                                         .default = .)))) |>
  mutate(across(n_fish,
                ~ case_when(reviewed &
                              is.na(.) ~ 0,
                            !reviewed ~ NA_real_,
                            .default = .)))

ts_quarter_hr |>
  group_by(year,
           direction) |>
  summarize(across(n_fish,
                   ~ sum(., na.rm = T)),
            .groups = "drop") |>
  pivot_wider(names_from = direction,
              values_from = n_fish) |>
  mutate(net = up - down)

ts_quarter_hr |>
  filter(!is.na(n_fish),
         !reviewed) |>
  as.data.frame()




#-----------------------------------------------------------------
# fit some time series models

# model data
# 30 min time steps
mod_half_df <-
  ts_half_hr |>
  select(year,
         direction,
         date_time,
         op_perc,
         n_fish)

# 15 min time steps
mod_quarter_df <-
  ts_quarter_hr |>
  select(year,
         direction,
         date_time,
         op_perc,
         n_fish)




# fit some ARIMA models, and make a few different flavors of predictions
ts_mods <-
  mod_half_df |>
  nest(ts_data = -c(year, direction)) |>
  add_column(time_step = "30 min",
             .before = 0) |>
  bind_rows(mod_quarter_df |>
              nest(ts_data = -c(year, direction)) |>
              add_column(time_step = "15 min",
                         .before = 0)) |>
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
                          ic = "aicc",
                          .progress = T),
         order = map_chr(auto_arima,
                         .f = function(x) {
                           arimaorder(x) %>%
                             as.vector() %>%
                             paste(collapse = " ")
                         }),
         sigma2 = map_dbl(auto_arima,
                          "sigma2"),
         se = sqrt(sigma2))

ts_preds <-
  ts_mods |>
  # make predictions based on best ARIMA model
  mutate(kalman_preds = map(ts,
                            .f = function(x) {
                              message("Making Kalman filter predictions.\n")
                              na_kalman(x,
                                        model = "auto.arima",
                                        smooth = T) %>%
                                as_tibble() %>%
                                rename(kalman_pred = x)
                            },
                            .progress = T),
         # predict based on linear interpolation
         lin_preds = map(ts,
                         .f = function(x) {
                           message("Making linear interpolations predictions.\n")
                           na_interpolation(x,
                                            option = "linear") %>%
                             as_tibble() %>%
                             rename(lin_pred = x)
                         },
                         .progress = T),
         # predict based on moving average
         ma_preds = map(ts,
                        .f = function(x) {
                          message("Making moving average predictions.\n")
                          na_ma(x,
                                k = 4,
                          ) %>%
                            as_tibble() %>%
                            rename(ma_pred = x)
                        },
                        .progress = T))


#-----------------------------------------------------------------
# save some objects
ts_df <-
  ts_preds |>
  filter(time_step == "30 min")

save(ts_half_hr,
     mod_half_df,
     ts_df,
     file = here("analysis/data/derived_data",
                 "time_series.rda"))

ts_df <-
  ts_preds |>
  filter(time_step == "15 min")

save(ts_quarter_hr,
     mod_quarter_df,
     ts_df,
     file = here("analysis/data/derived_data",
                 "time_series_15_min.rda"))
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

i = 16
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
