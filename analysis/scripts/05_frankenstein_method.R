# Author: Kevin See
# Purpose: Use multiple methods including regression to finalize estimates
# Created: 4/28/23
# Last Modified: 3/18/24
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

load(here("analysis/data/derived_data",
          "time_series.rda"))

#-----------------------------------------------------------------
hr_op = ops_df |>
  filter(time_scale == "Hour") |>
  pull(ops) |>
  extract2(1)

hr_periods <- hr_periods |>
  left_join(hr_op |>
              select(date_time, op_perc)) |>
  mutate(across(op_perc,
                ~ replace_na(., 0))) |>
  mutate(hr_int = 1:n(),
         hr_interval = interval(start = date_time,
                                end = date_time + minutes(59) + seconds(59)))


sthd_cnts <- sonar_sthd |>
  mutate(across(time,
                ~ if_else(is.na(.),
                          hour,
                          .)),
         dt = date + time) |>
  filter(!is.na(direction)) |>
  mutate(hr_int = NA_real_)

# tz(sthd_cnts$dt) = "America/Los_Angeles"

# which interval is each observation in?
sthd_cnts <-
  sthd_cnts |>
  mutate(hr_int = map_int(dt,
                          .f = function(x) {
                            min(which(x %within% hr_periods$hr_interval))
                          },
                          .progress = T))


ts_hr <- hr_periods |>
  crossing(direction = c("up", "down")) |>
  left_join(sthd_cnts %>%
              mutate(half_hr_per = recode(data_reviewed,
                                          "First 30" = "first",
                                          "Second 30" = "second")) |>
              group_by(hr_int,
                       half_hr_per) %>%
              count(direction,
                    name = "n_fish") %>%
              ungroup() %>%
              filter(direction %in% c("upstream",
                                      "downstream")) %>%
              mutate(across(direction,
                            ~ recode(.,
                                     "upstream" = "up",
                                     "downstream" = "down"))) |>
              pivot_wider(names_from = half_hr_per,
                          values_from = n_fish)) |>
  mutate(across(c(first),
                ~ if_else(data_reviewed %in% c("First 30 Only",
                                               "Full Hour")
                          & is.na(.),
                          0, .)),
         across(c(second),
                ~ if_else(data_reviewed %in% c("Second 30 Only",
                                               "Full Hour")
                          & is.na(.),
                          0, .)))


# some hours had only the 2nd half hour reviewed. Switch that to pretend it's the first half hour
ts_hr <- ts_hr |>
  filter(data_reviewed != "Second 30 Only") |>
  bind_rows(ts_hr |>
              filter(data_reviewed == "Second 30 Only") |>
              mutate(first = second,
                     second = NA_real_,
                     data_reviewed = "First 30 Only")) |>
  arrange(date_time,
          direction)


# divide into various groups of data to be analyzed
analysis_grps <-
  tibble(time_scale = as_factor(c('Hour',
                                  paste(24 / max(hrs_fct_grp), 'Hour Block'),
                                  'Day')),
         ts_df = list(ts_hr |>
                        mutate(fct_group = date_time,
                               across(fct_group,
                                      ~ factor(as.character(.),
                                               labels = 1:n_distinct(.)))) |>
                        select(fct_group,
                               everything()),

                      ts_hr |>
                        unite(fct_group, date, hr_fct, remove = F) |>
                        mutate(across(fct_group,
                                      ~ factor(as.character(.),
                                               labels = 1:n_distinct(.)))) |>
                        group_by(year,
                                 date,
                                 direction,
                                 hr_fct,
                                 fct_group) %>%
                        summarize(across(date_time,
                                         min),
                                  n_pers = n(),
                                  n_reviewed = sum(reviewed),
                                  n_full_hrs = sum(full_hr),
                                  across(c(data_recorded,
                                           data_reviewed),
                                         ~ paste(sort(unique(.)), collapse = "_")),
                                  across(c(first,
                                           second),
                                         ~ sum(., na.rm = T)),
                                  .groups = "drop") |>
                        mutate(full_hr = if_else(n_full_hrs > 0,
                                                 T, F),
                               reviewed = if_else(n_reviewed > 0,
                                                  T, F)) |>
                        mutate(hour = hour(date_time),
                               across(hour,
                                      ~ as.period(.,
                                                  unit = "hour"))) |>
                        select(fct_group,
                               any_of(names(ts_hr))),

                      ts_hr |>
                        mutate(fct_group = date,
                               across(fct_group,
                                      ~ factor(as.character(.),
                                               labels = 1:n_distinct(.)))) |>
                        group_by(year,
                                 date,
                                 direction,
                                 fct_group) %>%
                        summarize(across(date_time,
                                         min),
                                  n_pers = n(),
                                  n_reviewed = sum(reviewed),
                                  n_full_hrs = sum(full_hr),
                                  across(c(data_recorded,
                                           data_reviewed),
                                         ~ paste(sort(unique(.)), collapse = "_")),
                                  across(c(first,
                                           second),
                                         ~ sum(., na.rm = T)),
                                  .groups = "drop") |>
                        mutate(full_hr = if_else(n_full_hrs > 0,
                                                 T, F),
                               reviewed = if_else(n_reviewed > 0,
                                                  T, F)) |>
                        mutate(hour = hour(date_time),
                               across(hour,
                                      ~ as.period(.,
                                                  unit = "hour")),
                               hr_fct = 1) |>
                        select(fct_group,
                               any_of(names(ts_hr)))
         )) |>
  unnest(ts_df) |>
  mutate(across(second,
                ~ if_else(!full_hr,
                          NA_real_,
                          .)),
         across(first,
                ~ if_else(!reviewed,
                          NA_real_,
                          .))) |>
  select(-operational,
         -op_perc) |>
  left_join(ops_df |>
              unnest(ops) |>
              select(-ends_with("pers"))) |>
  nest(data = -c(time_scale,
                 direction,
                 reviewed,
                 full_hr))

#-----------------------------------------------------------------
# period comparison for all hours with first and second half hours fully recorded
comp_30_df <- analysis_grps %>%
  filter(full_hr,
         reviewed) %>%
  unnest(data) %>%
  mutate(total = first + second) |>
  arrange(time_scale,
          date_time,
          direction) |>
  add_column(est_type = "Full Hour Census",
             .before = "first")


comp_30_df |>
  filter(total > 0) |>
  filter(first > 0,
         second > 0) |>
  mutate(across(direction,
                str_to_title),
         across(direction,
                fct_rev)) |>
  ggplot(aes(x = first,
             y = second)) +
  geom_abline(linetype = 2,
              color = "red") +
  geom_point(#aes(color = as.factor(year)),
             position = position_jitter(width = 0.1,
                                        height = 0.1)) +
  geom_smooth(method = lm,
              formula = y ~ x - 1) +
  # geom_smooth(method = glm,
  #             formula = y ~ x - 1,
  #             method.args = list(family = "poisson")) +
  facet_wrap(~ direction + time_scale,
             nrow = 2,
             scales = "free") +
  scale_x_continuous(trans = "log",
                     breaks = scales::breaks_extended()) +
  scale_y_continuous(trans = "log",
                     breaks = scales::breaks_extended()) +
  labs(x = "Counted in First 30 Min.",
       y = "Counted in Second 30 Min.") +
  theme(text = element_text(size = 18),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))

ggsave(here("analysis/figures",
            "half_hr_comp.png"),
       width = 8,
       height = 6,
       bg = "transparent")


# group data by different time-scales and directions
# filter out hours with no fish at all
per_fit_df <- comp_30_df |>
  filter(total > 0) |>
  mutate(first > 0,
         second > 0) |>
  nest(model_df = -c(time_scale,
                     direction)) |>
  arrange(time_scale,
          direction)

# fit model and pull out coefficients, sigma, confidence intervals, etc.
per_fit_df %<>%
  mutate(mod = map(model_df,
                   .f = function(x) {
                     # lm(second ~ first - 1,
                     #    data = x)
                     lm(log(second) ~ log(first) - 1,
                        data = x |>
                          filter(first > 0,
                                 second > 0))
                     # glm(second ~ first - 1,
                     #     data = x,
                     #     family = "poisson")
                   }),
         coefs = map(mod,
                     .f = broom::tidy),
         ci = map(mod,
                  .f = function(x) {
                    ci = confint(x)
                    if(class(ci)[1] == "matrix") {
                      ci %<>%
                        as_tibble()
                    } else {
                      ci %<>%
                        enframe() %>%
                        pivot_wider()
                    }
                    return(ci)
                  })) %>%
  mutate(slope = map_dbl(coefs,
                         "estimate"),
         se = map_dbl(coefs,
                      "std.error"),
         sigma = map_dbl(mod,
                         .f = function(x) {
                           summary(x)$sigma
                         }),
         R2 = map_dbl(mod,
                      .f = function(x) {
                        summary(x)$r.squared
                      }),
         adj_R2 = map_dbl(mod,
                          .f = function(x) {
                            summary(x)$adj.r.squared
                          })) %>%
  unnest(ci) %>%
  relocate(ends_with("%"),
           .after = "se")

per_fit_df

# diagnostic plots
autoplot(per_fit_df$mod[[6]],
         1:6)


# per_fit_final <- per_fit_df

#-----------------------------------
# settled on modeling up and down separately, on daily scale
per_fit_final <- per_fit_df |>
  filter(time_scale == "Day",
         direction %in% c("up",
                          "down"))

#-----------------------------------
# what type of uncertainty interval should we use?
pred_type = c("confidence",
              "prediction")[1]

# make predictions for 2nd half hours
half_hr_pred <- analysis_grps %>%
  filter(!full_hr,
         reviewed) %>%
  inner_join(per_fit_final %>%
               select(time_scale,
                      direction,
                      mod),
             by = join_by(time_scale,
                          direction)) %>%
  mutate(preds = map2(mod,
                      data,
                      .f = function(x, y) {
                        predict(x,
                                newdata = y,
                                se.fit = T)
                      }),
         pred_sec = map(preds,
                     "fit"),
         se = map(preds,
                  "se.fit"),
         resid_scale = map(preds,
                           "residual.scale")) %>%
  mutate(pred_ci = map2(mod,
                        data,
                        .f = function(x, y) {
                          predict(x,
                                  newdata = y,
                                  interval = pred_type) %>%
                            as_tibble() %>%
                            select(lci = lwr,
                                   uci = upr)
                        })) %>%
  select(-mod, -preds) %>%
  unnest(cols = c(data, pred_sec,
                  se, resid_scale,
                  pred_ci)) |>
  # adjust predictions for log-log scale
  mutate(across(pred_sec,
                ~ if_else(se != Inf,
                          exp(. + (se^2 / 2)),
                          0))) |>
  mutate(across(se,
                ~ if_else(. == Inf, 0, .))) |>
  # # for prediction intervals (instead of confidence, adjust the standard error)
  rowwise() |>
  mutate(se = if_else(pred_type == "prediction",
                      sqrt(se^2 + resid_scale^2),
                      se)) |>
                      # if_else(se == 0,
                      #         resid_scale,
                      #         se))) |>
  ungroup() |>
  # force predictions of 2nd half hour match observed 1st half hour
  mutate(pred_sec = first,
         lci = qnorm(0.025, log(pred_sec), se),
         uci = qnorm(0.975, log(pred_sec), se)) |>
  # adjust upper CI if needed
  # mutate(across(uci,
  #               ~ if_else((is.na(.) | . %in% c(-Inf, Inf)) & first == 0,
  #                         qnorm(0.975, first, resid_scale),
  #                         .))) |>
  # transform confidence intervals back to standard scale
  mutate(across(c(lci, uci),
                exp)) |>
  # select(-resid_scale) |>
  select(-fct_group) %>%
  add_column(est_type = "Half Hour Exp.",
             .before = "first")


tmp <- half_hr_pred |>
  select(direction,
         date_time, data_reviewed,
         first, second,
         pred_sec, se,
         resid_scale,
         lci, uci)
tmp |>
  filter(first == 4)

tmp |>
  select(direction, first, pred_sec, se, lci, uci) |>
  distinct() |>
  arrange(first,
          direction)

comp_30_df |>
  nest(data = -c(time_scale,
                 direction))
half_hr_pred |>
  nest(data = -c(#time_scale,
                 direction))

analysis_grps |>
  filter(time_scale == "Day")

reviewed_pers <- comp_30_df |>
  select(-total) |>
  filter(time_scale %in% unique(half_hr_pred$time_scale)) |>
  relocate(time, op_perc,
           .after = date_time) |>
  add_column(se = 0,
             resid_scale = 0,
             lci = 0,
             uci = 0) |>
  bind_rows(half_hr_pred |>
              select(-second) |>
              rename(second = pred_sec))

ts_reviewed <- analysis_grps |>
  filter(time_scale %in% unique(reviewed_pers$time_scale),
         reviewed) |>
  unnest(data) |>
  select(time_scale:hr_interval,
         time, op_perc,
         first) |>
  left_join(reviewed_pers |>
              select(time_scale,
                     direction,
                     date_time,
                     est_type:uci)) |>
  arrange(time_scale,
          date_time,
          direction) |>
  filter(year > 2018) |>
  mutate(total = first + second,
         across(c(lci, uci),
                ~ first + .))

ts_reviewed |>
  ggplot(aes(x = date_time,
             y = total)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  geom_point(aes(color = est_type)) +
  facet_wrap(~ direction + year,
             scales = "free")


#-----------------------------------
# expand periods when not operating 100%
# set a threshold below which we won't expand by the operating percent
op_thres <- 0.4

ts_reviewed |>
  filter(op_perc < 1) |>
  mutate(above_thres = if_else(op_perc > op_thres,
                               T, F),
         saw_fish = if_else(total > 0,
                            T, F)) |>
  ggplot(aes(x = op_perc)) +
  geom_histogram(aes(fill = op_thres)) +
  scale_x_continuous(limits = c(0, 1))

ts_expanded <- ts_reviewed |>
  mutate(across(reviewed,
                ~ if_else(op_perc < op_thres, F, .)),
         across(c(total,
                  lci,
                  uci),
                ~ if_else(op_perc >= op_thres,
                          . / op_perc,
                          NA_real_)),
         across(est_type,
                ~ if_else(reviewed & op_perc >= op_thres & op_perc < 1,
                          "Missing Time Exp.",
                          if_else(op_perc < op_thres | !reviewed,
                                  "Missing Data",
                                  .))),
         across(c(se, lci, uci),
                ~ if_else(est_type == 'Missing Data',
                          NA_real_,
                          .)))

#-----------------------------------
# create full time series, including completely missing data

ts_est <- analysis_grps |>
  filter(time_scale %in% unique(reviewed_pers$time_scale),
         !reviewed) |>
  unnest(data) |>
  filter(year > 2018) |>
  mutate(est_type = "Missing Data") |>
  bind_rows(ts_expanded) |>
  select(any_of(names(ts_expanded))) |>
  arrange(time_scale,
          date_time,
          direction) |>
  # select(time_scale,
  #        direction,
  #        year,
  #        date_time,
  #        est_type,
  #        total, se,
  #        lci, uci) |>
  nest(data = -c(time_scale,
                 direction,
                 year)) %>%
  mutate(n_periods = map_dbl(data,
                             .f = function(x) {
                               nrow(x)
                             }),
         n_NA = map_dbl(data,
                        .f = function(x) {
                          sum(is.na(x$total))
                        }),
         perc_NA = n_NA / n_periods,
         # ts_zoo = map(data,
         #              .f = function(x) {
         #                zoo(x$total,
         #                    x$date_time)
         #              }),
         ts = map(data,
                  .f = function(x) {
                    ts(x$total)
                  })) %>%
  # ts = map(ts_zoo,
  #          as.ts)) %>%
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
         se = sqrt(sigma2))

# make some predictions based on various interpolation methods
ts_est %<>%
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

missing_preds <- ts_est %>%
  select(time_scale,
         direction,
         year,
         data,
         pred_se = se,
         ends_with("preds")) %>%
  unnest(cols = c(data, ends_with("preds"))) %>%
  filter(est_type == "Missing Data") |>
  select(-c(se, resid_scale,
            lci, uci, total)) |>
  pivot_longer(ends_with("pred"),
               names_to = "model",
               values_to = "est") %>%
  mutate(across(model,
                ~ str_remove(., "_pred$"))) %>%
  mutate(lci_ts = qnorm(0.025, est, pred_se),
         uci_ts = qnorm(0.975, est, pred_se)) |>
  mutate(across(c(est,
                  lci_ts),
                ~ if_else(. < 0, 0, .)))



ts_expanded |>
  select(-c(fct_group,
            hr_fct,
            hr_int,
            hr_interval,
            time)) |>
  filter(est_type != "Missing Data") |>
  bind_rows(missing_preds |>
              rename(total = est,
                     se = pred_se,
                     lci = lci_ts,
                     uci = uci_ts) |>
              filter(model == "kalman")) |>
  select(time_scale,
         direction,
         date_time,
         est_type,
         first, second:total,
         model) |>
  arrange(time_scale, date_time, direction,
          est_type)

ts_full <- analysis_grps |>
  filter(time_scale %in% unique(reviewed_pers$time_scale)) |>
  unnest(data) |>
  select(time_scale:full_hr,
         direction,
         year, date, date_time,
         data_recorded,
         data_reviewed,
         op_perc,
         first) |>
  left_join(ts_expanded |>
              select(-c(fct_group,
                        hr_fct,
                        hr_int,
                        hr_interval,
                        time)) |>
              filter(est_type != "Missing Data") |>
              bind_rows(missing_preds |>
                          filter(model == "kalman") |>
                          rename(total = est,
                                 se = pred_se,
                                 lci = lci_ts,
                                 uci = uci_ts)) |>
              select(time_scale,
                     direction,
                     date_time,
                     est_type,
                     first, second:total,
                     model)) |>
  relocate(est_type,
           .before = first) |>
  arrange(time_scale,
          date_time,
          direction)


save(ts_hr,
     analysis_grps,
     per_fit_df,
     half_hr_pred,
     ts_reviewed,
     op_thres,
     ts_expanded,
     ts_est,
     ts_full,
     file = here("analysis/data/derived_data",
                 "frankenstein_estimates.rda"))

#--------------------------------------------------------
