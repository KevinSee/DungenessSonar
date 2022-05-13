# Author: Kevin See
# Purpose: Deal with missing data
# Created: 4/27/22
# Last Modified: 4/27/22
# Notes:

#-----------------------------------------------------------------
# load packages
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(forecast)
library(zoo)
library(imputeTS)
# library(Amelia)
library(ggpubr)
# library(bayesforecast)
library(imputeTestbench)

theme_set(theme_bw())


#-----------------------------------------------------------------
# load estimated counts from several time scales
load(here("analysis/data/derived_data",
          "est_fish_cnts.rda"))

ts_df <- fish_cnts %>%
  mutate(date_time = if_else(time_scale == "Hour",
                             date + hour,
                             if_else(time_scale == "Day",
                                     date,
                                     date + hours((hr_fct - 1) * (24 / max(hrs_fct_grp)))))) %>%
  select(time_scale,
         year,
         date_time,
         est_type,
         total, se) %>%
  group_by(time_scale,
           year) %>%
  nest() %>%
  ungroup() %>%
  mutate(n_periods = map_dbl(data,
                             .f = function(x) {
                               nrow(x)
                             }),
         n_NA = map_dbl(data,
                        .f = function(x) {
                          sum(is.na(x))
                        }),
         perc_NA = n_NA / n_periods,
         ts_zoo = map(data,
                      .f = function(x) {
                        zoo(x$total,
                            x$date_time)
                      }),
         ts = map(ts_zoo,
                  as.ts)) %>%
  # fit some ARIMA models
  mutate(auto_arima = map(ts_zoo,
                          .f = auto.arima,
                          seasonal = F,
                          allowdrift = F,
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

ts_df %<>%
  # make predictions based on best ARIMA model
  mutate(kalman_preds = map(ts_zoo,
                           .f = function(x) {
                             na_kalman(x,
                                       model = "auto.arima",
                                       smooth = T) %>%
                               as_tibble() %>%
                               rename(kalman_pred = value)
                             }),
         lin_preds = map(ts_zoo,
                        .f = function(x) {
                          na_interpolation(x) %>%
                            as_tibble() %>%
                            rename(lin_pred = value)
                          }),
         ma_preds = map(ts_zoo,
                         .f = function(x) {
                           na_ma(x) %>%
                             as_tibble() %>%
                             rename(ma_pred = value)
                         }))


ts_df %>%
  select(time_scale,
         year,
         data,
         pred_se = se,
         ends_with("preds")) %>%
  unnest(cols = c(data, ends_with("preds"))) %>%
  mutate(across(contains("pred"),
                ~ if_else(est_type != "Missing Data",
                          NA_real_,
                          .))) %>%
  filter(is.na(total))


ts_df

p_list = vector("list",
                length = nrow(ts_df))
for(i in seq_along(ts_df$time_scale)) {
  p_list[[i]] <- ggplot_na_distribution(ts_df$ts_zoo[[i]]) +
    labs(title = paste(ts_df$time_scale[i], "in", ts_df$year[i]),
         subtitle = element_blank()) +
    theme(axis.title.y = element_blank())
}
ggarrange(plotlist = p_list,
          ncol = 3,
          nrow = 3,
          common.legend = T,
          legend = "bottom")


i = 3

test_ts <- ts_df$ts_zoo[[i]]
# test_ts <- ts_df$ts[[i]]

ggplot_na_distribution(test_ts)
ggplot_na_intervals(test_ts)
ggplot_na_gapsize(test_ts)

statsNA(test_ts)


m1 <- auto.arima(test_ts,
                 seasonal = F,
                 allowdrift = F,
                 ic = "aicc")
summary(m1)
sqrt(m1$sigma2)
plot(m1)

class(m1)

kr <- KalmanRun(test_ts,
                m1$model)
ks <- KalmanSmooth(test_ts,
                   m1$model)
head(ks$smooth)
head(ks$var)
ks$smooth[which(is.na(test_ts)),]

miss_id <- which(is.na(test_ts))

kal_est <- sapply(miss_id,
                  FUN = function(x, Z, alpha) Z %*% alpha[x,],
                  Z = m1$model$Z,
                  alpha = kr$states)

kalman_fit3 <- test_ts
kalman_fit3[miss_id] = kal_est


m2 <- arima(test_ts,
            order = arimaorder(m1))
summary(m2)
m2$sigma2
?forecast

predict(m2, 10)


# impute missing values several ways
na_kalman_arima = function(x) {
  na_kalman(x, model = "auto.arima")
}
imp_df <- impute_errors(test_ts,
                        methods = c('na_interpolation',
                                    'na_ma',
                                    'na_kalman',
                                    'na_kalman_arima'),
                        errorParameter = c("rmse"),
                        # blck = 8,
                        # blckper = F,
                        missPercentFrom = 2,
                        missPercentTo = 28,
                        interval = 2)
as_tibble(imp_df)
plot_errors(imp_df)

#------------------------------
inter_fit <- test_ts %>%
  na_interpolation()

spline_fit <- test_ts %>%
  na_interpolation(option = "spline")

stine_fit <- test_ts %>%
  na_interpolation(option = "stine")

ma_fit = test_ts %>%
  na_ma()

kalman_fit1 <- test_ts %>%
  na_kalman(model = "StructTS",
            smooth = T)

kalman_fit2 <- test_ts %>%
  na_kalman(model = "auto.arima",
            smooth = T)

zoo_fit <- test_ts %>%
  na.approx()

fore_fit <- test_ts %>%
  na.interp()

# predictions for missing values
pred_miss <- tibble(missing_loc = which(is.na(test_ts)),
                    interpolation = inter_fit[missing_loc],
                    spline = spline_fit[missing_loc],
                    stine = stine_fit[missing_loc],
                    MA = ma_fit[missing_loc],
                    kalman_1 = kalman_fit1[missing_loc],
                    kalman_2 = kalman_fit2[missing_loc],
                    kalman_3 = kalman_fit3[missing_loc],#,
                    # zoo = zoo_fit[missing_loc],
                    # forecast = fore_fit[missing_loc]
) %>%
  mutate(across(everything(),
                as.numeric)) %>%
  mutate(se = sqrt(m1$sigma2)) %>%
  mutate(date_time = ts_df$data[[i]] %>%
           filter(is.na(total)) %>%
           pull(date_time)) %>%
  relocate(date_time, .before = 1)

pred_miss

pred_miss %>%
  select(-missing_loc) %>%
  corrr::correlate()

pred_miss %>%
  mutate(across(-c(1, se),
                round_half_up)) %>%
  select(-missing_loc, - se) %>%
  corrr::correlate()

pred_miss %>%
  mutate(across(-c(1, se),
                ~ if_else(. < 0,
                          0, .))) %>%
  select(-missing_loc, -se) %>%
  corrr::correlate()


GGally::ggpairs(pred_miss[,-c(1, ncol(pred_miss))],
                lower = list(continuous = function(data,
                                                   mapping,
                                                   ...) {
                  p <- ggplot(data = data,
                              mapping = mapping) +
                    geom_abline(linetype = 2) +
                    geom_point(...)
                  p
                }))

plot_df <- test_ts %>%
  as_tibble() %>%
  rename(obs = value) %>%
  mutate(time = 1:n()) %>%
  mutate(is_na = is.na(obs)) %>%
  relocate(time, is_na, .before = 1) %>%
  mutate(lin_int = inter_fit,
         # spline = spline_fit,
         stine = stine_fit,
         ma = ma_fit,
         kalman_struct = kalman_fit1,
         kalman_arima = kalman_fit2,
         kalman_run = kalman_fit3) %>%
  mutate(across(-c(1,2),
                as.numeric)) %>%
  pivot_longer(cols = -c(1:3),
               names_to = "type",
               values_to = "value")

plot_df %>%
  ggplot(aes(x = time,
             y = obs)) +
  geom_line(color = 'lightblue') +
  geom_point(color = "darkgray") +
  geom_point(data = plot_df %>%
               filter(is_na),
             aes(y = value,
                 color = type)) +
  scale_color_brewer(palette = "Set1",
                     name = "Model") +
  theme(legend.position = "bottom")


# plot imputations
p1 <- ggplot_na_imputations(test_ts,
                            inter_fit) +
  labs(title = "Linear Interpolation Imputation")
p1a <- ggplot_na_imputations(test_ts,
                            spline_fit) +
  labs(title = "Spline Interpolation Imputation")
p1b <- ggplot_na_imputations(test_ts,
                            stine_fit) +
  labs(title = "Stine Interpolation Imputation")
ggarrange(p1, p1a, p1b,
          ncol = 1,
          nrow = 3,
          common.legend = T)


p2 <- ggplot_na_imputations(test_ts,
                            ma_fit) +
  labs(title = "Moving Average Imputation")
p3 <- ggplot_na_imputations(test_ts,
                            kalman_fit1) +
  labs(title = "Kalman Filter StructTS Imputation")
p4 <- ggplot_na_imputations(test_ts,
                            kalman_fit2) +
  labs(title = "Kalman Filter ARIMA Imputation")

ggarrange(p1, p2, p3, p4,
          ncol = 2,
          nrow = 2,
          common.legend = T)


#----------------------------------------
plot_df <- ts_df$data[[i]] %>%
  rename(obs = total,
         obs_se = se) %>%
  left_join(pred_miss %>%
              select(-missing_loc) %>%
              rename(pred_se = se,
                     kalman_struct = kalman_1,
                     kalman_arima = kalman_2,
                     kalman_run = kalman_3)) %>%
  pivot_longer(-c(date_time:obs_se, pred_se),
               names_to = "type",
               values_to = "value") %>%
  mutate(is_na = if_else(is.na(obs), T, F)) %>%
  relocate(is_na, .before = obs)

plot_df %>%
  # filter(obs != 0) %>%
  ggplot(aes(x = date_time,
             y = obs)) +
  geom_line(color = 'lightblue') +
  geom_errorbar(aes(ymin = qnorm(0.025, obs, obs_se),
                    ymax = qnorm(0.975, obs, obs_se)),
                width = 0,
                color = "gray70") +
  geom_point(color = "gray50") +
  geom_errorbar(data = plot_df %>%
                  filter(is_na),
                aes(ymin = qnorm(0.025, value, pred_se),
                    ymax = qnorm(0.975, value, pred_se),
                    color = type),
                width = 0) +
  geom_point(data = plot_df %>%
               filter(is_na),
             aes(y = value,
                 color = type)) +
  scale_color_brewer(palette = "Set1",
                     name = "Model") +
  theme(legend.position = "bottom")
