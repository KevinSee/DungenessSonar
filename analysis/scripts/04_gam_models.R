# Author: Kevin See
# Purpose: Fit GAM to time-series with missing data
# Created: 4/21/23
# Last Modified: 3/8/24
# Notes:

#-----------------------------------------------------------------
# load packages
library(tidyverse)
library(here)
library(magrittr)
library(janitor)
library(lubridate)
library(dataRetrieval)
library(ggfortify)
library(mgcv)
library(tidymv) # to be replaced by tidygam at some point
# library(tidygam)
library(colorspace)

theme_set(theme_bw())

#-----------------------------------------------------------------
# load data
load(here("analysis/data/derived_data",
          "time_series.rda"))

# # quick plot of data
# imputeTS::ggplot_na_imputations(ts_df$ts[[4]],
#                       ts_df$kalman_preds[[4]])
#
# ggplot(ts_half_hr,
#        aes(x = date_time,
#            y = n_fish)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~ direction + year,
#              nrow = 2,
#              scales = "free_x")

#------------------------------------------------
# time series of observed fish by the hour
obs_hr <- ts_half_hr |>
  mutate(date_hr = date + hour) |>
  group_by(year, date_hr, direction) |>
  summarize(across(c(reviewed,
                     operational,
                     full_hr,
                     n_fish),
                   ~ sum(., na.rm = T)),
            .groups = "drop") |>
  mutate(across(c(reviewed,
                  operational,
                  full_hr),
                ~ . / 2),
         across(full_hr,
                as.logical)) |>
  mutate(
    across(
      n_fish,
      ~ if_else(reviewed == 0,
                NA_real_, .))) |>
  filter(!is.na(date_hr))

# time series of observed fish by the day
obs_day <- ts_half_hr |>
  group_by(year, date, direction) |>
  summarize(across(c(reviewed,
                     operational,
                     full_hr,
                     n_fish),
                   ~ sum(., na.rm = T)),
            .groups = "drop") |>
  mutate(across(c(reviewed,
                  operational),
                ~ . / (24 * 2))) |>
  mutate(
    across(
      n_fish,
      ~ if_else(reviewed == 0,
                NA_real_, .)))

obs_day |>
  ggplot(aes(x = date,
             y = n_fish,
             color = direction)) +
  geom_point() +
  # geom_line() +
  facet_wrap(~ year,
             scales = "free")



#-----------------------------------------------------------------
# prep data for GAM
# query discharge data for Dungeness River
disc_day_df <- dataRetrieval::readNWISdv("12048000",
                                         parameterCd = "00060", # discharge
                                         startDate = as.character(min(ts_half_hr$date_time)),
                                         endDate = as.character(ceiling_date(max(ts_half_hr$date_time), unit = "days")),
                                         statCd = "00003") |>
  as_tibble() |>
  dataRetrieval::renameNWISColumns() |>
  rename(mean_discharge = Flow,
         date = Date) |>
  mutate(across(date,
                ~ ymd(as.character(.), tz = "America/Los_Angeles")),
         across(date,
                ~ floor_date(., unit = "days"))) |>
  select(-Flow_cd)

# at a finer temporal scale
disc_all_df <- dataRetrieval::readNWISuv(siteNumbers = "12048000",
                                         parameterCd = "00060", # discharge
                                         startDate = as.character(min(ts_half_hr$date_time)),
                                         # endDate = ymd(20190701),
                                         endDate = as.character(ceiling_date(max(ts_half_hr$date_time), unit = "days")),
                                         tz = "America/Los_Angeles") |>
  as_tibble() |>
  dataRetrieval::renameNWISColumns() |>
  rename(mean_discharge = Flow_Inst,
         date_time = dateTime) |>
  select(-c(Flow_Inst_cd,
            tz_cd))

# summarize at half hour scales
disc_hr_df <- disc_all_df |>
  mutate(date_hr = floor_date(date_time, unit = "hours"),
         time_period = if_else(str_detect(date_time, ":30:") |
                                 str_detect(date_time, ":45:"),
                               2, 1)) |>
  group_by(agency_cd,
           site_no,
           date_hr,
           time_period) |>
  summarize(across(date_time,
                   min),
            across(mean_discharge,
                   mean),
            .groups = "drop") |>
  select(-c(date_hr,
            time_period))

disc_day_df |>
  mutate(year = year(date)) |>
  mutate(date_time = date + hours(12)) |>
  filter(month(date_time) < 7) |>
  ggplot(aes(x = date_time,
             y = mean_discharge)) +
  geom_line(data = disc_all_df |>
              mutate(year = year(date_time)) |>
              filter(month(date_time) < 7),
            aes(color = "15 min")) +
  geom_line(aes(color = "Daily")) +
  facet_wrap(~ year,
             scales = "free_x")

# summarized at half hour intervals
# upstream observations
up_data <-
  ts_half_hr |>
  filter(direction == "up") |>
  mutate(day_of_year = yday(date),
         across(day_of_year,
                as.integer),
         across(year,
                as.factor),
         hour_of_day = hour(date_time),
         total_observed_hr = n_fish,
         prop_hr_sampled = as.numeric(reviewed)) |>
  mutate(across(hour_of_day,
                ~ if_else(str_detect(time, "30M"),
                          . + 0.5,
                          .)))

# downstream observations
down_data <-
  ts_half_hr |>
  filter(direction == "down") |>
  mutate(day_of_year = yday(date),
         across(day_of_year,
                as.integer),
         across(year,
                as.factor),
         hour_of_day = hour(date_time),
         total_observed_hr = n_fish,
         prop_hr_sampled = as.numeric(reviewed)) |>
  mutate(across(hour_of_day,
                ~ if_else(str_detect(time, "30M"),
                          . + 0.5,
                          .)))


# add discharge data
up_data %<>%
  left_join(disc_hr_df |>
              select(date_time,
                     mean_discharge)) |>
  left_join(disc_day_df |>
              select(date,
                     daily_discharge = mean_discharge)) |>
  mutate(across(mean_discharge,
                ~ case_when(is.na(.) ~ daily_discharge,
                            .default = .))) |>
  select(-daily_discharge)

down_data %<>%
  left_join(disc_hr_df |>
              select(date_time,
                     mean_discharge)) |>
  left_join(disc_day_df |>
              select(date,
                     daily_discharge = mean_discharge)) |>
  mutate(across(mean_discharge,
                ~ case_when(is.na(.) ~ daily_discharge,
                            .default = .))) |>
  select(-daily_discharge)


#--------------------------
# summarized at whole hour
# upstream observations

# up_data <-
#   ts_half_hr |>
#   filter(direction == "up") |>
#   group_by(year,
#            date,
#            hour,
#            full_hr) |>
#   summarize(across(date_time,
#                    min),
#             across(reviewed,
#                    sum),
#             across(n_fish,
#                    ~ sum(., na.rm = T)),
#             across(op_perc,
#                    mean),
#             .groups = "drop") |>
#   mutate(across(n_fish,
#                 ~ if_else(reviewed == 0 | op_perc == 0,
#                           NA_real_, .))) |>
#   mutate(day_of_year = yday(date),
#          across(year,
#                 as.factor),
#          hour_of_day = hour(date_time),
#          total_observed_hr = n_fish,
#          prop_hr_sampled = reviewed / 2)
#
# # downstream observations
# down_data <-
#   ts_half_hr |>
#   filter(direction == "down") |>
#   group_by(year,
#            date,
#            hour,
#            full_hr) |>
#   summarize(across(date_time,
#                    min),
#             across(reviewed,
#                    sum),
#             across(n_fish,
#                    ~ sum(., na.rm = T)),
#             across(op_perc,
#                    mean),
#             .groups = "drop") |>
#   mutate(across(n_fish,
#                 ~ if_else(reviewed == 0 | op_perc == 0,
#                           NA_real_, .))) |>
#   mutate(day_of_year = yday(date),
#          across(year,
#                 as.factor),
#          hour_of_day = hour(date_time),
#          total_observed_hr = n_fish,
#          prop_hr_sampled = reviewed / 2)
#
# # add discharge data
# up_data %<>%
#   left_join(disc_day_df |>
#               select(date,
#                      mean_discharge))
#
# down_data %<>%
#   left_join(disc_day_df |>
#               select(date,
#                      mean_discharge))


# upstream model
up_mod <- gam(total_observed_hr ~
                s(hour_of_day, bs = "cr") +
                s(mean_discharge) +
                # s(day_of_year) +
                # s(year, bs = "re") +
                s(day_of_year, year, bs = "fs") +
                offset(log(prop_hr_sampled)),
              family = nb,
              # method = "REML",
              optimizer = c("outer", "optim"),
              data = up_data)


down_mod <- gam(formula(up_mod),
                family = nb,
                data = down_data)


summary(up_mod)
print(up_mod)
plot(up_mod)
plot(down_mod)

#---------------------------------------------------------
# plot some of the effects
text_size = 16

up_hr_p <- plot_smooths(up_mod,
                        series = hour_of_day,
                        conditions = quos(prop_hr_sampled == 1),
                        transform = exp) +
  labs(x = "Hour of Day",
       y = "Expected Number Upstream\nFish / 30 min") +
  theme(plot.background = element_rect(fill='transparent', color=NA),
        legend.box.background = element_rect(fill='transparent'),
        text = element_text(size = text_size))

up_disch_p <- plot_smooths(up_mod,
                           series = mean_discharge,
                           # comparison = year,
                           conditions = quos(prop_hr_sampled == 1),
                           transform = exp) +
  scale_y_continuous(limits = c(NA, 0.3)) +
  scale_x_continuous(limits = c(NA, 2100)) +
  labs(x = "Discharge",
       y = "Expected Number Upstream\nFish / 30 min") +
  theme(plot.background = element_rect(fill='transparent', color=NA),
        legend.box.background = element_rect(fill='transparent'),
        text = element_text(size = text_size))

up_doy_p <- get_gam_predictions(up_mod,
                                series = day_of_year,
                                series_length = 151,
                                conditions = quos(prop_hr_sampled == 1),
                                exclude_random = F,
                                exclude_terms = s(mean_discharge)) |>
  as_tibble() |>
  mutate(across(day_of_year,
                as.integer),
         date = ymd(20201231) + days(day_of_year)) |>
  mutate(across(c(total_observed_hr,
                  starts_with("CI")),
                exp),
         across(c(total_observed_hr,
                  starts_with("CI")),
                ~ . * 2 * 24)) |>
  filter(month(date) < 6) |>
  ggplot(aes(x = date,
             y = total_observed_hr,
             color = year,
             fill = year)) +
  geom_ribbon(aes(ymin = CI_lower,
                  ymax = CI_upper),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  scale_y_continuous(limits = c(NA, 27)) +
  labs(x = "Date",
       y = "Expected Number Upstream\nFish / Day",
       color = "Year",
       fill = "Year") +
  theme(plot.background = element_rect(fill='transparent', color=NA),
        legend.box.background = element_rect(fill='transparent'),
        text = element_text(size = text_size))

ggpubr::ggarrange(up_hr_p,
                  up_disch_p,
                  up_doy_p +
                    theme(legend.position = "bottom"),
                  nrow = 1)

ggsave(here("analysis/figures",
            "gam_up_marginal.png"),
       width = 16,
       height = 7,
       bg='transparent')




down_hr_p <- plot_smooths(down_mod,
                          series = hour_of_day,
                          conditions = quos(prop_hr_sampled == 1),
                          transform = exp) +
  scale_y_continuous(limits = c(0, 0.4)) +
  labs(x = "Hour of Day",
       y = "Expected Number Downstream\nFish / 30 min") +
  theme(plot.background = element_rect(fill='transparent', color=NA),
        legend.box.background = element_rect(fill='transparent'),
        text = element_text(size = text_size))


down_disch_p <- plot_smooths(down_mod,
                             series = mean_discharge,
                             # comparison = year,
                             conditions = quos(prop_hr_sampled == 1),
                             transform = exp) +
  scale_y_continuous(limits = c(NA, 0.3)) +
  scale_x_continuous(limits = c(NA, 2100)) +
  labs(x = "Discharge",
       y = "Expected Number Downstream\nFish / 30 min") +
  theme(plot.background = element_rect(fill='transparent', color=NA),
        legend.box.background = element_rect(fill='transparent'),
        text = element_text(size = text_size))

ggpubr::ggarrange(up_disch_p,
                  down_disch_p,
                  nrow = 1)

ggpubr::ggarrange(up_hr_p,
                  up_disch_p,
                  nrow = 1)

ggsave(here("analysis/figures",
            "gam_hr_discharge_up.png"),
       width = 12,
       height = 6,
       bg='transparent')

ggpubr::ggarrange(down_hr_p,
                  down_disch_p,
                  nrow = 1)

ggsave(here("analysis/figures",
            "gam_hr_discharge_down.png"),
       width = 12,
       height = 6,
       bg='transparent')


down_doy_p <- get_gam_predictions(down_mod,
                                  series = day_of_year,
                                  series_length = 151,
                                  conditions = quos(prop_hr_sampled == 1),
                                  exclude_random = F,
                                  exclude_terms = s(mean_discharge)) |>
  as_tibble() |>
  mutate(across(day_of_year,
                as.integer),
         date = ymd(20201231) + days(day_of_year)) |>
  mutate(across(c(total_observed_hr,
                  starts_with("CI")),
                exp),
         across(c(total_observed_hr,
                  starts_with("CI")),
                ~ . * 2 * 24)) |>
  filter(month(date) < 6) |>
  ggplot(aes(x = date,
             y = total_observed_hr,
             color = year,
             fill = year)) +
  geom_ribbon(aes(ymin = CI_lower,
                  ymax = CI_upper),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  scale_y_continuous(limits = c(NA, 27)) +
  # scale_y_continuous(limits = c(NA, 66)) +
  labs(x = "Date",
       y = "Expected Number Downstream\nFish / Day",
       color = "Year",
       fill = "Year") +
  theme(plot.background = element_rect(fill='transparent', color=NA),
        legend.box.background = element_rect(fill='transparent'),
        text = element_text(size = text_size))

ggpubr::ggarrange(up_doy_p,
                  down_doy_p,
                  nrow = 1,
                  common.legend = T,
                  legend = "bottom")

ggsave(here("analysis/figures",
            "gam_doy.png"),
       width = 12,
       height = 6,
       bg='transparent')




#----------------------------------------------
# prediction
#----------------------------------------------
time_step <- if_else(sum(str_detect(up_data$date_time[12], ":30:")) > 0,
                     "30 min",
                     "1 hour")


newdata <- tibble(year = unique(up_data$year)) |>
  mutate(date_time =
           map(year,
               .f = function(yr) {
                 tibble(date_time = seq(ymd_hms(paste0(yr, "0201 00:00:00"), tz = "America/Los_Angeles"),
                                        ymd_hms(paste0(yr, "0615 00:00:00"), tz = "America/Los_Angeles"),
                                        by = time_step))
               })) |>
  unnest(date_time) |>
  mutate(date = floor_date(date_time,
                           unit = "days"),
         date_hr = date + hours(hour(date_time)),
         day_of_year = yday(date),
         hour_of_day = hour(date_time),
         across(year,
                as.factor)) |>
  filter(!is.na(date_time)) |>
  left_join(disc_hr_df |>
              select(date_time,
                     mean_discharge)) |>
  left_join(disc_day_df |>
              select(date,
                     daily_discharge = mean_discharge)) |>
  mutate(across(mean_discharge,
                ~ case_when(is.na(.) ~ daily_discharge,
                            .default = .))) |>
  select(-daily_discharge) |>
  mutate(prop_hr_sampled = 1)

# for half hour predictions
if(sum(str_detect(newdata$date_time[1:10], ":30:")) > 0){
  newdata %<>%
    mutate(across(hour_of_day,
                  ~ if_else(str_detect(date_time, ":30:"),
                            . + 0.5,
                            .)))
}


# predict steelhead moving upstream
beta <- coef(up_mod)
V <- vcov(up_mod)
num_beta_vecs <- 1000 # adjust this as needed
Cv <- chol(V) # Cholesky factorization of V
set.seed(123) # for reproducibility
nus <- rnorm(num_beta_vecs * length(beta)) # standard normal random variables
beta_sims <- beta + t(Cv) %*% matrix(nus, nrow = length(beta), ncol = num_beta_vecs) # simulated coefficient vectors
X <- model.matrix(up_mod, newdata) # design matrix for new data
z_sims <- X %*% beta_sims # linear predictor matrix
y_sims_up <- up_mod$family$linkinv(z_sims) # predicted response matrix

# predict steelhead moving downstream
beta <- coef(down_mod)
V <- vcov(down_mod)
# num_beta_vecs <- 1000 # adjust this as needed
Cv <- chol(V) # Cholesky factorization of V
set.seed(123) # for reproducibility
nus <- rnorm(num_beta_vecs * length(beta)) # standard normal random variables
beta_sims <- beta + t(Cv) %*% matrix(nus, nrow = length(beta), ncol = num_beta_vecs) # simulated coefficient vectors
X <- model.matrix(up_mod, newdata) # design matrix for new data
z_sims <- X %*% beta_sims # linear predictor matrix
y_sims_down <- down_mod$family$linkinv(z_sims) # predicted response matrix

rm(beta, V, num_beta_vecs,
   Cv, nus, beta_sims, X, z_sims)


all_draws <- newdata |>
  bind_cols(y_sims_up |>
              as_tibble(.names_repair = function(x) paste0("V", 1:ncol(x)))) |>
  pivot_longer(-any_of(names(newdata)),
               names_to = "draw",
               values_to = "value") |>
  mutate(direction = "up") |>
  bind_rows(newdata |>
              bind_cols(y_sims_down |>
                          as_tibble(.names_repair = function(x) paste0("V", 1:ncol(x)))) |>
              pivot_longer(-any_of(names(newdata)),
                           names_to = "draw",
                           values_to = "value") |>
              mutate(direction = "down")) |>
  mutate(across(draw,
                ~ str_remove(., "^V")),
         across(draw,
                as.integer))

# delete some draws that may be absurd
all_draws <- all_draws |>
  filter(value < 1e1)

#-----------------------------------------------------------------------
# save the all_draws object
write_rds(all_draws,
          file = here("analysis/data/derived_data",
                      paste0("mcmc_draws_all_",
                             str_replace(time_step, " ", "_"),
                             ".rds")))

#-----------------------------------------------------------------------
# replace predicted values with observed values when available
all_draws_wobs <- all_draws |>
  left_join(ts_half_hr |>
              filter(reviewed) |>
              select(date_time,
                     direction,
                     obs_fish = n_fish)) |>
  mutate(
    across(
      value,
      ~ if_else(!is.na(obs_fish) & prop_hr_sampled == 1,
                obs_fish,
                .)
    )
  )

# and save that object
write_rds(all_draws_wobs,
          file = here("analysis/data/derived_data",
                      paste0("mcmc_draws_all_wobs_",
                             str_replace(time_step, " ", "_"),
                             ".rds")))
#------------------------------------------------------
# decide which one to use
time_step = c("30 min",
              "1 hour")[1]

rm(all_draws,
   all_draws_wobs)
gc()

# all_draws <- read_rds(here("analysis/data/derived_data",
#                            paste0("mcmc_draws_all_",
#                                   str_replace(time_step, " ", "_"),
#                                   ".rds")))

all_draws <- read_rds(here("analysis/data/derived_data",
                           paste0("mcmc_draws_all_wobs_",
                                  str_replace(time_step, " ", "_"),
                                  ".rds")))
gc()
#-----------------------------------------------------------------------
# sum MCMC draws at certain time scales
hr_draws <- all_draws |>
  # mutate(across(value,
  #               round_half_up)) |>
  group_by(date_hr, direction, draw) |>
  summarize(across(value,
                   sum),
            .groups = "drop")

# save the MCMC draws as separate objects
write_rds(hr_draws,
          file = here("analysis/data/derived_data",
                      paste0("mcmc_draws_hour_",
                             str_replace(time_step, " ", "_"),
                             ".rds")))
gc()

# day scale
day_draws <- all_draws |>
  # mutate(across(value,
  #               round_half_up)) |>
  group_by(date, direction, draw) |>
  summarize(across(value,
                   sum),
            .groups = "drop")

# save the MCMC draws as separate objects
write_rds(day_draws,
          file = here("analysis/data/derived_data",
                      paste0("mcmc_draws_day_",
                             str_replace(time_step, " ", "_"),
                             ".rds")))

gc()

# year scale
yr_draws <- all_draws |>
  filter(month(date) < 6) |>
  # mutate(across(value,
  #               round_half_up)) |>
  group_by(year, direction, draw) |>
  summarize(across(value,
                   sum),
            .groups = "drop")

# save the MCMC draws as separate objects
write_rds(yr_draws,
          file = here("analysis/data/derived_data",
                      paste0("mcmc_draws_year_",
                             str_replace(time_step, " ", "_"),
                             ".rds")))

gc()

#-----------------------------------------------------------------------
# summary statistics
half_hr_est <- all_draws |>
  group_by(date_time,
           direction) |>
  summarize(
    across(
      value,
      list(mean = ~ mean(.),
           se = ~ sd(.)),
      .names = "{.fn}"),
    .groups = "drop") |>
  left_join(all_draws |>
              group_by(date_time,
                       direction) |>
              reframe(across(value,
                             ~ quantile(., c(0.025, 0.5, 0.975)))) %>%
              add_column(quantile = rep(c("2.5%", "50%", "97.5%"), nrow(.) / 3)) |>
              pivot_wider(names_from = quantile,
                          values_from = value))

hourly_est <- hr_draws |>
  group_by(date_hr,
           direction) |>
  summarize(
    across(
      value,
      list(mean = ~ mean(.),
           se = ~ sd(.)),
      .names = "{.fn}"),
    .groups = "drop") |>
  left_join(hr_draws |>
              group_by(date_hr,
                       direction) |>
              reframe(across(value,
                             ~ quantile(., c(0.025, 0.5, 0.975)))) %>%
              add_column(quantile = rep(c("2.5%", "50%", "97.5%"), nrow(.) / 3)) |>
              pivot_wider(names_from = quantile,
                          values_from = value))

daily_est <- day_draws |>
  group_by(date,
           direction) |>
  summarize(
    across(
      value,
      list(mean = ~ mean(.),
           se = ~ sd(.)),
      .names = "{.fn}"),
    .groups = "drop") |>
  left_join(day_draws |>
              group_by(date,
                       direction) |>
              reframe(across(value,
                             ~ quantile(., c(0.025, 0.5, 0.975)))) %>%
              add_column(quantile = rep(c("2.5%", "50%", "97.5%"), nrow(.) / 3)) |>
              pivot_wider(names_from = quantile,
                          values_from = value))

yearly_est <- yr_draws |>
  group_by(year,
           direction) |>
  summarize(
    across(
      value,
      list(mean = ~ mean(.),
           median = ~ median(.),
           se = ~ sd(.)),
      .names = "{.fn}"),
    .groups = "drop") |>
  left_join(yr_draws |>
              group_by(year,
                       direction) |>
              reframe(across(value,
                             ~ quantile(., c(0.025, 0.5, 0.975)))) %>%
              add_column(quantile = rep(c("2.5%", "50%", "97.5%"), nrow(.) / 3)) |>
              pivot_wider(names_from = quantile,
                          values_from = value))

# save a bunch of objects
save(ts_half_hr,
     time_step,
     newdata,
     up_mod,
     down_mod,
     half_hr_est,
     hourly_est,
     daily_est,
     # yearly_est,
     file = here("analysis/data/derived_data",
                 paste0("gam_",
                        str_replace(time_step, " ", "_"),
                        ".rda")))



#-----------------------------------------------------------------------
time_step = c("30 min",
              "1 hour")[1]
load(here("analysis/data/derived_data",
          paste0("gam_",
                 str_replace(time_step, " ", "_"),
                 ".rda")))


# compare annual estimates using draws from different time-scales
hourly_est |>
  filter(month(date_hr) < 6) |>
  mutate(year = year(date_hr)) |>
  group_by(year,
           direction) |>
  summarize(across(c(mean,
                     `50%`),
                   sum),
            .groups = "drop") |>
  mutate(time_scale = "hour") |>
  bind_rows(daily_est |>
              filter(month(date) < 6) |>
              mutate(year = year(date)) |>
              group_by(year,
                       direction) |>
              summarize(across(c(mean,
                                 `50%`),
                               sum),
                        .groups = "drop") |>
              mutate(time_scale = "day")) |>
  bind_rows(yearly_est |>
              mutate(across(year,
                            ~ as.integer(as.character(.)))) |>
              select(year:mean, `50%`) |>
              mutate(time_scale = "year")) |>
  pivot_wider(names_from = time_scale,
              values_from = c(mean, `50%`))

daily_est |>
  filter(month(date) < 6 |
           (month(date) == 6 & day(date) <= 15)) |>
  mutate(Year = year(date),
         across(Year,
                as.factor),
         date_plot = ymd(20201231) + days(yday(date)),
         across(direction,
                str_to_title),
         across(direction,
                fct_rev)) |>
  ggplot(aes(x = date_plot,
             y = `50%`,
             color = Year,
             fill = Year)) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`),
              color = NA,
              alpha = 0.4) +
  geom_line() +
  scale_y_continuous(limits = c(NA, 40)) +
  facet_wrap(~ direction) +
  theme(legend.position = "bottom")+
  labs(x = "Date",
       y = "Daily Estimate of Steelhead") +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))

ggsave(here("analysis/figures",
            "daily_est_gam.png"),
       width = 12,
       height = 6,
       bg = "transparent")

#------------------------------------------------------------
obs_fit_df <- half_hr_est |>
  inner_join(ts_half_hr |>
               filter(reviewed) |>
               select(date_time,
                      direction,
                      full_hr,
                      n_fish),
             by = join_by(date_time, direction))

obs_vs_fit_p <- obs_fit_df |>
  ggplot(aes(x = n_fish,
             y = `50%`,
             color = full_hr)) +
  geom_abline(linetype = 2) +
  geom_point(position = position_jitter(width = 0.2)) +
  # geom_smooth(method = loess,
  #             se = F) +
  scale_color_viridis_d(end = 0.8,
                        name = "Part of\nFull Hour\nReview?") +
  labs(x = "Observed",
       y = "Fitted",
       title = "Half Hour Comparison")

obs_vs_fit_p

ggsave(here("analysis/figures",
            "half_hr_obs_vs_fitted.pdf"),
       obs_vs_fit_p,
       width = 5,
       height = 5)

# sum up at day scale
obs_fit_df |>
  mutate(date = floor_date(date_time, "days")) |>
  group_by(date) |>
  summarize(across(c(full_hr,
                     mean,
                     `50%`,
                     n_fish),
                   sum),
            .groups = "drop") |>
  ggplot(aes(x = n_fish,
             y = `50%`)) +
  geom_abline(linetype = 2) +
  geom_point(position = position_jitter(width = 0.2)) +
  labs(x = "Observed",
       y = "Fitted")

obs_fit_df |>
  mutate(year = year(date_time),
         across(year,
                as_factor),
         resid = n_fish - mean) |>
  ggplot(aes(x = resid,
             color = year,
             fill = year)) +
  geom_histogram(position = "dodge") +
  geom_vline(xintercept = 0,
             linetype = 2)

obs_fit_df |>
  mutate(year = year(date_time),
         across(year,
                as_factor),
         resid = n_fish - mean) |>
  ggplot(aes(x = mean,
             y = resid)) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  geom_point() +
  geom_smooth(method = loess,
              color = "red",
              se = F) +
  labs(x = "Fitted",
       y = "Residual")

#-----------------------------------------------
load(here("analysis/data/derived_data",
          "frankenstein_estimates.rda"))

hourly_comp <- obs_hr |>
  select(date_hr,
         direction,
         obs_fish = n_fish) |>
  full_join(hourly_est |>
              select(date_hr,
                     direction,
                     mean, `50%`)) |>
  left_join(ts_df |>
              select(year, direction, ts_data,
                     ends_with("preds")) |>
              unnest(c(ts_data, ends_with("preds"))) |>
              mutate(date_hr = floor_date(date_time, unit = "hours")) |>
              group_by(date_hr, direction) |>
              summarize(
                across(
                  c(ends_with("pred")),
                  ~ sum(., na.rm = T)
                ),
                .groups = "drop"
              )) |>
  mutate(year = year(date_hr)) |>
  relocate(year,
           .before = 1) |>
  arrange(date_hr,
          direction)

hourly_est |>
  mutate(year = year(date_hr)) |>
  ggplot(aes(x = date_hr,
             y = `50%`,
             color = direction)) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`,
                  fill = direction),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  facet_wrap(~ year,
             ncol = 1,
             scales = "free_x")

hourly_comp |>
  filter(direction == "up") |>
  ggplot(aes(x = date_hr)) +
  geom_point(aes(y = `50%`,
                 color = "GAM")) +
  geom_point(aes(y = kalman_pred,
                 color = "Kalman Filter")) +
  geom_point(aes(y = obs_fish,
                 color = "Counted")) +
  scale_color_brewer(palette = "Set1",
                     name = "Estimate Type") +
  facet_wrap(~ year + direction,
             ncol = 2,
             scales = 'free') +
  theme(legend.position = "bottom")



# prepare comparisons at day scale
daily_comp <- daily_est |>
  left_join(obs_day |>
              select(-year)) |>
  left_join(ts_full |>
              mutate(across(
                direction,
                as.character
              )) |>
              select(date,
                     direction,
                     total,
                     est_type)) |>
  left_join(ts_df |>
              select(year, direction, ts_data,
                     ends_with("preds")) |>
              unnest(c(ts_data, ends_with("preds"))) |>
              mutate(date = floor_date(date_time, unit = "days")) |>
              group_by(date, direction) |>
              summarize(
                across(
                  c(ends_with("pred")),
                  ~ sum(., na.rm = T)
                ),
                .groups = "drop"
              )) |>
  mutate(year = year(date)) |>
  relocate(year,
           .before = 1)

# a couple comparison plots
p1 <- daily_comp |>
  ggplot(aes(x = total,
             y = `50%`,
             color = est_type)) +
  geom_abline(linetype = 2) +
  geom_point() +
  # geom_smooth(method = lm,
  #             formula = y ~ x - 1) +
  scale_color_brewer(palette = "Set1",
                     name = "Hour Data Type") +
  facet_wrap(~ direction,
             scales = "free") +
  labs(x = "Frankenstein Estimates",
       y = "GAM Estimates")

p2 <- daily_comp |>
  ggplot(aes(x = kalman_pred,
             y = `50%`,
             color = est_type)) +
  geom_abline(linetype = 2) +
  geom_point() +
  # geom_smooth(method = lm,
  #             formula = y ~ x - 1) +
  scale_color_brewer(palette = "Set1",
                     name = "Hour Data Type") +
  facet_wrap(~ direction,
             scales = "free") +
  labs(x = "Kalman Filter Estimates",
       y = "GAM Estimates")


p3 <- daily_comp |>
  ggplot(aes(x = total,
             y = kalman_pred,
             color = est_type)) +
  geom_abline(linetype = 2) +
  geom_point() +
  # geom_smooth(method = lm,
  #             formula = y ~ x - 1) +
  scale_color_brewer(palette = "Set1",
                     name = "Hour Data Type") +
  facet_wrap(~ direction,
             scales = "free") +
  labs(x = "Frankenstein Estimates",
       y = "Kalman Filter Estimates")

ggpubr::ggarrange(p1, p2, p3,
                  ncol = 1,
                  common.legend = T,
                  legend = "right")

daily_comp |>
  ggplot(aes(x = date)) +
  geom_point(aes(y = `50%`,
                 color = "GAM")) +
  geom_point(aes(y = total,
                 color = "Frankenstein")) +
  geom_point(aes(y = n_fish,
                 color = "Counted")) +
  scale_color_brewer(palette = "Set1",
                     name = "Estimate Type") +
  facet_wrap(~ direction + year,
             nrow = 2,
             scales = 'free') +
  theme(legend.position = "bottom")





daily_comp |>
  # group_by(year, direction, est_type) |>
  group_by(year, direction) |>
  summarize(
    across(
      c(mean,
        `50%`,
        n_fish,
        total,
        ends_with("pred")),
      ~ sum(., na.rm = T)
    ),
    .groups = "drop") |>
  filter(direction == "up") |>
  mutate(across(-c(year, direction, n_fish),
                ~ . * .75))

yearly_est  |>
  select(-c(se,
            `2.5%`,
            `97.5%`)) |>
  left_join(ts_df |>
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
              mutate(across(year,
                            as.factor)))


# daily_comp |>
#   group_by(year, direction) |>
#   summarize(across(c(n_fish,
#                      mean,
#                      `50%`,
#                      total,
#                      ends_with("pred")),
#                    ~ sum(., na.rm = T)),
#             .groups = "drop")


yearly_comp <- obs_day |>
  group_by(year, direction) |>
  summarize(across(n_fish,
                   ~ sum(., na.rm = T)),
            .groups = "drop") |>
  left_join(yearly_est |>
              mutate(across(year,
                            ~ as.integer(as.character(.)))) |>
              select(year:mean, `50%`)) |>
  left_join(ts_full |>
              mutate(across(
                direction,
                as.character
              )) |>
              group_by(year,
                       direction) |>
              summarize(across(total,
                               ~ sum(., na.rm = T)),
                        .groups = "drop")) |>
  left_join(ts_df |>
              select(year, direction, ts_data,
                     ends_with("preds")) |>
              unnest(c(ts_data, ends_with("preds"))) |>
              group_by(year, direction) |>
              summarize(
                across(
                  c(ends_with("pred")),
                  ~ sum(., na.rm = T)
                ),
                .groups = "drop"
              )) |>
  rename(obs_fish = n_fish,
         gam_mean = mean,
         gam_median = `50%`) |>
  rename(frankenstein = total)

# save annual estimates
yearly_comp |>
  mutate(across(where(is.double),
                round_half_up)) |>
  write_csv(file = here("analysis/data/derived_data",
                        "annual_directional_estimates.csv"))

yearly_comp |>
  pivot_longer(cols = obs_fish:ma_pred,
               names_to = "method",
               values_to = "n") |>
  pivot_wider(names_from = direction,
              values_from = n) |>
  mutate(net = up - down) |>
  select(-down, -up) |>
  pivot_wider(names_from = method,
              values_from = net)

yearly_est |>
  select(year,
         direction,
         est = median,
         se) |>
  pivot_wider(names_from = direction,
              values_from = c(est, se)) |>
  rowwise() |>
  mutate(escp = est_up - est_down,
         escp_se = sqrt(sum(c(se_down, se_up)^2))) |>
  ungroup() |>
  select(year,
         contains("escp"))

yr_draws |>
  pivot_wider(names_from = direction,
              values_from = value) |>
  mutate(escp = up - down) |>
  ggplot(aes(x = year,
             y = escp,
             fill = year)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(NA, 1500)) +
  labs(x = "Year",
       y = "Net Upstream Escapement") +
  theme(legend.position = "none",
        text = element_text(size = 21)) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))


ggsave(here("analysis/figures",
            "gam_year_est.png"),
       width = 6,
       height = 6)


#----------------------------------------------------
library(corrr)
corr_df <- yearly_comp |>
  select(-year) |>
  nest(ests = -direction) |>
  mutate(cor_mat = map(ests,
                       .f = function(x) {
                         corrr::correlate(x)
                       }),
         cor_df = map(cor_mat,
                      .f = function(x) {
                        x |>
                          shave() |>
                          stretch(na.rm = T)
                      }))
corr_df |>
  select(direction, cor_df) |>
  unnest(cor_df) |>
  filter(x != "obs_fish")

corr_df$cor_mat[[1]] |>
  shave() |>
  fashion()
corr_df$cor_mat[[2]] |>
  shave() |>
  fashion()


library(GGally)
yearly_comp |>
  select(-c(year, obs_fish)) %>%
  ggpairs(aes(color = direction),
          columns = 2:ncol(.),
          lower = list(continuous = function(data, mapping) {
            ggplot(data,
                   mapping) +
              geom_point() +
              geom_abline(linetype = 2)
          }))

daily_comp |>
  filter(mean < 100) |>
  select(direction,
         mean,
         `50%`,
         total,
         ends_with("pred")) %>%
  ggpairs(aes(color = direction),
          columns = 2:ncol(.),
          lower = list(continuous = function(data, mapping) {
            ggplot(data,
                   mapping) +
              geom_point() +
              geom_abline(linetype = 2,
                          color = "red") +
              geom_smooth(method = lm,
                          se = F,
                          color = "black")
          }))

daily_comp |>
  filter(!is.na(total)) |>
  nest(est_df = -c(est_type)) |>
  # nest(est_df = -c(year, est_type)) |>
  mutate(mean_cor = map_dbl(est_df,
                            .f = function(x) {
                              cor(x$mean, x$total,
                                  use = "pairwise.complete.obs")
                            }))

#-----------------------------------------------------------------------
# look at ratio of upstream to downstream fish by day
time_step = c("30 min",
              "1 hour")[1]

day_draws <- read_rds(here("analysis/data/derived_data",
                           paste0("mcmc_draws_day_",
                                  str_replace(time_step, " ", "_"),
                                  ".rds")))

up_down_draws <- day_draws |>
  filter(month(date) < 6) |>
  pivot_wider(names_from = direction,
              values_from = value) |>
  mutate(value = up / down)

up_down_draws |>
  group_by(date) |>
  summarize(
    across(
      value,
      list(mean = ~ mean(.),
           se = ~ sd(.)),
      .names = "{.fn}"),
    .groups = "drop") |>
  left_join(up_down_draws |>
              group_by(date) |>
              reframe(across(value,
                             ~ quantile(., c(0.025, 0.5, 0.975),
                                        na.rm = T))) %>%
              add_column(quantile = rep(c("2.5%", "50%", "97.5%"), nrow(.) / 3)) |>
              pivot_wider(names_from = quantile,
                          values_from = value)) |>
  mutate(year = year(date),
         jday = yday(date),
         plot_date = ymd("20211231") + days(jday)) |>
  ggplot(aes(x = plot_date,
             y = `50%`,
             color = as.factor(year),
             fill = as.factor(year))) +
  geom_hline(yintercept = 1,
             linetype = 2) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(NA, 20)) +
  scale_color_brewer(palette = "Set1",
                     name = "Year") +
  scale_fill_brewer(palette = "Set1",
                    name = "Year") +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "Upstream:Downstream Ratio") +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        text = element_text(size = 18))



ggsave(here("analysis/figures",
            "up_down_ratio.pdf"),
       width = 6,
       height = 6)

ggsave(here("analysis/figures",
            "up_down_ratio.png"),
       width = 8,
       height = 8)


# translate to percentage
up_down_perc <-
  day_draws |>
  group_by(date, draw) |>
  mutate(across(value,
                ~ . / sum(value))) |>
  ungroup() |>
  mutate(year = year(date),
         jday = yday(date))

up_down_perc |>
  group_by(date,
           year,
           jday,
           direction) |>
  summarize(
    across(
      value,
      list(mean = ~ mean(.),
           se = ~ sd(.)),
      .names = "{.fn}"),
    .groups = "drop") |>
  left_join(up_down_perc |>
              group_by(date,
                       year,
                       jday,
                       direction) |>
              reframe(across(value,
                             ~ quantile(., c(0.025, 0.5, 0.975),
                                        na.rm = T))) %>%
              add_column(quantile = rep(c("2.5%", "50%", "97.5%"), nrow(.) / 3)) |>
              pivot_wider(names_from = quantile,
                          values_from = value)) |>
  mutate(across(c(year, direction),
                as_factor),
         plot_date = ymd("20211231") + days(jday),
         across(direction,
                ~ str_to_title(.))) |>
  ggplot(aes(x = plot_date,
             y = mean,
             color = direction,
             fill = direction)) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  geom_smooth(se = F) +
  scale_color_discrete_qualitative(palette = "Dynamic",
                                   name = "Direction") +
  scale_fill_discrete_qualitative(palette = "Dynamic",
                                  name = "Direction") +
  geom_vline(xintercept = ymd("20210515"),
             linetype = 2) +
  facet_wrap(~ year) +
  labs(x = "Date",
       y = "Percentage")


# find day with maximum net upstream movement
day_draw_cnts <-
  day_draws |>
  pivot_wider(names_from = direction,
              values_from = value) |>
  mutate(net = up - down) |>
  mutate(year = year(date),
         jday = yday(date)) |>
  pivot_longer(cols = c(up, down, net),
               names_to = "direction")


day_summ <-
  day_draw_cnts |>
  group_by(date,
           year,
           jday,
           direction) |>
  summarize(
    across(
      value,
      list(mean = ~ mean(.),
           se = ~ sd(.)),
      .names = "{.fn}"),
    .groups = "drop") |>
  left_join(day_draw_cnts |>
              group_by(date,
                       year,
                       jday,
                       direction) |>
              reframe(across(value,
                             ~ quantile(., c(0.025, 0.5, 0.975),
                                        na.rm = T))) %>%
              add_column(quantile = rep(c("2.5%", "50%", "97.5%"), nrow(.) / 3)) |>
              pivot_wider(names_from = quantile,
                          values_from = value)) |>
  mutate(across(direction,
                ~ str_to_title(.)),
         across(c(year, direction),
                as_factor),
         across(direction,
                ~ fct_relevel(.,
                              "Up",
                              after = 0)),
         plot_date = ymd("20211231") + days(jday))

direction_max_day <-
  day_summ |>
  nest(est = -c(year, direction)) |>
  mutate(smooth_mod = map(est,
                          .f = function(x) {
                            loess(mean ~ jday,
                                  data = x)
                          }),
         preds = map(smooth_mod,
                     .f = function(x) {
                       tibble(jday = seq(min(day_summ$jday),
                                         max(day_summ$jday))) %>%
                         bind_cols(tibble(pred = predict(x,
                                           newdata = .)))
                     })) |>
  mutate(max_day = map_dbl(preds,
                           .f = function(x) {
                             x |>
                               arrange(desc(pred)) |>
                               slice(1) |>
                               pull(jday)
                           })) |>
  mutate(max_date = ymd("20211231") + days(max_day))



day_summ |>
  ggplot(aes(x = plot_date,
             y = mean,
             color = direction,
             fill = direction)) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  geom_smooth(se = F) +
  geom_vline(data = direction_max_day,
             aes(xintercept = max_date,
                 color = direction),
             linetype = 2) +
  geom_vline(data = NULL,
             xintercept = ymd("20220515"),
             linetype = 1) +
  geom_hline(yintercept = 0) +
  scale_color_discrete_qualitative(palette = "Dark 2",
                                   name = "Direction") +
  scale_fill_discrete_qualitative(palette = "Dark 2",
                                  name = "Direction") +

  facet_wrap(~ year) +
  labs(x = "Date",
       y = "Daily Estimate")


#---------------------------------------------
first_date <- hr_periods |>
  filter(reviewed) |>
  group_by(year) |>
  filter(date == min(date)) |>
  ungroup() |>
  select(year,
         first_date = date) |>
  distinct()

daily_est |>
  mutate(year = year(date)) |>
  left_join(first_date) |>
  filter(date < first_date) |>
  group_by(year,
           first_date) |>
  summarize(across(c(mean, `50%`),
                   sum),
            .groups = "drop")
