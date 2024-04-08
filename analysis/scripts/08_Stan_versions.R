# Author: Kevin See
# Purpose: Fit species composition data and GAMs to counts in same model
# Created: 4/1/24
# Last Modified: 4/1/24
# Notes:

#-----------------------------------------------------------------
# load packages
library(tidyverse)
library(here)
library(magrittr)
library(janitor)
library(lubridate)
library(mgcv)
library(colorspace)

library(dataRetrieval)
library(ggfortify)
library(tidymv) # to be replaced by tidygam at some point
# library(tidygam)
library(brms)

theme_set(theme_bw())

#-----------------------------------------------------------------
# load data
load(here("analysis/data/derived_data",
          "sonar_data.rda"))

load(here("analysis/data/derived_data",
          "spp_comp_data.rda"))

load(here("analysis/data/derived_data",
          "time_series.rda"))

#-----------------------------------------------------------------
# species comp model
brm_data <-
  spp_fl |>
  mutate(across(spp_fct,
                as.numeric))

fl_mod <-
  brm(spp_fct ~ s(fl_z, k = 5, m = 2, bs = "tp") +
        s(jday, k = 5, m = 2, bs = "tp"),
      data = brm_data,
      family = bernoulli,
      save_model = here("analysis/data/derived_data",
                        "spp_comp.txt"))

summary(fl_mod)

# marginal plots for various covariates
msms <- conditional_smooths(fl_mod)
plot(msms)

pp_check(fl_mod,
         ndraws = 25)
pp_check(fl_mod,
         ndraws = 25,
         type = "ecdf_overlay")


#-----------------------------------------------------------------
# extract all steelhead from sonar data: big fish and small ones that are predicted to be steelhead
sonar_pred_spp <- sonar_fish %>%
  filter(confidence == 1) |>
  rename(fork_length_cm = length) %>%
  mutate(fl_z = (fork_length_cm - unique(spp_fl$fl_mean)) / unique(spp_fl$fl_sd)) %>%
  mutate(jday = yday(date_time)) %>%
  bind_cols(predict(fl_mod,
                    newdata = .,
                    summary = T,
                    robust = F,
                    probs = c(0.025, 0.975)) %>%
              as_tibble() %>%
              clean_names() |>
              rename(prob_sthd = estimate,
                     prob_se = est_error,
                     prob_q025 = q2_5,
                     prob_q975 = q97_5)) %>%
  bind_cols(predict(fl_mod,
                    newdata = .,
                    summary = T,
                    robust = T) %>%
              as_tibble() %>%
              clean_names() |>
              select(prob_median = estimate,
                     prob_mad = est_error))


sonar_pred_spp |>
  filter(month(date) < 6 |
           (month(date) == 6 & day(date) <= 15)) |>
  mutate(plot_date = ymd(20211231) + days(jday)) |>
  ggplot(aes(x = plot_date,
             y = fork_length_cm,
             color = prob_sthd)) +
  geom_point(aes(size = 1/prob_se)) +
  scale_color_continuous_diverging(name = "Probability of Being a Steelhead",
                                   # palette = "Purple-Green",
                                   palette = "Cork",
                                   # palette = "Blue-Red 3",
                                   # palette = "Berlin",
                                   # rev = T,
                                   mid = 0.5,
                                   guide = guide_colorbar(barwidth = 12)) +
  geom_hline(yintercept = c(45, 67),
             linetype = 2,
             color = 'darkgray') +
  geom_point(data = spp_fl |>
               mutate(plot_date = ymd(20211231) + days(jday),
                      across(species,
                             ~ if_else(. != "Steelhead",
                                       "Not Sthd",
                                       .))),
             aes(shape = species),
             color = "gray20",
             fill = "gold",
             size = 3) +
  scale_shape_manual(values = c("Steelhead" = 21,
                                "Not Sthd" = 24),
                     name = "Obs. Species") +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "Fork Length (cm)") +
  theme(text = element_text(size = 20))

sonar_pred_spp |>
  ggplot(aes(x = fork_length_cm,
             y = prob_sthd)) +
  geom_point(aes(color = jday,
                 size = 1/ prob_se)) +
  scale_color_continuous_sequential(palette = "Viridis",
                                    name = "Julian\nDay") +
  labs(x = "Fork Length (cm)",
       y = "Prob. of Being a Steelhead",
       size = "Precision")
