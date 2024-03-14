# Author: Kevin See
# Purpose: Fit species composition data
# Created: 4/25/23
# Last Modified: 3/8/24
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

theme_set(theme_bw())

#-----------------------------------------------------------------
# load data
load(here("analysis/data/derived_data",
          "sonar_data.rda"))

load(here("analysis/data/derived_data",
          "spp_comp_data.rda"))


#-----------------------------------------------------------------
# fit a GAM model
fl_mod <-
  gam(spp_fct ~ s(fl_z, k = 5, m = 2, bs = "tp") +
        s(jday, k = 5, m = 2, bs = "tp"),
      data = spp_fl,
      family = binomial)

# fl_mod <-
#   gam(spp_fct ~ s(fl_z, k = 5, m = 2, bs = "tp") +
#         s(jday, year, bs = "fs"),
#       data = spp_fl |>
#         mutate(year = year(date),
#                across(year,
#                       as.factor)),
#       family = binomial)



# fl_mod <-
#   # gam(spp_fct ~ s(fl_z, jday, k = 5, m = 2, bs = "tp"),
#   gam(spp_fct ~ s(fl_z, jday),
#       data = spp_fl,
#       family = binomial)


#-----------------------------------------------------------------
# extract all steelhead from sonar data: big fish and small ones that are predicted to be steelhead
sonar_pred_spp <- sonar_fish %>%
  filter(confidence == 1) |>
  rename(fork_length_cm = length) %>%
  mutate(fl_z = (fork_length_cm - unique(spp_fl$fl_mean)) / unique(spp_fl$fl_sd)) %>%
  mutate(jday = yday(date_time)) %>%
  bind_cols(predict(fl_mod,
                    newdata = .,
                    type = "response",
                    se.fit = T) %>%
              as_tibble() %>%
              select(prob_sthd = fit,
                     prob_se = se.fit)) %>%
  mutate(p_alpha = prob_sthd^2 * ((1 - prob_sthd) / prob_se^2 - prob_sthd^-1),
         p_beta = p_alpha * (prob_sthd^-1 - 1))


sonar_sthd <- sonar_pred_spp |>
  select(-c(p_alpha, p_beta)) |>
  filter(sthd_length | prob_sthd > 0.5)


#-----------------------------------------------------------------
# save some objects
save(fl_mod,
     sonar_pred_spp,
     sonar_sthd,
     file = here("analysis/data/derived_data",
                 "spp_comp_pred.rda"))

#-----------------------------------------------------------------
# make some figures
load(here("analysis/data/derived_data",
          "spp_comp_pred.rda"))

library(tidymv)

plot_smooths(fl_mod,
             series = fl_z,
             transform = boot::inv.logit) +
  labs(x = "Z-Scored Length",
       y = "Probability of Being a Steelhead")

plot_smooths(fl_mod,
             series = jday,
             transform = boot::inv.logit) +
  labs(x = "Julian Day",
       y = "Probability of Being a Steelhead")



# get_gam_predictions(fl_mod,
#                     series = fl_z,
#                     series_length = 151,
#                     conditions = quos(jday == 100)) |>
#   as_tibble()
#
# pred_df <- crossing(fl_z = seq(-3, 2, by = 0.1),
#                     jday = seq(32, 183))
# predict(fl_mod,
#         newdata = pred_df,
#         se.fit = T) |>
#   as_tibble() |>
#   bind_cols(pred_df)


# what is the fork length when 50% of being a steelhead?
p_pred = 0.5
pred_tab <- crossing(fork_length_cm = seq(35, 75, by = 1),
                     survey_date = seq(ymd(20210203),
                                       ymd(20210701),
                                       by = "1 days")) %>%
  mutate(fl_z = (fork_length_cm - unique(spp_fl$fl_mean)) / unique(spp_fl$fl_sd),
         jday = yday(survey_date)) %>%
  bind_cols(predict(fl_mod,
                    newdata = .,
                    type = "response",
                    se.fit = T) %>%
              as_tibble() %>%
              select(prob_sthd = fit,
                     prob_se = se.fit))

pred_tab %>%
  filter(month(survey_date) < 6 |
           (month(survey_date) == 6 & day(survey_date) <= 15)) |>
  filter(between(fork_length_cm, 35, 75)) |>
  ggplot(aes(x = survey_date,
             y = fork_length_cm,
             fill = prob_sthd)) +
  geom_tile() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  # scale_fill_viridis_c(name = "Probability of Being a Steelhead",
  #                      guide = guide_colorbar(barwidth = 12)) +
  scale_fill_continuous_diverging(name = "Probability of Being a Steelhead",
                                  # palette = "Purple-Green",
                                  palette = "Cork",
                                  # palette = "Blue-Red 3",
                                  # rev = T,
                                  mid = 0.5,
                                  guide = guide_colorbar(barwidth = 12)) +
  geom_line(data = pred_tab %>%
              filter(month(survey_date) < 6 |
                       (month(survey_date) == 6 & day(survey_date) <= 15)) |>
              filter(between(fork_length_cm, 35, 75)) |>
              filter(prob_sthd >= p_pred) %>%
              group_by(survey_date) %>%
              filter(fork_length_cm == min(fork_length_cm)) %>%
              arrange(survey_date, fork_length_cm),
            color = "black",
            linewidth = 2) +
  geom_hline(yintercept = c(45, 67),
             linetype = 2,
             color = 'darkgray') +
  labs(x = "Date",
       y = "Fork Length (cm)") +
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))

ggsave(here("analysis/figures",
            "spp_comp.png"),
       width = 12,
       height = 6,
       bg = "transparent")

sonar_pred_spp |>
  filter(month(date) < 6 |
           (month(date) == 6 & day(date) <= 15)) |>
  mutate(plot_date = ymd(20211231) + days(jday)) |>
  ggplot(aes(x = plot_date,
             y = fork_length_cm,
             color = prob_sthd)) +
  geom_point() +
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
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "Fork Length (cm)") +
  theme(text = element_text(size = 20))#,
        # panel.background = element_rect(fill='transparent'),
        # plot.background = element_rect(fill='transparent', color=NA),
        # legend.background = element_rect(fill='transparent'),
        # legend.box.background = element_rect(fill='transparent'))

ggsave(here("analysis/figures",
            "spp_comp_pred.png"),
       width = 12,
       height = 6,
       bg = "transparent")
