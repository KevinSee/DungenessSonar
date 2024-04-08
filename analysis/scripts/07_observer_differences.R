# Author: Kevin See
# Purpose: Compare observers with Dungeness SONAR
# Created: 7/3/23
# Last Modified: 3/19/24
# Notes:

#-----------------------------------------------------------------
# load packages
library(tidyverse)
library(here)
library(magrittr)
library(janitor)
library(lubridate)
library(readxl)
library(colorspace)

theme_set(theme_bw())

#-----------------------------------------------------------------
# read in data
mult_obs_df <- read_excel(here("analysis/data/raw_data",
                               "2019 shared sonar days.xlsx")) |>
  clean_names() |>
  bind_rows(read_excel(here("analysis/data/raw_data",
                            "2020 shared sonar days.xlsx")) |>
              clean_names() |>
              rename(comments = comments_notes,
                     direction = downstreamirection)) |>
  bind_rows(read_excel(here("analysis/data/raw_data",
                            "2021 shared sonar days.xlsx")) |>
              clean_names() |>
              rename(comments = comments_notes) |>
              mutate(across(hour,
                            ~ date + hours(. / 100)))) |>
  bind_rows(read_excel(here("analysis/data/raw_data",
                            "2022 shared sonar days.xlsx"),
                       skip = 4) |>
              clean_names()) |>
  bind_rows(read_csv(here("analysis/data/raw_data",
                            "2023 shared sonar days.csv"),
                     show_col_types = F) |>
              clean_names() |>
              mutate(across(date, mdy),
                     across(hour,
                            ~ hms(as.character(.))),
                     across(hour,
                            ~ date + .))) |>
  mutate(date_time = ymd_hm(paste(year(date),
                                  month(date),
                                  day(date),
                                  hour(hour),
                                  minute(hour)))) |>
  relocate(date_time,
           .after = "hour") |>
  select(-c(hour),
         -c(full_or_partial,
            review_method,
            time,
            flag,
            data_recorded,
            data_reviewed)) |>
  filter(direction %in% c("upstream",
                          "downstream"),
         confidence == 1)

# assign a fish ID to each fish we think was being observed by multiple observers
mult_obs_fish <- mult_obs_df |>
  select(year:frame) |>
  arrange(date_time,
          frame,
          observer) |>
  mutate(id = 1:n()) |>
  group_by(date) |>
  mutate(n_obs = n_distinct(observer)) |>
  ungroup() |>
  mutate(frame_diff = frame - lead(frame),
         time_diff = date_time - lead(date_time),
         new_fish1 = lag(abs(frame_diff) > 100),
         new_fish2 = lag(abs(as.numeric(time_diff)) > 60),
         new_direction = direction != lag(direction),
         new_fish = if_else(new_fish1 | new_fish2 | new_direction,
                            T, F),
         fish_num = as.numeric(new_fish),
         across(fish_num,
                ~ replace_na(., 0)),
         across(fish_num,
                ~ cumsum(.) + 1)) |>
  select(id,
         any_of(names(mult_obs_df)),
         fish_num,
         n_obs) |>
  add_count(fish_num,
            name = "n_cnts") |>
  arrange(fish_num,
          observer,
          length) |>
  group_by(fish_num,
           observer) |>
  mutate(obs_fish = 1:n()) |>
  ungroup()

# some rows were assigned to the same fish when they should be different
mult_obs_fish |>
  # filter(fish_num == 8)
  filter(obs_fish > 1)

max_fish_num = max(mult_obs_fish$fish_num)

for(i in 1:max_fish_num) {
  fish_fixes <- mult_obs_fish |>
    filter(fish_num == i) |>
    filter(obs_fish > 1) |>
    nrow()
  if(fish_fixes == 0) next else{
    mult_obs_fish <- mult_obs_fish |>
      filter(fish_num != i) |>
      bind_rows(mult_obs_fish |>
                  filter(fish_num == i) |>
                  mutate(across(fish_num,
                                ~ if_else(obs_fish == 1,
                                          .,
                                          max_fish_num + (obs_fish - 1)))))
    max_fish_num = max(mult_obs_fish$fish_num)
  }
}

mult_obs_fish <-
  mult_obs_fish |>
  select(-obs_fish,
         -n_cnts) |>
  add_count(fish_num,
            name = "n_cnts") |>
  arrange(id)

mult_obs_fish |>
  filter(n_cnts > n_obs)

# # add some of the original columns back
# mult_obs_fish <- mult_obs_fish |>
#   left_join(mult_obs_df)


# summarize counts by date, and compare other observers to AS
as_comp_df <- mult_obs_df |>
  group_by(year,
           date,
           # date_time,
           observer,
           direction) |>
  summarize(n_fish = n(),
            .groups = "drop") |>
  pivot_wider(names_from = observer,
              values_from = n_fish) |>
  pivot_longer(-c(year:AS),
               names_to = "observer",
               values_to = "n_fish") |>
  filter(!is.na(n_fish)) |>
  mutate(agree_AS = if_else(n_fish == AS, T, F)) |>
  mutate(across(year,
                ~ as_factor(.)))

#----------------------------------------------------------------------
# save results
save(mult_obs_df,
     mult_obs_fish,
     as_comp_df,
     file = here("analysis/data/derived_data",
                 "obs_compare.rda"))


#-----------------------------------------------------------------
# compare counts
mult_obs_df |>
  group_by(year,
           date,
           observer,
           direction) |>
  summarize(n_fish = n(),
            .groups = "drop") |>
  pivot_wider(names_from = observer,
              values_from = n_fish) |>
  mutate(across(c(year,
                  direction),
                as_factor)) |>
  mutate(across(direction,
                ~ fct_recode(.,
                             "down" = "downstream",
                             "up" = "upstream"))) |>
  # remove one day that had a strange pair of observers (breaks the plot below)
  filter(date != ymd(20210501)) |>
  # filter(direction == "upstream") |>
  GGally::ggpairs(mapping = aes(color = direction,
                                fill = direction,
                                shape = year),
                  columns = 4:9,
                  lower = list(continuous = function(data,
                                                     mapping,
                                                     ...) {
                    ggplot(data,
                           mapping) +
                      geom_smooth(method = lm,
                                  alpha = 0.2) +
                      geom_abline(linetype = 2,
                                  color = 'black') +
                      geom_point()
                      # geom_point(position = position_jitter(width = 0.2,
                      #                                       height = 0.2))
                  })) +
  labs(title = "Daily Observer Correlations")

mult_obs_df |>
  group_by(year,
           date_time,
           observer,
           direction) |>
  summarize(n_fish = n(),
            .groups = "drop") |>
  pivot_wider(names_from = observer,
              values_from = n_fish) |>
  mutate(across(year,
                as_factor)) |>
  # filter(direction == "downstream") |>
  filter(direction == "upstream") |>
  select(4:last_col()) |>
  corrr::correlate() |>
  corrr::stretch(na.rm = T,
                 remove.dups = T)


mult_obs_df |>
  group_by(year,
           date_time,
           observer,
           direction) |>
  summarize(n_fish = n(),
            .groups = "drop") |>
  mutate(across(year,
                as_factor))

mult_obs_df |>
  mutate(month = month(date_time,
                       label = T)) |>
  group_by(year,
           month,
           direction) |>
  count(observer) |>
  mutate(mean_cnt = mean(n),
         sd_cnt = sd(n),
         AS_cnt = n[observer == "AS"],
         AS_bias = n - AS_cnt) |>
  ggplot(aes())





mult_obs_df |>
  mutate(date = floor_date(date_time,
                           unit = "days")) |>
  group_by(year,
           date,
           direction,
           observer) |>
  summarize(n_fish = n(),
            .groups = "drop") |>
  pivot_wider(names_from = observer,
              values_from = n_fish) |>
  tail()



yr_obs_cor <-
  mult_obs_df |>
  nest(obs_df = -c(year, direction)) |>
  arrange(desc(direction),
          year) |>
  mutate(n_obs = map_dbl(obs_df,
                         .f = function(x) {
                           n_distinct(x$observer)
                         }),
         n_cnts = map_dbl(obs_df,
                          nrow),
         n_fish = round(n_cnts / n_obs),
         n_AS = map_dbl(obs_df,
                        .f = function(x) {
                          sum(x$observer == "AS")
                        }),
         obs_corr = map(obs_df,
                        .f = function(x) {
                          x |>
                            group_by(date,
                                     observer) |>
                            summarize(n_fish = n(),
                                      .groups = "drop") |>
                            pivot_wider(names_from = observer,
                                        values_from = n_fish) |>
                            select(-date) |>
                            corrr::correlate() |>
                            corrr::stretch(na.rm = T,
                                           remove.dups = T) |>
                            rename(obs_1 = x,
                                   obs_2 = y)
                        })) |>
  unnest(obs_corr)


obs_cor <-
  mult_obs_df |>
  nest(obs_df = -c(direction)) |>
  arrange(desc(direction)) |>
  mutate(n_obs = map_dbl(obs_df,
                         .f = function(x) {
                           n_distinct(x$observer)
                         }),
         # n_cnts = map_dbl(obs_df,
         #                  nrow),
         # n_fish = round(n_cnts / n_obs),
         n_AS = map_dbl(obs_df,
                        .f = function(x) {
                          sum(x$observer == "AS")
                        }),
         obs_corr = map(obs_df,
                        .f = function(x) {
                          x |>
                            group_by(date,
                                     observer) |>
                            summarize(n_fish = n(),
                                      .groups = "drop") |>
                            pivot_wider(names_from = observer,
                                        values_from = n_fish) |>
                            select(-date) |>
                            corrr::correlate() |>
                            corrr::stretch(na.rm = T,
                                           remove.dups = T) |>
                            rename(obs_1 = x,
                                   obs_2 = y)
                        })) |>
  unnest(obs_corr)

yr_obs_cor
obs_cor

day_cnts <- mult_obs_fish |>
  group_by(year, date,
         direction,
         observer) |>
  summarize(n_cnts = n_distinct(fish_num),
            .groups = "drop") |>
  pivot_wider(names_from = observer,
              values_from = n_cnts)

comp_list = NULL
grp = 1
for(i in unique(mult_obs_fish$observer)) {
  for(j in unique(mult_obs_fish$observer)) {
    if(i == j) next
    cnt_comp <- day_cnts |>
      select(year:direction,
             all_of(c(i,j)))
    names(cnt_comp)[c(4, 5)] = c("obs_1", "obs_2")
    cnt_comp <- cnt_comp |>
      filter(!is.na(obs_1),
             !is.na(obs_2))

    cnt_totals <- cnt_comp |>
      group_by(year,
               direction) |>
      summarize(across(c(obs_1,
                         obs_2),
                       sum),
                .groups = "drop")

    names(cnt_totals)[c(3, 4)] = c(i, j)

    comp_list[[grp]] = cnt_totals |>
      pivot_longer(-c(year, direction),
                   names_to = "observer",
                   values_to = "cnts")

    grp = grp + 1

  }
}

map_df(comp_list,
       .f = identity,
       .id = "grp") |>
  arrange(desc(direction),
          year) |>
  pivot_wider(names_from = observer,
              values_from = cnts) |>
  select(any_of(names(day_cnts))) |>
  distinct()

  as.data.frame()

  # group_by(direction,
  #          year,
  #          observer,
  #          fish_num) |>
  # summarize(n_fish = n(),
  #           .groups = "drop") |>
  # pivot_wider(names_from = observer,
  #             values_from = n_fish) |>
  # arrange(desc(direction),
  #         year)


#--------------------------------------
as_comp_df |>
  group_by(direction,
           year,
           observer) |>
  summarize(n_hours = n(),
            n_agree = sum(agree_AS,
                          na.rm = T),
            prop_agree = n_agree / n_hours,
            .groups = "drop")
  # ggplot(aes(x = AS,
  #            y = JG,
  #            color = as_factor(year))) +
  # geom_abline(linetype = 2) +
  # geom_point(position = "jitter") +
  # facet_wrap(~ direction,
  #            scales = "free") +
  # scale_color_discrete_qualitative(name = "Year",
  #                                  palette = "Dark 2")


as_comp_df |>
  group_by(direction,
           # year,
           observer) |>
  summarize(n_years = n_distinct(year),
            n_hrs = n_distinct(date),
            mean_count = mean(AS,
                              na.rm = T),
            mean_bias = mean(n_fish - AS,
                             na.rm = T),
            median_bias = median(n_fish - AS,
                                 na.rm = T),
            mean_rel_bias = mean((n_fish - AS) / AS,
                                 na.rm = T),
            median_rel_bias = median((n_fish - AS) / AS,
                                 na.rm = T),
            RMSE = sqrt(mean((n_fish - AS)^2, na.rm = T)),
            MAE = mean(abs(n_fish - AS),
                       na.rm = T),
            MedAE = median(abs(n_fish - AS),
                         na.rm = T),
            MAPE = mean(abs((n_fish - AS) / AS),
                        na.rm = T) * 100,
            MedAPE = median(abs((n_fish - AS) / AS),
                            na.rm = T) * 100,
            .groups = "drop")

as_comp_df |>
  ggplot(aes(x = AS,
             y = n_fish,
             color = observer)) +
  geom_smooth(aes(fill = observer),
              alpha = 0.2,
              method = lm,
              formula = y ~ x - 1) +
  geom_abline(linetype = 2,
              color = "red") +
  geom_point(aes(shape = year),
             position = position_jitter(width = 0.25,
                                        height = 0.25)) +
  facet_wrap(~ direction,
             scales = "free") +
  # scale_color_brewer(name = "Observer",
  #                    palette = "Set3") +
  # scale_fill_brewer(name = "Observer",
  #                    palette = "Set3") +
  scale_color_discrete_qualitative(name = "Observer",
                                   palette = "Dark 2") +
  scale_fill_discrete_qualitative(name = "Observer",
                                  palette = "Dark 2") +
  labs(shape = "Year",
       y = "Observed Fish")

as_comp_df |>
  mutate(bias = n_fish - AS,
         rel_bias = bias / AS) |>
  ggplot(aes(x = bias,
             color = observer,
             fill = observer)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = "dodge") +
  geom_density(alpha = 0.3) +
  geom_vline(xintercept = 0) +
  facet_grid(year ~ direction)


as_comp_df |>
  nest(data = -c(direction,
                 observer)) |>
  mutate(n_days = map_int(data,
                          nrow)) |>
  arrange(desc(direction),
          desc(n_days)) |>
  mutate(lm_mod = map(data,
                      .f = function(x) {
                        lm(log(AS) ~ log(n_fish),
                           data = x)
                      })) |>
  mutate(intercept = map_dbl(lm_mod,
                             .f = function(x) {
                               coef(x)[1]
                             }),
         slope = map_dbl(lm_mod,
                             .f = function(x) {
                               coef(x)[2]
                             }),
         r2 = map_dbl(lm_mod,
                      .f = function(x) {
                        summary(x)$adj.r.squared
                        })) |>
  mutate(across(intercept,
                ~ round(., 2)))


#-----------------------------------------------------------------
# compare length measurements

mult_obs_fish |>
  filter(n_cnts > 1) |>
  select(-c(id,
            frame,
            n_obs,
            n_cnts)) |>
  pivot_wider(names_from = observer,
              values_from = length) |>
  mutate(across(c(year,
                  direction),
                as_factor)) |>
  mutate(across(direction,
                ~ fct_recode(.,
                             "down" = "downstream",
                             "up" = "upstream"))) |>
  GGally::ggpairs(mapping = aes(color = direction,
                                fill = direction,
                                shape = year),
                  columns = 6:10,
                  lower = list(continuous = function(data,
                                                     mapping,
                                                     ...) {
                    ggplot(data,
                           mapping) +
                      geom_abline(linetype = 2,
                                  color = 'gray') +
                      geom_point() +
                      geom_smooth(method = lm,
                                  formula = y ~ x,
                                  alpha = 0.2)

                  })) +
  labs(title = "Length Correlations")

# pull out fish that were counted by multiple observers
mult_obs_fish |>
  filter(n_cnts > 1) |>
  group_by(year,
           fish_num,
           direction,
           n_cnts) |>
  summarize(mean_length = mean(length),
            sd_length = sd(length),
            cv_length = sd(length) / mean(length),
            .groups = "drop") |>
  mutate(across(year,
                as.factor),
         across(direction,
                str_to_title)) |>
  group_by(year,
           direction) |>
  mutate(cv_mean = mean(cv_length)) |>
  ungroup() |>
  ggplot(aes(x = cv_length,
             color = year,
             fill = year)) +
  scale_color_brewer(name = "Year",
                     palette = "Set1") +
  scale_fill_brewer(name = "Year",
                    palette = "Set1") +
  geom_histogram(position = "dodge",
                 binwidth = 0.05) +
  geom_vline(xintercept = 0.15,
             linetype = 1) +
  geom_vline(aes(color = year,
                 xintercept = cv_mean),
             linetype = 2) +
  facet_wrap(~ direction) +
  labs(x = "CV of Length Measurements")


mult_obs_fish |>
  filter(n_cnts > 1) |>
  group_by(year,
           date_time,
           direction,
           fish_num,
           observer) |>
  summarize(
    across(
      length,
      mean),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = observer,
              values_from = length) |>
  pivot_longer(-c(year:AS),
               names_to = "observer",
               values_to = "length") |>
  filter(!is.na(length)) |>
  mutate(bias = length - AS,
         rel_bias = bias / AS) |>
  mutate(across(direction,
                str_to_title)) |>
  ggplot(aes(x = bias,
             color = observer,
             fill = observer)) +
  # geom_histogram(position = "dodge") +
  geom_histogram(aes(y = after_stat(density)),
                 position = "dodge") +
  geom_density(alpha = 0.3) +
  geom_vline(xintercept = 0) +
  facet_grid(year ~ direction) +
  labs(x = "Length Bias",
       title = "Compared to AS")


mult_obs_fish |>
  group_by(year,
           date_time,
           direction,
           fish_num,
           observer) |>
  summarize(
    across(
      length,
      mean),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = observer,
              values_from = length) |>
  pivot_longer(-c(year:AS),
               names_to = "observer",
               values_to = "length") |>
  filter(!is.na(AS),
         !is.na(length)) |>
  mutate(bias = length - AS,
         rel_bias = bias / AS) |>
  group_by(direction,
           # year,
           observer) |>
  summarize(n_fish = n_distinct(fish_num),
            RMSE = sqrt(mean(bias^2)),
            MAE = mean(bias),
            MedAE = median(bias),
            MAPE = mean(abs(rel_bias)) * 100,
            MedAPE = median(abs(rel_bias)) * 100,
            .groups = "drop")

