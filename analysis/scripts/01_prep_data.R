# Author: Kevin See
# Purpose: Read in data from SONAR
# Created: 3/15/22
# Last Modified: 1/15/26
# Notes:

#-----------------------------------------------------------------
# load packages
library(tidyverse)
library(here)
library(magrittr)
library(janitor)
library(lubridate)
library(anytime)
library(stringr)
library(readxl)
library(ggfortify)

theme_set(theme_bw())

#-----------------------------------------------------------------
# sonar data
# combine 2 files in 2025 first, and save as a csv

read_csv(here("analysis/data/raw_data",
              "2025 sonar_raw.csv"),
         show_col_types = F) |>
  clean_names() |>
  rename(data_recorded = full_or_partial,
         data_reviewed = review_method,
         comments = comments_notes) |>
  tidyr::fill(c(year:data_recorded),
              .direction = "down") |>
  mutate(across(c(data_reviewed,
                  observer),
                ~ case_when(is.na(time) ~ .,
                            is.na(.) &
                              !is.na(time) ~ lag(.),
                            .default = .)),
         across(date,
                mdy),
         across(hour,
                ~ hms(paste0(hour(.), ":", minute(.), ":", second(.))))) |>
  arrange(date,
          hour) |>
  select(-c(flow_cfs,
            flag)) |>
  distinct() |>
  mutate(dt_period = date + hour) |>
  group_by(dt_period) |>
  mutate(n_rows = n(),
         n_times = sum(!is.na(time)),
         n_na_time = sum(is.na(time)),
         n_na_review = sum(is.na(data_reviewed))) |>
  ungroup() |>
  mutate(keep = case_when(n_rows == 1 ~ T,
                          n_rows > 1 &
                            !is.na(time) ~ T,
                          .default = F)) |>
  filter(keep) |>
  select(-c(n_rows:keep,
            dt_period)) |>
  mutate(across(data_recorded,
                str_to_title),
         across(data_recorded,
                ~ case_match(.,
                             "No Sonar Data" ~ "No Data",
                             "Corrupt File" ~ "Corrupt",
                             "Sonar Out" ~ "No Data",
                             .default = .)),
         across(data_reviewed,
                ~ case_match(.,
                             "15 in" ~ "15 min",
                             "12 min" ~ "12 min",
                             .default = .)),
         across(data_reviewed,
                ~ case_when(data_recorded == "Full" &
                              !is.na(time) ~ "15 min",
                            .default = .))) |>
  write_csv(here("analysis/data/raw_data",
                 "2025 sonar.csv"))



sonar_raw <-
  bind_rows(
    read_csv(here("analysis/data/raw_data",
                  "2018 sonar.csv"),
             show_col_types = FALSE) %>%
      mutate(across(Hour,
                    hm),
             across(c(Length,
                      Frame),
                    as.numeric)),
    read_csv(here("analysis/data/raw_data",
                  "2019 sonar.csv"),
             show_col_types = FALSE) %>%
      mutate(across(Hour,
                    hms)),
    read_csv(here("analysis/data/raw_data",
                  "2020 sonar.csv"),
             show_col_types = FALSE) %>%
      mutate(across(Hour,
                    ~ str_pad(.,
                              width = 5,
                              side = "left",
                              pad = "0"))) %>%
      mutate(across(Hour,
                    hm)) %>%
      rename(comments = "Comments/Notes") %>%
      clean_names("upper_camel"),
    read_csv(here("analysis/data/raw_data",
                  "2021 sonar.csv"),
             show_col_types = FALSE) %>%
      mutate(across(Hour,
                    ~ hm(paste(str_sub(., 1,2),
                               str_sub(., 3, 4),
                               sep = ":")))) %>%
      rename(comments = "Comments/Notes") %>%
      clean_names("upper_camel"),
    read_csv(here("analysis/data/raw_data",
                  "2022 sonar.csv"),
             show_col_types = FALSE) %>%
      mutate(across(
        Hour,
        ~ str_remove(., ":")
      )) |>
      mutate(across(
        Hour,
        ~ hm(paste(str_sub(., 1,2),
                   str_sub(., 3,4),
                   sep = ":")))) %>%
      clean_names("upper_camel"),
    read_csv(here("analysis/data/raw_data",
                  "2023 sonar.csv"),
             show_col_types = FALSE) %>%
      mutate(across(
        Hour,
        ~ str_remove(., ":")
      )) |>
      mutate(across(
        Hour,
        ~ hm(paste(str_sub(., 1,2),
                   str_sub(., 3,4),
                   sep = ":")))) %>%
      clean_names("upper_camel"),
    read_csv(here("analysis/data/raw_data",
                  "2024 sonar.csv"),
             show_col_types = FALSE) %>%
      mutate(across(
        Hour,
        ~ str_remove(., ":")
      )) |>
      mutate(across(
        Hour,
        ~ hm(paste(str_sub(., 1,2),
                   str_sub(., 3,4),
                   sep = ":")))) %>%
      clean_names("upper_camel")) |>
  mutate(across(Date,
                mdy)) %>%
  bind_rows(
    read_csv(here("analysis/data/raw_data",
                  "2025 sonar.csv"),
             show_col_types = FALSE) |>
      mutate(
        across(
          hour,
          ~ case_when(str_detect(., "[:digit:]H") ~ hms(.),
                      str_detect(., "[:digit:]M") ~ ms(.),
                      str_detect(., "[:digit:]S") ~ ms("0M 0S"),
                      .default = NA))) |>
      clean_names("upper_camel")
  ) |>
  filter(!is.na(Year)) %>%
  # mutate(across(Date,
  #               anytime)) |>
  mutate(across(c(DataReviewed,
                  DataRecorded,
                  Time),
                str_to_sentence),
         across(Direction,
                str_to_lower),
         across(DataReviewed,
                ~ recode(.,
                         "No review" = "Not reviewed"))) |>
  mutate(across(Time,
                ~ if_else(!is.na(Comments) & str_detect(Comments, "No data"),
                          "No data",
                          .))) |>
  mutate(across(DataRecorded,
                ~ case_match(.,
                             "No sonar data" ~ "No data",
                             "Sonar out" ~ "No data",
                             "None" ~ "No data",
                             .default = .)),
         across(DataReviewed,
                ~ if_else(DataRecorded == "No data" & str_detect(DataReviewed, "30"),
                          "No data",
                          .))) %>%
  mutate(across(Observer,
                str_to_upper)) |>
  clean_names() %>%
  mutate(date_time = date + hour) %>%
  relocate(date_time,
           .after = hour) |>
  mutate(across(time,
                ~ str_replace(., "^24", "23"))) |>
  arrange(year,
          date_time,
          frame) |>
  select(-starts_with("bc"),
         -flag)

# tz(sonar_raw$date) = tz(sonar_raw$date_time) = "America/Los_Angeles"

# # fix mismatches between hour and time
# sonar_raw |>
#   filter(str_detect(time, "\\:")) |>
#   filter(!between(ymd_hms(paste(date, time)),
#                   date_time,
#                   date_time + minutes(30))) |>
#   mutate(across(hour,
#                 ~ paste0(str_pad(hour(.), 2,
#                                  "left", "0"),
#                                  ":",
#                          str_pad(minute(.), 2,
#                                  "right", "0")))) |>
#   select(year,
#          date,
#          hour,
#          time,
#          date_time)
#   clean_names("big_camel") |>
#   write_csv(here("analysis/data/derived_data",
#                  "MismatchTimes.csv"))

# fix some mismatched times for detected fish
fixed_hours <-
  sonar_raw |>
  mutate(row_num = 1:n()) |>
  filter(str_detect(time, "\\:")) |>
  mutate(across(time,
                ~ if_else(str_count(., "\\:") == 1,
                        paste0(., ":00"),
                        .))) |>
  mutate(hour_issue = case_when(year <= 2024 &
                                  !between(ymd_hms(paste(date, time)),
                                         date_time,
                                         date_time + minutes(30)) ~ T,
                                year == 2025 &
                                  !between(ymd_hms(paste(date, time)),
                                           date_time,
                                           date_time + minutes(15)) ~ T,
                              .default = F)) |>
  filter(hour_issue) |>
  mutate(half_hr = case_when(year < 2025 &
                               minute(hms(time)) < 30 ~ ":00",
                             year < 2025 &
                               minute(hms(time)) >= 30 ~ ":30",
                             year >= 2025 &
                               minute(hms(time)) < 15 ~ ":00",
                             year >= 2025 &
                               minute(hms(time)) >= 15 &
                               minute(hms(time)) < 30 ~ ":15",
                             year >= 2025 &
                               minute(hms(time)) >= 30 &
                               minute(hms(time)) < 45 ~ ":30",
                             year >= 2025 &
                               minute(hms(time)) >= 45 ~ ":45",
                             .default = NA_character_),
         new_hour = hm(paste(hour(hms(time)),
                             half_hr)),
         new_data_reviewed = case_when(half_hr %in% c(":00",
                                                      ":15") ~ "First 30",
                                       half_hr %in% c(":30",
                                                      ":45") ~ "Second 30",
                                       .default = NA_character_)) |>
  mutate(hour = new_hour,
         data_reviewed = new_data_reviewed,
         date_time = date + hour) |>
  select(row_num,
         all_of(names(sonar_raw)))

# tz(fixed_hours$date_time) = tz(sonar_raw$date_time)

sonar_raw <-
  sonar_raw |>
  mutate(row_num = 1:n()) |>
  anti_join(fixed_hours |>
              select(row_num)) |>
  bind_rows(fixed_hours) |>
  arrange(row_num) |>
  select(-row_num)


sonar_raw <-
  sonar_raw |>
  mutate(across(data_reviewed,
                ~ case_when(minute(hour) < 30  &
                              . %in% c("First 30",
                                       "15 min") ~ "First 30",
                            minute(hour) >= 30 &
                              . %in% c("First 30",
                                       "15 min") ~ "Second 30",
                            .default = .)))

# drop a couple records because they don't match
# one upstream and one downstream fish recorded during the 2nd half hour,
# but no data in the first half hour, at all
# sonar_raw %<>%
#   filter(!(date == ymd(20200322) &
#              hour == "30M 0S"))
#
# sonar_raw %<>%
#   filter(!(date == ymd(20200301) &
#              (str_detect(hour, "^0S") |
#                 str_detect(hour, "^30M") |
#                 str_detect(hour, "^1H"))))

# this happens a number of times. Decided to keep the records
missing_first_30 <-
  sonar_raw |>
  filter(data_reviewed == "Second 30") |>
  select(year,
         data_recorded_2 = data_recorded,
         data_reviewed_2 = data_reviewed,
         date,
         date_time) |>
  distinct() |>
  filter(year < 2025) |>
  mutate(across(date_time,
                ~ . - minutes(30))) |>
  mutate(has_second_30 = T) |>
  full_join(sonar_raw |>
              filter(minute(date_time) == 0) |>
              select(year,
                     date,
                     date_time) |>
              distinct() |>
              mutate(has_first_30 = T)) |>
  mutate(across(ends_with("_30"),
                ~ replace_na(., F))) |>
  filter(has_second_30,
         !has_first_30)

missing_first_30
tabyl(missing_first_30, year)


# sonar_raw |>
#   filter(data_reviewed == "Second 30") |>
#   select(date) |>
#   distinct() |>
#   left_join(sonar_raw |>
#               select(date,
#                      date_time,
#                      data_reviewed) |>
#               distinct()) |>
#   count(date,
#         data_reviewed) |>
#   pivot_wider(names_from = data_reviewed,
#               values_from = n) |>
#   filter(`First 30` < `Second 30`)



# add missing periods back in
sonar_raw <-
  sonar_raw |>
  bind_rows(missing_first_30 |>
              select(year,
                     data_recorded = data_recorded_2,
                     date_time) |>
              mutate(date = floor_date(date_time, unit = "days"),
                     hour = hm(paste(hour(date_time),
                                     minute(date_time))),
                     data_reviewed = "First 30",
                     comments = "Missing in original data, filled in by KS.") |>
              select(any_of(names(sonar_raw)))) |>
  arrange(date_time,
          frame)



# # add a couple missing row (assuming no fish detected)
# sonar_raw %<>%
#   bind_rows(sonar_raw %>%
#               filter(date == ymd(20190320),
#                      str_detect(hour, "15H")) %>%
#               mutate(hour = hour + minutes(30),
#                      date_time = date + hour,
#                      data_reviewed = "Second 30",
#                      across(c(direction),
#                             ~ NA_character_),
#                      across(c(length:confidence),
#                             ~ NA_real_))) %>%
#   arrange(date_time,
#           frame)


# sonar_raw |>
#   filter((data_recorded == "No data" |
#            data_reviewed == "No data"),
#          time != "No data") |>
#   tabyl(data_recorded)

# correct some columns in one row
sonar_raw <-
  sonar_raw |>
  mutate(across(data_recorded,
                ~ case_when(date == ymd(20200218) &
                              str_detect(hour, "^4H 30M") ~ "Full",
                            .default = .)),
         across(data_reviewed,
                ~ case_when(date == ymd(20200218) &
                              str_detect(hour, "^4H 30M") ~ "Second 30",
                            .default = .)))

# fix the formatting for some times
sonar_raw <-
  sonar_raw |>
  mutate(n_colon = str_count(time, ":")) |>
  # filter(n_colon != 1) |>
  mutate(across(time,
                ~ case_when(n_colon == 1 &
                              !is.na(.) ~ paste0(., ":00"),
                        .default = .))) |>
  select(-n_colon)

# a couple fish had missing lengths
miss_lengths <-
  read_csv(here("analysis/data/raw_data",
                "missing_lengths_BC.csv"),
           show_col_types = F) |>
  mutate(across(hour,
                hms),
         date_time = date + hour) |>
  mutate(across(confidence,
                ~ if_else(comments == "remove from dataset",
                          2,
                          .))) |>
  rename(length = fork_length_cm)

# remove the old version of these rows, and add the new ones back in
sonar_raw <-
  sonar_raw |>
  anti_join(miss_lengths |>
               select(year,
                      date, date_time,
                      frame)) |>
  bind_rows(miss_lengths)


# arrange final tibble
sonar_raw <-
  sonar_raw |>
  arrange(date_time,
          frame)

# when did sonar start/stop each year?
sonar_raw |>
  group_by(year) |>
  summarize(across(date,
                   list(min = ~ min(.),
                        max = ~ max(.))))

# expand days when one row describes 24 hours
expand_na_hr <-
  sonar_raw |>
  filter(is.na(hour)) |>
  select(year,
         date,
         data_recorded,
         data_reviewed,
         comments) |>
  distinct() |>
  crossing(
    crossing(hr = hours(seq(0, 23, by = 1)),
             min = minutes(c(0, 30))) |>
      mutate(hour = hr + min) |>
      select(hour)) |>
  mutate(date_time = date + hour) |>
  select(any_of(names(sonar_raw)))

sonar_raw <-
  sonar_raw |>
  filter(!is.na(hour)) |>
  bind_rows(expand_na_hr) |>
  arrange(date_time)

sonar_raw |>
  tabyl(data_reviewed,
        year)

sonar_raw <-
  sonar_raw |>
  group_by(date_time) |>
  mutate(n_rec = n_distinct(data_recorded)) |>
  mutate(across(data_recorded,
                ~ case_when(n_rec > 1 &
                              sum(. == "Partial") > 0 ~ "Partial",
                            n_rec > 1 &
                              sum(. == "No data") > 0 &
                              sum(. == "Full") > 0 ~ "Partial",
                            .default = .))) |>
  ungroup() |>
  select(-n_rec)


#--------------------------------------------------
# which hours do we want to group together?
#--------------------------------------------------
# 6 hour blocks
hrs_fct_grp <- rep(1:4, each = 6) |>
  set_names(0:23)

# link to SONAR data to determine if SONAR was operational or data was reviewed
sonar_review <-
  sonar_raw |>
  mutate(across(data_reviewed,
                ~ case_when(str_detect(., "[:digit:] to [:digit:]") ~ "Partial",
                            str_detect(., "min") ~ "Partial",
                            data_recorded == "Poor image" ~ "Not reviewable",
                            data_recorded == "Corrupt" ~ "Not reviewable",
                            data_recorded == "No data" ~ "Not reviewed",
                            time == "No review" ~ "Not reviewed",
                            data_recorded == "Partial" ~ "Not reviewed",
                            .default = .)),
         across(data_reviewed,
                ~ case_match(.,
                             "Unable" ~ "Not reviewed",
                             "No data" ~ "Not reviewed",
                             "Partial" ~ "Not reviewed",
                             .default = .))) |>
  select(year,
         date_time,
         data_recorded,
         data_reviewed) %>%
  distinct() %>%
  mutate(reviewed = case_when(data_recorded == "Full" &
                                data_reviewed %in% c("First 30",
                                                   "Second 30") ~ T,
                              data_recorded == "Full" &
                                data_reviewed %in% c("Not reviewable",
                                                   "Not reviewed") ~ F,
                              data_recorded != "Full" ~ F,
                              .default = NA)) |>
  # select(-data_reviewed) |>
  distinct()

# sum(duplicated(sonar_review$date_time))

# sonar_review |>
#   filter(date_time %in% date_time[duplicated(date_time)]) |>
#   select(date_time) |>
#   distinct() |>
#   left_join(sonar_raw) |>
#   select(date_time,
#          data_recorded,
#          data_reviewed,
#          time,
#          comments)

# tz(sonar_review$date_time) = "America/Los_Angeles"

# set up tibble containing all 15 minute periods that sonar was operating
min_date = "0115"
max_date = "0615"

quarter_hr_periods <-
  expand(sonar_raw,
         year) |>
  mutate(dt_df = map(year,
                     .f = function(yr) {
                       tibble(date_time = seq(from = as.POSIXct(ymd(paste(yr, min_date))),
                                              to = as.POSIXct(ymd(paste(yr, max_date))) + days(1) - minutes(15),
                                              by = "15 mins"))
                     })) |>
  unnest(dt_df) |>
  mutate(date = floor_date(date_time,
                           unit = "day"),
         hour = floor_date(date_time,
                           unit = "hour"),
         time = difftime(date_time, date,
                         units = "mins"),
         hour = difftime(hour, date,
                         units = "mins"),
         across(c(time,
                  hour),
                as.period),
         max_dt = date_time + minutes(15) - seconds(1)) |>
  relocate(max_dt,
           .after = date_time) |>
  left_join(sonar_review,
            by = join_by(year,
                         date_time)) |>
  mutate(
    across(
      c(data_recorded,
        data_reviewed,
        reviewed),
      ~ case_when(is.na(.) &
                    !is.na(lag(.)) &
                    (str_detect(time, "15M") |
                       str_detect(time, "45M")) ~ lag(.),
                  .default = .)),
    across(
      data_recorded,
      ~ case_when(is.na(.) &
                    lag(. == "Full") &
                    lag(data_reviewed == "First 30") ~ "Full",
                  is.na(.) &
                    lag(. == "Full", 2) &
                    lag(data_reviewed == "First 30", 2) ~ "Full",
                  .default = .))) |>
  mutate(
    across(
      reviewed,
      ~ replace_na(.,
                   FALSE)
    ),
    operational = case_when(reviewed |
                              data_recorded == "Full" ~ T,
                            .default = F))

# consolidate to half hour periods
half_hr_periods <-
  quarter_hr_periods |>
  mutate(half_hr_period = case_when(str_detect(time, "15M") |
                                      str_detect(time, "45M") ~ date_time - minutes(15),
                                    .default = date_time)) |>
  select(year,
         date_time,
         half_hr_period,
         data_recorded:operational) |>
  group_by(half_hr_period) |>
  mutate(n_row = n(),
         n_rec = n_distinct(data_recorded),
         n_rev = n_distinct(data_reviewed),
         sum_rev = sum(reviewed),
         sum_ops = sum(operational)) |>
  ungroup() |>
  mutate(across(data_recorded,
                ~ case_when(n_rec > 1 ~ "Partial",
                            .default = .)),
         across(data_reviewed,
                ~ case_when(n_rev > 1 ~ "Not reviewed",
                            .default = .)),
         across(reviewed,
                ~ case_when(sum_rev == n_row ~ T,
                            .default = F)),
         across(operational,
                ~ case_when(sum_ops == n_row ~ T,
                            .default = F))) |>
  select(year,
         date_time = half_hr_period,
         data_recorded:operational) |>
  distinct() |>
  mutate(date = floor_date(date_time,
                           unit = "day"),
         hour = floor_date(date_time,
                           unit = "hour"),
         time = difftime(date_time, date,
                         units = "mins"),
         hour = difftime(hour, date,
                         units = "mins"),
         across(c(time,
                  hour),
                as.period),
         max_dt = date_time + minutes(30) - seconds(1)) |>
  relocate(max_dt,
           .after = date_time) |>
  select(any_of(names(quarter_hr_periods))) |>
  mutate(hr_fct = hrs_fct_grp[as.character(hour(date_time))]) |>
  relocate(hr_fct, .after = "time")


quarter_hr_periods |>
# half_hr_periods |>
  mutate(hr = as.numeric(hour) / (60*60),
         half_hr = as.numeric(time) / (60*60)) %>%
  mutate(plot_date = as.Date(paste(year(date), month(date), mday(date)), format = "%Y %m %d")) |>
  mutate(state = case_when(operational &
                             reviewed ~ "Reviewed",
                           operational &
                             !reviewed ~ "Not Reviewed",
                           !operational ~ "Not Operational",
                           .default = NA_character_)) |>
  ggplot(aes(x = plot_date,
             y = half_hr,
             color = state,
             fill = state)) +
  geom_tile() +
  scale_fill_viridis_d(direction = -1,
                       name = "Sonar\nState") +
  scale_color_viridis_d(direction = -1,
                        name = "Sonar\nState") +
  facet_wrap(~ year,
             scales = "free_x") +
  scale_x_date(breaks = scales::breaks_pretty(7)) +
  labs(x = "Date",
       y = "Hour") +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

#
#
# min_date = "0101"
# max_date = "0630"
#
# half_hr_periods <-
#   expand(sonar_raw,
#          year) |>
#   filter(year < 2025) |>
#   mutate(dt_df = map(year,
#                      .f = function(yr) {
#                        tibble(date_time = seq(from = as.POSIXct(ymd(paste(yr, min_date))),
#                                               to = as.POSIXct(ymd(paste(yr, max_date))) + days(1) - minutes(30),
#                                               by = "30 mins"))
#                      })) |>
#   unnest(dt_df) |>
#   mutate(date = floor_date(date_time,
#                            unit = "day"),
#          hour = floor_date(date_time,
#                            unit = "hour"),
#          time = difftime(date_time, date,
#                          units = "mins"),
#          hour = difftime(hour, date,
#                          units = "mins"),
#          across(c(time,
#                   hour),
#                 as.period)) |>
#   left_join(sonar_review,
#             by = join_by(year,
#                          date_time)) |>
#   # left_join(sonar_raw %>%
#   #             mutate(data_reviewed = if_else(data_recorded %in% c("None", "Partial", "Poor Image"),
#   #                                            "Not reviewed",
#   #                                            data_reviewed)) %>%
#   #             mutate(date_time = date + hour) |> #,
#   #                    # across(date_time,
#   #                    #        ~ ymd_hms(as.character(.), tz = "America/Los_Angeles"))) |>
#   #             select(year, date_time,
#   #                    data_recorded,
#   #                    data_reviewed) %>%
#   #             distinct() %>%
#   #             filter(data_recorded %in% c("Full")) |>
#   #             mutate(reviewed = if_else(data_reviewed == "Not reviewed",
#   #                                       FALSE,
#   #                                       TRUE))) %>%
#   mutate(
#     across(
#       reviewed,
#       ~ replace_na(.,
#                    FALSE)
#     )) %>%
#   # mutate(reviewed = if_else(!is.na(data_reviewed) & data_reviewed == "No data",
#   #                           F, reviewed)) %>%
#   mutate(data_recorded = if_else(is.na(data_recorded) &
#                                    lag(data_recorded == "Full") &
#                                    lag(data_reviewed == "First 30"),
#                                  "Full",
#                                  data_recorded)) %>%
#   mutate(operational = if_else(reviewed |
#                                  data_recorded == "Full",
#                                T, F)) %>%
#   mutate(hr_fct = hrs_fct_grp[as.character(hour(date_time))]) %>%
#   relocate(hour, hr_fct, .after = "time") %>%
#   # fix one data_reviewed entry
#   mutate(data_reviewed = if_else(str_detect(time, "30M") &
#                                    data_recorded == "Full" &
#                                    data_reviewed == "First 30",
#                                  "Second 30",
#                                  data_reviewed)) %>%
#   # fix some NAs in data review
#   mutate(data_reviewed = if_else(operational &
#                                    data_recorded == "Full" &
#                                    is.na(data_reviewed),
#                                  "Not reviewed",
#                                  data_reviewed)) %>%
#   mutate(across(
#     operational,
#     ~ replace_na(., F)
#   )) |>
#   mutate(across(data_recorded,
#                 ~ replace_na(., "No data")),
#          across(data_reviewed,
#                 ~ replace_na(., "No data")),
#          across(data_reviewed,
#                 ~ if_else(data_recorded == "No data" & . == "Not reviewed",
#                           "No data",
#                           .)))

# which hours had the 2nd 30 min reviewed?
full_hrs <-
  half_hr_periods %>%
  filter(data_reviewed %in% c("First 30",
                              "Second 30")) %>%
  group_by(date, hour) %>%
  mutate(n_pers = n(),
         n_first = sum(str_detect(data_reviewed, "First")),
         n_second = sum(str_detect(data_reviewed, "Second"))) %>%
  # filter(year == 2022) |>
  # as.data.frame() |> head(10)
  filter(n_pers == 2,
         n_first == 1,
         n_second == 1) %>%
  arrange(date_time) %>%
  ungroup() %>%
  select(year,
         date_time,
         date,
         hour) |>
  distinct() |>
  mutate(full_hr = T)

# add column indicating if full hour was reviewed
quarter_hr_periods <-
  quarter_hr_periods %>%
  left_join(full_hrs |>
              select(date,
                     hour,
                     full_hr) |>
              distinct(),
            by = join_by(date, hour)) |>
  mutate(across(
    full_hr,
    ~ replace_na(., F)
  ))

half_hr_periods <-
  half_hr_periods %>%
  left_join(full_hrs |>
              select(date,
                     hour,
                     full_hr) |>
              distinct(),
            by = join_by(date, hour)) |>
  mutate(across(
    full_hr,
    ~ replace_na(., F)
  ))

sum(duplicated(quarter_hr_periods$date_time))
sum(duplicated(half_hr_periods$date_time))

# consolidate into hour periods
hr_periods <-
  half_hr_periods |>
  reframe(across(date_time,
                 ~ floor_date(.,
                              unit = "hours")),
          across(c(data_recorded,
                   data_reviewed,
                   reviewed,
                   operational,
                   full_hr),
                 ~ paste(., collapse = "_")),
          .by = c(year, date, hour, hr_fct)) |>
  distinct() |>
  relocate(date_time,
           .after = "hour") |>
  mutate(
    across(
      data_recorded,
      ~ recode(.,
               "Full_Full" = "Full",
               "Full_No data" = "Partial",
               "No data_Full" = "Partial",
               "No data_No data" = "No data")),
    across(
      data_reviewed,
      ~ recode(.,
               "First 30_Not reviewed" = "First 30 Only",
               "First 30_Second 30" = "Full Hour",
               "No data_No data" = "No data",
               "Not reviewed_No data" = "No data",
               "No data_Second 30" = "Second 30 Only")),
    across(
      reviewed,
      ~ if_else(str_detect(., "TRUE"),
                T,
                F)),
    across(operational,
           ~ if_else(data_recorded == "No data",
                     F, T)),
    across(full_hr,
           ~ if_else(. == "TRUE_TRUE",
                     T, F))
  )


# generate percent of each time step sonar was operational
quarter_hr_op <-
  quarter_hr_periods |>
  group_by(year,
           date,
           time,
           hour) %>%
  summarize(across(date_time,
                   min),
            tot_pers = n(),
            op_pers = sum(operational),
            op_perc = op_pers / tot_pers,
            .groups = "drop")

half_hr_op <-
  half_hr_periods %>%
  group_by(year,
           date,
           time,
           hour,
           hr_fct) %>%
  summarize(across(date_time,
                   min),
            tot_pers = n(),
            op_pers = sum(operational),
            op_perc = op_pers / tot_pers,
            .groups = "drop")

hr_op <- half_hr_periods %>%
  group_by(year,
           date,
           hour,
           hr_fct,) %>%
  summarize(across(date_time,
                   min),
            tot_pers = n(),
            op_pers = sum(operational),
            op_perc = op_pers / tot_pers,
            .groups = "drop")

hrs_op <- half_hr_periods %>%
  group_by(year,
           date,
           hr_fct) %>%
  summarize(hour = hour[date_time == min(date_time)],
            across(date_time,
                   min),
            tot_pers = n(),
            op_pers = sum(operational),
            op_perc = op_pers / tot_pers,
            .groups = "drop") |>
  select(any_of(names(half_hr_op)))

day_op <- half_hr_periods %>%
  group_by(year,
           date) %>%
  summarize(hour = hour[date_time == min(date_time)],
            hr_fct = hr_fct[date_time == min(date_time)],
            across(date_time,
                   min),
            tot_pers = n(),
            op_pers = sum(operational),
            op_perc = op_pers / tot_pers,
            .groups = "drop") |>
  select(any_of(names(half_hr_op)))

ops_df <-
  tibble(time_scale = as_factor(c("15 min",
                                  "Half Hour",
                                  'Hour',
                                  paste(24 / max(hrs_fct_grp), 'Hour Block'),
                                  'Day')),
         ops = list(quarter_hr_op,
                    half_hr_op,
                    hr_op,
                    hrs_op,
                    day_op))


#--------------------------------------------------
# pull out records of fish detections
#--------------------------------------------------
sonar_fish <-
  sonar_raw %>%
  # made the decision to drop 2018 data for a variety of reasons
  # filter(year != 2018) |>
  # decide to filter all observations after June 15
  filter(date <= ymd(paste(year, "0615"))) |>
  filter(data_recorded != "Partial",
         confidence == 1) %>%
  filter(!is.na(frame)) %>%
  # mutate(sthd_length = if_else(length > 67, T, F)) %>%
  mutate(notes = time,
         across(time,
                hms)) |>
  select(-c(behavior,
            bc_review,
            flag,
            bc_comments_qaqc))

#--------------------------------------------------
# species composition data
#--------------------------------------------------
# using new file from Bethany Craig with 2021 - 2024 data
spp_comp_file <- "Dungeness_sppcomp_data_2024_BC.xlsx"
# now includes 2025
spp_comp_file <- "Dungeness_sppcomp_data_2025_08272025.xlsx"

excel_sheets(here("analysis",
                  "data",
                  "raw_data",
                  spp_comp_file))

spp_comp <-
  tibble(year = excel_sheets(here("analysis",
                                  "data",
                                  "raw_data",
                                  spp_comp_file))) |>
  mutate(across(year,
                as.integer)) |>
  mutate(spp_comp_data = map(year,
                             .f = function(x) {
                               yr_data <-
                                 read_excel(here("analysis",
                                                 "data",
                                                 "raw_data",
                                                 spp_comp_file),
                                            sheet = as.character(x),
                                            skip = case_when(x < 2025 ~ 2,
                                                             x == 2025 ~ 0,
                                                             .default = 0)) |>
                                 clean_names() |>
                                 rlang::set_names(nm =
                                                    function(nm) {
                                                      str_replace(nm,
                                                                  "^tag_",
                                                                  "floy_tag_") |>
                                                        str_replace("girth$",
                                                                    "girth_mm") |>
                                                        str_replace("dorsal_height$",
                                                                    "dorsal_height_mm") |>
                                                        str_replace("_length$",
                                                                    "_length_mm") |>
                                                        str_replace("acoutic",
                                                                    "acoustic")
                                                    }) |>
                                 mutate(across(floy_tag_number,
                                               as.character),
                                        across(flow_cfs,
                                               ~ str_remove(., "^~")),
                                        across(c(flow_cfs,
                                                 count,
                                                 ends_with("_mm")),
                                               as.numeric)) |>
                                 filter(!is.na(date))

                               return(yr_data)
                             })) |>
  unnest(spp_comp_data) |>
  select(-x27) |>
  rename(comments = condition_comments) |>
  mutate(fork_length_cm = fork_length_mm / 10,
         poh_length_cm = poh_length_mm / 10) |>
  relocate(fork_length_cm,
           .after = "fork_length_mm") |>
  relocate(poh_length_cm,
           .after = "poh_length_mm") |>
  mutate(across(species,
                ~ fct_recode(.,
                             "Bull Trout" = "BT",
                             "Cutthroat" = "CT",
                             "Rainbow" = "RB",
                             "Steelhead" = "SH",
                             "Chinook" = "CH")),
         across(c(species,
                  floy_tag_color),
                str_to_title),
         across(floy_tag_color,
                ~ fct_recode(.,
                             NULL = "No Tag")),
         across(gear,
                str_to_lower),
         across(gear,
                ~ fct_recode(.,
                             "hook and line" = "h&l",
                             "hook and line" = "hl",
                             "tangle net" = "tn")),
         across(site,
                str_to_title),
         across(site,
                ~ str_replace(.,
                              "Usgs", "USGS") |>
                  str_replace("Us", "US") |>
                  str_replace("Ds", "DS") |>
                  str_replace("Gage", "Gauge") |>
                  str_replace("School House", "Schoolhouse")),
         across(acoustic_tag_id_number,
                ~ case_when(is.na(.) &
                              !is.na(acoustic_tag_number) ~ str_split_i(acoustic_tag_number, "\\/", 1) |>
                              as.numeric(),
                            .default = .)),
         across(acoustic_tag_serial_number,
                ~ case_when(is.na(.) &
                              !is.na(acoustic_tag_number) ~ str_split_i(acoustic_tag_number, "\\/", 2) |>
                              as.numeric(),
                            .default = .))) |>
  select(-acoustic_tag_number) |>
  mutate(across(starts_with("recapture"),
                ~ case_when(. == "Y" ~ TRUE,
                            . == "N" ~ FALSE,
                            .default = NA))) |>
  arrange(date,
          species,
          count)

# add some missing lengths to fish that have been recaptured without length data
# known lengths
known_length <-
  spp_comp |>
  filter(!is.na(fork_length_cm),
         (!is.na(floy_tag_number) |
            !is.na(acoustic_tag_id_number) |
            !is.na(tissue_number_bar_code_number))) |>
  select(year,
         known_date = date,
         species,
         floy_tag_number,
         acoustic_tag_id_number,
         acoustic_tag_serial_number,
         tissue_number_bar_code_number,
         old_fl_cm = fork_length_cm,
         old_fl_mm = fork_length_mm)

spp_comp |>
  filter(is.na(fork_length_cm),
         !is.na(species),
         species != "No Catch") |>
  left_join(known_length |>
              select(-c(starts_with("acoustic"),
                        tissue_number_bar_code_number)) |>
              filter(!is.na(floy_tag_number)),
            by = join_by(year,
                         species,
                         floy_tag_number,
                         closest(date >= known_date))) |>
  rename(known_date_floy = known_date) |>
  left_join(known_length |>
              select(-c(starts_with("floy"),
                        tissue_number_bar_code_number)) |>
              filter(!is.na(acoustic_tag_id_number)),
            by = join_by(year,
                         species,
                         acoustic_tag_id_number,
                         acoustic_tag_serial_number,
                         closest(date >= known_date))) |>
  rename(known_date_acoustic = known_date) |>
  mutate(known_date = case_when(!is.na(known_date_acoustic) ~ known_date_acoustic,
                               !is.na(known_date_floy) ~ known_date_floy,
                               .default = NA_Date_)) |>
  select(year:species,
         known_date,
         acoustic_tag_id_number,
         acoustic_tag_serial_number,
         floy_tag_number,
         fork_length_cm,
         contains("old_fl")) |>
  mutate(date_diff = difftime(date, known_date, units = "days")) |>
  arrange(desc(date_diff))


spp_comp <-
  spp_comp |>
  left_join(known_length |>
              select(-c(starts_with("floy"),
                        tissue_number_bar_code_number)) |>
              filter(!is.na(acoustic_tag_id_number)),
            by = join_by(year,
                         species,
                         acoustic_tag_id_number,
                         acoustic_tag_serial_number,
                         closest(date >= known_date))) |>
  mutate(across(fork_length_cm,
                ~ case_when(is.na(.) &
                              !is.na(old_fl_cm) ~ old_fl_cm,
                            .default = .)),
         across(fork_length_mm,
                ~ case_when(is.na(.) &
                              !is.na(old_fl_mm) ~ old_fl_mm,
                            .default = .))) |>
  select(all_of(names(spp_comp))) |>
  left_join(known_length |>
              select(-c(starts_with("acoustic"),
                        tissue_number_bar_code_number)) |>
              filter(!is.na(floy_tag_number)),
            by = join_by(year,
                         species,
                         floy_tag_number,
                         closest(date >= known_date))) |>
  mutate(across(fork_length_cm,
                ~ case_when(is.na(.) &
                              !is.na(old_fl_cm) ~ old_fl_cm,
                            .default = .)),
         across(fork_length_mm,
                ~ case_when(is.na(.) &
                              !is.na(old_fl_mm) ~ old_fl_mm,
                            .default = .))) |>
  select(all_of(names(spp_comp))) |>
  mutate(across(fork_length_cm,
                ~ case_when(is.na(.) &
                              tissue_number_bar_code_number == "recapture" &
                              str_detect(comments, "22BJ0008") ~ known_length$old_fl_cm[known_length$tissue_number_bar_code_number == "22BJ0008" & !is.na(known_length$tissue_number_bar_code_number)],
                            .default = .)),
         across(fork_length_mm,
                ~ case_when(is.na(.) &
                              tissue_number_bar_code_number == "recapture" &
                              str_detect(comments, "22BJ0008") ~ known_length$old_fl_mm[known_length$tissue_number_bar_code_number == "22BJ0008" & !is.na(known_length$tissue_number_bar_code_number)],
                            .default = .)))

# what fish are left with missing lengths?
spp_comp |>
  filter(is.na(fork_length_cm),
         species != "No Catch") |>
  as.data.frame()
# only fish left wasn't actually captured (see comments)

spp_fl <-
  spp_comp |>
  filter(rmu < 6,
         (month(date) <= 6 |
            month(date) == 6 & mday(date) <= 15),
         (is.na(site) | str_detect(site, "Gray Wolf", negate = TRUE))) |>
  select(year,
         date,
         species,
         gear,
         mark_status,
         fork_length_cm) |>
  filter(!is.na(fork_length_cm)) |>
  mutate(spp_fct = if_else(species == "Steelhead",
                           1, 0)) %>%
  mutate(across(c(species,
                  spp_fct),
                as_factor)) %>%
  mutate(jday = yday(date),
         fl_mean = mean(fork_length_cm),
         fl_sd = sd(fork_length_cm),
         fl_z = (fork_length_cm - fl_mean) / fl_sd)

spp_fl |>
  tabyl(species,
        year) |>
  adorn_totals("both")

#--------------------------------------------------
# save data
#--------------------------------------------------
save(sonar_fish,
     sonar_raw,
     file = here("analysis/data/derived_data",
                 "sonar_data.rda"))

save(hrs_fct_grp,
     quarter_hr_periods,
     half_hr_periods,
     hr_periods,
     ops_df,
     file = here("analysis/data/derived_data",
                 "ops_data.rda"))

save(spp_comp,
     spp_fl,
     file = here("analysis/data/derived_data",
                 "spp_comp_data.rda"))

#----------------------------------------------------
load(here("analysis/data/derived_data",
          "ops_data.rda"))

half_hr_periods %>%
  filter(month(date) < 6 |
           (month(date) == 6 & day(date) <= 15)) |>
  mutate(hr = as.numeric(hour) / (60*60)) %>%
  mutate(date = as.Date(paste(month(date), mday(date)), format = "%m %d")) %>%
  ggplot(aes(x = date,
             y = hr,
             color = operational,
             fill = operational)) +
  geom_tile() +
  scale_fill_viridis_d(direction = -1,
                       name = "Sonar\nOperational") +
  scale_color_viridis_d(direction = -1,
                        name = "Sonar\nOperational") +
  facet_wrap(~ year,
             scales = "fixed") +
  scale_x_date(breaks = scales::breaks_pretty(7)) +
  labs(x = "Date",
       y = "Hour") +
  theme(legend.position = "bottom",
        text = element_text(size = 18)) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))

ggsave(here("analysis/figures",
            "sonar_operational.png"),
       width = 7,
       height = 6,
       bg = "transparent")

half_hr_periods %>%
  filter(month(date) < 6 |
           (month(date) == 6 & day(date) <= 15)) |>
  mutate(hr = as.numeric(hour) / (60*60),
         half_hr = as.numeric(time) / (60*60)) %>%
  mutate(date = as.Date(paste(month(date), mday(date)), format = "%m %d")) %>%
  ggplot(aes(x = date,
             y = half_hr,
             color = reviewed,
             fill = reviewed)) +
  geom_tile() +
  scale_fill_viridis_d(direction = -1,
                       name = "Sonar\nReviewed") +
  scale_color_viridis_d(direction = -1,
                        name = "Sonar\nReviewed") +
  facet_wrap(~ year,
             scales = "fixed") +
  scale_x_date(breaks = scales::breaks_pretty(7)) +
  labs(x = "Date",
       y = "Hour") +
  theme(legend.position = "bottom",
        text = element_text(size = 18)) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))

ggsave(here("analysis/figures",
            "sonar_reviewed.png"),
       width = 9,
       height = 6,
       bg = "transparent")


quarter_hr_periods |>
  # half_hr_periods |>
  mutate(hr = as.numeric(hour) / (60*60),
         half_hr = as.numeric(time) / (60*60)) %>%
  mutate(plot_date = as.Date(paste(year(date), month(date), mday(date)), format = "%Y %m %d")) |>
  mutate(state = case_when(operational &
                             reviewed ~ "Reviewed",
                           operational &
                             !reviewed ~ "Not Reviewed",
                           !operational ~ "Not Operational",
                           .default = NA_character_)) |>
  ggplot(aes(x = plot_date,
             y = half_hr,
             color = state,
             fill = state)) +
  geom_tile() +
  scale_fill_viridis_d(direction = -1,
                       name = "Sonar\nState") +
  scale_color_viridis_d(direction = -1,
                        name = "Sonar\nState") +
  facet_wrap(~ year,
             scales = "free_x") +
  scale_x_date(breaks = scales::breaks_pretty(7)) +
  labs(x = "Date",
       y = "Hour") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))

ggsave(here("analysis/figures",
            "sonar_state.png"),
       width = 9,
       height = 6,
       bg = "transparent")
