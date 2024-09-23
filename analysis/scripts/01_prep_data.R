# Author: Kevin See
# Purpose: Read in data from SONAR
# Created: 3/15/22
# Last Modified: 5/14/24
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
sonar_raw <-
  read_csv(here("analysis/data/raw_data",
                           "2018 sonar.csv"),
                      show_col_types = FALSE) %>%
  mutate(across(Hour,
                hm),
         across(c(Length,
                  Frame),
                as.numeric)) %>%
  bind_rows(read_csv(here("analysis/data/raw_data",
                          "2019 sonar.csv"),
                     show_col_types = FALSE) %>%
              mutate(across(Hour,
                            hms))) %>%
  bind_rows(read_csv(here("analysis/data/raw_data",
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
              clean_names("upper_camel")) %>%
  bind_rows(read_csv(here("analysis/data/raw_data",
                          "2021 sonar.csv"),
                     show_col_types = FALSE) %>%
              mutate(across(Hour,
                            ~ hm(paste(str_sub(., 1,2),
                                       str_sub(., 3, 4),
                                       sep = ":")))) %>%
              rename(comments = "Comments/Notes") %>%
              clean_names("upper_camel")) %>%
  bind_rows(read_csv(here("analysis/data/raw_data",
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
              clean_names("upper_camel")) %>%
  bind_rows(read_csv(here("analysis/data/raw_data",
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
              clean_names("upper_camel")) |>
  filter(!is.na(Year)) %>%
  # mutate(across(Date,
  #               anytime)) |>
  mutate(across(Date,
                mdy)) %>%
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
                ~ if_else(. == "None" & (Time == "No data" | is.na(Time)),
                          "No data",
                          .)),
         across(DataReviewed,
                ~ if_else(DataRecorded == "No data" & str_detect(DataReviewed, "30"),
                          "No data",
                          .))) %>%
  clean_names() %>%
  mutate(date_time = date + hour) %>%
  mutate(across(time,
                ~ str_replace(., "^24", "23"))) |>
  arrange(date_time,
          frame)

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

# fix some mismatched times
fixed_hours <-
  sonar_raw |>
  mutate(row_num = 1:n()) |>
  filter(str_detect(time, "\\:")) |>
  mutate(across(time,
                ~ if_else(str_count(., "\\:") == 1,
                        paste0(., ":00"),
                        .))) |>
  mutate(hour_issue = if_else(!between(ymd_hms(paste(date, time)),
                                         date_time,
                                         date_time + minutes(30)),
                              T, F)) |>
  filter(hour_issue) |>
  mutate(half_hr = if_else(minute(hms(time)) >= 30, ":30", ":00"),
         new_hour = hm(paste(hour(hms(time)),
                             half_hr)),
         new_data_reviewed = if_else(half_hr == ":30",
                                     "Second 30",
                                     "First 30")) |>
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
                ~ if_else(str_detect(hour, "30M") &
                            . == "First 30",
                          "Second 30",
                          .)))

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
  select(data_recorded_2 = data_recorded,
         data_reviewed_2 = data_reviewed,
         date,
         date_time) |>
  distinct() |>
  mutate(across(date_time,
                ~ . - minutes(30))) |>
  mutate(has_second_30 = T) |>
  full_join(sonar_raw |>
              filter(minute(date_time) == 0) |>
              select(date,
                     date_time) |>
              distinct() |>
              mutate(has_first_30 = T)) |>
  mutate(across(ends_with("_30"),
                ~ replace_na(., F))) |>
  filter(has_second_30,
         !has_first_30)

missing_first_30



sonar_raw |>
  filter(data_reviewed == "Second 30") |>
  select(date) |>
  distinct() |>
  left_join(sonar_raw |>
              select(date,
                     date_time,
                     data_reviewed) |>
              distinct()) |>
  count(date,
        data_reviewed) |>
  pivot_wider(names_from = data_reviewed,
              values_from = n) |>
  filter(`First 30` < `Second 30`)



# add missing periods back in
sonar_raw <-
  sonar_raw |>
  bind_rows(missing_first_30 |>
              select(data_recorded = data_recorded_2,
                     date_time) |>
              mutate(year = year(date_time),
                     date = floor_date(date_time, unit = "days"),
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
                ~ case_when(date == ymd(20200218) & str_detect(hour, "^4H 30M") ~ "Full",
                            .default = .)),
         across(data_reviewed,
                ~ case_when(date == ymd(20200218) & str_detect(hour, "^4H 30M") ~ "Second 30",
                            .default = .)))

# fix the formatting for some times
sonar_raw <-
  sonar_raw |>
  mutate(n_colon = str_count(time, ":")) |>
  # filter(n_colon == 1) |>
  mutate(time = if_else(n_colon == 1 & !is.na(time),
                        paste0(time, ":00"),
                        time)) |>
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

#--------------------------------------------------
# which hours do we want to group together?
#--------------------------------------------------
# 6 hour blocks
hrs_fct_grp <- rep(1:4, each = 6) |>
  set_names(0:23)

# link to SONAR data to determine if SONAR was operational or data was reviewed
sonar_review <-
  sonar_raw %>%
  mutate(data_reviewed = if_else(data_recorded %in% c("None", "Partial", "Poor Image"),
                                 "Not reviewed",
                                 data_reviewed)) %>%
  mutate(date_time = date + hour) |>
  select(year,
         # date,
         # hour,
         date_time,
         data_recorded,
         data_reviewed) %>%
  distinct() %>%
  filter(data_recorded %in% c("Full")) |>
  mutate(reviewed = if_else(data_reviewed == "Not reviewed",
                            FALSE,
                            TRUE))

sum(duplicated(sonar_review$date_time))

# sonar_review |>
#   filter(date_time %in% date_time[duplicated(date_time)]) |>
#   select(date_time) |>
#   distinct() |>
#   left_join(sonar_raw)

# tz(sonar_review$date_time) = "America/Los_Angeles"

# set up tibble containing all half hour periods that sonar was operating
half_hr_periods <- tibble(year = sort(unique(sonar_raw$year))) |>
  mutate(min = ymd(paste(year, "0201"), tz = tz(sonar_review$date_time)),
         max = ymd(paste(year, "0731"), tz = tz(sonar_review$date_time))) |>
  mutate(across(c(min,
                  max),
                as.POSIXct),
         across(min,
                ~ floor_date(., unit = "days")),
         across(max,
                ~ ceiling_date(., unit = "days")),
         across(max,
                ~ . + dhours(23.5))) |>
  mutate(date_time = map2(min,
                          max,
                          .f = function(x, y) seq(x, y, by = 30*60))) %>%
  select(-min, -max) %>%
  unnest(date_time) %>%
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
                as.period)) |>
  left_join(sonar_review,
            by = join_by(year,
                         date_time)) |>
  # left_join(sonar_raw %>%
  #             mutate(data_reviewed = if_else(data_recorded %in% c("None", "Partial", "Poor Image"),
  #                                            "Not reviewed",
  #                                            data_reviewed)) %>%
  #             mutate(date_time = date + hour) |> #,
  #                    # across(date_time,
  #                    #        ~ ymd_hms(as.character(.), tz = "America/Los_Angeles"))) |>
  #             select(year, date_time,
  #                    data_recorded,
  #                    data_reviewed) %>%
  #             distinct() %>%
  #             filter(data_recorded %in% c("Full")) |>
  #             mutate(reviewed = if_else(data_reviewed == "Not reviewed",
  #                                       FALSE,
  #                                       TRUE))) %>%
  mutate(
    across(
      reviewed,
      ~ replace_na(.,
                   FALSE)
    )) %>%
  # mutate(reviewed = if_else(!is.na(data_reviewed) & data_reviewed == "No data",
  #                           F, reviewed)) %>%
  mutate(data_recorded = if_else(is.na(data_recorded) &
                                   lag(data_recorded == "Full") &
                                   lag(data_reviewed == "First 30"),
                                 "Full",
                                 data_recorded)) %>%
  mutate(operational = if_else(reviewed |
                                 data_recorded == "Full",
                               T, F)) %>%
  mutate(hr_fct = hrs_fct_grp[as.character(hour(date_time))]) %>%
  relocate(hour, hr_fct, .after = "time") %>%
  # fix one data_reviewed entry
  mutate(data_reviewed = if_else(str_detect(time, "30M") &
                                   data_recorded == "Full" &
                                   data_reviewed == "First 30",
                                 "Second 30",
                                 data_reviewed)) %>%
  # fix some NAs in data review
  mutate(data_reviewed = if_else(operational &
                                   data_recorded == "Full" &
                                   is.na(data_reviewed),
                                 "Not reviewed",
                                 data_reviewed)) %>%
  mutate(across(
    operational,
    ~ replace_na(., F)
  )) |>
  mutate(across(data_recorded,
                ~ replace_na(., "No data")),
         across(data_reviewed,
                ~ replace_na(., "No data")),
         across(data_reviewed,
                ~ if_else(data_recorded == "No data" & . == "Not reviewed",
                          "No data",
                          .)))

# which hours had the 2nd 30 min reviewed?
full_hrs <- half_hr_periods %>%
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
  select(all_of(names(half_hr_periods)))

# add column indicating if full hour was reviewed
half_hr_periods <- half_hr_periods %>%
  left_join(full_hrs |>
              mutate(full_hr = T)) |>
  mutate(across(
    full_hr,
    ~ replace_na(., F)
  ))

sum(duplicated(half_hr_periods$date_time))

# consolidate into hour periods
hr_periods <- half_hr_periods |>
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
half_hr_op <- half_hr_periods %>%
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

ops_df <- tibble(time_scale = as_factor(c("Half Hour",
                                          'Hour',
                                          paste(24 / max(hrs_fct_grp), 'Hour Block'),
                                          'Day')),
                 ops = list(half_hr_op,
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
  filter(month(date) < 6 |
           (month(date) == 6 & day(date) <= 15)) %>%
  filter(data_recorded != "Partial",
         confidence == 1) %>%
  filter(!is.na(frame)) %>%
  # mutate(sthd_length = if_else(length > 67, T, F)) %>%
  mutate(notes = time,
         across(time,
                hms))

#--------------------------------------------------
# species composition data
#--------------------------------------------------
# spp_comp_2021 <- read_excel(here("analysis/data/raw_data",
#                                  "Species Comp ALL.xlsx"),
#                             "2021 lengths") %>%
#   clean_names() %>%
#   mutate(spp = recode(species,
#                       "Resident rainbow" = "Resident RB")) %>%
#   rename(date = survey_date,
#          rml = rm_lower,
#          rmu = rm_upper,
#          age = scale_age,
#          mark_status = mark,
#          gear = survey_type,
#          fork_length_cm = fork_length,
#          poh_length_cm = poh) |>
#   mutate(fork_length_mm = fork_length_cm * 10,
#          poh_length_mm = poh_length_cm * 10)
#
# spp_comp_2022 <- read_excel(here("analysis/data/raw_data",
#                                  "Dungeness_sppcomp_data_2022_FINAL.xlsx"),
#                             "BullTrout",
#                             skip = 2) %>%
#   mutate(across(
#     contains("(mm)"),
#     as.numeric
#   )) |>
#   bind_rows(read_excel(here("analysis/data/raw_data",
#                             "Dungeness_sppcomp_data_2022_FINAL.xlsx"),
#                        "Steelhead",
#                        skip = 2) |>
#               mutate(across(
#                 contains("(mm)"),
#                 as.numeric
#               ))) %>%
#   bind_rows(read_excel(here("analysis/data/raw_data",
#                             "Dungeness_sppcomp_data_2022_FINAL.xlsx"),
#                        "Other",
#                        skip = 2) |>
#               mutate(across(
#                 contains("(mm)"),
#                 as.numeric
#               ))) %>%
#   clean_names() |>
#   rename(comments = condition_comments) |>
#   mutate(fork_length_cm = fork_length_mm / 10,
#          poh_length_cm = poh_length_mm / 10)
#
# spp_comp_old <- spp_comp_2021 |>
#   bind_rows(spp_comp_2022 |>
#               filter(!is.na(as.numeric(count)))) |>
#   select(all_of(intersect(names(spp_comp_2022), names(spp_comp_2021)))) |>
#   mutate(year = year(date)) |>
#   relocate(year, .before = 1) |>
#   relocate(fork_length_cm,
#            .after = "fork_length_mm") |>
#   relocate(poh_length_cm,
#            .after = "poh_length_mm") |>
#   mutate(across(gear,
#                 ~ fct_relabel(.,
#                               ~ if_else(str_detect(., "Hook"),
#                                         "hook and line",
#                                         .))),
#          across(gear,
#                 ~ fct_relabel(.,
#                               ~ str_to_lower(.))),
#          across(species,
#                 ~ recode(.,
#                          "RAINBOW" = "Resident rainbow",
#                          "Bull Trout" = "Bull trout")),
#          across(mark_status,
#                 ~ recode(.,
#                          "AD" = "Marked",
#                          "UM" = "Unmarked")),
#          across(mark_status,
#                 ~ if_else(species == "Bull trout" & is.na(.),
#                           "Unmarked",
#                           .)))

# using new file from Kathryn Sutton
# spp_comp_file <- "Dungeness_sppcomp_data_2023_FINAL.xlsx"
# spp_comp_file <- "Dungeness_sppcomp_data_2023_FINAL_KSupdate03012024.xlsx"
spp_comp_file <- "Dungeness_sppcomp_data_2023_FINAL_BC_KS05082024.xlsx"

spp_comp <-
  read_excel(here("analysis/data/raw_data",
                  spp_comp_file),
             "2023",
             skip = 2) %>%
  mutate(
    across(
      contains("(mm)"),
      ~ as.numeric(str_remove(., "~"))),
    across(
      Count,
      as.character
    )) |>
  rlang::set_names( ~ str_remove(., " \\(mm\\)$")) |>
  bind_rows(read_excel(here("analysis/data/raw_data",
                            spp_comp_file),
                       "2022",
                       skip = 2) |>
              mutate(across(
                contains("(mm)"),
                ~ as.numeric(str_remove(., "~"))
              )) |>
              rename("Floy Tag #" = "Tag #",
                     "Floy Tag Color" = "Tag Color") |>
              rlang::set_names( ~ str_remove(., " \\(mm\\)$"))) %>%
  bind_rows(read_excel(here("analysis/data/raw_data",
                            spp_comp_file),
                       "2021",
                       skip = 2) |>
              mutate(across(
                c(contains("(mm)"),
                  contains("Length"),
                  contains("Height"),
                  contains("Girth")),
                ~ as.numeric(str_remove(., "~"))
              )) |>
              rename("Floy Tag #" = "Tag #",
                     "Floy Tag Color" = "Tag Color") |>
              rlang::set_names( ~ str_remove(., " \\(mm\\)$"))) %>%
  clean_names() |>
  rename(comments = condition_comments,
         fork_length_mm = fork_length,
         poh_length_mm = poh_length) |>
  mutate(fork_length_cm = fork_length_mm / 10,
         poh_length_cm = poh_length_mm / 10) |>
  mutate(year = year(date)) |>
  relocate(year, .before = 1) |>
  relocate(fork_length_cm,
           .after = "fork_length_mm") |>
  relocate(poh_length_cm,
           .after = "poh_length_mm") |>
  mutate(across(gear,
                str_to_lower),
         # across(gear,
         #        ~ recode(.,
         #                 "gn" = "gill net")),
         across(site,
                str_to_title),
         across(site,
                ~ str_replace_all(., "Usgs", "USGS")),
         across(site,
                ~ str_replace_all(., "Ds", "DS")),
         across(site,
                ~ str_replace_all(., "Us", "US")),

         across(site,
                ~ str_replace_all(., "Bt", "BT")),
         across(site,
                ~ str_replace_all(., "Rb", "RB")),
         across(site,
                ~ str_replace_all(., "Sh", "SH")),
         across(site,
                ~ str_replace_all(., "Gage", "Gauge")),
         across(site,
                ~ str_remove(., "\\.$"))) |>
  mutate(across(c(species,
                  floy_tag_color),
                str_to_title),
         across(species,
                ~ case_when(count == "NO CATCH" ~ "No Catch",
                            .default = .)),
         across(count,
                ~ case_when(str_detect(., "-") ~ str_split(., "-", simplify = T)[,1],
                            .default = .)),
         across(count,
                as.numeric)) |>
  # mutate(across(floy_tag_number,
  #               ~ case_match(.,
  #                            "89/90" ~ "0089/0090",
  #                            "90/89" ~ "0089/0090",
  #                            "-" ~ NA_character_,
  #                            .default = .)),
  #        across(recapture,
  #               ~ case_match(.,
  #                            "Yes" ~ "Y",
  #                            "-" ~ NA_character_,
  #                            .default = .))) |>
mutate(across(starts_with("recapture"),
              ~ case_when(. == "Y" ~ TRUE,
                          . == "N" ~ FALSE,
                          .default = NA))) |>
  arrange(date,
          species,
          count)

# any fish described as a recapture in the comments?
spp_comp |>
  filter(str_detect(comments, regex("recap", ignore_case = T)) |
           recapture_prev_year |
           recapture_this_year |
           (floy_tag_number %in% floy_tag_number[duplicated(floy_tag_number)] &
              !is.na(floy_tag_number))) |>
  select(date, species,
         count,
         site,
         contains("recapture"),
         contains("floy_tag"),
         # scale_card_number,
         fork_length_cm,
         comments) |>
  as.data.frame()

# fish without a length?
miss_length <-
  spp_comp |>
  filter(species != "No Catch",
         is.na(fork_length_cm),
         recapture_this_year)

miss_length |>
  as.data.frame()
# all missing lengths are recaps, according to comments, and most are bull trout

# get all known bull trout lengths
known_bt_lngth <-
  spp_comp |>
  filter(species == "Bull Trout",
         !is.na(fork_length_cm)) |>
  select(known_date = date,
         species,
         floy_tag_number,
         old_fl = fork_length_cm) |>
  distinct()


# fill in missing lengths for bull trout
miss_length_bt <-
  miss_length |>
  filter(species == "Bull Trout") |>
  left_join(known_bt_lngth,
            by = join_by(species,
                         floy_tag_number,
                         closest(date >= known_date))) |>
  mutate(across(fork_length_cm,
                ~ case_when(is.na(.) ~ old_fl,
                            .default = .))) |>
  select(all_of(names(spp_comp)))

# one steelhead recap with missing length
miss_length |>
  filter(species == "Steelhead") |>
  as.data.frame()

# look at the initial record for that fish
spp_comp |>
  filter(date == ymd(20220309),
         species == "Steelhead") |>
  as.data.frame()

# fill in missing lengths for steelhead
miss_length_sh <-
  miss_length |>
  filter(species == "Steelhead") |>
  left_join(spp_comp |>
              filter(tissue_number_bar_code_number == "22BJ0008") |>
              select(known_date = date,
                     species,
                     old_fl = fork_length_cm) |>
              distinct(),
            by = join_by(species,
                         closest(date >= known_date))) |>
  mutate(across(fork_length_cm,
                ~ case_when(is.na(.) ~ old_fl,
                            .default = .))) |>
  select(all_of(names(spp_comp)))



spp_comp <-
  spp_comp |>
  anti_join(miss_length) |>
  bind_rows(miss_length_bt,
            miss_length_sh) |>
  arrange(date,
          species,
          count)


# spp_comp |>
#   filter(rmu < 6,
#          month(date) <= 6) |>
#   mutate(across(species,
#                 str_to_sentence)) |>
#   count(year, date, species,
#         name = "new") |>
#   filter(year %in% unique(spp_comp_old$year)) |>
#   full_join(spp_comp_old |>
#               filter((rmu < 6 | is.na(rmu)),
#                      month(date) <= 6) |>
#               count(year, date, species,
#                     name = "old")) |>
#   mutate(diff = new - old) |>
#   arrange(date,
#           species) |>
#   # filter(is.na(old))
#   # filter(is.na(new))
#   filter(diff != 0)


spp_fl <- spp_comp |>
# spp_fl <- spp_comp_old |>
  filter(rmu < 6,
         (month(date) <= 6 |
            month(date) == 6 & mday(date) <= 15),
         (is.na(site) | str_detect(site, "Gray Wolf", negate = TRUE))) |>
  select(date,
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
  mutate(year = year(date)) |>
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
