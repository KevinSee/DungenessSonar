# Author: Kevin See
# Purpose: Read in data from SONAR
# Created: 3/15/22
# Last Modified: 3/15/22
# Notes:

#-----------------------------------------------------------------
# load packages
library(tidyverse)
library(here)
library(magrittr)
library(janitor)
library(lubridate)
library(ggfortify)

theme_set(theme_bw())

#-----------------------------------------------------------------
# sonar data
sonar_raw <- read_csv(here("analysis/data/raw_data",
                           "2019 sonar.csv")) %>%
  mutate(across(Hour,
                hms)) %>%
  bind_rows(read_csv(here("analysis/data/raw_data",
                          "2020 sonar.csv")) %>%
              mutate(across(Hour,
                            str_pad,
                            width = 5,
                            side = "left",
                            pad = "0")) %>%
              mutate(across(Hour,
                            hm)) %>%
              rename(comments = `Comments/Notes`) %>%
              clean_names("upper_camel")) %>%
  bind_rows(read_csv(here("analysis/data/raw_data",
                          "2021 sonar.csv")) %>%
              mutate(across(Hour,
                            ~ hm(paste(str_sub(., 1,2),
                                       str_sub(., 3, 4),
                                       sep = ":")))) %>%
              rename(comments = `Comments/Notes`) %>%
              clean_names("upper_camel")) %>%
  filter(!is.na(Year)) %>%
  mutate(across(Date,
                mdy)) %>%
  mutate(across(Time,
                recode,
                "No Data" = "No data",
                "no fish" = "No fish",
                "No Fish" = "No fish"),
         Time = if_else(str_detect(Comments, "No data"),
                        "No data",
                        Time),
         across(Direction,
                recode,
                "Downstream" = "downstream",
                "Upstream" = "upstream")) %>%
  clean_names() %>%
  mutate(date_time = date + hour)


# pull out records of fish detections
sonar_fish <- sonar_raw %>%
  filter(!is.na(frame)) %>%
  mutate(sthd_length = if_else(length >= 68, T, F)) %>%
  mutate(across(time,
                hms))

#-----------------------------------------------------------------
# determine operational times
hrs_summ <- sonar_raw %>%
  group_by(year) %>%
  summarize(across(date_time,
                   list(min = min,
                        max = max),
                   na.rm = T,
                   .names = "{.fn}")) %>%
  mutate(across(c(min, max),
                floor_date,
                unit = "hour")) %>%
  mutate(n_hrs = difftime(max, min, units = "hours"),
         across(n_hrs,
                as.numeric)) %>%
  rowwise() %>%
  mutate(hrs = list(min + dhours(0:n_hrs)),
         ints = list(interval(start = hrs,
                              end = lead(hrs) - dseconds(1))))

hr_periods <- hrs_summ %>%
  select(year, ints) %>%
  unnest(ints) %>%
  mutate(start_int = int_start(ints),
         end_int = int_end(ints),
         end_int = if_else(is.na(end_int),
                           start_int,
                           end_int)) %>%
  mutate(ints = interval(start = start_int,
                         end = end_int)) %>%
  select(-start_int,
         -end_int) %>%
  rowwise() %>%
  mutate(operational = if_else(sum(sonar_raw$date_time %within% ints, na.rm = T) > 0,
                               T, F)) %>%
  ungroup() %>%
  mutate(date = floor_date(int_start(ints), "day"),
         date_time = floor_date(int_start(ints), "hour"),
         hour = difftime(date_time, date,
                         units = "hours"),
         across(hour,
                as.period)) %>%
  select(year, date, hour, everything()) %>%
  arrange(date_time)

#-----------------------------------------------------------------
# species composition
# species comp by tangle netting
spp_comp <- read_csv(here("analysis/data/raw_data",
                          "species comp.csv")) %>%
  clean_names() %>%
  mutate(across(date,
                mdy)) %>%
  select(date:sthd, notes)

# species comp by length
spp_fl <- readxl::read_excel(here("analysis/data/raw_data",
                                  "Species Comp ALL.xlsx"),
                             "2021 lengths") %>%
  clean_names() %>%
  mutate(spp = recode(species,
                      "Resident rainbow" = "Resident RB")) %>%
  select(spp,
         survey_date,
         fork_length) %>%
  mutate(across(spp,
                as_factor)) %>%
  mutate(fl_mean = mean(fork_length),
         fl_sd = sd(fork_length),
         fl_z = (fork_length - fl_mean) / fl_sd)

# # older version
# spp_fl <- readxl::read_excel(here("analysis/data/raw_data",
#                                   "Species Comp ALL.xlsx"),
#                              "2021",
#                              range = "S3:U38") %>%
#   mutate(across(everything(),
#                 ~ if_else(. > 100,
#                           . / 10, .))) %>%
#   pivot_longer(everything(),
#                names_to = "spp",
#                values_to = "fork_length",
#                values_drop_na = T) %>%
#   mutate(across(spp,
#                 as_factor)) %>%
#   mutate(fl_mean = mean(fork_length),
#          fl_sd = sd(fork_length),
#          fl_z = (fork_length - fl_mean) / fl_sd)

#-----------------------------------------------------------------
