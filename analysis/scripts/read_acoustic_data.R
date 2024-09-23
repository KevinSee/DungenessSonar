# Author: Kevin See
# Purpose: read in acoustic tag data
# Created: 9/13/24
# Last Modified: 9/13/24
# Notes: this data is from the Dungeness
# one receiver was stolen (one of lowest ones), so missing most of March
# replaced once it was noticed

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(readxl)
library(PITcleanr)
library(sf)
library(ggrepel)

theme_set(theme_bw())

#-----------------------------------------------------------------
# all steelhead tags
sthd_tags <-
  read_excel(here("analysis/data/raw_data",
                  "MostRecentDL_09042024",
                  "Steelhead_extAccousticTelem_2024_Tags.xlsx")) |>
  clean_names() |>
  # fill in missing sites with a random name
  mutate(across(site,
                ~ case_when(transmitter_id == "A69-1604-34025" ~ "Missing Site 1",
                            transmitter_id == "A69-1604-34033" ~ "Missing Site 2",
                            .default = .)))

# receiver info
rec_info <-
  read_excel(here("analysis/data/raw_data",
                  "MostRecentDL_09042024",
                  "DungenessR_receivers.xlsx")) |>
  clean_names() |>
  rename(notes = x16)

# construct a configuration file
# ptagis_config <- buildConfig()
config_file <-
  rec_info |>
  mutate(config_id = 100,
         antenna_id = "01",
         site_type = "RT",
         end_date = NA_Date_) |>
  select(site_code = serial_number,
         config_id,
         antenna_id,
         node = serial_number,
         start_date = date_deployed,
         end_date,
         site_type,
         site_name = station_name,
         latitude,
         longitude) |>
  mutate(across(c(site_code,
                  node),
                as.character)) |>
  # add tagging sites
  bind_rows(sthd_tags |>
              mutate(start_date = min(tagging_release_date),
                     end_date = NA_Date_,
                     site_type = "Tag") |>
              select(site_code = site,
                     node = site,
                     start_date,
                     end_date,
                     site_type,
                     site_name = site,
                     latitude,
                     longitude) |>
              distinct())


#-----------------------------------------------------------------
#
sites_sf <-
  config_file |>
  st_as_sf(coords = c("longitude",
                      "latitude"),
           crs = 4326)


flowlines_list <-
  queryFlowlines(sites_sf,
                 root_site_code = config_file |>
                   filter(latitude == max(latitude)) |>
                   pull(site_code))

flowlines <-
  flowlines_list$flowlines |>
  st_transform(crs = st_crs(sites_sf))

basin <-
  flowlines_list$basin |>
  st_transform(crs = st_crs(sites_sf))

# map of basin and sites
map_p <-
  ggplot() +
  geom_sf(data = basin) +
  geom_sf(data = flowlines,
          color = "blue") +
  geom_sf(data = sites_sf,
          aes(color = site_type),
          size = 3) +
  geom_label_repel(data = sites_sf,
                   aes(label = site_code,
                       geometry = geometry),
                   stat = "sf_coordinates")
map_p
map_p +
  coord_sf(ylim = c(48.05, 48.16))


# build parent_child
parent_child <-
  sites_sf |>
  filter(site_type == "RT") |>
  buildParentChild(flowlines,
                   # rm_na_parent = T,
                   ) |>
  select(-ends_with("hydro")) |>
  # manually add info about tagging sites
  bind_rows(
    sites_sf |>
      filter(site_type != "RT") |>
      st_drop_geometry() |>
      select(parent = site_code) |>
      mutate(child = case_when(parent %in% c("Meat Hole",
                                             "Sonar city",
                                             "Schoolhouse pool") ~ "488878",
                               parent %in% c("Missing Site 1",
                                             "Missing Site 2",
                                             "Wardside run",
                                             "Post Biden Run") ~ "488880",
                               .default = NA_character_))) #|>
  # bind_rows(sites_sf |>
  #             filter(site_type != "RT") |>
  #             st_drop_geometry() |>
  #             select(child = site_code) |>
  #             mutate(parent = case_when(child %in% c("Meat Hole",
  #                                                    "Sonar city",
  #                                                    "Schoolhouse pool") ~ "490933",
  #                                       child %in% c("Missing Site 1",
  #                                                    "Missing Site 2",
  #                                                    "Wardside run",
  #                                                    "Post Biden Run") ~ "488878",
  #                                       .default = NA_character_))) |>
  # filter(!(parent == "490933" & child == "488878"),
  #        !(parent == "488878" & child == "488880"))

plotNodes(parent_child,
          label_size = 3)

buildPaths(parent_child,
           direction = "d")
listParents("488878",
            parent_child)

#-----------------------------------------------------------------
# download detections
det_df <-
  tibble(folder_loc = list.dirs(here("analysis/data/raw_data",
                                      "MostRecentDL_09042024"),
                                 full.names = T)) |>
  slice(-1) |>
  mutate(dets = map(folder_loc,
                    .f = function(x) {
                      tibble(file_name = list.files(x)) |>
                        filter(str_detect(file_name, "csv$")) |>
                        mutate(file_dets = map(file_name,
                                               .f = function(nm) {
                                                 read_csv(paste(x, nm, sep = "/"),
                                                          show_col_types = F) |>
                                                   clean_names()
                                               })) |>
                        unnest(file_dets)
                    })) |>
  select(dets) |>
  unnest(dets) |>
  select(station_name,
         receiver,
         transmitter,
         date_and_time_utc) |>
  mutate(event_type_name = "Observation") |>
  # add detections from tagging
  bind_rows(sthd_tags |>
              select(station_name = site,
                     receiver = site,
                     transmitter = transmitter_id,
                     date_and_time_utc = tagging_release_date) |>
              mutate(event_type_name = "Mark"))

# pull out steelhead tags and compress the data
sthd_comp <-
  det_df |>
  mutate(event_site_code_value = str_remove(receiver, "^VR2Tx-"),
         # event_type_name = "Observation",
         antenna_group_configuration_value = 100,
         antenna_id = "01") |>
  rename(tag_code = transmitter,
         event_date_time_value = date_and_time_utc) |>
  filter(tag_code %in% sthd_tags$transmitter_id) |>
  # readCTH() |>
  compress(configuration = config_file,
           units = "days",
           ignore_event_vs_release = T,
           filter_orphan_disown_tags = F) |>
  left_join(sthd_tags |>
              select(tag_code = transmitter_id,
                     sex)) |>
  relocate(sex,
           .after = "tag_code")

# save as Excel file
writexl::write_xlsx(sthd_comp,
                    here("analysis/data/derived_data",
                         "PITcleanr_2024.xlsx"))


# out of all steelhead tags, how many had any acoustic detections
nrow(sthd_tags)
sthd_comp |>
  filter(slot > 1) |>
  select(tag_code) |>
  distinct() |>
  nrow()

sthd_comp |>
  filter(event_type_name == "Observation") |>
  group_by(node) |>
  summarize(across(n_dets,
                   sum),
            .groups = "drop")


# sthd_comp |>
#   # filter(slot > 1) |>
#   pull(n_dets) |> sum()
#
# det_df |>
#   filter(transmitter %in% sthd_tags$transmitter_id)
#

sthd_comp |>
  # tabyl(node)
  filter(node %in% c("488878",
                     "490933"))



sthd_comp |>
  addDirection(parent_child)


sthd_comp |>
  filter(event_type_name != "Mark") |>
  select(tag_code,
         sex) |>
  distinct() |>
  left_join(sthd_comp)


sthd_comp |>
  filter(node %in% c("488878",
                     "490933")) |>
  select(tag_code) |>
  distinct() |>
  left_join(sthd_comp)
