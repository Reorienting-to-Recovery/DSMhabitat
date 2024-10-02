# this code caches HRL data using curves from the SBR
# this uses HRL flows with EFF flows in dry years

library(DSMhabitat)
library(tidyverse)
library(lubridate)
library(DSMflow)
library(readxl)
library(sf)
library(riceflows4ff)


# create habitat functions ------------------------------------------------

# these are already created in another script
source("data-raw/cache_va_hrl_data.R")

# generate new habitat objects --------------------------------------------

flows <- DSMflow::flows_cfs$LTO_12a_eff_dy |> # TODO this has to be merged into main for DSMflow and re-downloaded
  filter(year(date) >= 1979 & year(date) <= 2000) |> 
  pivot_longer(`Antelope Creek`:`San Joaquin River`,
               names_to = "watershed",
               values_to = "flow_cfs") |>
  mutate(month = month(date),
         year = year(date))

update_hrl_habitat_from_SBR <- function(flows, watershed_name, hab_fn,
                                        is_rearing) {
  
  if(is_rearing) {
    filtered_flows <- flows |> 
      filter(year != 1979)
  } else {
    filtered_flows <- flows
  }
  
  new_hab <- filtered_flows |>
    filter(watershed == watershed_name) |>
    mutate(new_hab = hab_fn(flow_cfs)) |>
    pivot_wider(id_cols = month, 
                names_from = year, 
                values_from = new_hab) |>
    select(-month) |> 
    as.matrix()
  
  rownames(new_hab) <- month.abb
  
  return(new_hab)
  
}

# update spawning
# start with the baseline habitat run with HRL flows
# TODO we have to update this to call on baseline habitat
fr_spawn_update <- DSMhabitat::fr_spawn$r_to_r_lto_12a_baseline

# add together existing HFC and VA LFC for Feather
fr_spawn_update["Feather River", , ] <- DSMhabitat::acres_to_square_meters(update_hrl_habitat_from_SBR(flows, "Feather River",
                                                                                                       va_spawn_lfc, FALSE) + 
                                                                             update_hrl_habitat_from_SBR(flows, "Feather River",
                                                                                                         existing_spawn_hfc, FALSE))
fr_spawn_update["Mokelumne River", , ] <- update_hrl_habitat_from_SBR(flows, "Mokelumne River",
                                                                      mokelumne_spw_fn, FALSE)
fr_spawn_update["Tuolumne River", , ] <- update_hrl_habitat_from_SBR(flows, "Tuolumne River",
                                                                     tuolumne_spw_fn, FALSE)
fr_spawn <- DSMhabitat::fr_spawn
fr_spawn$r_to_r_hrl <- fr_spawn_update

# update ic rearing
fr_juv_update <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline
fr_juv_update["American River", , ] <- update_hrl_habitat_from_SBR(flows, "American River",
                                                                   american_juv_fn, TRUE)
# feather add together existing hfc and VA lfc
fr_juv_update["Feather River", , ] <- DSMhabitat::acres_to_square_meters(update_hrl_habitat_from_SBR(flows, "Feather River",
                                                                                                     va_rearing_lfc_juv, TRUE) +
                                                                           update_hrl_habitat_from_SBR(flows, "Feather River",
                                                                                                       existing_rearing_hfc_juv, TRUE))
fr_juv_update["Mokelumne River", , ] <- update_hrl_habitat_from_SBR(flows, "Mokelumne River",
                                                                    mokelumne_juv_fn, TRUE)
fr_juv_update["Tuolumne River", , ] <- update_hrl_habitat_from_SBR(flows, "Tuolumne River",
                                                                   tuolumne_juv_fn, TRUE)
fr_juv_update["Yuba River", , ] <- update_hrl_habitat_from_SBR(flows, "Yuba River",
                                                               yuba_juv_fn, TRUE)
fr_juv <- DSMhabitat::fr_juv
fr_juv$r_to_r_hrl <- fr_juv_update

# update fp rearing
fr_fp_update <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline
# feather fp function by itself is fine, not distinguished by HFC and LFC
fr_fp_update["Feather River", , ] <- DSMhabitat::acres_to_square_meters(update_hrl_habitat_from_SBR(flows, "Feather River",
                                                                                                    va_fp, TRUE))
fr_fp_update["Tuolumne River", , ] <- update_hrl_habitat_from_SBR(flows, "Tuolumne River",
                                                                  tuolumne_fp_fn, TRUE)
fr_fp_update["Yuba River", , ] <- update_hrl_habitat_from_SBR(flows, "Yuba River",
                                                              yuba_fp_fn, TRUE)
fr_fp <- DSMhabitat::fr_fp
fr_fp$r_to_r_hrl <- fr_fp_update

# now update objects
usethis::use_data(fr_spawn, overwrite = TRUE)
usethis::use_data(fr_juv, overwrite = TRUE)
usethis::use_data(fr_fp, overwrite = TRUE)



# Update floodplain -------------------------------------------------------

# Sutter

sutter_habitat_hrl <- DSMhabitat::sutter_habitat$lto_12a |> 
  as_tibble() |> 
  mutate(month = month.abb) |> 
  pivot_longer(`1980`:`2000`,
               names_to = "year",
               values_to = "sqm") |> 
  mutate(acres = DSMhabitat::square_meters_to_acres(sqm),
         total_acres = ifelse(month %in% c("Dec", "Jan", "Feb") & 
                                acres < 20000, 20000, acres),
         sqm = DSMhabitat::acres_to_square_meters(total_acres)) |> 
  pivot_wider(id_cols = month, 
              values_from = sqm,
              names_from = year) |> 
  select(-c(month)) |> 
  as.matrix() 

rownames(sutter_habitat_hrl) <- month.abb

sutter_habitat <- DSMhabitat::sutter_habitat
sutter_habitat$r_to_r_hrl <- sutter_habitat_hrl

usethis::use_data(sutter_habitat, overwrite = TRUE)


# fish food production ----------------------------------------------------

watersheds <- 
  riceflows4ff::ff_watersheds |> 
  mutate(watershed_group = case_when(
    watershed_name %in% c("Sutter Basin - Upper East", 
                          "Sutter Basin - Lower East",
                          "Gilsizer Slough-Snake River - Upper",
                          "Gilsizer Slough-Snake River - Lower") ~ "Sutter Basin",
    watershed_name %in% c("Colusa Basin Drainage Canal",
                          "Colusa Trough - Lower", 
                          "Colusa Trough - Middle",
                          "Colusa Trough - Upper",
                          "Sycamore Slough - Upper West", 
                          "Freshwater Creek",
                          "Stone Corral Creek", 
                          "Logan Creek", 
                          "Willow Creek", 
                          "Walker Creek",
                          "Colusa Drain") ~ "Colusa Basin")) |>
  filter(!is.na(watershed_group)) |>
  group_by(watershed_group) |>
  summarize() |>
  st_union(by_feature = TRUE)

wet_dry_areas <- 
  watersheds |>
  st_intersection(riceflows4ff::ff_wetdry) |>
  mutate(area_ac = st_area(geometry) |> 
           units::set_units("acres") |>
           units::drop_units()) |>
  group_by(watershed_group, wet_dry) |>
  summarize(area_ac = sum(area_ac)) |>
  ungroup() |>
  st_drop_geometry() |>
  pivot_wider(names_from = wet_dry, values_from = area_ac) |> 
  mutate(proportion = 20000 / Dry)

sac_proportion_added <- wet_dry_areas |>
  filter(watershed_group == "Colusa Basin") |> 
  pull(proportion)

sutter_proportion_added <- wet_dry_areas |>
  filter(watershed_group == "Sutter Basin") |> 
  pull(proportion)


# use methods for TMH to scale curve for Colusa (not sutter? already up to 20,000)

lower_sac_habitat_added <- DSMhabitat::fr_fp$lto_12a["Lower Sacramento River", , ] * (1 + sac_proportion_added)
lower_mid_sac_habitat_added <- DSMhabitat::fr_fp$lto_12a["Lower-mid Sacramento River", , ] * (1 + sac_proportion_added)
# scale on top of 20,000 added previously
# TODO check this assumption
sutter_habitat_added <- DSMhabitat::sutter_habitat$r_to_r_hrl * (1 + sutter_proportion_added)


sutter_habitat <- DSMhabitat::sutter_habitat
sutter_habitat$r_to_r_hrl[1:2,] <- sutter_habitat_added[1:2,]
rownames(sutter_habitat$r_to_r_hrl) <- month.abb

usethis::use_data(sutter_habitat, overwrite = TRUE)

fr_fp <- DSMhabitat::fr_fp
fr_fp$r_to_r_hrl["Lower Sacramento River", c("Jan", "Feb"), ] <- lower_sac_habitat_added[c("Jan", "Feb"),]
fr_fp$r_to_r_hrl["Lower-mid Sacramento River", c("Jan", "Feb"), ] <- lower_mid_sac_habitat_added[c("Jan", "Feb"),]

usethis::use_data(fr_fp, overwrite = TRUE)

