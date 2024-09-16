# this code caches HRL data using curves from the SBR
# outlined in a vignette add_SRB_hrl_data.Rmd

library(DSMhabitat)
library(tidyverse)
library(lubridate)
library(DSMflow)
library(readxl)
library(sf)
#remotes::install_github("flowwest/riceflows4ff")
library(riceflows4ff)


# American ----------------------------------------------------------------
american_hrl <- read_excel("data-raw/R2R_HRL_habitat_inputs/data/HRL_habitat_commitments_for_R2R_American.xlsx",
                           range = "A1:G10") |> 
  janitor::clean_names() |>
  mutate(duration_inundated_days = ifelse(duration_inundated_days == "N/A", NA, duration_inundated_days),
         water_year_type = ifelse(water_year_type == "N/A", NA, water_year_type),
         run = ifelse(run == "fall run", "fall", run)) |> 
  mutate(flow_cfs = as.numeric(flow_to_inundate_suitable_acres),
         flow_cfs = ifelse(is.na(flow_cfs), 20000, flow_cfs), 
         scenario = "HRL", 
         watershed = "American River",
         FR_juv_sqm_hrl = DSMhabitat::acres_to_square_meters(suitable_acres)) |> 
  select(watershed, flow_cfs, FR_juv_sqm_hrl, scenario)

existing_rearing <- DSMhabitat::american_river_instream |>
  filter(!is.na(FR_juv_sqm)) |> 
  select(flow_cfs, FR_juv_sqm)

existing_floodplain <- DSMhabitat::american_river_floodplain |> 
  filter(!is.na(FR_floodplain_acres)) |> 
  select(flow_cfs, FR_floodplain_acres)

existing_rearing_fn <- approxfun(existing_rearing$flow_cfs, existing_rearing$FR_juv_sqm, rule = 2)

existing_fp_fn <- approxfun(existing_floodplain$flow_cfs, existing_floodplain$FR_floodplain_acres, rule = 2)

existing_and_HRL <- american_hrl |> 
  mutate(existing_rear = existing_rearing_fn(flow_cfs),
         existing_fp = existing_fp_fn(flow_cfs),
         fp_HRL = ifelse(flow_cfs <= 5200, NA, DSMhabitat::square_meters_to_acres(FR_juv_sqm_hrl)),
         existing_and_HRL = ifelse(flow_cfs <= 5200, existing_rear + FR_juv_sqm_hrl, existing_fp + fp_HRL)) |> 
  select(-scenario)


# Feather -----------------------------------------------------------------

feather_hrl <- read_csv(here::here("data-raw", "R2R_HRL_habitat_inputs", "data", "feather_flow_habitat.csv")) |> 
  filter(scenario == "VA") |> 
  select(-location) |> 
  mutate(habitat_area_sqm = DSMhabitat::acres_to_square_meters(habitat_area_acres),
         scenario = "HRL") |> 
  select(watershed, flow_cfs, habitat_type, habitat_area_sqm) |> 
  pivot_wider(id_cols = c(watershed, flow_cfs), 
              names_from = habitat_type,
              values_from = habitat_area_sqm) |> 
  rename(FR_spw_sqm_hrl = Spawning,
         FR_juv_ic_sqm_hrl = `Instream rearing`,
         FR_juv_fp_sqm_hrl = `Floodplain rearing`)

# Mokelumne ---------------------------------------------------------------

spawning <- read_excel('data-raw/R2R_HRL_habitat_inputs/data/moke_flow_to_habitat_RB_edits.xlsx', 
                       sheet = 'spawning', 
                       range = 'A1:D5') |> 
  mutate(FR_spawn_acres_VA_enhanced = FR_spawn_acres_VA_enhanced + FR_spawn_acres_existing,
         FR_sqm_hrl = DSMhabitat::acres_to_square_meters(FR_spawn_acres_VA_enhanced),
         lifestage = "Spawning") |> 
  select(flow_cfs, FR_sqm_hrl, lifestage)

rearing <- read_excel('data-raw/R2R_HRL_habitat_inputs/data/moke_flow_to_habitat_RB_edits.xlsx', 
                      sheet = 'instream_rearing', 
                      range = 'A1:G17') |> 
  mutate(FR_juv_acres_VAadditional = 0.5*(FR_fry_acres_VA_enhanced),
         FR_fry_acres_VA_enhanced = 0.5*(FR_fry_acres_VA_enhanced) + FR_fry_acres_existing,
         FR_juv_acres_VA_enhanced = 0.5*(FR_juv_acres_VA_enhanced) + FR_juv_acres_existing,
         FR_juv_sqm_hrl = acres_to_square_meters(FR_juv_acres_VA_enhanced),
         FR_fry_sqm_hrl = acres_to_square_meters(FR_fry_acres_VA_enhanced)) |> 
  pivot_longer(FR_juv_sqm_hrl:FR_fry_sqm_hrl,
               names_to = "lifestage",
               values_to = "FR_sqm_hrl") |> 
  select(flow_cfs, FR_sqm_hrl, lifestage) 

mokelumne_hrl <- bind_rows(spawning, rearing) |> 
  mutate(scenario = "HRL",
         watershed = "Mokelumne") |> 
  pivot_wider(id_cols = flow_cfs,
              names_from = lifestage,
              values_from = FR_sqm_hrl) |> 
  rename(FR_spw_sqm_hrl = Spawning)

# Tuolumne ----------------------------------------------------------------

# this is done in tuolumne_river_r2r_hrl_habitat.rmd

# Yuba --------------------------------------------------------------------

# this is done in yuba_river_r2r_hrl_habitat.rmd


# create habitat functions ------------------------------------------------

# american
american_juv_fn <- approxfun(american_hrl$flow_cfs,
                             american_hrl$FR_juv_sqm_hrl,
                             rule = 2)

# feather
feather_spw_fn <- approxfun(feather_hrl$flow_cfs, feather_hrl$FR_spw_sqm_hrl,
                            rule = 2)
feather_juv_fn <- approxfun(feather_hrl$flow_cfs, feather_hrl$FR_juv_ic_sqm_hrl,
                            rule = 2)
feather_fp_fn <- approxfun(feather_hrl$flow_cfs, feather_hrl$FR_juv_fp_sqm_hrl,
                           rule = 2)

# mokelumne
mokelumne_spw_fn <- approxfun(mokelumne_hrl$flow_cfs,
                              mokelumne_hrl$FR_spw_sqm_hrl,
                              rule = 2)
mokelumne_juv_fn <- approxfun(mokelumne_hrl$flow_cfs,
                              mokelumne_hrl$FR_juv_sqm_hrl,
                              rule = 2)
# TODO not doing fry for mokelumne

# tuolumne
tuolumne_hrl <- read_csv(here::here("data-raw", "R2R_HRL_habitat_inputs", "data", "tuolumne_hrl.csv")) |> 
  mutate(fp_sqm_va = DSMhabitat::acres_to_square_meters(fp_acres_va))

tuolumne_spw_fn <- approxfun(tuolumne_hrl$flow_cfs,
                             tuolumne_hrl$fr_spw_sqm_va,
                             rule = 2)
tuolumne_juv_fn <- approxfun(tuolumne_hrl$flow_cfs,
                             tuolumne_hrl$fr_juv_sqm_va,
                             rule = 2)
tuolumne_fp_fn <- approxfun(tuolumne_hrl$flow_cfs,
                            tuolumne_hrl$fp_sqm_va,
                            rule = 2)

# yuba
yuba_hrl <- read_csv(here::here("data-raw", "R2R_HRL_habitat_inputs", "data", "yuba_hrl.csv"))

yuba_juv_fn <- approxfun(yuba_hrl$flow_cfs,
                         yuba_hrl$fr_juv_ic_sqm,
                         rule = 2)
yuba_fp_fn <- approxfun(yuba_hrl$flow_cfs,
                        yuba_hrl$fr_juv_fp_sqm,
                        rule = 2)


# generate new habitat objects --------------------------------------------

flows <- DSMflow::flows_cfs$LTO_12a |>
  # flows <- DSMflow::flows_cfs$biop_itp_2018_2019 |>
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
fr_spawn_update <- DSMhabitat::fr_spawn$r_to_r_lto_12a_baseline

fr_spawn_update["Feather River", , ] <- update_hrl_habitat_from_SBR(flows, "Feather River",
                                                                    feather_spw_fn, FALSE)
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
fr_juv_update["Feather River", , ] <- update_hrl_habitat_from_SBR(flows, "Feather River",
                                                                  feather_juv_fn, TRUE)
fr_juv_update["Mokelumne River", , ] <- update_hrl_habitat_from_SBR(flows, "Mokelumne River",
                                                                    mokelumne_juv_fn, TRUE)
fr_juv_update["Tuolumne River", , ] <- update_hrl_habitat_from_SBR(flows, "Tuolumne River",
                                                                   tuolumne_juv_fn, TRUE)
fr_juv_update["Yuba River", , ] <- update_hrl_habitat_from_SBR(flows, "Yuba River",
                                                               feather_juv_fn, TRUE)
fr_juv <- DSMhabitat::fr_juv
fr_juv$r_to_r_hrl <- fr_juv_update

# update fp rearing
fr_fp_update <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline
fr_fp_update["Feather River", , ] <- update_hrl_habitat_from_SBR(flows, "Feather River",
                                                                 feather_fp_fn, TRUE)
fr_fp_update["Tuolumne River", , ] <- update_hrl_habitat_from_SBR(flows, "Tuolumne River",
                                                                  tuolumne_fp_fn, TRUE)
fr_fp_update["Yuba River", , ] <- update_hrl_habitat_from_SBR(flows, "Yuba River",
                                                              feather_fp_fn, TRUE)
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
sutter_habitat$r_to_r_hrl <- sutter_habitat_added

usethis::use_data(sutter_habitat, overwrite = TRUE)

fr_fp <- DSMhabitat::fr_fp
fr_fp$r_to_r_hrl["Lower Sacramento River", , ] <- lower_sac_habitat_added
fr_fp$r_to_r_hrl["Lower-mid Sacramento River", , ] <- lower_mid_sac_habitat_added

usethis::use_data(fr_fp, overwrite = TRUE)


# scratch -----------------------------------------------------------------

# habitat_change_rice_fields_hrl <- function(wet_dry_areas,
#                                            watershed, lifestage) {
#   # calculate total sq meters added per watershed and habitat type
#   hab <- "floodplain rearing"
#   ws <- watershed
#   selected_run <- "fall"
#   lifestage <- "fp"
#   calsim_version <- "LTO_12a"
#   
#   # TODO confirm methods for these
#   # we don't just want to bump up to 20,000 acres (which in any case is already done for Sutter)
#   # we want to scale up the curve by the proportion. So multiply the existing habitat
#   # by the proportion to get the total ?
#   project_hab_added <- wet_dry_areas |> 
#     mutate(acres_to_add = Dry * proportion,
#            sqm_to_add = DSMhabitat::acres_to_square_meters(acres_to_add))
#   
#   if(watershed %in% c("Lower-mid Sacramento River",
#                       "Lower Sacramento River")) {
#     thirty_day_mean_exceedence <- existing_cfs_median_comparison_point(habitat_type,
#                                                                        watershed, species,
#                                                                        calsim_version)
#     sit_habitat <- DSMhabitat::set_floodplain_habitat(watershed, species, thirty_day_mean_exceedence)
#   }
#   if(watershed == "Sutter Bypass") {
#     thirty_day_mean_exceedence <- existing_cfs_median_comparison_point(habitat_type,
#                                                                        watershed, species,
#                                                                        calsim_version)
#     sutter1 <- DSMhabitat::set_bypass_habitat('sutter1', thirty_day_mean_exceedence)
#     sutter2 <- DSMhabitat::set_bypass_habitat('sutter2', thirty_day_mean_exceedence)
#     sutter3 <- DSMhabitat::set_bypass_habitat('sutter3', thirty_day_mean_exceedence)
#     sutter4 <- DSMhabitat::set_bypass_habitat('sutter4', thirty_day_mean_exceedence)
#     sit_habitat <- mean(sutter1, sutter2, sutter3, sutter4)
#   }
#   
#   # find proportion of habitat added
#   # TODO resolve yuba floodplain problem
#   prop_added <- ifelse(sit_habitat == 0, 0, project_hab_sqmeters/sit_habitat)
#   return(prop_added)
# }
