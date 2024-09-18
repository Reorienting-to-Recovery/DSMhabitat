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

# code ported directly from va-habitat/habitat-functions/Feather/feather.Rmd

calsim <- read_excel(here::here("data-raw", "R2R_HRL_habitat_inputs", "data",
                                'Feather_LFC_pulse_flow_DRAFT_051619.xlsm'), 
                     sheet = 'Output')

# existing spawning
upper_spawning <- read_excel(here::here("data-raw", "R2R_HRL_habitat_inputs", "data", 'Feathe_River_Upper_and_Lower_Salmon_and_Steelhead_Spawning_RSI_for_FERC_relicensing.xlsx'),
                             sheet = 'Upper', range = 'A2:B32') |> 
  mutate(section = 'LFC')

lower_spawning <- read_excel(here::here("data-raw", "R2R_HRL_habitat_inputs", "data", 'Feathe_River_Upper_and_Lower_Salmon_and_Steelhead_Spawning_RSI_for_FERC_relicensing.xlsx'),
                             sheet = 'Lower', range = 'A2:B32') |> 
  mutate(section = 'HFC')

feather_spawn_length <- DSMhabitat::watershed_lengths |> 
  filter(watershed == 'Feather River', species == 'fr', lifestage == 'spawning') |> 
  pull(feet)

lfc_spawn_length <- 42240
hfc_spawn_length <- feather_spawn_length - lfc_spawn_length

lfc_spawning <- upper_spawning |> 
  mutate(suitable_acres = round(`Chinook RSI`/1000*lfc_spawn_length/43560, 2)) |> 
  select(flow_cfs = `Flow (cfs)`, suitable_acres, section)

hfc_spawning <- lower_spawning |> 
  mutate(suitable_acres = round(`Chinook RSI`/1000*hfc_spawn_length/43560, 2)) |> 
  select(flow_cfs = `Flow (cfs)`, suitable_acres, section)

existing_spawn_lfc <- approxfun(lfc_spawning$flow_cfs, lfc_spawning$suitable_acres,
                                rule = 2)

existing_spawn_hfc <- approxfun(hfc_spawning$flow_cfs, hfc_spawning$suitable_acres,
                                rule = 2)

# existing ic rearing
upper_fry <- read_excel(here::here("data-raw", "R2R_HRL_habitat_inputs", "data", 'fry_juv_rsi_addendum_2.xlsx'), sheet = 'upper_fry')
lower_fry <- read_excel(here::here("data-raw", "R2R_HRL_habitat_inputs", "data", 'fry_juv_rsi_addendum_2.xlsx'), sheet = 'lower_fry')
upper_juv <- read_excel(here::here("data-raw", "R2R_HRL_habitat_inputs", "data", 'fry_juv_rsi_addendum_2.xlsx'), sheet = 'upper_juv')
lower_juv <- read_excel(here::here("data-raw", "R2R_HRL_habitat_inputs", "data", 'fry_juv_rsi_addendum_2.xlsx'), sheet = 'lower_juv')


feather_length <- DSMhabitat::watershed_lengths |> 
  filter(watershed == 'Feather River', species == 'fr', lifestage == 'rearing') |> 
  pull(feet)

# refactor to use just one variable 
lfc_length <- 42240
hfc_length <- feather_length - lfc_length

# use low no cover instead of 0.0 no cover
rearing_lfc <- bind_rows(upper_fry, upper_juv) |> 
  filter(cover != 'No Cover 0.0') |> 
  select(-cover) |> 
  mutate(suitable_acres = round(rsi/1000*lfc_length/43560, 2), flow_cfs = round(flow_cfs)) |>
  select(flow_cfs, suitable_acres, lifestage, section)

rearing_hfc <- bind_rows(lower_fry, lower_juv) |> 
  filter(cover != 'No Cover 0.0') |> 
  select(-cover) |> 
  mutate(suitable_acres = round(rsi/1000*hfc_length/43560, 2), flow_cfs = round(flow_cfs)) |>
  select(flow_cfs, suitable_acres, lifestage, section)

rearing_hfc_fry <- rearing_hfc |> 
  filter(lifestage == 'fry')

rearing_hfc_juv <- rearing_hfc |> 
  filter(lifestage == 'juvenile')

rearing_lfc_fry <- rearing_lfc |> 
  filter(lifestage == 'fry')

rearing_lfc_juv <- rearing_lfc |> 
  filter(lifestage == 'juvenile')

existing_rearing_hfc_fry <- approxfun(rearing_hfc_fry$flow_cfs, rearing_hfc_fry$suitable_acres,
                                      rule = 2)
existing_rearing_hfc_juv <- approxfun(rearing_hfc_juv$flow_cfs, rearing_hfc_juv$suitable_acres,
                                      rule = 2)

existing_rearing_lfc_fry <- approxfun(rearing_lfc_fry$flow_cfs, rearing_lfc_fry$suitable_acres,
                                      rule = 2)
existing_rearing_lfc_juv <- approxfun(rearing_lfc_juv$flow_cfs, rearing_lfc_juv$suitable_acres,
                                      rule = 2)

# existing fp
existing_fp <- DSMhabitat::feather_river_floodplain |> 
  mutate(suitable_acres = FR_floodplain_acres * .27) |> 
  select(flow_cfs, suitable_acres)

existing_fp_hab <- approxfun(existing_fp$flow_cfs, existing_fp$suitable_acres, rule = 2)

# VA 
va_proj <- read_excel(here::here("data-raw", "R2R_HRL_habitat_inputs", "data", 'VA_Habitat_value_of_FR_projects_updated_5-9-19.xlsx'), 
                      sheet = 'sadie', range = 'A1:L12')

# VA spawn
new_spawning <- va_proj |>
  filter(habitat_type == 'spawning') |>
  select(reach:max_flow_suitability) |>
  mutate(acres = 3.75 + 11.6) |>
  unique()

new_spawning_acres <- approxfun(x = c(new_spawning$min_flow, new_spawning$target_flow, new_spawning$max_flow),
                                y = c(new_spawning$acres * new_spawning$min_flow_suitability, 
                                      new_spawning$acres * new_spawning$target_flow_suitability,
                                      new_spawning$acres * new_spawning$max_flow_suitability),
                                yleft = 0, rule = 2)

va_enhanced_spawn_lfc <- tibble(
  flow_cfs = lfc_spawning$flow_cfs,
  suitable_acres = existing_spawn_lfc(flow_cfs) + new_spawning_acres(flow_cfs)
)

va_spawn_lfc <- approxfun(va_enhanced_spawn_lfc$flow_cfs, va_enhanced_spawn_lfc$suitable_acres,
                          rule = 2)

# VA rear
new_rearing <- va_proj |>
  filter(habitat_type == 'rearing') |>
  select(reach:max_flow_suitability) |>
  mutate(acres = 2.25 + 3) |>
  unique()

new_rearing_acres <- approxfun(x = c(new_rearing$min_flow, new_rearing$target_flow, new_rearing$max_flow),
                               y = c(new_rearing$acres * new_rearing$min_flow_suitability, 
                                     new_rearing$acres * new_rearing$target_flow_suitability,
                                     new_rearing$acres * new_rearing$max_flow_suitability),
                               yleft = 0, rule = 2)

va_enhanced_rearing_lfc_fry <- tibble(
  flow_cfs = rearing_lfc_fry$flow_cfs,
  suitable_acres = existing_rearing_lfc_fry(flow_cfs) + new_rearing_acres(flow_cfs),
  lifestage = 'fry',
  section = 'LFC'
)

va_enhanced_rearing_lfc_juv <- tibble(
  flow_cfs = rearing_lfc_juv$flow_cfs,
  suitable_acres = existing_rearing_lfc_juv(flow_cfs) + new_rearing_acres(flow_cfs),
  lifestage = 'juvenile',
  section = 'LFC'
)

va_rearing_lfc_fry <- approxfun(va_enhanced_rearing_lfc_fry$flow_cfs, 
                                va_enhanced_rearing_lfc_fry$suitable_acres,
                                rule = 2)
va_rearing_lfc_juv <- approxfun(va_enhanced_rearing_lfc_juv$flow_cfs, 
                                va_enhanced_rearing_lfc_juv$suitable_acres,
                                rule = 2)

# VA floodplain rearing
fp <- va_proj |>
  filter(habitat_type == 'floodplain rearing') |> 
  select(reach:max_flow_suitability)

fp_3000 <- fp |>
  filter(min_flow == 3000) |>
  mutate(acres = 550) |>
  select(-reach) |>
  unique()

new_fp_3000_acres <- approxfun(x = c(fp_3000$min_flow, fp_3000$target_flow, fp_3000$max_flow),
                               y = c(fp_3000$acres * fp_3000$min_flow_suitability, 
                                     fp_3000$acres * fp_3000$target_flow_suitability,
                                     fp_3000$acres * fp_3000$max_flow_suitability),
                               yleft = 0, rule = 2)

fp_30000 <- fp |>
  filter(min_flow == 30000) |>
  mutate(acres = 10 + 220 + 25 + 550) |>
  unique()
new_fp_30000_acres <- approxfun(x = c(fp_30000$min_flow, fp_30000$target_flow, fp_30000$max_flow),
                                y = c(fp_30000$acres * fp_30000$min_flow_suitability, 
                                      fp_30000$acres * fp_30000$target_flow_suitability,
                                      fp_30000$acres * fp_30000$max_flow_suitability),
                                yleft = 0, rule = 2)

fp_4000 <- fp |>
  filter(acres == '100-600') |>
  separate(acres, c('min_acres', 'max_acres'), sep = '-') |>
  mutate(min_acres = 50,
         max_acres = 300,
         min_suit_acres = as.numeric(min_acres) * min_flow_suitability,
         max_suit_acres = as.numeric(max_acres) * max_flow_suitability)
new_fp_4000_acres <- approxfun(x = c(fp_4000$min_flow, fp_4000$target_flow),
                               y = c(fp_4000$min_suit_acres, fp_4000$max_suit_acres),
                               yleft = 0, rule = 2)

flows <- existing_fp$flow_cfs

va_enhanced_fp <- tibble(
  flow_cfs = flows,
  suitable_acres = existing_fp$suitable_acres + new_fp_4000_acres(flows) + 
    new_fp_30000_acres(flows) + new_fp_3000_acres(flows)
)

va_only_fp <- tibble(
  flow_cfs = flows,
  suitable_acres = new_fp_4000_acres(flows) + 
    new_fp_30000_acres(flows) + new_fp_3000_acres(flows)
)

va_fp <- approxfun(va_enhanced_fp$flow_cfs, va_enhanced_fp$suitable_acres, rule = 2)


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
                                                               feather_juv_fn, TRUE)
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
