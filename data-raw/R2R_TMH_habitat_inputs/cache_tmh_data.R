library(tidyverse)
library(DSMhabitat)
library(lubridate)

source('data-raw/R2R_TMH_habitat_inputs/tmh_helper_functions.R')


# file to cache Theoretical Max Habitat data objects 
# and some exploratory plots to compare SIT existing 
# to TMH. 


# FALL RUN: does not extend past reservoirs -------------------------------
all_existing_and_tmh_data_fall_run <- readRDS('data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs_fall_run.rdata')

all_hab_data_long <- all_existing_and_tmh_data_fall_run |>
  rename(spwn_acres_max = max_spawning_acres,
         rear_acres_max = max_rearing_acres, 
         flood_acres_max = max_floodplain_acres) |> 
  pivot_longer(cols = c(spwn_acres_max:flood_acres_max), names_to = "metric") |>
  separate(metric, c('hab', 'unit', 'lifestage'), "_") |>
  glimpse()


# update DSMhabitat values ------------------------------------------------

watersheds <- unique(all_hab_data_long$watershed)
watersheds_trunc <- watersheds[!(watersheds %in%  c('North Delta', "South Delta"))]

## spawning ----------------------------------------------------------------

r_to_r_tmh_fr_spawn <- DSMhabitat::fr_spawn$biop_itp_2018_2019

for(i in 1:length(watersheds_trunc)) {
  ws = watersheds_trunc[i]
  habitat = "spwn"
  
  max_hab_acres <- all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    pull(value)
  
  existing_acres <- existing_acres_fun(ws, 'spawn') 
  
  # Note:when existing SIT habitat is greater than TMH, we are still using TMH so the WUA would get scaled down
  if(existing_acres == 0 | is.na(existing_acres)) {
    adj_factor = 1
  } else {
    adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
  }
  
  new_hab_acres <- DSMhabitat::fr_spawn$biop_itp_2018_2019[ws , , ] * adj_factor
  
  r_to_r_tmh_fr_spawn[ws, , ] <- new_hab_acres 
  
}


## inchannel habitat to both fry and juvenile habitat objects ---------------
### set r_to_r_baseline_fr_juv and fry 
r_to_r_tmh_fr_juv <- DSMhabitat::fr_juv$biop_itp_2018_2019
r_to_r_tmh_fr_fry <- DSMhabitat::fr_fry$biop_itp_2018_2019

for(i in 1:length(watersheds_trunc)) {
  ws = watersheds_trunc[i]
  habitat = "rear"
  
  max_hab_acres <- all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    pull(value)
  
  existing_acres_juv <- existing_acres_fun(ws, 'rear_juv') 
  existing_acres_fry <- existing_acres_fun(ws, 'rear_fry') 
  
  # Note: when existing SIT habitat is greater than TMH, we are still using TMH so the WUA would get scaled down
  adj_factor_juv = (max_hab_acres - existing_acres_juv) / existing_acres_juv + 1
  adj_factor_fry = (max_hab_acres - existing_acres_fry) / existing_acres_fry + 1
  
  add_max_hab_juv <- DSMhabitat::fr_juv$biop_itp_2018_2019[ws , , ] * adj_factor_juv
  add_max_hab_fry <- DSMhabitat::fr_fry$biop_itp_2018_2019[ws , , ] * adj_factor_fry
  
  r_to_r_tmh_fr_juv[ws, , ] <- add_max_hab_juv 
  r_to_r_tmh_fr_fry[ws, , ] <- add_max_hab_fry 
  
}



##floodplain: -------------------------------------------------------------
r_to_r_tmh_fr_flood <- DSMhabitat::fr_fp$biop_itp_2018_2019

for(i in 1:length(watersheds_trunc)) {
  ws = watersheds_trunc[i]
  habitat = "flood"
  
  max_hab_acres <- all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    pull(value)
  
  existing_acres <- existing_acres_fun(ws, 'floodplain') 
  
  # print(paste(ws, max_hab_acres, existing_acres))
  
  # Note: when existing SIT habitat is greater than TMH, we are still using TMH 
  # so the WUA would get scaled down
  if(existing_acres == 0) {
    adj_factor = 1
  } else {
    adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
  }
  
  
  new_hab_acres <- DSMhabitat::fr_fp$biop_itp_2018_2019[ws , , ] * adj_factor
  
  r_to_r_tmh_fr_flood[ws, , ] <- new_hab_acres 
  
}

##delta: -------------------------------------------------------------------

watersheds <- c('North Delta', 'South Delta')
r_to_r_tmh_delta <- DSMhabitat::delta_habitat$sit_input

for(i in 1:length(watersheds)) {
  ws = watersheds[i]
  habitat = "rear"
  
  # see: TMH methodology for calcs 
  max_hab_df = data.frame(watershed = c("North Delta", "South Delta"),
                             max_hab = c(41720, 102792)) 
  
  max_hab_acres <- max_hab_df |> 
    filter(watershed == ws) |> 
    pull(max_hab)
  # Instead of taking hab at the median flow to compare take median hab 
  # Check in with Mark on this assumption 
  existing_acres <- median(DSMhabitat::delta_habitat$sit_input[ , , ws]) |> 
    DSMhabitat::square_meters_to_acres()
  
  # Note: If the maximum theoretical habitat was less than the existing SIT habitat, 
  # the theoretical maximum habitat value was used for baseline and model runs. 
  adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
  
  new_hab_acres <- DSMhabitat::delta_habitat$sit_input[ , , ws] * adj_factor
  
  r_to_r_tmh_delta[, , ws ] <- new_hab_acres 

################################################################################
################################################################################
# SPRING AND WINTER RUNS: extend past reservoirs/dams ---------------------
################################################################################
################################################################################

all_existing_and_tmh_data_wr_sr <- readRDS('data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs_spring_winter_run.rdata')

all_hab_data_long <- all_existing_and_tmh_data_wr_sr |>
  rename(spwn_acres_max = max_spawning_acres,
         rear_acres_max = max_rearing_acres, 
         flood_acres_max = max_floodplain_acres) |> 
  pivot_longer(cols = c(spwn_acres_max:flood_acres_max), names_to = "metric") |>
  separate(metric, c('hab', 'unit', 'lifestage'), "_") |>
  glimpse()


# update DSMhabitat values ------------------------------------------------

watersheds <- unique(all_hab_data_long$watershed)
watersheds_trunc <- watersheds[!(watersheds %in%  c('North Delta', "South Delta"))]

## spawning ----------------------------------------------------------------

r_to_r_tmh_sr_spawn <- DSMhabitat::sr_spawn$biop_itp_2018_2019
r_to_r_tmh_wr_spawn <- DSMhabitat::wr_spawn$biop_itp_2018_2019
for(i in 1:length(watersheds_trunc)) {
  ws = watersheds_trunc[i]
  habitat = "spwn"
  
  max_hab_acres <- all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    pull(value)
  
  existing_acres <- existing_acres_fun(ws, 'spawn') 
  
  # Note:when existing SIT habitat is greater than TMH, we are still using TMH so the WUA would get scaled down
  if(existing_acres == 0 | is.na(existing_acres)) {
    adj_factor = 1
  } else {
    adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
  }
  
  new_hab_acres_wr <- DSMhabitat::wr_spawn$biop_itp_2018_2019[ws , , ] * adj_factor
  new_hab_acres_sr <- DSMhabitat::sr_spawn$biop_itp_2018_2019[ws , , ] * adj_factor
  
  r_to_r_tmh_wr_spawn[ws, , ] <- new_hab_acres_wr
  r_to_r_tmh_sr_spawn[ws, , ] <- new_hab_acres_sr 
  
}


## inchannel habitat to both fry and juvenile habitat objects ---------------
### set r_to_r_baseline_fr_juv and fry 
r_to_r_tmh_wr_juv <- DSMhabitat::wr_juv$biop_itp_2018_2019
r_to_r_tmh_wr_fry <- DSMhabitat::wr_fry$biop_itp_2018_2019
r_to_r_tmh_sr_juv <- DSMhabitat::sr_juv$biop_itp_2018_2019
r_to_r_tmh_sr_fry <- DSMhabitat::sr_fry$biop_itp_2018_2019

for(i in 1:length(watersheds_trunc)) {
  ws = watersheds_trunc[i]
  habitat = "rear"
  
  max_hab_acres <- all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    pull(value)
  
  existing_acres_juv <- existing_acres_fun(ws, 'rear_juv') 
  existing_acres_fry <- existing_acres_fun(ws, 'rear_fry') 
  
  # Note: when existing SIT habitat is greater than TMH, we are still using TMH so the WUA would get scaled down
  adj_factor_juv = (max_hab_acres - existing_acres_juv) / existing_acres_juv + 1
  adj_factor_fry = (max_hab_acres - existing_acres_fry) / existing_acres_fry + 1
  
  add_max_hab_juv_wr <- DSMhabitat::wr_juv$biop_itp_2018_2019[ws , , ] * adj_factor_juv
  add_max_hab_fry_wr <- DSMhabitat::wr_fry$biop_itp_2018_2019[ws , , ] * adj_factor_fry
  
  add_max_hab_juv_sr <- DSMhabitat::sr_juv$biop_itp_2018_2019[ws , , ] * adj_factor_juv
  add_max_hab_fry_sr <- DSMhabitat::sr_fry$biop_itp_2018_2019[ws , , ] * adj_factor_fry
  
  r_to_r_tmh_wr_juv[ws, , ] <- add_max_hab_juv_wr 
  r_to_r_tmh_wr_fry[ws, , ] <- add_max_hab_fry_wr 
  
  r_to_r_tmh_sr_juv[ws, , ] <- add_max_hab_juv_sr 
  r_to_r_tmh_sr_fry[ws, , ] <- add_max_hab_fry_sr
  
}



##floodplain: -------------------------------------------------------------
r_to_r_tmh_sr_flood <- DSMhabitat::sr_fp$biop_itp_2018_2019
r_to_r_tmh_wr_flood <- DSMhabitat::wr_fp$biop_itp_2018_2019

for(i in 1:length(watersheds_trunc)) {
  ws = watersheds_trunc[i]
  habitat = "flood"
  
  max_hab_acres <- all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    pull(value)
  
  existing_acres <- existing_acres_fun(ws, 'floodplain') 
  
  # print(paste(ws, max_hab_acres, existing_acres))
  
  # Note: when existing SIT habitat is greater than TMH, we are still using TMH 
  # so the WUA would get scaled down
  if(existing_acres == 0) {
    adj_factor = 1
  } else {
    adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
  }
  
  
  new_hab_acres_wr <- DSMhabitat::wr_fp$biop_itp_2018_2019[ws , , ] * adj_factor
  new_hab_acres_sr <- DSMhabitat::sr_fp$biop_itp_2018_2019[ws , , ] * adj_factor
  
  r_to_r_tmh_wr_flood[ws, , ] <- new_hab_acres_wr
  r_to_r_tmh_sr_flood[ws, , ] <- new_hab_acres_sr
  
  
}

##delta: -------------------------------------------------------------------

watersheds <- c('North Delta', 'South Delta')
r_to_r_tmh_delta <- DSMhabitat::delta_habitat$sit_input

for(i in 1:length(watersheds)) {
  ws = watersheds[i]
  habitat = "rear"
  
  # see: TMH methodology for calcs 
  max_hab_df = data.frame(watershed = c("North Delta", "South Delta"),
                          max_hab = c(41720, 102792)) 
  
  max_hab_acres <- max_hab_df |> 
    filter(watershed == ws) |> 
    pull(max_hab)
  # Instead of taking hab at the median flow to compare take median hab 
  # Check in with Mark on this assumption 
  existing_acres <- median(DSMhabitat::delta_habitat$sit_input[ , , ws]) |> 
    DSMhabitat::square_meters_to_acres()
  
  # Note: If the maximum theoretical habitat was less than the existing SIT habitat, 
  # the theoretical maximum habitat value was used for baseline and model runs. 
  adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
  
  new_hab_acres <- DSMhabitat::delta_habitat$sit_input[ , , ws] * adj_factor
  
  r_to_r_tmh_delta[, , ws ] <- new_hab_acres 
  
}

# save data objects -------------------------------------------------------

# Save as data object to DSMhabitat
## FLOODPLAIN:
### Fall run:
fr_fp <- c(DSMhabitat::fr_fp[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_flood))
usethis::use_data(fr_fp, overwrite = TRUE)
### Spring Run:
sr_fp <- c(DSMhabitat::sr_fp[1:3], r_to_r_tmh = list(r_to_r_tmh_sr_flood))
usethis::use_data(sr_fp, overwrite = TRUE)
#### Winter Run:
wr_fp <- c(DSMhabitat::wr_fp[1:3], r_to_r_tmh = list(r_to_r_tmh_wr_flood))
usethis::use_data(wr_fp, overwrite = TRUE)

## IN CHANNEL REARING:
### Fall Run: 
fr_fry <- c(DSMhabitat::fr_fry[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_fry))
usethis::use_data(fr_fry, overwrite = TRUE)

fr_juv <- c(DSMhabitat::fr_juv[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_juv))
usethis::use_data(fr_juv, overwrite = TRUE)
### Spring Run: 
sr_fry <- c(DSMhabitat::sr_fry[1:3], r_to_r_tmh = list(r_to_r_tmh_sr_fry))
usethis::use_data(sr_fry, overwrite = TRUE)

sr_juv <- c(DSMhabitat::sr_juv[1:3], r_to_r_tmh = list(r_to_r_tmh_sr_juv))
usethis::use_data(sr_juv, overwrite = TRUE)
### Winter Run: 
wr_fry <- c(DSMhabitat::wr_fry[1:3], r_to_r_tmh = list(r_to_r_tmh_wr_fry))
usethis::use_data(wr_fry, overwrite = TRUE)

wr_juv <- c(DSMhabitat::wr_juv[1:3], r_to_r_tmh = list(r_to_r_tmh_wr_juv))
usethis::use_data(wr_juv, overwrite = TRUE)

## SPAWNING: 
### Fall run: 
fr_spawn <- c(DSMhabitat::fr_spawn[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_spawn))
usethis::use_data(fr_spawn, overwrite = TRUE)
### Spring Run: 
sr_spawn <- c(DSMhabitat::sr_spawn[1:3], r_to_r_tmh = list(r_to_r_tmh_sr_spawn))
usethis::use_data(sr_spawn, overwrite = TRUE)
### Winter Run:
wr_spawn <- c(DSMhabitat::wr_spawn[1:3], r_to_r_tmh = list(r_to_r_tmh_wr_spawn))
usethis::use_data(wr_spawn, overwrite = TRUE)

delta_habitat <- c(DSMhabitat::delta_habitat[1:2], r_to_r_tmh = list(r_to_r_tmh_delta))
usethis::use_data(delta_habitat, overwrite = TRUE)



# Exploratory Plots:  -----------------------------------------------------
## spawning plot:  ---------------------------------------------------------
### fall run: 
tmh_comparison_plot(tmh_data = r_to_r_tmh_fr_spawn, 
                    sit_habitat = DSMhabitat::fr_spawn$biop_itp_2018_2019, "spawn")

tmh_comparison_plot(tmh_data = r_to_r_tmh_wr_spawn, 
                    sit_habitat = DSMhabitat::wr_spawn$biop_itp_2018_2019, "spawn")

tmh_comparison_plot(tmh_data = r_to_r_tmh_sr_spawn, 
                    sit_habitat = DSMhabitat::sr_spawn$biop_itp_2018_2019, "spawn")

## fry and juv plots:  -----------------------------------------------------
tmh_comparison_plot(tmh_data = r_to_r_tmh_fr_fry, 
                    sit_habitat = DSMhabitat::fr_fry$biop_itp_2018_2019, "fry")

# winter run
tmh_comparison_plot(tmh_data = r_to_r_tmh_wr_fry, 
                    sit_habitat = DSMhabitat::wr_fry$biop_itp_2018_2019, "fry")
# spring run 
tmh_comparison_plot(tmh_data = r_to_r_tmh_sr_fry, 
                    sit_habitat = DSMhabitat::sr_fry$biop_itp_2018_2019, "fry")

## floodplain exploratory plot:  -------------------------------------------
# fall run: 
tmh_comparison_plot(tmh_data = r_to_r_tmh_fr_flood, 
                    sit_habitat = DSMhabitat::fr_fp$biop_itp_2018_2019, "flood")
# winter run:
tmh_comparison_plot(tmh_data = r_to_r_tmh_wr_flood, 
                    sit_habitat = DSMhabitat::wr_fp$biop_itp_2018_2019, "flood")
# spring run: 
tmh_comparison_plot(tmh_data = r_to_r_tmh_sr_flood, 
                    sit_habitat = DSMhabitat::sr_fp$biop_itp_2018_2019, "flood")
