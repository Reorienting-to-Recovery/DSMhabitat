# file to cache Theoretical Max Habitat data objects 
# and some exploratory plots to compare SIT existing 
# to TMH

all_habitat_data <- readRDS('data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs.rdata')


all_hab_data_long <- all_habitat_data |>
  mutate(watershed = ifelse(watershed == "Lower San Joaquin River", "San Joaquin River", watershed)) |> 
  rename(spwn_acres_max = max_spawning_acres,
         rear_acres_max = max_rearing_acres, 
         flood_acres_max = max_floodplain_acres) |> 
  pivot_longer(cols = c(spwn_flow:flood_acres_max), names_to = "metric") |>
  separate(metric, c('hab', 'unit', 'lifestage'), "_") |>
  mutate(max_hab = ifelse(lifestage == "max", "max_hab", NA),
         lifestage = ifelse(lifestage == "max", NA, lifestage),
         unit = ifelse(unit == "flow", "cfs", unit)) |> 
  glimpse()


# update DSMhabitat values ------------------------------------------------

watersheds <- unique(all_hab_data_long$watershed)
watersheds <- watersheds[!(watersheds %in%  c('North Delta', "South Delta"))]

## spawning ----------------------------------------------------------------

r_to_r_tmh_fr_spawn <- DSMhabitat::fr_spawn$biop_itp_2018_2019

for(i in 1:length(watersheds)) {
  ws = watersheds[i]
  habitat = "spwn"
  
  max_hab_acres <- all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    filter(max_hab == "max_hab") |> 
    pull(value)
  
  existing_acres =   all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    filter(unit != "cfs") |>  
    filter(is.na(max_hab)) |> 
    pull(value)
  
  # Note: If the maximum theoretical habitat was less than the existing SIT habitat, 
  # the theoretical maximum habitat value was used for baseline and model runs. 
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

for(i in 1:length(watersheds)) {
  ws = watersheds[i]
  habitat = "rear"
  
  max_hab_acres <- all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    filter(max_hab == "max_hab") |> 
    pull(value)
  
  existing_acres_juv =   all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    filter(unit != "cfs") |>  
    filter(is.na(max_hab)) |> 
    filter(lifestage == "juv") |> 
    pull(value)
  
  existing_acres_fry =   all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    filter(unit != "cfs") |>  
    filter(is.na(max_hab)) |> 
    filter(lifestage == "fry") |> 
    pull(value)
  
  # Note: If the maximum theoretical habitat was less than the existing SIT habitat, 
  # the theoretical maximum habitat value was used for baseline and model runs. 
  adj_factor_juv = (max_hab_acres - existing_acres_juv) / existing_acres_juv + 1
  adj_factor_fry = (max_hab_acres - existing_acres_fry) / existing_acres_fry + 1
  
  add_max_hab_juv <- DSMhabitat::fr_juv$biop_itp_2018_2019[ws , , ] * adj_factor_juv
  add_max_hab_fry <- DSMhabitat::fr_fry$biop_itp_2018_2019[ws , , ] * adj_factor_fry
  
  r_to_r_tmh_fr_juv[ws, , ] <- add_max_hab_juv 
  r_to_r_tmh_fr_fry[ws, , ] <- add_max_hab_fry 
  
}



##floodplain: -------------------------------------------------------------
r_to_r_tmh_fr_flood <- DSMhabitat::fr_fp$biop_itp_2018_2019

for(i in 1:length(watersheds)) {
  ws = watersheds[i]
  habitat = "flood"
  
  max_hab_acres <- all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    filter(max_hab == "max_hab") |> 
    pull(value)
  
  existing_acres =   all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    filter(unit != "cfs") |>  
    filter(is.na(max_hab)) |> 
    pull(value)
  
  # Note: If the maximum theoretical habitat was less than the existing SIT habitat, 
  # the theoretical maximum habitat value was used for baseline and model runs. 
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
r_to_r_tmh_delta <- DSMhabitat::delta_habitat

for(i in 1:length(watersheds)) {
  ws = watersheds[i]
  habitat = "rear"
  
  max_hab_acres <- all_hab_data_long |> 
    filter(watershed == ws & hab == habitat) |> 
    filter(max_hab == "max_hab") |> 
    pull(value)
  
  # Instead of taking hab at the median flow to compare take median hab 
  # Check in with Mark on this assumption 
  existing_acres <- median(DSMhabitat::delta_habitat$sit_input[ , , ws]) |> 
    DSMhabitat::square_meters_to_acres()
  
  # Note: If the maximum theoretical habitat was less than the existing SIT habitat, 
  # the theoretical maximum habitat value was used for baseline and model runs. 
  adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
  
  new_hab_acres <- DSMhabitat::delta_habitat$sit_input[ , , ws] * adj_factor
  
  r_to_r_tmh_delta$sit_input[, , ws ] <- new_hab_acres 
  
}

# save data objects -------------------------------------------------------

# Save as data object to DSMhabitat

fr_fp <- c(DSMhabitat::fr_fp[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_flood))
usethis::use_data(fr_fp, overwrite = TRUE)

fr_fry <- c(DSMhabitat::fr_fry[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_fry))
usethis::use_data(fr_fry, overwrite = TRUE)

fr_juv <- c(DSMhabitat::fr_juv[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_juv))
usethis::use_data(fr_juv, overwrite = TRUE)

fr_spawn <- c(DSMhabitat::fr_spawn[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_spawn))
usethis::use_data(fr_spawn, overwrite = TRUE)

# TODO: placeholder for when we trust the delta values 

# Exploratory Plots:  -----------------------------------------------------

## spawning plot:  ---------------------------------------------------------
r_to_r_max_habitat <- r_to_r_tmh_fr_spawn |> 
  DSMhabitat::square_meters_to_acres()

sit_habitat <- DSMhabitat::fr_spawn$biop_itp_2018_2019 |> DSMhabitat::square_meters_to_acres()

spawn <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1979:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    sit_habitat = as.vector(sit_habitat),
    r_to_r_max_habitat = as.vector(r_to_r_max_habitat)) |> 
  filter(watershed %in% c("American River", 
                          "Upper Sacramento River", 
                          "Paynes Creek", 
                          "Clear Creek"))

spawn |> 
  transmute(watershed, date = lubridate::ymd(paste(year, month, 1)), 
            sit_habitat, r_to_r_max_habitat) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()


## fry and juv plots:  -----------------------------------------------------

r_to_r_tmh_habitat <- r_to_r_tmh_fr_fry |> 
  DSMhabitat::square_meters_to_acres()

sit_habitat <- DSMhabitat::fr_fry$biop_itp_2018_2019 |> DSMhabitat::square_meters_to_acres()

ic_fry <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    sit_habitat = as.vector(sit_habitat),
    r_to_r_tmh_habitat = as.vector(r_to_r_tmh_habitat)) |> 
  filter(watershed %in% c("American River", 
                          "Tuolumne River",
                          "Upper Sacramento River",
                          "Upper-mid Sacramento River"
  ))

ic_fry |> 
  transmute(watershed, date = lubridate::ymd(paste(year, month, 1)), 
            sit_habitat, r_to_r_tmh_habitat) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()


## floodplain exploratory plot:  -------------------------------------------

r_to_r_max_habitat <- r_to_r_tmh_fr_flood |> 
  DSMhabitat::square_meters_to_acres()

sit_habitat <- DSMhabitat::fr_fp$biop_itp_2018_2019 |> DSMhabitat::square_meters_to_acres()

flood <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    sit_habitat = as.vector(sit_habitat),
    r_to_r_max_habitat = as.vector(r_to_r_max_habitat)) |> 
  filter(watershed %in% c("American River", 
                          "Upper Sacramento River", 
                          "Paynes Creek", 
                          "Clear Creek"))

flood |> 
  transmute(watershed, date = lubridate::ymd(paste(year, month, 1)), 
            sit_habitat, r_to_r_max_habitat) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()


## delta plots -------------------------------------------------------------

r_to_r_tmh_delta <- r_to_r_tmh_delta$sit_input |> 
  DSMhabitat::square_meters_to_acres()

sit_habitat <- DSMhabitat::delta_habitat$sit_input |> DSMhabitat::square_meters_to_acres()

delta <- expand_grid(
  watershed = c("North Delta", "South Delta"),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    sit_habitat = as.vector(sit_habitat),
    r_to_r_tmh_delta = as.vector(r_to_r_tmh_delta)) |> 
  filter(watershed %in% c("North Delta", "South Delta"))

delta |> 
  transmute(watershed, date = lubridate::ymd(paste(year, month, 1)), 
            sit_habitat, r_to_r_tmh_delta) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()
