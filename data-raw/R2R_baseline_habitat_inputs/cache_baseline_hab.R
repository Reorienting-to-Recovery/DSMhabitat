library(tidyverse)
source("data-raw/R2R_baseline_habitat_inputs/generate_hab_helper_functions.R")

# Update DSMhabitat values 
# TODO ALL fall run for now, generatlize for all later 
# TODO add in habitats for the ones that are "Confirm if this is for adult or juvenile" 

# Update spawning --------------------------------------------------------------
# TODO see if I can functionalize this, for now this works 
# watershed <- c("American River", "Upper Sacramento River") # list all spawning watersheds with projects

# set r_to_r_baseline_fr_spawn to 
r_to_r_baseline_fr_spawn <- DSMhabitat::fr_spawn$biop_itp_2018_2019

# Add american river spawning habitat 
add_project_habitat <- DSMhabitat::fr_spawn$biop_itp_2018_2019["American River" , , ] * 
  hab_prop_change_from_projects("spawning", "American River", "fr", "adult", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_spawn$biop_itp_2018_2019["American River", , ] + add_project_habitat

r_to_r_baseline_fr_spawn["American River", , ] <- updated_habitat 

# add clear creek spawning habitat
add_project_habitat <- DSMhabitat::fr_spawn$biop_itp_2018_2019["Clear Creek" , , ] * 
  hab_prop_change_from_projects("spawning", "Clear Creek", "fr", "adult", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_spawn$biop_itp_2018_2019["Clear Creek", , ] + add_project_habitat

r_to_r_baseline_fr_spawn["Clear Creek", , ] <- updated_habitat 

# add paynes creek spawning habitat
add_project_habitat <- DSMhabitat::fr_spawn$biop_itp_2018_2019["Paynes Creek" , , ] * 
  hab_prop_change_from_projects("spawning", "Paynes Creek", "fr", "adult", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_spawn$biop_itp_2018_2019["Paynes Creek", , ] + add_project_habitat

r_to_r_baseline_fr_spawn["Paynes Creek", , ] <- updated_habitat 

# Add sacramento river spawning habitat 
add_project_habitat <- DSMhabitat::fr_spawn$biop_itp_2018_2019["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("spawning", "Upper Sacramento River", "fr", "adult", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_spawn$biop_itp_2018_2019["Upper Sacramento River", , ] + add_project_habitat

r_to_r_baseline_fr_spawn["Upper Sacramento River", , ] <- updated_habitat 

# check habitat differences 
# upper sac and american river should be different (larger)
r_to_r_baseline_fr_spawn == DSMhabitat::fr_spawn$biop_itp_2018_2019

# Save as data object to DSMhabitat
fr_spawn <- c(DSMhabitat::fr_spawn, r_to_r_baseline = list(r_to_r_baseline_fr_spawn))
usethis::use_data(fr_spawn, overwrite = TRUE)

# Exploratory plot 
r_to_r_baseline_habitat <- r_to_r_baseline_fr_spawn |> 
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
    r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
  filter(watershed %in% c("American River", 
                          "Upper Sacramento River", 
                          "Paynes Creek", 
                          "Clear Creek"))

spawn |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            sit_habitat, r_to_r_baseline_habitat) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()




# Add inchannel habitat to both fry and juvenile habitat objects ---------------
# set r_to_r_baseline_fr_juv and fry 
r_to_r_baseline_fr_juv <- DSMhabitat::fr_juv$biop_itp_2018_2019
r_to_r_baseline_fr_fry <- DSMhabitat::fr_fry$biop_itp_2018_2019

# Add american river juv habitat 
add_project_habitat <- DSMhabitat::fr_juv$biop_itp_2018_2019["American River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "juv", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_juv$biop_itp_2018_2019["American River", , ] + add_project_habitat

r_to_r_baseline_fr_juv["American River", , ] <- updated_habitat 

# Add american river fry habitat 
add_project_habitat <- DSMhabitat::fr_fry$biop_itp_2018_2019["American River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "fry", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_fry$biop_itp_2018_2019["American River", , ] + add_project_habitat

r_to_r_baseline_fr_fry["American River", , ] <- updated_habitat 

# add tuolumne river juv habitat
add_project_habitat <- DSMhabitat::fr_juv$biop_itp_2018_2019["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Tuolumne River", "fr", "juv", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_juv$biop_itp_2018_2019["Tuolumne River", , ] + add_project_habitat

r_to_r_baseline_fr_juv["Tuolumne River", , ] <- updated_habitat 

# add tuolumne river fry habitat
add_project_habitat <- DSMhabitat::fr_fry$biop_itp_2018_2019["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Tuolumne River", "fr", "fry", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_fry$biop_itp_2018_2019["Tuolumne River", , ] + add_project_habitat

r_to_r_baseline_fr_fry["Tuolumne River", , ] <- updated_habitat 

# Upper sac river 
# Add upper sac river juv habitat 
add_project_habitat <- DSMhabitat::fr_juv$biop_itp_2018_2019["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "fr", "juv", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_juv$biop_itp_2018_2019["Upper Sacramento River", , ] + add_project_habitat

r_to_r_baseline_fr_juv["Upper Sacramento River", , ] <- updated_habitat 

# Add upper sacramento  river fry habitat 
add_project_habitat <- DSMhabitat::fr_fry$biop_itp_2018_2019["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "fr", "fry", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_fry$biop_itp_2018_2019["Upper Sacramento River", , ] + add_project_habitat

r_to_r_baseline_fr_fry["Upper Sacramento River", , ] <- updated_habitat 

# add Upper-mid Sacramento River
# Add upper mid sac river juv habitat 
add_project_habitat <- DSMhabitat::fr_juv$biop_itp_2018_2019["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "fr", "juv", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_juv$biop_itp_2018_2019["Upper-mid Sacramento River", , ] + add_project_habitat

r_to_r_baseline_fr_juv["Upper-mid Sacramento River", , ] <- updated_habitat 

# Add upper mid sacramento river fry habitat 
add_project_habitat <- DSMhabitat::fr_fry$biop_itp_2018_2019["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "fr", "fry", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_fry$biop_itp_2018_2019["Upper-mid Sacramento River", , ] + add_project_habitat

r_to_r_baseline_fr_fry["Upper-mid Sacramento River", , ] <- updated_habitat 

# check r to r vs dsm habitat 
# upper-mid sac, upper sac, and american river should be different 
r_to_r_baseline_fr_juv == DSMhabitat::fr_juv$biop_itp_2018_2019
r_to_r_baseline_fr_fry == DSMhabitat::fr_fry$biop_itp_2018_2019

# Save as data object to DSMhabitat
fr_juv <- c(DSMhabitat::fr_juv, r_to_r_baseline = list(r_to_r_baseline_fr_juv))
usethis::use_data(fr_juv, overwrite = TRUE)

# Save as data object to DSMhabitat
fr_fry <- c(DSMhabitat::fr_fry, r_to_r_baseline = list(r_to_r_baseline_fr_fry))
usethis::use_data(fr_fry, overwrite = TRUE)

# Exploratory plot 
r_to_r_baseline_habitat <- r_to_r_baseline_fr_juv |> 
  DSMhabitat::square_meters_to_acres()

sit_habitat <- DSMhabitat::fr_juv$biop_itp_2018_2019 |> DSMhabitat::square_meters_to_acres()

ic_juv <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    sit_habitat = as.vector(sit_habitat),
    r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
  filter(watershed %in% c("American River", 
                          "Tuolumne River",
                          "Upper Sacramento River",
                          "Upper-mid Sacramento River"
  ))

ic_juv |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            sit_habitat, r_to_r_baseline_habitat) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# Exploratory plot 
r_to_r_baseline_habitat <- r_to_r_baseline_fr_fry |> 
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
    r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
  filter(watershed %in% c("American River", 
                          "Tuolumne River",
                          "Upper Sacramento River",
                          "Upper-mid Sacramento River"))

ic_fry|> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            sit_habitat, r_to_r_baseline_habitat) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()



# Add floodplain habitat -------------------------------------------------------
r_to_r_baseline_fr_fp <- DSMhabitat::fr_fp$biop_itp_2018_2019

# Add Lower-mid sacramento river floodplain habitat 
add_project_habitat <- DSMhabitat::fr_fp$biop_itp_2018_2019["Lower-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Lower-mid Sacramento River" , 
                                "fr", "juv", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_fp$biop_itp_2018_2019["Lower-mid Sacramento River" , , ] + add_project_habitat

r_to_r_baseline_fr_fp["Lower-mid Sacramento River" , , ] <- updated_habitat 

# TUolumne river floodplain 
add_project_habitat <- DSMhabitat::fr_fp$biop_itp_2018_2019["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Tuolumne River" , 
                                "fr", "juv", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_fp$biop_itp_2018_2019["Tuolumne River" , , ] + add_project_habitat

r_to_r_baseline_fr_fp["Tuolumne River" , , ] <- updated_habitat 

# Add Yuba floodlplain habitat 
add_project_habitat <- DSMhabitat::fr_fp$biop_itp_2018_2019["Yuba River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Yuba River" , 
                                "fr", "juv", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::fr_fp$biop_itp_2018_2019["Yuba River" , , ] + add_project_habitat

r_to_r_baseline_fr_fp["Yuba River" , , ] <- updated_habitat 


# check hab differences 
# yuba and lower-mid sac should be different 
r_to_r_baseline_fr_fp == DSMhabitat::fr_fp$biop_itp_2018_2019

# Save as data object to DSMhabitat
fr_fp <- c(DSMhabitat::fr_fp, r_to_r_baseline = list(r_to_r_baseline_fr_fp))
usethis::use_data(fr_fp, overwrite = TRUE)

# Exploratory plot 
r_to_r_baseline_habitat <- r_to_r_baseline_fr_fp |> 
  DSMhabitat::square_meters_to_acres()

sit_habitat <- DSMhabitat::fr_fp$biop_itp_2018_2019 |> DSMhabitat::square_meters_to_acres()

fp <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    sit_habitat = as.vector(sit_habitat),
    r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
  filter(watershed %in% c("Yuba River", "Lower-mid Sacramento River", "Tuolumne River"))

fp |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            sit_habitat, r_to_r_baseline_habitat) |> 
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()




# Add north delta habitat ------------------------------------------------------
r_to_r_baseline_delta <- DSMhabitat::delta_habitat
add_project_habitat <- DSMhabitat::delta_habitat[ , ,"North Delta" ] * 
  hab_prop_change_from_projects("floodplain rearing", "North Delta" , 
                                "fr", "juv", "biop_itp_2018_2019")
updated_habitat <- DSMhabitat::delta_habitat[ , ,"North Delta" ] + add_project_habitat

r_to_r_baseline_delta[ , , "North Delta"] <- updated_habitat 

# check hab differences 
# north delta should be different 
r_to_r_baseline_delta == DSMhabitat::delta_habitat



# Exploratory plot 
r_to_r_baseline_habitat <- r_to_r_baseline_delta[,, "North Delta"] |> 
  DSMhabitat::square_meters_to_acres()

sit_habitat <- DSMhabitat::delta_habitat[,, "North Delta"] |> DSMhabitat::square_meters_to_acres()

delta <- expand_grid(
  watershed = "North Delta",
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    sit_habitat = as.vector(sit_habitat),
    r_to_r_baseline_habitat = as.vector(r_to_r_baseline_habitat)) |> 
  filter(watershed %in% c("North Delta"))

delta |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            sit_habitat, r_to_r_baseline_habitat) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# Save as data object to DSMhabitat
delta_habitat <- c(sit_input = list(DSMhabitat::delta_habitat), r_to_r_baseline = list(r_to_r_baseline_delta))
usethis::use_data(delta_habitat, overwrite = TRUE)