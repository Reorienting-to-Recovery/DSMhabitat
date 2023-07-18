library(tidyverse)
source("data-raw/R2R_baseline_habitat_inputs/generate_hab_helper_functions.R")

# FALL RUN ---------------------------------------------------------------------
# Update DSMhabitat values 
# set r_to_r_baseline_fr_spawn to 
r_to_r_max_flow_fr_spawn <- DSMhabitat::fr_spawn$run_of_river

# Add american river spawning habitat 
add_project_habitat <- DSMhabitat::fr_spawn$run_of_river["American River" , , ] * 
  hab_prop_change_from_projects("spawning", "American River", "fr", "adult", "run_of_river")
updated_habitat <- DSMhabitat::fr_spawn$run_of_river["American River", , ] + add_project_habitat

r_to_r_max_flow_fr_spawn["American River", , ] <- updated_habitat 

# add clear creek spawning habitat
add_project_habitat <- DSMhabitat::fr_spawn$run_of_river["Clear Creek" , , ] * 
  hab_prop_change_from_projects("spawning", "Clear Creek", "fr", "adult", "run_of_river")
updated_habitat <- DSMhabitat::fr_spawn$run_of_river["Clear Creek", , ] + add_project_habitat

r_to_r_max_flow_fr_spawn["Clear Creek", , ] <- updated_habitat 

# add paynes creek spawning habitat
add_project_habitat <- DSMhabitat::fr_spawn$run_of_river["Paynes Creek" , , ] * 
  hab_prop_change_from_projects("spawning", "Paynes Creek", "fr", "adult", "run_of_river")
updated_habitat <- DSMhabitat::fr_spawn$run_of_river["Paynes Creek", , ] + add_project_habitat

r_to_r_max_flow_fr_spawn["Paynes Creek", , ] <- updated_habitat 

# Add sacramento river spawning habitat 
add_project_habitat <- DSMhabitat::fr_spawn$run_of_river["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("spawning", "Upper Sacramento River", "fr", "adult", "run_of_river")
updated_habitat <- DSMhabitat::fr_spawn$run_of_river["Upper Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_fr_spawn["Upper Sacramento River", , ] <- updated_habitat 

# check habitat differences 
# upper sac and american river should be different (larger)
r_to_r_max_flow_fr_spawn == DSMhabitat::fr_spawn$run_of_river

# Save as data object to DSMhabitat
current_fr_spawn <- DSMhabitat::fr_spawn
current_fr_spawn$max_flow_w_hab_projects <- r_to_r_max_flow_fr_spawn
fr_spawn <- current_fr_spawn
usethis::use_data(fr_spawn, overwrite = TRUE)

# Exploratory plot 
r_to_r_max_flow_habitat <- r_to_r_max_flow_fr_spawn |> 
  DSMhabitat::square_meters_to_acres()

run_of_river_flow <- DSMhabitat::fr_spawn$run_of_river |> DSMhabitat::square_meters_to_acres()

spawn <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1979:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river_flow = as.vector(run_of_river_flow),
    r_to_r_max_flow_habitat = as.vector(r_to_r_max_flow_habitat)) |> 
  filter(watershed %in% c("American River", 
                          "Upper Sacramento River", 
                          "Paynes Creek", 
                          "Clear Creek"))

spawn |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river_flow, r_to_r_max_flow_habitat) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()




# Add inchannel habitat to both fry and juvenile habitat objects ---------------
# set r_to_r_baseline_fr_juv and fry 
r_to_r_max_flow_fr_juv <- DSMhabitat::fr_juv$run_of_river
r_to_r_max_flow_fr_fry <- DSMhabitat::fr_fry$run_of_river

# Add american river juv habitat 
add_project_habitat <- DSMhabitat::fr_juv$run_of_river["American River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::fr_juv$run_of_river["American River", , ] + add_project_habitat

r_to_r_max_flow_fr_juv["American River", , ] <- updated_habitat 

# Add american river fry habitat 
add_project_habitat <- DSMhabitat::fr_fry$run_of_river["American River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "fry", "run_of_river")
updated_habitat <- DSMhabitat::fr_fry$run_of_river["American River", , ] + add_project_habitat

r_to_r_max_flow_fr_fry["American River", , ] <- updated_habitat 

# add tuolumne river juv habitat
add_project_habitat <- DSMhabitat::fr_juv$run_of_river["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Tuolumne River", "fr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::fr_juv$run_of_river["Tuolumne River", , ] + add_project_habitat

r_to_r_max_flow_fr_juv["Tuolumne River", , ] <- updated_habitat 

# add tuolumne river fry habitat
add_project_habitat <- DSMhabitat::fr_fry$run_of_river["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Tuolumne River", "fr", "fry", "run_of_river")
updated_habitat <- DSMhabitat::fr_fry$run_of_river["Tuolumne River", , ] + add_project_habitat

r_to_r_max_flow_fr_fry["Tuolumne River", , ] <- updated_habitat 

# Upper sac river 
# Add upper sac river juv habitat 
add_project_habitat <- DSMhabitat::fr_juv$run_of_river["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "fr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::fr_juv$run_of_river["Upper Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_fr_juv["Upper Sacramento River", , ] <- updated_habitat 

# Add upper sacramento  river fry habitat 
add_project_habitat <- DSMhabitat::fr_fry$run_of_river["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "fr", "fry", "run_of_river")

updated_habitat <- DSMhabitat::fr_fry$run_of_river["Upper Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_fr_fry["Upper Sacramento River", , ] <- updated_habitat 

# add Upper-mid Sacramento River
# Add upper mid sac river juv habitat 
add_project_habitat <- DSMhabitat::fr_juv$run_of_river["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "fr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::fr_juv$run_of_river["Upper-mid Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_fr_juv["Upper-mid Sacramento River", , ] <- updated_habitat 

# Add upper mid sacramento river fry habitat 
add_project_habitat <- DSMhabitat::fr_fry$run_of_river["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "fr", "fry", "run_of_river")
updated_habitat <- DSMhabitat::fr_fry$run_of_river["Upper-mid Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_fr_fry["Upper-mid Sacramento River", , ] <- updated_habitat 

# check r to r vs dsm habitat 
# upper-mid sac, upper sac, and american river should be different 
r_to_r_max_flow_fr_juv == DSMhabitat::fr_juv$run_of_river
r_to_r_max_flow_fr_fry == DSMhabitat::fr_fry$run_of_river

# Save as data object to DSMhabitat
current_fr_juv <- DSMhabitat::fr_juv
current_fr_juv$max_flow_w_hab_projects <- r_to_r_max_flow_fr_juv
fr_juv <- current_fr_juv
usethis::use_data(fr_juv, overwrite = TRUE)

# Save as data object to DSMhabitat
current_fr_fry <- DSMhabitat::fr_fry
current_fr_fry$max_flow_w_hab_projects <- r_to_r_max_flow_fr_fry
fr_fry <- current_fr_fry
usethis::use_data(fr_fry, overwrite = TRUE)

# Exploratory plot 
r_to_r_max_flow_fr_juv <- r_to_r_max_flow_fr_juv |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::fr_juv$run_of_river |> DSMhabitat::square_meters_to_acres()

ic_juv <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_fr_juv = as.vector(r_to_r_max_flow_fr_juv)) |> 
  filter(watershed %in% c("American River", 
                          "Tuolumne River",
                          "Upper Sacramento River",
                          "Upper-mid Sacramento River"
  ))

ic_juv |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_fr_juv) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# Exploratory plot 
r_to_r_max_flow_fr_fry <- r_to_r_max_flow_fr_fry |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::fr_fry$run_of_river |> DSMhabitat::square_meters_to_acres()

ic_fry <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_fr_fry = as.vector(r_to_r_max_flow_fr_fry)) |> 
  filter(watershed %in% c("American River", 
                          "Tuolumne River",
                          "Upper Sacramento River",
                          "Upper-mid Sacramento River"))

ic_fry|> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_fr_fry) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()



# Add floodplain habitat -------------------------------------------------------
r_to_r_max_flow_fr_fp <- DSMhabitat::fr_fp$run_of_river

# Add Lower-mid sacramento river floodplain habitat 
add_project_habitat <- DSMhabitat::fr_fp$run_of_river["Lower-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Lower-mid Sacramento River" , 
                                "fr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::fr_fp$run_of_river["Lower-mid Sacramento River" , , ] + add_project_habitat

r_to_r_max_flow_fr_fp["Lower-mid Sacramento River" , , ] <- updated_habitat 

# TUolumne river floodplain 
add_project_habitat <- DSMhabitat::fr_fp$run_of_river["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Tuolumne River" , 
                                "fr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::fr_fp$run_of_river["Tuolumne River" , , ] + add_project_habitat

r_to_r_max_flow_fr_fp["Tuolumne River" , , ] <- updated_habitat 

# Add Yuba floodlplain habitat 
add_project_habitat <- DSMhabitat::fr_fp$run_of_river["Yuba River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Yuba River" , 
                                "fr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::fr_fp$run_of_river["Yuba River" , , ] + add_project_habitat

r_to_r_max_flow_fr_fp["Yuba River" , , ] <- updated_habitat 


# check hab differences 
# yuba and lower-mid sac should be different 
r_to_r_max_flow_fr_fp == DSMhabitat::fr_fp$run_of_river

# Save as data object to DSMhabitat
current_fr_fp <- DSMhabitat::fr_fp
current_fr_fp$max_flow_w_hab_projects <- r_to_r_max_flow_fr_fp
fr_fp <- current_fr_fp
usethis::use_data(fr_fp, overwrite = TRUE)

# Exploratory plot 
r_to_r_max_flow_fr_fp <- r_to_r_max_flow_fr_fp |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::fr_fp$run_of_river |> DSMhabitat::square_meters_to_acres()

fp <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_fr_fp = as.vector(r_to_r_max_flow_fr_fp)) |> 
  filter(watershed %in% c("Yuba River", "Lower-mid Sacramento River", "Tuolumne River"))

fp |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_fr_fp) |> 
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# SPRING RUN ---------------------------------------------------------------------
# Update DSMhabitat values 
# set r_to_r_baseline_fr_spawn to 
r_to_r_max_flow_sr_spawn <- DSMhabitat::sr_spawn$run_of_river

# Add cottonwood creek spawning habitat 
add_project_habitat <- DSMhabitat::sr_spawn$run_of_river["Cottonwood Creek", , ] * 
  hab_prop_change_from_projects("spawning", "Cottonwood Creek", "sr", "adult", "run_of_river")
updated_habitat <- DSMhabitat::sr_spawn$run_of_river["Cottonwood Creek", , ] + add_project_habitat

r_to_r_max_flow_sr_spawn["Cottonwood Creek", , ] <- updated_habitat 

# add deer creek spawning habitat
add_project_habitat <- DSMhabitat::sr_spawn$run_of_river["Deer Creek" , , ] * 
  hab_prop_change_from_projects("spawning", "Deer Creek", "sr", "adult", "run_of_river")
updated_habitat <- DSMhabitat::sr_spawn$run_of_river["Deer Creek", , ] + add_project_habitat

r_to_r_max_flow_sr_spawn["Deer Creek", , ] <- updated_habitat 

# Add sacramento river spawning habitat 
add_project_habitat <- DSMhabitat::sr_spawn$run_of_river["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("spawning", "Upper Sacramento River", "sr", "adult", "run_of_river")
updated_habitat <- DSMhabitat::sr_spawn$run_of_river["Upper Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_sr_spawn["Upper Sacramento River", , ] <- updated_habitat 

# check habitat differences 
# upper sac and american river should be different (larger)
r_to_r_max_flow_sr_spawn == DSMhabitat::sr_spawn$run_of_river

# Save as data object to DSMhabitat
current_sr_spawn <- DSMhabitat::sr_spawn
current_sr_spawn$max_flow_w_hab_projects <- r_to_r_max_flow_sr_spawn
sr_spawn <- current_sr_spawn
usethis::use_data(sr_spawn, overwrite = TRUE)

# Exploratory plot 
r_to_r_max_flow_sr_spawn <- r_to_r_max_flow_sr_spawn |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::sr_spawn$run_of_river |> DSMhabitat::square_meters_to_acres()

spawn <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1979:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_sr_spawn = as.vector(r_to_r_max_flow_sr_spawn)) |> 
  filter(watershed %in% c("Cottonwood Creek", 
                          "Upper Sacramento River", 
                          "Deer Creek"))

spawn |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_sr_spawn) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()




# Add inchannel habitat to both fry and juvenile habitat objects ---------------
# set r_to_r_baseline_fr_juv and fry 
r_to_r_max_flow_sr_juv <- DSMhabitat::sr_juv$run_of_river
r_to_r_max_flow_sr_fry <- DSMhabitat::sr_fry$run_of_river

# add tuolumne river juv habitat
add_project_habitat <- DSMhabitat::sr_juv$run_of_river["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Tuolumne River", "sr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::sr_juv$run_of_river["Tuolumne River", , ] + add_project_habitat

r_to_r_max_flow_sr_juv["Tuolumne River", , ] <- updated_habitat 

# add tuolumne river fry habitat
add_project_habitat <- DSMhabitat::sr_fry$run_of_river["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Tuolumne River", "sr", "fry", "run_of_river")
updated_habitat <- DSMhabitat::sr_fry$run_of_river["Tuolumne River", , ] + add_project_habitat

r_to_r_max_flow_sr_fry["Tuolumne River", , ] <- updated_habitat 

# Upper sac river 
# Add upper sac river juv habitat 
add_project_habitat <- DSMhabitat::sr_juv$run_of_river["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "sr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::sr_juv$run_of_river["Upper Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_sr_juv["Upper Sacramento River", , ] <- updated_habitat 

# Add upper sacramento  river fry habitat 
add_project_habitat <- DSMhabitat::sr_fry$run_of_river["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "sr", "fry", "run_of_river")

updated_habitat <- DSMhabitat::sr_fry$run_of_river["Upper Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_sr_fry["Upper Sacramento River", , ] <- updated_habitat 

# add Upper-mid Sacramento River
# Add upper mid sac river juv habitat 
add_project_habitat <- DSMhabitat::sr_juv$run_of_river["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "sr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::sr_juv$run_of_river["Upper-mid Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_sr_juv["Upper-mid Sacramento River", , ] <- updated_habitat 

# Add upper mid sacramento river fry habitat 
add_project_habitat <- DSMhabitat::sr_fry$run_of_river["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "sr", "fry", "run_of_river")
updated_habitat <- DSMhabitat::sr_fry$run_of_river["Upper-mid Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_sr_fry["Upper-mid Sacramento River", , ] <- updated_habitat 

# check r to r vs dsm habitat 
# upper-mid sac, upper sac, and american river should be different 
r_to_r_max_flow_sr_juv == DSMhabitat::sr_juv$run_of_river
r_to_r_max_flow_sr_fry == DSMhabitat::sr_fry$run_of_river

# Save as data object to DSMhabitat
current_sr_juv <- DSMhabitat::sr_juv
current_sr_juv$max_flow_w_hab_projects <- r_to_r_max_flow_sr_juv
sr_juv <- current_sr_juv
usethis::use_data(sr_juv, overwrite = TRUE)

# Save as data object to DSMhabitat
current_sr_fry <- DSMhabitat::sr_fry
current_sr_fry$max_flow_w_hab_projects <- r_to_r_max_flow_sr_fry
sr_fry <- current_sr_fry
usethis::use_data(sr_fry, overwrite = TRUE)

# Exploratory plot 
r_to_r_max_flow_sr_juv <- r_to_r_max_flow_sr_juv |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::sr_juv$run_of_river |> DSMhabitat::square_meters_to_acres()

ic_juv <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_sr_juv = as.vector(r_to_r_max_flow_sr_juv)) |> 
  filter(watershed %in% c("Tuolumne River",
                          "Upper Sacramento River",
                          "Upper-mid Sacramento River"
  ))

ic_juv |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_sr_juv) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# Exploratory plot 
r_to_r_max_flow_sr_fry <- r_to_r_max_flow_sr_fry |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::sr_fry$run_of_river |> DSMhabitat::square_meters_to_acres()

ic_fry <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_sr_fry = as.vector(r_to_r_max_flow_sr_fry)) |> 
  filter(watershed %in% c("Tuolumne River",
                          "Upper Sacramento River",
                          "Upper-mid Sacramento River"))

ic_fry|> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_sr_fry) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()



# Add floodplain habitat -------------------------------------------------------
r_to_r_max_flow_sr_fp <- DSMhabitat::sr_fp$run_of_river

# Add Lower-mid sacramento river floodplain habitat 
add_project_habitat <- DSMhabitat::sr_fp$run_of_river["Lower-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Lower-mid Sacramento River" , 
                                "sr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::sr_fp$run_of_river["Lower-mid Sacramento River" , , ] + add_project_habitat

r_to_r_max_flow_sr_fp["Lower-mid Sacramento River" , , ] <- updated_habitat 

# Tuolumne river floodplain 
add_project_habitat <- DSMhabitat::sr_fp$run_of_river["Tuolumne River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Tuolumne River" , 
                                "sr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::sr_fp$run_of_river["Tuolumne River" , , ] + add_project_habitat

r_to_r_max_flow_sr_fp["Tuolumne River" , , ] <- updated_habitat 

# Add Yuba floodlplain habitat 
add_project_habitat <- DSMhabitat::sr_fp$run_of_river["Yuba River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Yuba River" , 
                                "sr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::sr_fp$run_of_river["Yuba River" , , ] + add_project_habitat

r_to_r_max_flow_sr_fp["Yuba River" , , ] <- updated_habitat 


# check hab differences 
# yuba and lower-mid sac should be different 
r_to_r_max_flow_sr_fp == DSMhabitat::sr_fp$run_of_river

# Save as data object to DSMhabitat
current_sr_fp <- DSMhabitat::sr_fp
current_sr_fp$max_flow_w_hab_projects <- r_to_r_max_flow_sr_fp
sr_fp <- current_sr_fp
usethis::use_data(sr_fp, overwrite = TRUE)

# Exploratory plot 
r_to_r_max_flow_sr_fp <- r_to_r_max_flow_sr_fp |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::sr_fp$run_of_river |> DSMhabitat::square_meters_to_acres()

fp <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_sr_fp = as.vector(r_to_r_max_flow_sr_fp)) |> 
  filter(watershed %in% c("Yuba River", "Lower-mid Sacramento River", "Tuolumne River"))

fp |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_sr_fp) |> 
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# WINTER RUN ---------------------------------------------------------------------
# Update DSMhabitat values 
# set r_to_r_baseline_fr_spawn to 
r_to_r_max_flow_wr_spawn <- DSMhabitat::wr_spawn$run_of_river

# Add sacramento river spawning habitat 
add_project_habitat <- DSMhabitat::wr_spawn$run_of_river["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("spawning", "Upper Sacramento River", "wr", "adult", "run_of_river")
updated_habitat <- DSMhabitat::wr_spawn$run_of_river["Upper Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_wr_spawn["Upper Sacramento River", , ] <- updated_habitat 

# check habitat differences 
# upper sac and american river should be different (larger)
r_to_r_max_flow_wr_spawn == DSMhabitat::wr_spawn$run_of_river

# Save as data object to DSMhabitat
current_wr_spawn <- DSMhabitat::wr_spawn
current_wr_spawn$max_flow_w_hab_projects <- r_to_r_max_flow_wr_spawn
wr_spawn <- current_wr_spawn
usethis::use_data(wr_spawn, overwrite = TRUE)

# Exploratory plot 
r_to_r_max_flow_wr_spawn <- r_to_r_max_flow_wr_spawn |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::wr_spawn$run_of_river |> DSMhabitat::square_meters_to_acres()

spawn <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1979:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_wr_spawn = as.vector(r_to_r_max_flow_wr_spawn)) |> 
  filter(watershed %in% c("Upper Sacramento River"))

spawn |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_wr_spawn) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# Add inchannel habitat to both fry and juvenile habitat objects ---------------
# set r_to_r_baseline_fr_juv and fry 
r_to_r_max_flow_wr_juv <- DSMhabitat::wr_juv$run_of_river
r_to_r_max_flow_wr_fry <- DSMhabitat::wr_fry$run_of_river

# Upper sac river 
# Add upper sac river juv habitat 
add_project_habitat <- DSMhabitat::wr_juv$run_of_river["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "wr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::wr_juv$run_of_river["Upper Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_wr_juv["Upper Sacramento River", , ] <- updated_habitat 

# Add upper sacramento  river fry habitat 
add_project_habitat <- DSMhabitat::wr_fry$run_of_river["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "wr", "fry", "run_of_river")

updated_habitat <- DSMhabitat::wr_fry$run_of_river["Upper Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_wr_fry["Upper Sacramento River", , ] <- updated_habitat 

# add Upper-mid Sacramento River
# Add upper mid sac river juv habitat 
add_project_habitat <- DSMhabitat::wr_juv$run_of_river["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "wr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::wr_juv$run_of_river["Upper-mid Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_wr_juv["Upper-mid Sacramento River", , ] <- updated_habitat 

# Add upper mid sacramento river fry habitat 
add_project_habitat <- DSMhabitat::wr_fry$run_of_river["Upper-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper-mid Sacramento River", 
                                "wr", "fry", "run_of_river")
updated_habitat <- DSMhabitat::wr_fry$run_of_river["Upper-mid Sacramento River", , ] + add_project_habitat

r_to_r_max_flow_wr_fry["Upper-mid Sacramento River", , ] <- updated_habitat 

# check r to r vs dsm habitat 
# upper-mid sac, upper sac, and american river should be different 
r_to_r_max_flow_wr_juv == DSMhabitat::wr_juv$run_of_river
r_to_r_max_flow_wr_fry == DSMhabitat::wr_fry$run_of_river

# Save as data object to DSMhabitat
current_wr_juv <- DSMhabitat::wr_juv
current_wr_juv$max_flow_w_hab_projects <- r_to_r_max_flow_wr_juv
wr_juv <- current_wr_juv
usethis::use_data(wr_juv, overwrite = TRUE)

# Save as data object to DSMhabitat
current_wr_fry <- DSMhabitat::wr_fry
current_wr_fry$max_flow_w_hab_projects <- r_to_r_max_flow_wr_fry
wr_fry <- current_wr_fry
usethis::use_data(wr_fry, overwrite = TRUE)

# Exploratory plot 
r_to_r_max_flow_wr_juv <- r_to_r_max_flow_wr_juv |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::wr_juv$run_of_river |> DSMhabitat::square_meters_to_acres()

ic_juv <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_wr_juv = as.vector(r_to_r_max_flow_wr_juv)) |> 
  filter(watershed %in% c( "Upper Sacramento River",
                          "Upper-mid Sacramento River"
  ))

ic_juv |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_wr_juv) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# Exploratory plot 
r_to_r_max_flow_wr_fry <- r_to_r_max_flow_wr_fry |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::wr_fry$run_of_river |> DSMhabitat::square_meters_to_acres()

ic_fry <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_wr_fry = as.vector(r_to_r_max_flow_wr_fry)) |> 
  filter(watershed %in% c("Upper Sacramento River",
                          "Upper-mid Sacramento River"))

ic_fry|> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_wr_fry) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()



# Add floodplain habitat -------------------------------------------------------
r_to_r_max_flow_wr_fp <- DSMhabitat::wr_fp$run_of_river

# Add Lower-mid sacramento river floodplain habitat 
add_project_habitat <- DSMhabitat::wr_fp$run_of_river["Lower-mid Sacramento River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Lower-mid Sacramento River" , 
                                "wr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::wr_fp$run_of_river["Lower-mid Sacramento River" , , ] + add_project_habitat

r_to_r_max_flow_wr_fp["Lower-mid Sacramento River" , , ] <- updated_habitat 

# check hab differences 
# yuba and lower-mid sac should be different 
r_to_r_max_flow_wr_fp == DSMhabitat::sr_fp$run_of_river

# Save as data object to DSMhabitat
current_wr_fp <- DSMhabitat::wr_fp
current_wr_fp$max_flow_w_hab_projects <- r_to_r_max_flow_wr_fp
wr_fp <- current_wr_fp
usethis::use_data(wr_fp, overwrite = TRUE)

# Exploratory plot 
r_to_r_max_flow_wr_fp <- r_to_r_max_flow_wr_fp |> 
  DSMhabitat::square_meters_to_acres()

run_of_river <- DSMhabitat::wr_fp$run_of_river |> DSMhabitat::square_meters_to_acres()

fp <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    run_of_river = as.vector(run_of_river),
    r_to_r_max_flow_wr_fp = as.vector(r_to_r_max_flow_wr_fp)) |> 
  filter(watershed %in% c("Lower-mid Sacramento River"))

fp |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            run_of_river, r_to_r_max_flow_wr_fp) |> 
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'))) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# all runs - delta 
# 
# Add north delta habitat ------------------------------------------------------
r_to_r_baseline_delta <- DSMhabitat::delta_habitat$sit_habitat
add_project_habitat <- DSMhabitat::delta_habitat$sit_habitat[ , ,"North Delta" ] * 
  hab_prop_change_from_projects("floodplain rearing", "North Delta" , 
                                "fr", "juv", "run_of_river")
updated_habitat <- DSMhabitat::delta_habitat$sit_habitat[ , ,"North Delta" ] + add_project_habitat

r_to_r_baseline_delta[ , , "North Delta"] <- updated_habitat 

# check hab differences 
# north delta should be different 
r_to_r_baseline_delta == DSMhabitat::delta_habitat$sit_habitat



# Exploratory plot 
r_to_r_baseline_habitat <- r_to_r_baseline_delta[,, "North Delta"] |> 
  DSMhabitat::square_meters_to_acres()

sit_habitat <- DSMhabitat::delta_habitat$sit_habitat[,, "North Delta"] |> DSMhabitat::square_meters_to_acres()

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
delta_habitat <- modifyList(DSMhabitat::delta_habitat, list(sit_habitat = DSMhabitat::delta_habitat$sit_habitat, 
                                                            r_to_r_baseline = r_to_r_baseline_delta,
                                                            max_flow_w_hab_projects = r_to_r_baseline_delta)) # same as ba
usethis::use_data(delta_habitat, overwrite = TRUE)
