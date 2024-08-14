# Cache HRL habitat 

all_existing_and_hrl_data <- readRDS(here::here('data-raw', "R2R_HRL_habitat_inputs", "all_habitat_data_for_hrl_inputs_all_runs.rdata")) 

# source functions: 
source(here::here('data-raw', "R2R_HRL_habitat_inputs", "hrl_helper_functions.R"))

# FALL RUN ---------------------------------------------------------------------
# spawning:  --------------------------------------------------------------
# American, Upper Sac, Feather 

# American river
r_to_r_lto_12a_baseline_fr_spawn <- DSMhabitat::fr_spawn$r_to_r_lto_12a_baseline

add_project_habitat <- DSMhabitat::fr_spawn$r_to_r_lto_12a_baseline["American River" , , ] * 
  hab_prop_change_from_projects("spawning", "American River", "fr", "spawn", "LTO_12a")
updated_habitat <- DSMhabitat::fr_spawn$r_to_r_lto_12a_baseline["American River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_spawn["American River", , ] <- updated_habitat 

# Upper Sacramento River:
add_project_habitat <- DSMhabitat::fr_spawn$r_to_r_lto_12a_baseline["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("spawning", "Upper Sacramento River", "fr", "spawn", "LTO_12a")
updated_habitat <- DSMhabitat::fr_spawn$r_to_r_lto_12a_baseline["Upper Sacramento River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_spawn["Upper Sacramento River", , ] <- updated_habitat 

# Feather River: 
add_project_habitat <- DSMhabitat::fr_spawn$r_to_r_lto_12a_baseline["Feather River" , , ] * 
  hab_prop_change_from_projects("spawning", "Feather River", "fr", "spawn", "LTO_12a")
updated_habitat <- DSMhabitat::fr_spawn$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_spawn["Feather River", , ] <- updated_habitat 

# double check the changes: 
r_to_r_lto_12a_baseline_fr_spawn == DSMhabitat::fr_spawn$r_to_r_lto_12a_baseline

# Save as data object to DSMhabitat
current_fr_spawn <- DSMhabitat::fr_spawn
current_fr_spawn$r_to_r_hrl <- r_to_r_lto_12a_baseline_fr_spawn
fr_spawn <- current_fr_spawn
usethis::use_data(fr_spawn, overwrite = TRUE)

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_fr_spawn |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- fr_spawn$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "spawn" , 
                    watersheds = c("Feather River", "American River", "Upper Sacramento River")) 


# In channel rearing  -----------------------------------------------------
# Juv ---------------------------------------------------------------------
# American, Sacramento, Yuba, Mokelumne, Feather 
all_existing_and_hrl_data |> filter(habitat_type == "inchannel rearing" & run == "fall") |> 
  pull(watershed)

r_to_r_lto_12a_baseline_fr_juv <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline

# American river
add_project_habitat <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline["American River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline["American River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_juv["American River", , ] <- updated_habitat 

# Sacramento 
# TODO: flagging since Sacramento was changed to Upper Sacramento
add_project_habitat <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "fr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline["Upper Sacramento River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_juv["Upper Sacramento River", , ] <- updated_habitat 

# Yuba
add_project_habitat <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline["Yuba River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Yuba River", "fr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline["Yuba River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_juv["Yuba River", , ] <- updated_habitat 

# Mokelumne 
add_project_habitat <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline["Mokelumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Mokelumne River", "fr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline["Mokelumne River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_juv["Mokelumne River", , ] <- updated_habitat 

# Feather 
add_project_habitat <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline["Feather River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Feather River", "fr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::fr_juv$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_juv["Feather River", , ] <- updated_habitat 

# compare: 
r_to_r_lto_12a_baseline_fr_juv == DSMhabitat::fr_juv$r_to_r_lto_12a_baseline

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_fr_juv |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- fr_juv$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "juv" , 
                    watersheds = c("Feather River", "American River", "Mokelumne River", "Yuba River",
                                   "Upper Sacramento River"))

# Save as data object to DSMhabitat
current_fr_juv <- DSMhabitat::fr_juv
current_fr_juv$r_to_r_hrl <- r_to_r_lto_12a_baseline_fr_juv
fr_juv <- current_fr_juv
usethis::use_data(fr_juv, overwrite = TRUE)


# Fry ---------------------------------------------------------------------
# American, Sacramento, Yuba, Mokelumne, Feather 
all_existing_and_hrl_data |> filter(habitat_type == "inchannel rearing" & run == "fall") |> 
  pull(watershed)

r_to_r_lto_12a_baseline_fr_fry <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline

# American river
add_project_habitat <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline["American River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline["American River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_fry["American River", , ] <- updated_habitat 

# Sacramento 
# TODO: flagging since Sacramento was changed to Upper Sacramento
add_project_habitat <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "fr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline["Upper Sacramento River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_fry["Upper Sacramento River", , ] <- updated_habitat 

# Yuba
add_project_habitat <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline["Yuba River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Yuba River", "fr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline["Yuba River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_fry["Yuba River", , ] <- updated_habitat 

# Mokelumne 
add_project_habitat <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline["Mokelumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Mokelumne River", "fr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline["Mokelumne River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_fry["Mokelumne River", , ] <- updated_habitat 

# Feather 
add_project_habitat <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline["Feather River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Feather River", "fr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::fr_fry$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_fry["Feather River", , ] <- updated_habitat 

# compare: 
r_to_r_lto_12a_baseline_fr_fry == DSMhabitat::fr_fry$r_to_r_lto_12a_baseline

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_fr_fry |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- fr_fry$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "fry" , 
                    watersheds = c("Feather River", "American River", "Mokelumne River", "Yuba River",
                                   "Upper Sacramento River"))

# Save as data object to DSMhabitat
current_fr_fry <- DSMhabitat::fr_fry
current_fr_fry$r_to_r_hrl <- r_to_r_lto_12a_baseline_fr_fry
fr_fry <- current_fr_fry
usethis::use_data(fr_fry, overwrite = TRUE)

# Floodplain Rearing ------------------------------------------------------
# North Delta, Yuba River, Sutter Bypass, Mokelume River, Feather River 
all_existing_and_hrl_data |> filter(habitat_type == "floodplain rearing" & run == "fall") |> 
  pull(watershed)

r_to_r_lto_12a_baseline_fr_fp <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline

# Yuba river
add_project_habitat <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline["Yuba River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Yuba River", "fr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline["Yuba River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_fp["Yuba River", , ] <- updated_habitat 

# Mokelumne River 
add_project_habitat <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline["Mokelumne River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Mokelumne River", "fr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline["Mokelumne River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_fp["Mokelumne River", , ] <- updated_habitat 

# Feather River
add_project_habitat <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline["Feather River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Feather River", "fr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_fp["Feather River", , ] <- updated_habitat 

# Sutter Bypass
# Note: all Sutter Bypass values are zero so there is no proportional change with the new HRL data 
# THIS should probably be addressed 
add_project_habitat <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline["Sutter Bypass" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Sutter Bypass", "fr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::fr_fp$r_to_r_lto_12a_baseline["Sutter Bypass", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_fr_fp["Sutter Bypass", , ] <- updated_habitat 

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_fr_fp |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- fr_fp$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "flood" , 
                    watersheds = c("Feather River", "Yuba River", "Mokelumne River", "Sutter Bypass"))

# Save as data object to DSMhabitat
current_fr_fp <- DSMhabitat::fr_fp
current_fr_fp$r_to_r_hrl <- r_to_r_lto_12a_baseline_fr_fp
fr_fp <- current_fr_fp
usethis::use_data(fr_fp, overwrite = TRUE)

# North delta: 
# # TODO: does this need to be r_to_r_baseline_lto? We will need that data object 
r_to_r_baseline_delta <- DSMhabitat::delta_habitat$r_to_r_baseline
add_project_habitat <- DSMhabitat::delta_habitat$r_to_r_baseline[,,"North Delta" ] * 
  hab_prop_change_from_projects("floodplain rearing", "North Delta", "fr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::delta_habitat$r_to_r_baseline[ , ,"North Delta" ] + add_project_habitat
r_to_r_baseline_delta[ , , "North Delta"] <- updated_habitat 

# Delta plot: 
r_to_r_hrl_habitat<- r_to_r_baseline_delta[,, "North Delta"] |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline <- DSMhabitat::delta_habitat$r_to_r_baseline[,, "North Delta"] |> DSMhabitat::square_meters_to_acres()

delta <- expand_grid(
  watershed = "North Delta",
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    r_to_r_baseline = as.vector(r_to_r_baseline),
    r_to_r_hrl_habitat = as.vector(r_to_r_hrl_habitat)) |> 
  filter(watershed %in% c("North Delta"))

delta |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            r_to_r_baseline, r_to_r_hrl_habitat) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# Save as data object to DSMhabitat
current_delta <- DSMhabitat::delta_habitat
current_delta$r_to_r_hrl <- r_to_r_baseline_delta
delta_habitat <- current_delta
usethis::use_data(delta_habitat, overwrite = TRUE)


# SPRING RUN --------------------------------------------------------------
# spawning:  --------------------------------------------------------------
# American, Upper Sac, Feather 
all_existing_and_hrl_data |> filter(habitat_type == "spawning" & run == "spring") |> 
  pull(watershed)

r_to_r_lto_12a_baseline_sr_spawn <- DSMhabitat::sr_spawn$r_to_r_lto_12a_baseline

# American river
# Removing since no spawning in American spring run:
# add_project_habitat <- DSMhabitat::sr_spawn$r_to_r_lto_12a_baseline["American River" , , ] * 
#   hab_prop_change_from_projects("spawning", "American River", "sr", "spawn", "LTO_12a")
# updated_habitat <- DSMhabitat::sr_spawn$r_to_r_lto_12a_baseline["American River", , ] + add_project_habitat 
# r_to_r_lto_12a_baseline_sr_spawn["American River", , ] <- updated_habitat 

# Upper Sacramento River:
add_project_habitat <- DSMhabitat::sr_spawn$r_to_r_lto_12a_baseline["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("spawning", "Upper Sacramento River", "sr", "spawn", "LTO_12a")
updated_habitat <- DSMhabitat::sr_spawn$r_to_r_lto_12a_baseline["Upper Sacramento River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_spawn["Upper Sacramento River", , ] <- updated_habitat 

# Feather River: 
add_project_habitat <- DSMhabitat::sr_spawn$r_to_r_lto_12a_baseline["Feather River" , , ] * 
  hab_prop_change_from_projects("spawning", "Feather River", "sr", "spawn", "LTO_12a")
updated_habitat <- DSMhabitat::sr_spawn$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_spawn["Feather River", , ] <- updated_habitat 

# double check the changes: 
r_to_r_lto_12a_baseline_sr_spawn == DSMhabitat::sr_spawn$r_to_r_lto_12a_baseline

# Save as data object to DSMhabitat
current_sr_spawn <- DSMhabitat::sr_spawn
current_sr_spawn$r_to_r_hrl <- r_to_r_lto_12a_baseline_sr_spawn
sr_spawn <- current_sr_spawn
usethis::use_data(sr_spawn, overwrite = TRUE)

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_sr_spawn |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- sr_spawn$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "spawn" , 
                    watersheds = c("Feather River", "Upper Sacramento River")) 


# In channel rearing  -----------------------------------------------------
# Juv ---------------------------------------------------------------------
# American, Sacramento, Yuba, Mokelumne, Feather 
all_existing_and_hrl_data |> filter(habitat_type == "inchannel rearing" & run == "spring") |> 
  pull(watershed)

r_to_r_lto_12a_baseline_sr_juv <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline

# American river
# Note: that it is scaled off of Fall Run similar to how cache-habitat is done. Otherwise, American River SR will be all NAs 
add_project_habitat <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline["American River" , , ] *
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline["American River", , ] + add_project_habitat
r_to_r_lto_12a_baseline_sr_juv["American River", , ] <- updated_habitat

# Sacramento 
# TODO: flagging since Sacramento was changed to Upper Sacramento
add_project_habitat <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "sr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline["Upper Sacramento River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_juv["Upper Sacramento River", , ] <- updated_habitat 

# Yuba
add_project_habitat <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline["Yuba River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Yuba River", "sr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline["Yuba River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_juv["Yuba River", , ] <- updated_habitat 

# Mokelumne 
add_project_habitat <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline["Mokelumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Mokelumne River", "sr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline["Mokelumne River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_juv["Mokelumne River", , ] <- updated_habitat 

# Feather 
add_project_habitat <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline["Feather River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Feather River", "sr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::sr_juv$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_juv["Feather River", , ] <- updated_habitat 

# compare: 
r_to_r_lto_12a_baseline_sr_juv == DSMhabitat::sr_juv$r_to_r_lto_12a_baseline

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_sr_juv |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- sr_juv$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "juv" , 
                    watersheds = c("Feather River", "American River", "Mokelumne River", "Yuba River",
                                   "Upper Sacramento River"))

# Save as data object to DSMhabitat
current_sr_juv <- DSMhabitat::sr_juv
current_sr_juv$r_to_r_hrl <- r_to_r_lto_12a_baseline_sr_juv
sr_juv <- current_sr_juv
usethis::use_data(sr_juv, overwrite = TRUE)

# Fry ---------------------------------------------------------------------
# American, Sacramento, Yuba, Mokelumne, Feather 
r_to_r_lto_12a_baseline_sr_fry <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline

# American river
# Using Fall Run as scaling
add_project_habitat <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline["American River" , , ] *
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline["American River", , ] + add_project_habitat
r_to_r_lto_12a_baseline_sr_fry["American River", , ] <- updated_habitat

# Sacramento 
# TODO: flagging since Sacramento was changed to Upper Sacramento
add_project_habitat <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "sr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline["Upper Sacramento River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_fry["Upper Sacramento River", , ] <- updated_habitat 

# Yuba
add_project_habitat <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline["Yuba River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Yuba River", "sr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline["Yuba River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_fry["Yuba River", , ] <- updated_habitat 

# Mokelumne 
add_project_habitat <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline["Mokelumne River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Mokelumne River", "sr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline["Mokelumne River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_fry["Mokelumne River", , ] <- updated_habitat 

# Feather 
add_project_habitat <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline["Feather River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Feather River", "sr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::sr_fry$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_fry["Feather River", , ] <- updated_habitat 

# compare: 
r_to_r_lto_12a_baseline_sr_fry == DSMhabitat::sr_fry$r_to_r_lto_12a_baseline

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_sr_fry |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- sr_fry$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "fry" , 
                    watersheds = c("Feather River", "American River", "Mokelumne River", "Yuba River",
                                   "Upper Sacramento River"))

# Save as data object to DSMhabitat
current_sr_fry <- DSMhabitat::sr_fry
current_sr_fry$r_to_r_hrl <- r_to_r_lto_12a_baseline_sr_fry
sr_fry <- current_sr_fry
usethis::use_data(sr_fry, overwrite = TRUE)

# Floodplain Rearing ------------------------------------------------------
# North Delta, Yuba River, Sutter Bypass, Mokelume River, Feather River 
all_existing_and_hrl_data |> filter(habitat_type == "floodplain rearing" & run == "spring") |> 
  pull(watershed)

r_to_r_lto_12a_baseline_sr_fp <- DSMhabitat::sr_fp$r_to_r_lto_12a_baseline

# Yuba river
add_project_habitat <- DSMhabitat::sr_fp$r_to_r_lto_12a_baseline["Yuba River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Yuba River", "sr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::sr_fp$r_to_r_lto_12a_baseline["Yuba River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_fp["Yuba River", , ] <- updated_habitat 

# Mokelumne River 
add_project_habitat <- DSMhabitat::sr_fp$r_to_r_lto_12a_baseline["Mokelumne River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Mokelumne River", "sr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::sr_fp$r_to_r_lto_12a_baseline["Mokelumne River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_fp["Mokelumne River", , ] <- updated_habitat 

# Feather River
add_project_habitat <- DSMhabitat::sr_fp$r_to_r_lto_12a_baseline["Feather River" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Feather River", "sr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::sr_fp$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_fp["Feather River", , ] <- updated_habitat 

# Sutter Bypass
# Note: all Sutter Bypass values are zero so there is no proportional change with the new HRL data 
# THIS should probably be addressed 
add_project_habitat <- DSMhabitat::sr_fp$r_to_r_lto_12a_baseline["Sutter Bypass" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Sutter Bypass", "sr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::sr_fp$r_to_r_lto_12a_baseline["Sutter Bypass", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_sr_fp["Sutter Bypass", , ] <- updated_habitat 

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_sr_fp |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- sr_fp$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "flood" , 
                    watersheds = c("Feather River", "Yuba River", "Mokelumne River", "Sutter Bypass"))

# Save as data object to DSMhabitat
current_sr_fp <- DSMhabitat::sr_fp
current_sr_fp$r_to_r_hrl <- r_to_r_lto_12a_baseline_sr_fp
sr_fp <- current_sr_fp
usethis::use_data(sr_fp, overwrite = TRUE)


# WINTER RUN --------------------------------------------------------------
# spawning:  --------------------------------------------------------------
# Upper Sac
all_existing_and_hrl_data |> filter(habitat_type == "spawning" & run == "winter") |> 
  pull(watershed)

r_to_r_lto_12a_baseline_wr_spawn <- DSMhabitat::wr_spawn$r_to_r_lto_12a_baseline

# Upper Sacramento River:
add_project_habitat <- DSMhabitat::wr_spawn$r_to_r_lto_12a_baseline["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("spawning", "Upper Sacramento River", "wr", "spawn", "LTO_12a")
updated_habitat <- DSMhabitat::wr_spawn$r_to_r_lto_12a_baseline["Upper Sacramento River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_wr_spawn["Upper Sacramento River", , ] <- updated_habitat 

# double check the changes: 
r_to_r_lto_12a_baseline_wr_spawn == DSMhabitat::wr_spawn$r_to_r_lto_12a_baseline

# Save as data object to DSMhabitat
current_wr_spawn <- DSMhabitat::wr_spawn
current_wr_spawn$r_to_r_hrl <- r_to_r_lto_12a_baseline_wr_spawn
wr_spawn <- current_wr_spawn
usethis::use_data(wr_spawn, overwrite = TRUE)

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_wr_spawn |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- wr_spawn$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "spawn" , 
                    watersheds = c("Upper Sacramento River")) 


# In channel rearing  -----------------------------------------------------
# Juv ---------------------------------------------------------------------
# American, Sacramento,  Mokelumne, Feather 
all_existing_and_hrl_data |> filter(habitat_type == "inchannel rearing" & run == "winter") |> 
  pull(watershed)

r_to_r_lto_12a_baseline_wr_juv <- DSMhabitat::wr_juv$r_to_r_lto_12a_baseline

# American river
# scaling off of Fall Run 
add_project_habitat <- DSMhabitat::wr_juv$r_to_r_lto_12a_baseline["American River" , , ] *
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::wr_juv$r_to_r_lto_12a_baseline["American River", , ] + add_project_habitat
r_to_r_lto_12a_baseline_wr_juv["American River", , ] <- updated_habitat

# Sacramento 
# TODO: flagging since Sacramento was changed to Upper Sacramento
add_project_habitat <- DSMhabitat::wr_juv$r_to_r_lto_12a_baseline["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "wr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::wr_juv$r_to_r_lto_12a_baseline["Upper Sacramento River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_wr_juv["Upper Sacramento River", , ] <- updated_habitat 

# Mokelumne 
# scaling off of FR
add_project_habitat <- DSMhabitat::wr_juv$r_to_r_lto_12a_baseline["Mokelumne River" , , ] *
  hab_prop_change_from_projects("inchannel rearing", "Mokelumne River", "fr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::wr_juv$r_to_r_lto_12a_baseline["Mokelumne River", , ] + add_project_habitat
r_to_r_lto_12a_baseline_wr_juv["Mokelumne River", , ] <- updated_habitat

# Feather 
# scaling off of FR
add_project_habitat <- DSMhabitat::wr_juv$r_to_r_lto_12a_baseline["Feather River" , , ] *
  hab_prop_change_from_projects("inchannel rearing", "Feather River", "fr", "juv", "LTO_12a")
updated_habitat <- DSMhabitat::wr_juv$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat
r_to_r_lto_12a_baseline_wr_juv["Feather River", , ] <- updated_habitat

# compare: 
r_to_r_lto_12a_baseline_wr_juv == DSMhabitat::wr_juv$r_to_r_lto_12a_baseline

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_wr_juv |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- wr_juv$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "juv" , 
                    watersheds = c("Feather River", "American River", "Mokelumne River", 
                                   "Upper Sacramento River"))

# Save as data object to DSMhabitat
current_wr_juv <- DSMhabitat::wr_juv
current_wr_juv$r_to_r_hrl <- r_to_r_lto_12a_baseline_wr_juv
wr_juv <- current_wr_juv
usethis::use_data(wr_juv, overwrite = TRUE)

# Fry ---------------------------------------------------------------------
# American, Sacramento, Mokelumne, Feather 
r_to_r_lto_12a_baseline_wr_fry <- DSMhabitat::wr_fry$r_to_r_lto_12a_baseline

# American river
# using fall run for scaling 
add_project_habitat <- DSMhabitat::wr_fry$r_to_r_lto_12a_baseline["American River" , , ] *
  hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::wr_fry$r_to_r_lto_12a_baseline["American River", , ] + add_project_habitat
r_to_r_lto_12a_baseline_wr_fry["American River", , ] <- updated_habitat

# Sacramento 
# TODO: flagging since Sacramento was changed to Upper Sacramento
add_project_habitat <- DSMhabitat::wr_fry$r_to_r_lto_12a_baseline["Upper Sacramento River" , , ] * 
  hab_prop_change_from_projects("inchannel rearing", "Upper Sacramento River", "wr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::wr_fry$r_to_r_lto_12a_baseline["Upper Sacramento River", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_wr_fry["Upper Sacramento River", , ] <- updated_habitat 

# Mokelumne 
# using fall run as scaling
add_project_habitat <- DSMhabitat::wr_fry$r_to_r_lto_12a_baseline["Mokelumne River" , , ] *
  hab_prop_change_from_projects("inchannel rearing", "Mokelumne River", "fr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::wr_fry$r_to_r_lto_12a_baseline["Mokelumne River", , ] + add_project_habitat
r_to_r_lto_12a_baseline_wr_fry["Mokelumne River", , ] <- updated_habitat

# Feather 
# using fall run as scaling 
add_project_habitat <- DSMhabitat::wr_fry$r_to_r_lto_12a_baseline["Feather River" , , ] *
  hab_prop_change_from_projects("inchannel rearing", "Feather River", "fr", "fry", "LTO_12a")
updated_habitat <- DSMhabitat::wr_fry$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat
r_to_r_lto_12a_baseline_wr_fry["Feather River", , ] <- updated_habitat

# compare: 
r_to_r_lto_12a_baseline_wr_fry == DSMhabitat::wr_fry$r_to_r_lto_12a_baseline

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_wr_fry |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- wr_fry$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "fry" , 
                    watersheds = c("Feather River", "American River", "Mokelumne River", 
                                   "Upper Sacramento River"))

# Save as data object to DSMhabitat
current_wr_fry <- DSMhabitat::wr_fry
current_wr_fry$r_to_r_hrl <- r_to_r_lto_12a_baseline_wr_fry
wr_fry <- current_wr_fry
usethis::use_data(wr_fry, overwrite = TRUE)

# Floodplain Rearing ------------------------------------------------------
# North Delta, Sutter Bypass, Mokelume River, Feather River 
all_existing_and_hrl_data |> filter(habitat_type == "floodplain rearing" & run == "winter") |> 
  pull(watershed)

r_to_r_lto_12a_baseline_wr_fp <- DSMhabitat::wr_fp$r_to_r_lto_12a_baseline

# Mokelumne River 
# using fall run as scaling
add_project_habitat <- DSMhabitat::wr_fp$r_to_r_lto_12a_baseline["Mokelumne River" , , ] *
  hab_prop_change_from_projects("floodplain rearing", "Mokelumne River", "fr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::wr_fp$r_to_r_lto_12a_baseline["Mokelumne River", , ] + add_project_habitat
r_to_r_lto_12a_baseline_wr_fp["Mokelumne River", , ] <- updated_habitat

# Feather River
# TODO: all NAs..
add_project_habitat <- DSMhabitat::wr_fp$r_to_r_lto_12a_baseline["Feather River" , , ] *
  hab_prop_change_from_projects("floodplain rearing", "Feather River", "fr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::wr_fp$r_to_r_lto_12a_baseline["Feather River", , ] + add_project_habitat
r_to_r_lto_12a_baseline_wr_fp["Feather River", , ] <- updated_habitat

# Sutter Bypass
# Note: all Sutter Bypass values are zero so there is no proportional change with the new HRL data 
# THIS should probably be addressed 
add_project_habitat <- DSMhabitat::wr_fp$r_to_r_lto_12a_baseline["Sutter Bypass" , , ] * 
  hab_prop_change_from_projects("floodplain rearing", "Sutter Bypass", "wr", "fp", "LTO_12a")
updated_habitat <- DSMhabitat::wr_fp$r_to_r_lto_12a_baseline["Sutter Bypass", , ] + add_project_habitat 
r_to_r_lto_12a_baseline_wr_fp["Sutter Bypass", , ] <- updated_habitat 

# exploratory plot: 
r_to_r_hrl_habitat<- r_to_r_lto_12a_baseline_wr_fp |>
  DSMhabitat::square_meters_to_acres()
r_to_r_baseline_lto <- wr_fp$r_to_r_lto_12a_baseline |> DSMhabitat::square_meters_to_acres()
hrl_comparison_plot(new_data = r_to_r_hrl_habitat, old_data = r_to_r_baseline_lto, hab_type = "flood" , 
                    watersheds = c("Feather River", "Mokelumne River", "Sutter Bypass"))

# Save as data object to DSMhabitat
current_wr_fp <- DSMhabitat::wr_fp
current_wr_fp$r_to_r_hrl <- r_to_r_lto_12a_baseline_wr_fp
wr_fp <- current_wr_fp
usethis::use_data(wr_fp, overwrite = TRUE)

