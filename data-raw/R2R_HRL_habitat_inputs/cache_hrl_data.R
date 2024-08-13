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
# Juvenile: 
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

