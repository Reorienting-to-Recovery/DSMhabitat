library(tidyverse)
library(dplyr)
library(lubridate)

calsim_30_day <- function(data) {
  dur_30 <-  data |>
    mutate(water_year = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) |> 
    group_by(water_year) |>
    mutate(roll_mean = zoo::rollapply(flow_cfs, FUN = min, 
                                      width = month(date), fill = NA, align = "left")) |>
    summarise(stat_in_duration = mean(roll_mean, na.rm = TRUE)) |>
    mutate(dist = round(cume_dist(-stat_in_duration), 3)) |>
    arrange(dist)
  
  interpolate_probs_30 <- approxfun(x = dur_30$dist, y = dur_30$stat_in_duration)
  d30 <- interpolate_probs_30(0.5) 
  
  return(d30)
}


# Pull existing flow comparison point
existing_cfs_median_comparison_point <- function (habitat_type, watershed, species, calsim_version) {
  spawning_months <- case_when(species == "fr" ~ c(10:12)
                               # fill in the rest 
                               )
  rearing_months <- case_when(species == "fr" ~ c(1:8)
                               # fill in the rest 
                              )
  if (habitat_type == "spawning") {
    DSMflow::flows_cfs[[calsim_version]] |> 
      filter(date >= as_date("1979-01-01")) |> 
      filter(month(date) %in% spawning_months) |> 
      pull(watershed) |> 
      median()
  } else if (habitat_type == "inchannel rearing") {
    DSMflow::flows_cfs[[calsim_version]] |> 
      filter(date >= as_date("1979-01-01")) |> 
      filter(month(date) %in% rearing_months) |> 
      pull(watershed) |> 
      median()
  } else if (habitat_type == "floodplain rearing") {
    if (watershed == "Lower-mid Sacramento River") {
      flood = DSMflow::flows_cfs[[calsim_version]] |>
        filter(date >= as_date("1979-01-01")) |> 
        select(`Lower-mid Sacramento River1`, `Lower-mid Sacramento River2`, date) |>
        mutate(flow_cfs = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 * `Lower-mid Sacramento River2`)
        calsim_30_day(flood)
    } else {
    flood = DSMflow::flows_cfs[[calsim_version]] |>
      filter(date >= as_date("1979-01-01")) |> 
      select(watershed, date) |>
      rename(flow_cfs = watershed)
    calsim_30_day(flood)
    }
  }
}

existing_cfs_median_comparison_point("inchannel rearing", "American River", "fr", 'biop_itp_2018_2019')
existing_cfs_median_comparison_point("floodplain rearing", "American River", "fr", 'biop_itp_2018_2019')

  
# calculate the proportion change frm projects 
hab_prop_change_from_projects <- function(habitat_type, watershed, species, lifestage, calsim_version) {
  # calculate total sq meters added per watershed and habitat type 
  hab <- habitat_type
  ws <- watershed 
  
  # pull project hab out of project catalog 
  project_hab_added <- read_csv("data-raw/R2R_baseline_habitat_inputs/R2R_project_catalog_fall_summary.csv") |> 
    mutate(suitable_acres = case_when(habitat_type == "spawning" ~ total_acres * .12, 
                                      habitat_type == "inchannel rearing" ~ total_acres * .1, 
                                      habitat_type == "floodplain rearing" ~ total_acres * .9)) |> 
    filter(watershed == ws & habitat_type == hab) |> pull(suitable_acres)
  
  project_hab_sqmeters <- DSMhabitat::acres_to_square_meters(project_hab_added)

  if (habitat_type == "inchannel rearing") {
    median_flow <- existing_cfs_median_comparison_point(habitat_type, watershed, species, calsim_version)
    sit_habitat <- DSMhabitat::set_instream_habitat(watershed, species, lifestage, median_flow)
  } 
  if (habitat_type == "spawning") {
    month <- 2 #TODO check in with mark on if we want to compare to Acids boards in or out 
    median_flow <- existing_cfs_median_comparison_point(habitat_type, watershed, species, calsim_version)
    sit_habitat <- DSMhabitat::set_spawning_habitat(watershed, species, median_flow, month)
    
  }
  if (habitat_type == "floodplain rearing" & watershed != "North Delta") {
    thirty_day_mean_exceedence <- existing_cfs_median_comparison_point(habitat_type, 
                                                                       watershed, species, 
                                                                       calsim_version)
    sit_habitat <- DSMhabitat::set_floodplain_habitat(watershed, species, thirty_day_mean_exceedence)
  }
  if (watershed == "North Delta") {
  # Instead of taking hab at the median flow to compare take median hab 
  # Check in with Mark on this assumption 
  sit_habitat <- median(DSMhabitat::delta_habitat[ , , "North Delta"])
  }
  
  # find proportion of habitat added 
  # TODO resolve yuba floodplain problem 
  prop_added <- ifelse(sit_habitat == 0, 0, project_hab_sqmeters/sit_habitat) 
  return(prop_added)
} 

# hab_prop_change_from_projects("floodplain rearing", "North Delta", "fr", "juv", "biop_itp_2018_2019")
hab_prop_change_from_projects("floodplain rearing", "American River", "fr", "juv", "biop_itp_2018_2019")
hab_prop_change_from_projects("spawning", "American River", "fr", "adult", "biop_itp_2018_2019")

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
