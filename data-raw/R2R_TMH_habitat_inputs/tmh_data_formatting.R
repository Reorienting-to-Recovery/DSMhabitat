library(tidyverse)
library(DSMflow)

spwn_perc_suitable <- 0.12

floodplain_perc_suitable <- 0.9

rearing_perc_suitable_low <- 0.1
rearing_perc_suitable_0_1 <- 0.78
rearing_perc_suitable_1_2 <- 0.78
rearing_perc_suitable_2_4 <- 0.585
rearing_perc_suitable_4_8 <- 0.195

run <- 'Fall Run' #'Spring Run' 'Winter Run'

gis_calcs <- function(data) {
  data |> 
    group_by(river) |> 
    mutate(mean_channel_width = mean(width_measurements, na.rm = TRUE),
           in_channel_area = mean_channel_width * river_distance_covered,
           floodplain_area = inflection_point_area - in_channel_area,
           perc_increase_of_floodplain_area = (floodplain_area - in_channel_area)/floodplain_area,
           mean_inflection_width = mean_channel_width  * (1 + mean(perc_increase_of_floodplain_area, na.rm = TRUE))) |> 
    select(river, mean_channel_width, mean_inflection_width, river_length_feet) |> 
    filter(!is.na(river_length_feet)) 
}

existing_flow_cfs <- function(habitat_type, watershed_input, bypass = FALSE) {
  
  flow_df <- if (bypass == FALSE) {DSMflow::flows_cfs$biop_itp_2018_2019} else {DSMflow::bypass_flows$biop_itp_2018_2019}
  
  if(habitat_type == "spawning") {
    flow_df |> 
      filter(date >= as_date("1980-01-01")) |> 
      filter(month(date) %in% spawning_months) |> 
      pull(watershed_input) |> 
      median()
  } else if(habitat_type == "rearing") {
    flow_df |> 
      filter(date >= as_date("1980-01-01")) |> 
      filter(month(date) %in% rearing_months) |> 
      pull(watershed_input) |> 
      median()
  } else if(habitat_type == "flood") {
    flood = flow_df |> 
      filter(date >= as_date("1980-01-01")) |> 
      select(watershed_input, date) |> 
      rename(flow_cfs = watershed_input) 
    
    exceedance_probs_monthly_fun <- exceedance_probs_monthly(flood, roll_stat, annual_stat)
    calsim_30_day(flood, exceedance_probs_monthly_fun)
    
  }
}

# url <- 'https://docs.google.com/spreadsheets/d/1-3WZAcOzrk5ugZq4CAYK1LRLP0005NmjwpifNQHZ_iA/edit#gid=0'

gradients <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/River Length Summary.xlsx', sheet = "Watershed Summary Table") |> 
  janitor::clean_names() |> 
  fill(watershed) |> 
  filter(!is.na(river)) |> 
  mutate(gradient = ifelse(hqt_boundary == "Valley Lowland (<0.4%)", 0, gradient)) |> 
  mutate(river_length_ft_grad = as.numeric(length_miles)*5280) |> 
  select(river = watershed, dam, river_length_ft_grad, gradient) |> 
  mutate(perc_suitable_rearing = ifelse(gradient < 0, 0.1, 
                                        ifelse(gradient == 0, rearing_perc_suitable_low,
                                               ifelse(gradient > 0 & gradient < 1 , rearing_perc_suitable_0_1, 
                                                      ifelse(gradient > 1 & gradient < 2, rearing_perc_suitable_1_2,
                                                             ifelse(gradient > 2 & gradient < 4, rearing_perc_suitable_2_4,
                                                                    ifelse(gradient > 4, rearing_perc_suitable_4_8, "uhoh"))))))) |> 
  mutate(perc_suitable_rearing = as.numeric(perc_suitable_rearing)) |> 
  mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) |> 
  glimpse()

gradients |> 
  ggplot(aes(x = river, y = gradient)) + geom_point() + coord_flip()

gradients |> 
  ggplot(aes(x = river, y = perc_suitable_rearing)) + geom_point() + coord_flip()


# cache CVPIA habitat values ----------------------------------------------
# TODO: this could be removed when functionalized:

watersheds_labels <- DSMscenario::watershed_labels[-c(17, 22)] # removes bypasses 

# not included in floodplain suitability factor 
modeling_in_suitable_area <- c("Antelope Creek", "Battle Creek", "Bear Creek", 
                               "Cow Creek", "Mill Creek", "Paynes Creek", 
                               "Deer Creek",'Upper Sacramento River',
                               'Upper-mid Sacramento River','Lower Sacramento River')

spawning_months <- c(10:12)
rearing_months <- c(1:8)

cvpia_habitat_data <- data.frame()
for(i in 1:length(watersheds_labels)) {
  watershed_input <- watersheds_labels[i]
  
  if (watershed_input == "Lower-mid Sacramento River") {
    # The Lower-mid Sacramento River has two nodes, one above Fremont Weir (C134) and one below (C160). 
    # rearing: 
    rear_flow1 <- existing_flow_cfs("rearing", "Lower-mid Sacramento River1")
    rear_flow2 <- existing_flow_cfs("rearing", "Lower-mid Sacramento River2")
    
    rear_acres_juv <- square_meters_to_acres(DSMhabitat::set_instream_habitat('Lower-mid Sacramento River', 
                                                                              species = "fr", life_stage = "juv",
                                                                              rear_flow1, rear_flow2))
    rear_acres_fry <- square_meters_to_acres(DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                                                              "fr", "fry",  rear_flow1, rear_flow2))
    
    # floodplain: 
    flood_flow1 <- existing_flow_cfs("flood", "Lower-mid Sacramento River1")
    flood_flow2 <- existing_flow_cfs("flood", "Lower-mid Sacramento River2")
    flow_acres = square_meters_to_acres(set_floodplain_habitat("Lower-mid Sacramento River", "fr", 
                                                               flood_flow1, flood_flow2))
    
    
    spwn_acres = NA
    spwn_flow = NA
    rear_flow = NA
    flood_flow = NA
    
  } else {
    spwn_flow <- existing_flow_cfs("spawning", watershed_input)
    spwn_acres <- square_meters_to_acres(set_spawning_habitat(watershed_input, "fr", spwn_flow, month = 10))
    
    rear_flow <- existing_flow_cfs("rearing", watershed_input)
    rear_acres_juv <- square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed_input, "fr", "juv", rear_flow))
    rear_acres_fry <- square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed_input, "fr", "fry", rear_flow))
    
    flood_flow <- existing_flow_cfs("flood", watershed_input)
    flood_acres <- square_meters_to_acres(DSMhabitat::set_floodplain_habitat(watershed_input, "fr", flood_flow))
  }
  
  if (!(watershed_input %in% modeling_in_suitable_area)) {
    flood_acres <- DSMhabitat::apply_suitability(flood_acres)
  }
  
  cvpia_habitat_data <- bind_rows(cvpia_habitat_data, 
                           tibble(
                             watershed = watershed_input, 
                             spwn_flow = spwn_flow, 
                             spwn_acres = spwn_acres, 
                             rear_flow = rear_flow, 
                             rear_acres_juv = rear_acres_juv,
                             rear_acres_fry = rear_acres_fry,
                             flood_flow = flood_flow, 
                             flood_acres = flood_acres
                           )
  )
  
}

# write_csv(sit_outputs,'../CVPIA_habitat_data.csv')


# Above Dam Calculations --------------------------------------------------

# read in modeled values as inputs
above_dam <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Above Dam") |> 
   janitor::clean_names() 

above_dam_calcs <- above_dam |> 
  gis_calcs() |> 
  glimpse()
            
above_dam_acres <- above_dam_calcs |> 
  group_by(river) |> 
  mutate(above_dam_spawning_acres = mean_channel_width * river_length_feet * spwn_perc_suitable / 43560,
         above_dam_floodplain_acres = (mean_inflection_width - mean_channel_width) * river_length_feet * floodplain_perc_suitable / 43560) |>
  ungroup() |> 
  inner_join(gradients |> filter(dam == "above dam")) |> 
  mutate(above_dam_rearing_acres = mean_channel_width * river_length_ft_grad * perc_suitable_rearing / 43560) |> 
  select(-river_length_ft_grad, -gradient, -perc_suitable_rearing) |>
  group_by(river, mean_channel_width, mean_inflection_width, above_dam_spawning_acres, above_dam_floodplain_acres) |> 
  summarise(above_dam_rearing_acres = sum(above_dam_rearing_acres)) |> 
  glimpse()

# Below Dam Calculations --------------------------------------------------

below_dam <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Below Dam") |> 
  janitor::clean_names() 

below_dam_cals <- below_dam |> 
  gis_calcs() |> 
  glimpse()

below_dam_acres <- below_dam_cals |> 
  group_by(river) |> 
  mutate(below_dam_spawning_acres = mean_channel_width * river_length_feet * spwn_perc_suitable / 43560,
         below_dam_floodplain_acres = (mean_inflection_width - mean_channel_width) * river_length_feet * floodplain_perc_suitable / 43560) |>
  ungroup() |> 
  mutate(regulated = "yes") |>
  inner_join(gradients |> filter(dam == "below dam")) |> 
  mutate(below_dam_rearing_acres = mean_channel_width * river_length_ft_grad * perc_suitable_rearing / 43560) |> 
  select(-river_length_ft_grad, -gradient, -perc_suitable_rearing) |>
  group_by(river, mean_channel_width, mean_inflection_width, below_dam_spawning_acres, below_dam_floodplain_acres, regulated) |> 
  summarise(below_dam_rearing_acres = sum(below_dam_rearing_acres)) 

# HEC RAS -----------------------------------------------------------------
# all below dam: 

hec_ras <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Hec Ras") |> 
  janitor::clean_names() 

hec_ras_calc <- hec_ras |>  
  mutate(perc_suitable_rearing = ifelse(gradient == 0, rearing_perc_suitable_low,
                                        ifelse(gradient > 0 & gradient < 1 , rearing_perc_suitable_0_1, 
                                               ifelse(gradient > 1 & gradient < 2, rearing_perc_suitable_1_2,
                                                      ifelse(gradient > 2 & gradient < 4, rearing_perc_suitable_2_4,
                                                             ifelse(gradient > 4, rearing_perc_suitable_4_8, "uhoh")))))) |> 
  mutate(perc_suitable_rearing = as.numeric(perc_suitable_rearing)) |> 
  group_by(river) |> 
  mutate(channel_area_acres = channel_width * channel_length / 43560,
         below_dam_floodplain_acres = (projected_wse_area_acres - channel_area_acres) * floodplain_perc_suitable,
         below_dam_rearing_acres = ifelse(!is.na(hec_ras_channel_area_for_median_flow_during_rearing_acres), 
                                          hec_ras_channel_area_for_median_flow_during_rearing_acres * perc_suitable_rearing,
                                          channel_area_acres * perc_suitable_rearing), 
         below_dam_spawning_acres = ifelse(!is.na(hec_ras_channel_area_for_median_flow_during_spawning_acres), 
                                           hec_ras_channel_area_for_median_flow_during_spawning_acres * 
                                             spwn_perc_suitable, channel_area_acres * spwn_perc_suitable), 
         regulated = "yes + modeled with HEC RAS") |> 
  select(river, regulated, below_dam_floodplain_acres:below_dam_spawning_acres) |> 
  group_by(river, regulated) |> 
  summarise(below_dam_floodplain_acres = sum(below_dam_floodplain_acres),
            below_dam_rearing_acres = sum(below_dam_rearing_acres),
            below_dam_spawning_acres = sum(below_dam_spawning_acres))

# add the below dam to the hec ras model outputs 
all_below_dam_acres <- bind_rows(below_dam_acres, hec_ras_calc) 


# total above and below dam acres  ---------------------------------------

if(run %in% c("Winter Run", "Spring Run")) {
  total_acres <- all_below_dam_acres |> ungroup() |> 
    mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) |> 
    select(-mean_channel_width, -mean_inflection_width) |> 
    left_join(above_dam_acres |> ungroup() |>  
                 mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) |> 
                 select(-c(mean_channel_width, mean_inflection_width))
    ) |>  
    group_by(river, regulated) |> 
    summarise(max_spawning_acres = sum(below_dam_spawning_acres, above_dam_spawning_acres, na.rm = TRUE), 
              max_rearing_acres = sum(below_dam_rearing_acres, above_dam_rearing_acres, na.rm = TRUE), 
              max_floodplain_acres = sum(below_dam_floodplain_acres, above_dam_floodplain_acres, na.rm = TRUE)) |> 
    glimpse()
} else if (run == "Fall Run") {
  ## FALL RUN: 
  ### does not include above dam
  total_acres <- all_below_dam_acres |> ungroup() |> 
    select(-c(mean_channel_width, mean_inflection_width)) |> 
    mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) |> 
    group_by(river, regulated) |> 
    summarise(max_spawning_acres = below_dam_spawning_acres, 
              max_rearing_acres = below_dam_rearing_acres, 
              max_floodplain_acres = below_dam_floodplain_acres) |> 
    glimpse()
}
# Unregulated Calculations ------------------------------------------------

unregulated <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Unregulated") |> 
  janitor::clean_names() 

unreg_calcs <- unregulated |> 
  gis_calcs() |> 
  glimpse()

unreg_acres <- unreg_calcs |> 
  group_by(river) |> 
  mutate(max_spawning_acres = mean_channel_width * river_length_feet * spwn_perc_suitable / 43560,
         max_floodplain_acres = (mean_inflection_width - mean_channel_width) * river_length_feet * floodplain_perc_suitable / 43560) |>
  select(-c(mean_inflection_width, river_length_feet)) |> 
  inner_join(gradients |> filter(dam == "unregulated")) |> 
  mutate(max_rearing_acres = mean_channel_width * river_length_ft_grad * perc_suitable_rearing / 43560) |> 
  select(-river_length_ft_grad, -gradient, -perc_suitable_rearing) |>
  group_by(river, mean_channel_width, max_spawning_acres, max_floodplain_acres) |> 
  summarise(max_rearing_acres = sum(max_rearing_acres)) |> 
  mutate(regulated = "no") |> 
  glimpse()
  

# Combine Regulated and Non Regulated into a single DF --------------------

all_max_habitat <- bind_rows(total_acres, unreg_acres) |> 
  select(river:max_floodplain_acres) |> 
  rename(watershed = river) |> 
  glimpse()


# write.csv(all_max_habitat, 'data/all_watersheds_max_habitat.csv')

# aggregate widths/lengths ------------------------------------------------

hec_ras_calc <- hec_ras |> 
  rename(mean_channel_width = channel_width) |> 
  select(river, mean_channel_width) |> 
  mutate(dam = "below dam") |> 
  glimpse()

widths_lengths <- bind_rows(above_dam_calcs |> mutate(dam = "above dam"), 
                  below_dam_cals |>  mutate(dam = "below dam"), 
                  unreg_calcs |> mutate(dam = "unregulated"),
                  hec_ras_calc)

#saveRDS(widths_lengths, '../r2r_model_inputs_app/data/above_dam_extents/river_widths_lengths.rds')


tmh_acres <- bind_rows(above_dam_acres |> mutate(dam = "above dam"), 
                            below_dam_acres |>  mutate(dam = "below dam"), 
                            unreg_acres |> mutate(dam = "unregulated"),
                            hec_ras_calc |> mutate(dam = "below dam"))

# format_all_data ---------------------------------------------------------

# cvpia_habitat_data <- read_csv('data-raw/R2R_TMH_habitat_inputs/CVPIA_habitat_data.csv') |> 
#   glimpse()

all_habitat_data <- full_join(cvpia_habitat_data, all_max_habitat) |> 
  select(-regulated) |> 
  glimpse()

if(run == "Fall Run") {
  #write_csv(all_habitat_data, "../r2r_model_inputs_app/data-raw/all_habitat_data.csv")
  saveRDS(all_habitat_data, "data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs_fall_run.rdata")
} else {
  saveRDS(all_habitat_data, "data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs_spring_winter_run.rdata")
  
}



