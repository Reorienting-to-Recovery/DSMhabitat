library(tidyverse)

source('data-raw/R2R_TMH_habitat_inputs/tmh_helper_functions.R')

spwn_perc_suitable <- 0.12

floodplain_perc_suitable <- 0.9

rearing_perc_suitable_low <- 0.1
rearing_perc_suitable_0_1 <- 0.78
rearing_perc_suitable_1_2 <- 0.78
rearing_perc_suitable_2_4 <- 0.585
rearing_perc_suitable_4_8 <- 0.195

run <- 'Fall Run' #'Spring Run' 'Winter Run'

# url <- 'https://docs.google.com/spreadsheets/d/1-3WZAcOzrk5ugZq4CAYK1LRLP0005NmjwpifNQHZ_iA/edit#gid=0'

gradients <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/River Length Summary.xlsx', sheet = "Watershed Summary Table") |> 
  janitor::clean_names() |> 
  fill(watershed) |> 
  filter(!is.na(river)) |> 
  mutate(gradient = ifelse(hqt_boundary == "Valley Lowland (<0.4%)", 0, gradient)) |> 
  mutate(river_length_ft_grad = as.numeric(length_miles)*5280) |> 
  select(river = watershed, dam, river_length_ft_grad, gradient) |> 
  mutate(perc_suitable_rearing = case_when(gradient < 0 ~ 0.1, 
                                           gradient == 0 ~ rearing_perc_suitable_low,
                                           gradient > 0 & gradient < 1 ~ rearing_perc_suitable_0_1, 
                                           gradient > 1 & gradient < 2 ~ rearing_perc_suitable_1_2,
                                           gradient > 2 & gradient < 4 ~ rearing_perc_suitable_2_4,
                                           gradient > 4 ~ rearing_perc_suitable_4_8 
  )) |> 
  mutate(perc_suitable_rearing = as.numeric(perc_suitable_rearing)) |> 
  glimpse()



# read in modeled values as inputs
below_dam <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Below Dam") |> 
  janitor::clean_names() 

below_dam_calcs_sj <- below_dam |> 
  gis_calcs() |> 
  filter(river == "Lower San Joaquin River") |> 
  mutate(river_length_feet = 180.1*5280)

# from merced to friant 
below_dam_acres_sj <- below_dam_calcs_sj |> 
  group_by(river) |> 
  mutate(below_dam_spawning_acres = mean_channel_width * river_length_feet * spwn_perc_suitable / 43560,
         below_dam_floodplain_acres = (mean_inflection_width - mean_channel_width) * river_length_feet * floodplain_perc_suitable / 43560) |>
  ungroup() |> 
  inner_join(gradients |> filter(dam == "below dam")) |> 
  mutate(below_dam_rearing_acres = mean_channel_width * 950928 * perc_suitable_rearing / 43560) |> 
  select(-river_length_ft_grad, -gradient, -perc_suitable_rearing) |>
  group_by(river, mean_channel_width, mean_inflection_width, below_dam_spawning_acres, below_dam_floodplain_acres) |> 
  summarise(below_dam_rearing_acres = sum(below_dam_rearing_acres)) |> 
  mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) |> 
  glimpse()

# above Friant 
# read in modeled values as inputs
above_dam <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Above Dam") |> 
  janitor::clean_names() 

above_dam_calcs_sj <- above_dam |> 
  gis_calcs() |> 
  filter(river == "Upper San Joaquin River") |> 
  mutate(river_length_feet = 37.23*5280)

above_dam_calcs_sj |> 
  group_by(river) |> 
  mutate(above_dam_spawning_acres = mean_channel_width * river_length_feet * spwn_perc_suitable / 43560,
         above_dam_floodplain_acres = (mean_inflection_width - mean_channel_width) * river_length_feet * floodplain_perc_suitable / 43560) |>
  ungroup() |> 
  inner_join(gradients |> filter(dam == "above dam")) |> 
  mutate(above_dam_rearing_acres = mean_channel_width * river_length_feet * perc_suitable_rearing / 43560) |> 
  select(-river_length_ft_grad, -gradient, -perc_suitable_rearing) |>
  group_by(river, mean_channel_width, mean_inflection_width, above_dam_spawning_acres, above_dam_floodplain_acres) |> 
  summarise(above_dam_rearing_acres = sum(above_dam_rearing_acres)) |> 
  mutate(river = ifelse(grepl("San Joaquin River", river), "San Joaquin River", river)) 
