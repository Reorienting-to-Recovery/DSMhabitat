library(tidyverse)

spwn_perc_suitable <- 0.12

floodplain_perc_suitable <- 0.9

rearing_perc_suitable_low <- 0.1
rearing_perc_suitable_0_1 <- 0.78
rearing_perc_suitable_1_2 <- 0.78
rearing_perc_suitable_2_4 <- 0.585
rearing_perc_suitable_4_8 <- 0.195

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
      glimpse()

gradients |> 
  ggplot(aes(x = river, y = gradient)) + geom_point() + coord_flip()

gradients |> 
  ggplot(aes(x = river, y = perc_suitable_rearing)) + geom_point() + coord_flip()

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


# total above and below dam acres  ----------------------------------------

total_acres <- all_below_dam_acres |> 
  select(-c(mean_channel_width, mean_inflection_width)) |> 
  left_join(above_dam_acres |> 
              select(-c(mean_channel_width, mean_inflection_width))
            ) |> 
  group_by(river, regulated) |> 
  summarise(max_spawning_acres = sum(below_dam_spawning_acres, above_dam_spawning_acres, na.rm = TRUE), 
            max_rearing_acres = sum(below_dam_rearing_acres, above_dam_rearing_acres, na.rm = TRUE), 
            max_floodplain_acres = sum(below_dam_floodplain_acres, above_dam_floodplain_acres, na.rm = TRUE)) |> 
  glimpse()



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


# format_all_data ---------------------------------------------------------

cvpia_habitat_data <- read_csv('data-raw/R2R_TMH_habitat_inputs/CVPIA_habitat_data.csv') |> 
  mutate(watershed = ifelse(watershed == "San Joaquin River", "Lower San Joaquin River", watershed)) |> 
  glimpse()

all_habitat_data <- full_join(cvpia_habitat_data, all_max_habitat) |> 
  select(-regulated) |> 
  glimpse()

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


### spawning:
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
  adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
  
  add_max_hab <- DSMhabitat::fr_spawn$biop_itp_2018_2019[ws , , ] * adj_factor
  
  updated_habitat_max_hab <- DSMhabitat::fr_spawn$biop_itp_2018_2019[ws, , ] + add_max_hab
  
  r_to_r_tmh_fr_spawn[ws, , ] <- updated_habitat_max_hab 
  
}

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
