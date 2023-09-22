# Calculate the percentage of spawning habitat that is above the dam in 
# each tributary, and create a data object above_dam_spawn_hab_prop = 
#   c(prop for each 31 watersheds, if no above dam habitat 0)

selected_species <- 'sr'
selected_lifestage <- 'spawning'

above_dam <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Above Dam") |> 
  janitor::clean_names() |> 
  group_by(river) |> 
  summarise(above_dam_length_feet = sum(river_length_feet, na.rm = TRUE)) |> 
  rename(watershed = river) 

below_dam <- DSMhabitat::watershed_lengths |> 
  select(watershed, feet, lifestage, species) |> 
  filter(species == 'sr',
         lifestage == selected_lifestage) |> 
  bind_rows(DSMhabitat::watershed_lengths |> 
              select(watershed, feet, lifestage, species) |> 
              filter(species == 'fr',
                     lifestage == selected_lifestage, 
                     watershed  %in% c('Thomes Creek', 'Calaveras River', 'Cosumnes River', 'Merced River')) |> 
              mutate(species = "sr")) 

above_dam |> 
  right_join(below_dam)

# read in modeled values as inputs
readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', sheet = "Above Dam") |> 
  janitor::clean_names() |> 
  group_by(river) |> 
  summarise(above_dam_length_feet = sum(river_length_feet, na.rm = TRUE)) |> 
  rename(watershed = river) |> 
  full_join(DSMhabitat::watershed_lengths |> 
              filter(!(watershed %in%  c('Thomes Creek', 'Calaveras River', 'Cosumnes River', 'Merced River')))) |> 
  select(watershed, above_dam_length_feet, feet, lifestage, species) |> 
  filter(lifestage == selected_lifestage) |> 
  bind_rows(DSMhabitat::watershed_lengths |> 
              select(watershed, feet, lifestage, species) |> 
              filter(species == 'fr',
                     lifestage == selected_lifestage, 
                     watershed  %in% c('Thomes Creek', 'Calaveras River', 'Cosumnes River', 'Merced River')) |> 
              mutate(species = "sr")) |> 
  filter(if (selected_species == "sr") species == selected_species else "fr") |> 
  View()
  

  

  data.frame(x = 1:5) %>% 
    filter(if (y=="") x>3 else x<3) %>%  
    tail(1)




#mutate(above_dam_length_feet = ifelse(watershed == "Merced River", 295680, above_dam_length_feet))







# Calculate the percentage of rearing habitat that is above the dam in 
# each tributary, and create a data object - fry and juv 
# above_dam_rearing_hab_prop = c(prop for each 31 watersheds, if no above dam habitat 0)