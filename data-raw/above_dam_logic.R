# Calculate the percentage of spawning habitat that is above the dam in 
# each tributary, and create a data object above_dam_spawn_hab_prop = 
#   c(prop for each 31 watersheds, if no above dam habitat 0)

# This script is built for winter run and spring run 
# spawning and rearing
selected_species <- 'wr'
selected_lifestage <- 'rearing'


get_proportion <- function(selected_species, selected_lifestage) {
  above_dam <- readxl::read_excel('data-raw/R2R_TMH_habitat_inputs/Cleaned Floodplain Width Calculations.xlsx', 
                                  sheet = "Above Dam") |> 
    janitor::clean_names() |> 
    group_by(river) |> 
    summarise(above_dam_length_feet = sum(river_length_feet, na.rm = TRUE)) |> 
    rename(watershed = river) 
  
  below_dam_abbr <- DSMhabitat::watershed_lengths |> 
    select(watershed, feet, lifestage, species) |> 
    filter(species == selected_species,
           lifestage == selected_lifestage) 
  
  if(selected_species == "sr") {
    below_dam <- below_dam_abbr |> bind_rows(DSMhabitat::watershed_lengths |> 
                                               select(watershed, feet, lifestage, species) |> 
                                               filter(species == 'fr',
                                                      lifestage == selected_lifestage, 
                                                      watershed  %in% c('Merced River', 'American River')) |> 
                                               mutate(species = "sr")) 
  } else if (selected_species == "wr") {
    below_dam <-  below_dam_abbr |> 
      bind_rows(DSMhabitat::watershed_lengths |> 
                  select(watershed, feet, lifestage, species) |> 
                  filter(lifestage == selected_lifestage,
                         species == "fr", 
                         watershed != "Battle Creek")) |> 
      mutate(species = "wr") 
    
    if (selected_lifestage == "spawning") {
      below_dam <- below_dam |> filter(watershed == "Upper Sacramento River")
    }
    
    
  }
  
  above_dam_hab_prop <- above_dam |> 
    right_join(below_dam) |> 
    full_join(DSMflow::watershed_ordering) |> 
    arrange(order) |> 
    mutate(percent_above_dam = above_dam_length_feet / (above_dam_length_feet + feet),
           percent_above_dam = ifelse(is.na(percent_above_dam), 0, percent_above_dam)) |> 
    pull(watershed, percent_above_dam)
  
  return(above_dam_hab_prop)
}

# TODO: should we use the entire reach length for below dam rather than the spawning/rearing subset in order to align with above dam? 
# NOTE: WR rearing - use fall run as proxy except for Battle Creek 
get_proportion("wr", "rearing") 
