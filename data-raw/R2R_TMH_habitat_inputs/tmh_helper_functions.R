library(tidyverse)
library(DSMhabitat)
library(lubridate)

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

roll_stat = min
annual_stat = max

exceedance_probs_monthly <- function(data, roll_stat, annual_stat) {
  annual_durations <- data %>%
    mutate(water_year = ifelse(month(date) %in% 10:12, year(date) + 1, year(date))) 
  
  annual_stats <- annual_durations %>%
    group_by(water_year) %>%
    summarise(stat_in_duration = annual_stat(flow_cfs, na.rm = TRUE))
  
  annual_stats %>%
    mutate(dist = round(cume_dist(-stat_in_duration), 3)) %>%
    arrange(dist)
  
}


calsim_30_day <- function(data, exceedance_function) {
  dur_30_min_max <- exceedance_probs_monthly(data, roll_stat = roll_stat, annual_stat = annual_stat)
  
  interpolate_probs_30_min_max <- approxfun(x = dur_30_min_max$dist, 
                                            dur_30_min_max$stat_in_duration)
  
  d30 <- interpolate_probs_30_min_max(0.5) 
  
  return(d30)
}


spawning_months <- c(10:12)
rearing_months <- c(1:8)

# not included in floodplain suitability factor 
modeling_in_suitable_area <- c("Antelope Creek", "Battle Creek", "Bear Creek", 
                               "Cow Creek", "Mill Creek", "Paynes Creek", 
                               "Deer Creek",'Upper Sacramento River',
                               'Upper-mid Sacramento River','Lower Sacramento River', 'Lower-mid Sacramento River')

existing_acres_fun <- function(watershed_input, habitat_type) {

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
    flood_acres = square_meters_to_acres(set_floodplain_habitat("Lower-mid Sacramento River", "fr", 
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
  
  if (habitat_type == "rear_juv"){
    return(rear_acres_juv)
  } else if (habitat_type == "rear_fry") {
    return(rear_acres_fry)
  } else if (habitat_type == "spawn") {
    return(spwn_acres)
  } else if (habitat_type == "floodplain") {
    return(flood_acres)
  }
}
