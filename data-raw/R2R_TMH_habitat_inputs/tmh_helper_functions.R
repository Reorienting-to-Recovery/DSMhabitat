library(tidyverse)
library(DSMhabitat)
library(lubridate)

spawning_months <- function(species) {
  switch(species, 
         "fr" = c(10:12),
         "sr" = c(9:10),
         "wr" = c(5:7))
}

rearing_months <- function(species) {
  switch(species, 
         "fr" = c(1:8),
         "sr" = c(1:8, 11, 12),
         "wr" = c(9:12, 1:5))
}

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

format_all_hab_data_long <- function(tmh_data_formatting) {
  tmh_data_formatting |> 
    rename(spwn_acres_max = max_spawning_acres,
           rear_acres_max = max_rearing_acres, 
           flood_acres_max = max_floodplain_acres) |> 
    pivot_longer(cols = c(spwn_acres_max:flood_acres_max), names_to = "metric") |>
    separate(metric, c('hab', 'unit', 'lifestage'), "_") 
}

all_existing_and_tmh_data_fun <- function(species) {
  if(species == "fr") {
    all_existing_and_tmh_data <- readRDS('data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs_fall_run.rdata') |> 
      format_all_hab_data_long()
  } else {
    all_existing_and_tmh_data <- readRDS('data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs_spring_winter_run.rdata') |> 
      format_all_hab_data_long()
  }
}

## spawning ----------------------------------------------------------------
spawn_tmh_processing <- function(watersheds, species, calsim_run) {
  
  all_existing_and_tmh_data <- all_existing_and_tmh_data_fun(species)
  
  r_to_r_tmh_spawn <- switch(species, 
                             "fr" = DSMhabitat::fr_spawn[[calsim_run]],
                             "sr" = DSMhabitat::sr_spawn[[calsim_run]],
                             "wr" = DSMhabitat::wr_spawn[[calsim_run]])
  
  for(i in 1:length(watersheds)) {
    ws = watersheds[i]
    habitat = "spwn"
    
    max_hab_acres <- all_existing_and_tmh_data |> 
      filter(watershed == ws & hab == habitat) |> 
      pull(value)
    
    existing_acres <- existing_acres_fun(watershed_input = ws, habitat_type = 'spawn', species = species, calsim_run = calsim_run) 
    
    if(existing_acres == 0 | is.na(existing_acres)) {
      adj_factor = 1
    } else {
      adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
    }
    
    new_hab_acres <- 
      switch(species, 
             "fr" = DSMhabitat::fr_spawn[[calsim_run]][ws , , ] * adj_factor,
             "sr" = DSMhabitat::sr_spawn[[calsim_run]][ws , , ] * adj_factor,
             "wr" = DSMhabitat::wr_spawn[[calsim_run]][ws , , ] * adj_factor
      )
    
    r_to_r_tmh_spawn[ws, , ] <- new_hab_acres 
  }
  
  return(r_to_r_tmh_spawn)
}

## In channel and Fry Rearing ----------------------------------------------------------------
rearing_tmh_processing <- function(watersheds, species, calsim_run) {
  
  all_existing_and_tmh_data <- all_existing_and_tmh_data_fun(species)
  
  r_to_r_tmh_juv <- switch(species, 
                           "fr" = DSMhabitat::fr_juv[[calsim_run]],
                           "sr" = DSMhabitat::sr_juv[[calsim_run]],
                           "wr" = DSMhabitat::wr_juv[[calsim_run]])
  r_to_r_tmh_fry <- switch(species, 
                           "fr" = DSMhabitat::fr_fry[[calsim_run]],
                           "sr" = DSMhabitat::sr_fry[[calsim_run]],
                           "wr" = DSMhabitat::wr_fry[[calsim_run]])
  
  for(i in 1:length(watersheds)) {
    ws = watersheds[i]
    habitat = "rear"
    
    max_hab_acres <- all_existing_and_tmh_data |> 
      filter(watershed == ws & hab == habitat) |> 
      pull(value)
    
    existing_acres_juv <- existing_acres_fun(watershed_input = ws, habitat_type = 'rear_juv', species = species, calsim_run = calsim_run) 
    existing_acres_fry <- existing_acres_fun(watershed_input = ws, habitat_type = 'rear_fry', species = species, calsim_run = calsim_run) 
    
    adj_factor_juv = (max_hab_acres - existing_acres_juv) / existing_acres_juv + 1
    adj_factor_fry = (max_hab_acres - existing_acres_fry) / existing_acres_fry + 1
    
    add_max_hab_juv <- 
      switch(species, 
             "fr" = DSMhabitat::fr_juv[[calsim_run]][ws , , ] * adj_factor_juv,
             "sr" = DSMhabitat::sr_juv[[calsim_run]][ws , , ] * adj_factor_juv,
             "wr" = DSMhabitat::wr_juv[[calsim_run]][ws , , ] * adj_factor_juv
      )
    
    add_max_hab_fry <- 
      switch(species, 
             "fr" = DSMhabitat::fr_fry[[calsim_run]][ws , , ] * adj_factor_fry,
             "sr" = DSMhabitat::sr_fry[[calsim_run]][ws , , ] * adj_factor_fry,
             "wr" = DSMhabitat::wr_fry[[calsim_run]][ws , , ] * adj_factor_fry
      )
    
    
    r_to_r_tmh_juv[ws, , ] <- add_max_hab_juv 
    r_to_r_tmh_fry[ws, , ] <- add_max_hab_fry 
  }
  
  return(list("fry" = r_to_r_tmh_fry, "juv" = r_to_r_tmh_juv))
}

## floodplain  ----------------------------------------------------------------
floodplain_tmh_processing <- function(watersheds, species, calsim_run) {
  
  all_existing_and_tmh_data <- all_existing_and_tmh_data_fun(species)
  
  r_to_r_tmh_flood <- switch(species, 
                             "fr" = DSMhabitat::fr_fp[[calsim_run]],
                             "sr" = DSMhabitat::sr_fp[[calsim_run]],
                             "wr" = DSMhabitat::wr_fp[[calsim_run]])
  
  for(i in 1:length(watersheds)) {
    ws = watersheds[i]
    habitat = "flood"
    
    max_hab_acres <- all_existing_and_tmh_data |> 
      filter(watershed == ws & hab == habitat) |> 
      pull(value)
    
    existing_acres <- existing_acres_fun(watershed_input = ws, habitat_type = 'floodplain', species = species, calsim_run = calsim_run) 
    
    if(existing_acres == 0 | is.na(existing_acres)) {
      adj_factor = 1
    } else {
      adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
    }
    
    new_hab_acres <- 
      switch(species, 
             "fr" = DSMhabitat::fr_fp[[calsim_run]][ws , , ] * adj_factor,
             "sr" = DSMhabitat::sr_fp[[calsim_run]][ws , , ] * adj_factor,
             "wr" = DSMhabitat::wr_fp[[calsim_run]][ws , , ] * adj_factor
      )
    
    r_to_r_tmh_flood[ws, , ] <- new_hab_acres 
  }
  
  return(r_to_r_tmh_flood)
}

# Delta: 
delta_tmh_processing <- function(watersheds = c('North Delta', 'South Delta')) {
  
  r_to_r_tmh_delta <- DSMhabitat::delta_habitat
  
  for(i in 1:length(watersheds)) {
    ws = watersheds[i]
    habitat = "rear"
    
    # see: TMH methodology for calcs 
    max_hab_df = data.frame(watershed = c("North Delta", "South Delta"),
                            max_hab = c(41720, 102792)) 
    
    max_hab_acres <- max_hab_df |> 
      filter(watershed == ws) |> 
      pull(max_hab)
    # Instead of taking hab at the median flow to compare take median hab 
    # Check in with Mark on this assumption 
    existing_acres <- median(DSMhabitat::delta_habitat[ , , ws]) |> 
      DSMhabitat::square_meters_to_acres()
    
    # Note: If the maximum theoretical habitat was less than the existing SIT habitat, 
    # the theoretical maximum habitat value was used for baseline and model runs. 
    adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
    
    new_hab_acres <- DSMhabitat::delta_habitat[ , , ws] * adj_factor
    
    r_to_r_tmh_delta[, , ws ] <- new_hab_acres 
  }
  return(r_to_r_tmh_delta)
}

existing_flow_cfs <- function(habitat_type, watershed_input, bypass = FALSE, species, calsim_run) {
  
  flow_df <- if (bypass == FALSE) {DSMflow::flows_cfs[[calsim_run]]} else {DSMflow::bypass_flows[[calsim_run]]}
  
  if(habitat_type == "spawning") {
    flow_df |> 
      filter(date >= as_date("1980-01-01")) |> 
      filter(month(date) %in% spawning_months(species)) |> 
      pull(watershed_input) |> 
      median()
  } else if(habitat_type == "rearing") {
    flow_df |> 
      filter(date >= as_date("1980-01-01")) |> 
      filter(month(date) %in% rearing_months(species)) |> 
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

# not included in floodplain suitability factor 
modeling_in_suitable_area <- c("Antelope Creek", "Battle Creek", "Bear Creek", 
                               "Cow Creek", "Mill Creek", "Paynes Creek", 
                               "Deer Creek",'Upper Sacramento River',
                               'Upper-mid Sacramento River','Lower Sacramento River', 'Lower-mid Sacramento River')

existing_acres_fun <- function(watershed_input, habitat_type, species,  calsim_run) {
  
  if (watershed_input == "Lower-mid Sacramento River") {
    # The Lower-mid Sacramento River has two nodes, one above Fremont Weir (C134) and one below (C160). 
    # rearing: 
    rear_flow1 <- existing_flow_cfs("rearing", "Lower-mid Sacramento River1", species = species, calsim_run = calsim_run)
    rear_flow2 <- existing_flow_cfs("rearing", "Lower-mid Sacramento River2", species = species, calsim_run = calsim_run)
    
    rear_acres_juv <- square_meters_to_acres(DSMhabitat::set_instream_habitat('Lower-mid Sacramento River', 
                                                                              species = species, life_stage = "juv",
                                                                              rear_flow1, rear_flow2))
    rear_acres_fry <- square_meters_to_acres(DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                                                              species, "fry",  rear_flow1, rear_flow2))
    
    # floodplain: 
    flood_flow1 <- existing_flow_cfs("flood", "Lower-mid Sacramento River1", species = species, calsim_run = calsim_run)
    flood_flow2 <- existing_flow_cfs("flood", "Lower-mid Sacramento River2", species = species, calsim_run = calsim_run)
    flood_acres <-  square_meters_to_acres(set_floodplain_habitat("Lower-mid Sacramento River", species, 
                                                                flood_flow1, flood_flow2))
    
    
    spwn_acres = NA
    spwn_flow = NA
    rear_flow = NA
    flood_flow = NA
    
  } else {
    spwn_flow <- existing_flow_cfs("spawning", watershed_input, species = species, calsim_run = calsim_run)
    spwn_acres <- square_meters_to_acres(set_spawning_habitat(watershed_input, species, spwn_flow, month = median(spawning_months(species))))
    
    if(species == "wr") {
      if(watershed_input != c('Upper Sacramento River', 'Upper-mid Sacramento River',
                              'Lower-mid Sacramento River', 'Battle Creek')) {
        # use fall run as proxy for most watersheds: 
        rear_flow <- existing_flow_cfs("rearing", watershed_input, species = 'fr', calsim_run = calsim_run)
        rear_acres_juv <- square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed_input, 'fr', "juv", rear_flow))
        rear_acres_fry <- square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed_input, 'fr', "fry", rear_flow))
      } else {
        rear_flow <- existing_flow_cfs("rearing", watershed_input, species = 'wr', calsim_run = calsim_run)
        rear_acres_juv <- square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed_input, 'wr', "juv", rear_flow))
        rear_acres_fry <- square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed_input, 'wr', "fry", rear_flow))
      }
      
    } else {
      rear_flow <- existing_flow_cfs("rearing", watershed_input, species = species, calsim_run = calsim_run)
      rear_acres_juv <- square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed_input, species, "juv", rear_flow))
      rear_acres_fry <- square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed_input, species, "fry", rear_flow))
    }
    
    flood_flow <- existing_flow_cfs("flood", watershed_input, species = species, calsim_run = calsim_run)
    flood_acres <- square_meters_to_acres(DSMhabitat::set_floodplain_habitat(watershed_input, species, flood_flow))
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

# TMH Plots: 
tmh_comparison_plot <- function(tmh_data, sit_habitat, hab_type) {
  
  year = switch(hab_type, 
                "spawn" = c(1979:2000),
                "juv" = c(1980:2000),
                "fry" = c(1980:2000),
                "flood" = c(1980:2000)
  )
  
  r_to_r_max_habitat <- tmh_data |> 
    DSMhabitat::square_meters_to_acres()
  
  sit_habitat <- sit_habitat |> DSMhabitat::square_meters_to_acres()
  
  plot <- expand_grid(
    watershed = factor(DSMscenario::watershed_labels, 
                       levels = DSMscenario::watershed_labels),
    month = 1:12,
    year = year) |> 
    arrange(year, month, watershed) |> 
    mutate(
      sit_habitat = as.vector(sit_habitat),
      r_to_r_max_habitat = as.vector(r_to_r_max_habitat)) 
  
  plot |> 
    transmute(watershed, date = lubridate::ymd(paste(year, month, 1)), 
              sit_habitat, r_to_r_max_habitat) |> 
    gather(version, acres, -watershed, -date)  |> 
    ggplot(aes(date, acres, color = version)) +
    geom_line(alpha = .75) + 
    facet_wrap(~watershed, scales = 'free_y') + 
    theme_minimal() + 
    theme(legend.position="top", 
          legend.title = element_blank())
}


