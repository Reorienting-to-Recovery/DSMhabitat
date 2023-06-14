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
    all_existing_and_tmh_data <- readRDS('data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs_all_runs.rdata') |> 
      filter(run == "Fall Run") |> 
      format_all_hab_data_long()
  } else {
    all_existing_and_tmh_data <- readRDS('data-raw/R2R_TMH_habitat_inputs/all_habitat_data_for_tmh_inputs_all_runs.rdata') |> 
      filter(run == "Winter and Spring Run") |> 
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
    ws <- watersheds[i]
    
    max_hab_acres <- all_existing_and_tmh_data |> 
      filter(watershed == ws & hab == 'spwn') |> 
      pull(value)
    
    existing_acres <- existing_acres_fun(watershed_input = ws, habitat_type = 'spawning', selected_species = species, 
                                         selected_life_stage = "spawning", calsim_run = calsim_run) 
    
    if(any(existing_acres == 0 || is.na(existing_acres) || is.null(existing_acres))) {
      adj_factor = 1
    } else {
      adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
    }
    
    print(ws)
    print(adj_factor)
    
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
    ws <- watersheds[i]
    
    max_hab_acres <- all_existing_and_tmh_data %>%
      filter(watershed == ws & hab == 'rear') %>%
      pull(value)
    
    existing_acres_juv <- existing_acres_fun(watershed_input = ws, habitat_type = 'rearing', selected_species = species, selected_life_stage = "juv", calsim_run = calsim_run) 
    existing_acres_fry <- existing_acres_fun(watershed_input = ws, habitat_type = 'rearing', selected_species = species, selected_life_stage = "fry", calsim_run = calsim_run) 
    
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
    ws <- watersheds[i]
    habitat = "flood"
    
    max_hab_acres <- all_existing_and_tmh_data |> 
      filter(watershed == ws & hab == habitat) |> 
      pull(value)
    
    existing_acres <- existing_acres_fun(watershed_input = ws, habitat_type = 'flood', selected_species = species, selected_life_stage = "flood", calsim_run = calsim_run) 
    
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
  
  r_to_r_tmh_delta <- DSMhabitat::delta_habitat$sit_habitat
  
  for(i in 1:length(watersheds)) {
      ws <-watersheds[i]
     habitat = "rear"
    
    # see: TMH methodology for calcs 
    max_hab_df = data.frame(watershed = c("North Delta", "South Delta"),
                            max_hab = c(41720, 102792)) 
    
    max_hab_acres <- max_hab_df |> 
      filter(watershed == ws) |> 
      pull(max_hab)
    # Instead of taking hab at the median flow to compare take median hab 
    # Check in with Mark on this assumption 
    existing_acres <- median(DSMhabitat::delta_habitat$sit_habitat[ , , ws]) |> 
      DSMhabitat::square_meters_to_acres()
    
    # Note: If the maximum theoretical habitat was less than the existing SIT habitat, 
    # the theoretical maximum habitat value was used for baseline and model runs. 
    adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
    
    new_hab_acres <- DSMhabitat::delta_habitat$sit_habitat[ , , ws] * adj_factor
    
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

existing_acres_fun <- function(watershed_input, habitat_type, selected_species, selected_life_stage = NULL, calsim_run) {
  
  if (watershed_input == "Lower-mid Sacramento River") {
    # The Lower-mid Sacramento River has two nodes, one above Fremont Weir (C134) and one below (C160). 
    # rearing: 
    flow1 <- existing_flow_cfs(habitat_type, "Lower-mid Sacramento River1", species = selected_species, calsim_run = calsim_run)
    flow2 <- existing_flow_cfs(habitat_type, "Lower-mid Sacramento River2", species = selected_species, calsim_run = calsim_run)
    
    acres <- switch(habitat_type, 
                    "flood" = square_meters_to_acres(set_floodplain_habitat("Lower-mid Sacramento River", 
                                                                            species = selected_species, 
                                                                            flow1, flow2)),
                    "rearing" = square_meters_to_acres(DSMhabitat::set_instream_habitat("Lower-mid Sacramento River", 
                                                                                        species = selected_species, 
                                                                                        selected_life_stage,
                                                                                        flow1, flow2)) # No spawn on lower-mid sac 
    )
  } else {
    flow <- existing_flow_cfs(habitat_type, watershed_input, species = selected_species, calsim_run = calsim_run)
    acres <- switch(habitat_type, 
                    "flood" = square_meters_to_acres(set_floodplain_habitat(watershed = watershed_input, 
                                                                            species = selected_species, 
                                                                            flow = flow)),
                    "rearing" = square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed = watershed_input, 
                                                                                        species = selected_species, 
                                                                                        life_stage = selected_life_stage,
                                                                                        flow = flow)),
                    "spawning" = square_meters_to_acres(DSMhabitat::set_spawning_habitat(watershed = watershed_input, 
                                                                                         species = selected_species, 
                                                                                         flow = flow, 
                                                                                         month = median(spawning_months(selected_species)))))
    
    if((selected_species == "wr" & 
       selected_life_stage == "rearing" & 
       !(watershed_input %in% c('Upper Sacramento River', 'Upper-mid Sacramento River',
                            'Lower-mid Sacramento River', 'Battle Creek')))) {
        # use fall run as proxy for most watersheds: 
        flow <- existing_flow_cfs(habitat_type, watershed_input, species = "fr", calsim_run = calsim_run)
        acres <- switch(selected_life_stage, 
                      "juv" = square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed = watershed_input, species = 'fr', life_stage = "juv", flow = flow)),
                      "fry" = square_meters_to_acres(DSMhabitat::set_instream_habitat(watershed = watershed_input, species = 'fr', life_stage = "fry", flow = flow))
        )
    }
  }
  modeling_in_suitable_area <- c("Antelope Creek", "Battle Creek", "Bear Creek", 
                                 "Cow Creek", "Mill Creek", "Paynes Creek", 
                                 "Deer Creek",'Upper Sacramento River',
                                 'Upper-mid Sacramento River','Lower Sacramento River', 'Lower-mid Sacramento River')
  
  if ((!watershed_input %in% modeling_in_suitable_area) & habitat_type == "flood") {
    acres <- DSMhabitat::apply_suitability(acres)
  }
  return(acres)
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


