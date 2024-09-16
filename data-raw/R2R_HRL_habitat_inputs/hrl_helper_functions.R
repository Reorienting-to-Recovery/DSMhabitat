library(tidyverse)
library(DSMhabitat)
library(lubridate)
library(DSMflow)

# Plotting Function -------------------------------------------------------
hrl_comparison_plot <- function(new_data, old_data, hab_type, watersheds) {
  
  year = switch(hab_type, 
                "spawn" = c(1979:2000),
                "juv" = c(1980:2000),
                "fry" = c(1980:2000),
                "flood" = c(1980:2000)
  )
  
  r_to_r_baseline_hrl <- new_data |> 
    DSMhabitat::square_meters_to_acres()
  
  r_to_r_baseline_lto <- old_data |> DSMhabitat::square_meters_to_acres()
  
  plot <- expand_grid(
    watershed = factor(DSMscenario::watershed_labels, 
                       levels = DSMscenario::watershed_labels),
    month = 1:12,
    year = year) |> 
    arrange(year, month, watershed) |> 
    mutate(
      r_to_r_baseline_lto = as.vector(r_to_r_baseline_lto),
      r_to_r_baseline_hrl = as.vector(r_to_r_baseline_hrl)) |> 
    filter(watershed %in% watersheds)
  
  plot |> 
    transmute(watershed, date = lubridate::ymd(paste(year, month, 1)), 
              r_to_r_baseline_lto, r_to_r_baseline_hrl) |> 
    gather(version, acres, -watershed, -date)  |> 
    ggplot(aes(date, acres, color = version)) +
    geom_line(alpha = .75) + 
    facet_wrap(~watershed, scales = 'free_y') + 
    theme_minimal() + 
    theme(legend.position="top", 
          legend.title = element_blank())
}


# from baseline hab: ------------------------------------------------------

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
  spawning_months <- switch(species, 
                            "fr" = c(10:12),
                            "sr" = c(7:10),
                            "wr" = c(5:7))
  rearing_months <- switch(species,
                           "fr" = c(1:8), 
                           "sr" = c(1:5),
                           "wr" = c(1:5))
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
        filter(month(date) %in% rearing_months) |> 
        select(`Lower-mid Sacramento River1`, `Lower-mid Sacramento River2`, date) |>
        mutate(flow_cfs = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 * `Lower-mid Sacramento River2`)
      calsim_30_day(flood) 
      } else if (watershed == "Sutter Bypass") {
        # TODO: we need to check this methodology. Here, I took the mean sutter flow and ran the 
        # 30 day exceedance on that
        mean_sutter <- DSMflow::bypass_flows[[calsim_version]] |> 
          filter(date >= as_date("1979-01-01")) |> 
          select(date, sutter1:sutter4) |> 
          filter(month(date) %in% rearing_months) |> 
          rowwise() |> 
          mutate(flow_cfs = mean(sutter1:sutter4)) |> 
          select(date, flow_cfs)
        calsim_30_day(mean_sutter)
      } else {
        flood = DSMflow::flows_cfs[[calsim_version]] |>
        filter(date >= as_date("1979-01-01")) |> 
        filter(month(date) %in% rearing_months) |> 
        select(watershed, date) |>
        rename(flow_cfs = watershed)
      calsim_30_day(flood)
    }
  }
}

existing_cfs_median_comparison_point("inchannel rearing", "American River", "fr", 'biop_itp_2018_2019')
existing_cfs_median_comparison_point("floodplain rearing", "Tuolumne River", "fr", 'biop_itp_2018_2019')


# calculate the proportion change frm projects 
# TODO: update with HRL data 
hab_prop_change_from_projects <- function(habitat_type, watershed, species, lifestage, calsim_version) {
  # calculate total sq meters added per watershed and habitat type
  hab <- habitat_type
  ws <- watershed

  selected_run <- switch(species,
                         "fr" = "fall",
                         "sr" = "spring",
                         "wr" = "winter") 

  # pull project hab out of hrl
  project_hab_added <- readRDS(here::here('data-raw', 'R2R_HRL_habitat_inputs', "archive", 'all_habitat_data_for_hrl_inputs_all_runs.rdata')) |>
    #mutate(suitable_acres = total_acres * percent_suitable) |>
    group_by(watershed, habitat_type, run) |>
    summarize(suitable_acres = sum(total_acres)) |>
    filter(watershed == ws & habitat_type == hab & run == selected_run) |> pull(suitable_acres)

  project_hab_sqmeters <- DSMhabitat::acres_to_square_meters(project_hab_added)
  if (habitat_type == "inchannel rearing" & watershed == "Upper-mid Sacramento River") {
    median_flow <- existing_cfs_median_comparison_point(habitat_type, watershed, species, calsim_version)
    sit_habitat <- DSMhabitat::set_instream_habitat(watershed, "fr", lifestage, median_flow)
  }
  if (habitat_type == "inchannel rearing" & watershed != "Upper-mid Sacramento River") {
    median_flow <- existing_cfs_median_comparison_point(habitat_type, watershed, species, calsim_version)
    sit_habitat <- DSMhabitat::set_instream_habitat(watershed, species, lifestage, median_flow)
  }
  if (habitat_type == "spawning") {
    month <- 2 #TODO check in with mark on if we want to compare to Acids boards in or out
    median_flow <- existing_cfs_median_comparison_point(habitat_type, watershed, species, calsim_version)
    sit_habitat <- DSMhabitat::set_spawning_habitat(watershed, species, median_flow, month)

  }
  if(habitat_type == "floodplain rearing" & !(watershed %in% c("North Delta", "Sutter Bypass"))) {
    thirty_day_mean_exceedence <- existing_cfs_median_comparison_point(habitat_type,
                                                                       watershed, species,
                                                                       calsim_version)
    sit_habitat <- DSMhabitat::set_floodplain_habitat(watershed, species, thirty_day_mean_exceedence)
  }
  if(habitat_type == "floodplain rearing" & watershed == "Sutter Bypass") {
    thirty_day_mean_exceedence <- existing_cfs_median_comparison_point(habitat_type,
                                                                       watershed, species,
                                                                       calsim_version)
    sutter1 <- DSMhabitat::set_bypass_habitat('sutter1', thirty_day_mean_exceedence)
    sutter2 <- DSMhabitat::set_bypass_habitat('sutter2', thirty_day_mean_exceedence)
    sutter3 <- DSMhabitat::set_bypass_habitat('sutter3', thirty_day_mean_exceedence)
    sutter4 <- DSMhabitat::set_bypass_habitat('sutter4', thirty_day_mean_exceedence)
    sit_habitat <- mean(sutter1, sutter2, sutter3, sutter4)
  }
  if (habitat_type == "floodplain rearing" & watershed == "Yuba River") {
   # Using the median flow since 30 day exceedance is a value of 0 for Yuba: 
     median_flow = DSMflow::flows_cfs[[calsim_version]] |>
      filter(date >= as_date("1979-01-01")) |> 
      filter(month(date) %in% c(1:8)) |> 
      select("Yuba River", date) |>
      rename(flow_cfs = "Yuba River") |> 
      summarise(median(flow_cfs))
    
    sit_habitat <- DSMhabitat::set_floodplain_habitat(watershed, species, median_flow)
  }

  if (habitat_type == "floodplain rearing" & watershed == "Tuolumne River") {
    # pull comparison flow from FlowWest modeling instead of using the 30 day exceedence
    comparison_flow <- 2500
    sit_habitat <- DSMhabitat::set_floodplain_habitat(watershed, species, comparison_flow)
  }
  if (watershed == "North Delta") {
    # Instead of taking hab at the median flow to compare take median hab
    # Check in with Mark on this assumption
    sit_habitat <- median(DSMhabitat::delta_habitat$r_to_r_baseline[ , , "North Delta"])
  }

  # find proportion of habitat added
  # TODO resolve yuba floodplain problem
  prop_added <- ifelse(sit_habitat == 0, 0, project_hab_sqmeters/sit_habitat)
  return(prop_added)
}

# hab_prop_change_from_projects("floodplain rearing", "North Delta", "fr", "juv", "biop_itp_2018_2019")
hab_prop_change_from_projects("floodplain rearing", "Tuolumne River", "fr", "juv", "biop_itp_2018_2019")
hab_prop_change_from_projects("spawning", "Cottonwood Creek", "sr", "adult", "biop_itp_2018_2019")

