library(tidyverse)
library(readxl)
library(glue)

# function for partially modeled watersheds---------------------------------
# ws = watershed
# df = flow to area relationship dataframe for watershed
# tibble(flow_cfs, modeled_floodplain_area_acres)
scale_fp_flow_area_partial_model <- function(ws, df) {

  watershed_metadata <- filter(DSMhabitat::floodplain_modeling_metadata, 
                               watershed == ws)
  
  species_present <- filter(DSMhabitat::watershed_species_present, 
                            watershed_name == ws)
  
  rearing_extents <- filter(DSMhabitat::watershed_lengths, watershed == ws, lifestage == "rearing") %>% 
    select(watershed, species, miles) %>% 
    spread(species, miles)

  fp_area_per_mile_modeled <- df$modeled_floodplain_area_acres / watershed_metadata$modeled_length_mi
  
  low_gradient <- filter(DSMhabitat::low_gradient_lengths, watershed_name == ws)

  # fall run floodplain area
  #.1 is downscaling for high gradient
  fp_area_FR <- (fp_area_per_mile_modeled * low_gradient$fr) +
    (fp_area_per_mile_modeled * (rearing_extents$fr - low_gradient$fr) * 0.1)

  threshold_flow <- df$flow_cfs[max(which(df$modeled_floodplain_area_acres == 0))]
  
  result <- data.frame(
    flow_cfs = df$flow_cfs,
    FR_floodplain_acres = fp_area_FR
  )

  # late fall run floodplain area
  if (species_present$lfr) {
    fp_area_LFR <- (fp_area_per_mile_modeled * low_gradient$lfr) +
      (fp_area_per_mile_modeled * (rearing_extents$lfr - low_gradient$lfr) * 0.1)
    
    result <- bind_cols(result, LFR_floodplain_acres = fp_area_LFR)
  }

  # winter run floodplain area
  if (species_present$wr) {
    fp_area_WR <- (fp_area_per_mile_modeled * low_gradient$wr) +
      (fp_area_per_mile_modeled * (rearing_extents$wr - low_gradient$wr) * 0.1)
    
    result <- bind_cols(result, WR_floodplain_acres = fp_area_WR)
  }
  
  # spring run floodplain area
  if (species_present$sr) {
    fp_area_SR <- (fp_area_per_mile_modeled * low_gradient$sr) +
      (fp_area_per_mile_modeled * (rearing_extents$sr - low_gradient$sr) * 0.1)

    result <- bind_cols(result, SR_floodplain_acres = fp_area_SR)
  }
  
  # steel head floodplain area
  if (species_present$st) {
    fp_area_ST <- (fp_area_per_mile_modeled * low_gradient$st) +
      (fp_area_per_mile_modeled * (rearing_extents$st - low_gradient$st) * 0.1)
    
    result <- bind_cols(result, ST_floodplain_acres = fp_area_ST)
  }

  return(
    filter(mutate(result, watershed = ws), flow_cfs >= threshold_flow)
  )

}

# function for non-modeled watersheds---------------------------------
# ws = watershed
scale_fp_flow_area <- function(ws) {
  # TODO make sure all these helper data tables have all species
  watershed_metadata <- filter(DSMhabitat::floodplain_modeling_metadata, 
                                watershed == ws)
  
  low_gradient <- filter(DSMhabitat::low_gradient_lengths, watershed_name == ws)
  
  rearing_extents <- filter(DSMhabitat::watershed_lengths, watershed == ws, lifestage == "rearing") %>% 
    select(watershed, species, miles) %>% 
    spread(species, miles)
  
  proxy_watershed_metadata <- filter(DSMhabitat::floodplain_modeling_metadata, 
                                      watershed == watershed_metadata$scaling_watershed)
  
  species_present <- filter(DSMhabitat::watershed_species_present, 
                            watershed_name == ws)  
  
  proxy_data <- switch(watershed_metadata$scaling_watershed,
                       "Deer Creek" = DSMhabitat::deer_creek_floodplain,
                       "Tuolumne River" = DSMhabitat::tuolumne_river_floodplain,
                       "Cottonwood Creek" = DSMhabitat::cottonwood_creek_floodplain)
  
  # scale flow
  scaled_flow <- proxy_data$flow_cfs * watershed_metadata$dec_jun_mean_flow_scaling
  
  # fall run area
  # divide floodplain area by watershed length of proxy watershed to get area/mile, scale to hydrology
  scaled_area_per_mile_FR <- (proxy_data$FR_floodplain_acres / proxy_watershed_metadata$modeled_length_mi) *
    watershed_metadata$dec_jun_mean_flow_scaling
  
  # apportion area by high gradient/low gradient, .1 is downscaling for high gradient
  fp_area_FR <- (scaled_area_per_mile_FR * low_gradient$fr) +
    (scaled_area_per_mile_FR * (rearing_extents$fr - low_gradient$fr) * 0.1)
  
  result <- data.frame(
    flow_cfs = scaled_flow,
    FR_floodplain_acres = fp_area_FR
  )
  
  if (species_present$lfr) {
    # latefall floodplain area
    scaled_area_per_mile_LFR <- (proxy_data$LFR_floodplain_acres / proxy_watershed_metadata$modeled_length_mi) *
      watershed_metadata$dec_jun_mean_flow_scaling
    
    fp_area_LFR <-(scaled_area_per_mile_LFR * low_gradient$lfr) +
      (scaled_area_per_mile_LFR * (rearing_extents$lfr - low_gradient$lfr) * 0.1)
    
    result <- bind_cols(result, LFR_floodplain_acres = fp_area_LFR)
  }
  
  if (species_present$wr) {
    # steelhead floodplain area
    scaled_area_per_mile_WR <- (proxy_data$WR_floodplain_acres / proxy_watershed_metadata$modeled_length_mi) *
      watershed_metadata$dec_jun_mean_flow_scaling
    
    fp_area_WR <-(scaled_area_per_mile_WR * low_gradient$wr) +
      (scaled_area_per_mile_WR * (rearing_extents$wr - low_gradient$wr) * 0.1)
    
    result <- bind_cols(result, WR_floodplain_acres = fp_area_WR)
  }
  
  if (species_present$sr) {
    # spring run floodplain area
    scaled_area_per_mile_SR <- (proxy_data$SR_floodplain_acres / proxy_watershed_metadata$modeled_length_mi) *
      watershed_metadata$dec_jun_mean_flow_scaling
    
    fp_area_SR <-(scaled_area_per_mile_SR * low_gradient$sr) +
      (scaled_area_per_mile_SR * (rearing_extents$sr - low_gradient$sr) * 0.1)
    
    result <- bind_cols(result, SR_floodplain_acres = fp_area_SR)
  }
  
  if (species_present$st) {
    # steelhead floodplain area
    scaled_area_per_mile_ST <- (proxy_data$ST_floodplain_acres / proxy_watershed_metadata$modeled_length_mi) *
      watershed_metadata$dec_jun_mean_flow_scaling
    
    fp_area_ST <-(scaled_area_per_mile_ST * low_gradient$st) +
      (scaled_area_per_mile_ST * (rearing_extents$st - low_gradient$st) * 0.1)
    
    result <- bind_cols(result, ST_floodplain_acres = fp_area_ST)
  }
  
  return(mutate(result, watershed = ws))
}


# modeling details------------------------------------
print_model_details <- function(ws, species) {
  
  species_present <- subset(DSMhabitat::watershed_species_present, 
                            watershed_name == ws, species, drop = TRUE)  
  
  if (!species_present) return(NULL)

  watershed_doc_vars <- filter(DSMhabitat::floodplain_modeling_metadata, 
                               watershed == ws)
  
  rearing_extents <- filter(DSMhabitat::watershed_lengths, watershed == ws, lifestage == "rearing") %>% 
    select(watershed, species, miles) %>% 
    spread(species, miles)
  
  rearing_length <- round(rearing_extents[, species][[1]], 1)
  low_grad <- round(subset(DSMhabitat::low_gradient_lengths, watershed_name == ws,                           species, drop = TRUE), 1)
  high_grad <- round(rearing_length - low_grad, 1)
  modeled_length <- round(watershed_doc_vars$modeled_length_mi, 1)
  
  watershed_method <- watershed_doc_vars$method
  model_name <- watershed_doc_vars$model_name
  flow_scale <- round(watershed_doc_vars$dec_jun_mean_flow_scaling * 100)
  high_grad_factor <- .1
  watershed_name <- ws
  channel_area_modeled <- filter(DSMhabitat::fr_wetted_channel_area, watershed == ws)$channel_area_of_FR_rearing_extent

  if (str_detect(watershed_method, 'scaled')) {
    proxies <- c('Deer Creek', 'Cottonwood Creek', 'Tuolumne River')
    names(proxies) <- c('dc', 'cc', 'tr')
    
    proxy_ws <- proxies[str_remove(watershed_method, 'scaled_')]
    
    return(
      glue(' There was no watershed specific hydraulic modeling available for {watershed_name}.
            A flow to inundated floodplain area relationship was generated for {watershed_name}
            by scaling the flow to inundated floodplain area relationship for {proxy_ws}.
            This scaling used the ratio of mean flow from December to June between the modeled
            and unmodeled watershed. Flows and corresponding inundated floodplain areas per unit length
            were calculated for {watershed_name} as {flow_scale}% of {proxy_ws}.
            Of the entire mapped {rearing_length} miles rearing
            extent in {watershed_name}, {low_grad} miles were classified as low gradient and
            {high_grad} miles were classified as high gradient based on a geomorphic analysis
            of long profile slopes and valley widths. The area per unit length was scaled by a
            factor of {high_grad_factor} for the high gradient extent.
            There was no scaling factor applied to the low gradient extent.')
    )
  }
  
  description <- switch(watershed_method,
                        'full_model_nmfs' = 
                          glue('The entire mapped rearing extent of {rearing_length} 
                          miles was modeled using {model_name}. The high quality depth 
                          and high quality velocity ("Pref11") "BankArea" result was 
                          used as the floodplain area. High quality velocities were assumed 
                          to be less than or equal to 0.15 meters per second, and 
                          high quality depths were assumed to be between 0.2 meters and 1.5 meters.'),
                        'full_model' = 
                          glue("The entire mapped rearing extent of {rearing_length} 
                          miles was modeled using {model_name}. An active channel 
                          area of {channel_area_modeled} acres, estimated through remote sensing analysis, 
                          was subtracted from total inundated area to obtain inundated floodplain area."),
                        'part_model' = 
                          glue('A {modeled_length} mile portion of the entire mapped 
                          rearing extent of {rearing_length} miles was modeled using {model_name}. 
                          Of the entire mapped rearing extent, {low_grad} miles were classified 
                          as low gradient and {high_grad} miles were classified as high gradient 
                          based on a geomorphic analysis of long profile slopes and valley widths. 
                          The floodplain area per unit length was determined for the modeled 
                          extent and used to approximate areas for the non-modeled extent. 
                          The area per unit length was scaled by a factor of {high_grad_factor} 
                          for the high gradient extent. There was no scaling factor applied 
                          to the low gradient extent.')
                        
  )
  
  return(description)

}

