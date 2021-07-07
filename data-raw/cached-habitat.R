library(tidyr)
library(dplyr)
library(purrr)
library(lubridate)
library(DSMhabitat)

watersheds <- DSMhabitat::watershed_species_present$watershed_name[-32]
watersheds_order <- DSMhabitat::watershed_species_present %>% 
  select(order, watershed = watershed_name)

#' Generate SIT Model Compatible Array
#' @description transforms to array data structure for SIT model input
#' @name create_Sit_array
#' @param input a vector of data, length = 252 for 12 months and 20 years of data
#' @return 3 dimension array [location, month, year]
create_SIT_array <- function(input) {
  
  output <- array(NA, dim = c(nrow(input), 12, ncol(input) / 12))
  index <-  1
  for (i in seq(1, ncol(input), 12)) {
    output[ , , index] <- as.matrix(input[ , i:(i + 11)])
    index <- index + 1
  }
  return(output)
  
}

# functions ---------
get_flow <- function(watershed, years = c(1980, 1999)) {
  
  # get the flow values at the dates
  dplyr::pull(dplyr::filter(dplyr::select(DSMflow::flows_cfs, date, watershed),
                            lubridate::year(date) >= years[1],
                            lubridate::year(date) <= years[2]), 2)
}

get_rear_hab_all <- function(watersheds, species, life_stage, years = 1980:1999) {
  total_obs <- 12 * length(years)
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed, range(years))
    habitat <- DSMhabitat::set_instream_habitat(watershed,
                                                species = species,
                                                life_stage = life_stage,
                                                flow = flows)
    tibble(
      year = rep(years, each = 12),
      month = rep(1:12, length(years)),
      watershed = watershed,
      hab_sq_m = habitat)
  })
  
  # deal with sacramento special cases
  # lower-mid sac
  low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', range(years))
  low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', range(years))
  
  low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
    DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                     species = species,
                                     life_stage = life_stage,
                                     flow = flow, flow2 = flow2)
  })
  
  low_mid_sac <- tibble(
    year = rep(years, each = 12),
    month = rep(1:12, length(years)),
    watershed = 'Lower-mid Sacramento River',
    hab_sq_m = low_mid_sac_hab)
  
  hab <- bind_rows(most, low_mid_sac) %>%
    spread(watershed, hab_sq_m) %>%
    bind_cols(tibble(`Sutter Bypass` = rep(0, total_obs),
                     `Yolo Bypass` = rep(0, total_obs))) %>%
    gather(watershed, habitat, -year, -month) %>%
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
    select(date, watershed, habitat) %>%
    spread(date, habitat) %>%
    left_join(watersheds_order) %>%
    arrange(order) %>%
    select(-watershed, -order) %>%
    create_SIT_array()
  
  return(hab)
}

get_spawn_hab_all <- function(watersheds, species, years = 1979:2000) {
  total_obs <- 12 * length(years)
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed, years=range(years))
    habitat <- DSMhabitat::set_spawning_habitat(watershed,
                                                species = species,
                                                flow = flows)
    tibble(
      year = rep(years, each = 12),
      month = rep(1:12, length(years)),
      watershed = watershed,
      hab_sq_m = habitat)
  })
  
  # deal with sacramento special cases
  # upper sac
  up_sac_flows <- get_flow('Upper Sacramento River', years=range(years))
  months <- rep(1:12, length(years))
  up_sac_hab <- map2_dbl(months, up_sac_flows, function(month, flow) {
    DSMhabitat::set_spawning_habitat('Upper Sacramento River',
                                     species = species,
                                     flow = flow, month = month)
  })
  
  up_sac <- tibble(
    year = rep(years, each = 12),
    month = rep(1:12, length(years)),
    watershed = 'Upper Sacramento River',
    hab_sq_m = up_sac_hab)
  
  hab <-   bind_rows(most, up_sac) %>%
    spread(watershed, hab_sq_m) %>%
    bind_cols(tibble(`Sutter Bypass` = rep(NA, total_obs),
                     `Yolo Bypass` = rep(NA, total_obs),
                     `Upper-mid Sacramento River` = rep(NA, total_obs),
                     `Lower-mid Sacramento River` = rep(NA, total_obs),
                     `Lower Sacramento River` = rep(NA, total_obs),
                     `San Joaquin River` = rep(NA, total_obs))) %>%
    gather(watershed, habitat, -year, -month) %>%
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
    select(date, watershed, habitat) %>%
    spread(date, habitat) %>%
    left_join(watersheds_order) %>%
    arrange(order) %>%
    select(-watershed, -order) %>%
    create_SIT_array()
  
  
  return(hab)
}

get_floodplain_hab_all <- function(watersheds, species, years = 1980:1999) {
  total_obs <- 12 * length(years)
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed, range(years))
    habitat <- DSMhabitat::apply_suitability(DSMhabitat::set_floodplain_habitat(watershed, species, flows))
    
    tibble(
      year = rep(years, each = 12),
      month = rep(1:12, length(years)),
      watershed = watershed,
      hab_sq_m = habitat)
  })
  
  # deal with sac, already in square meters
  # upper sac
  up_sac_flow <- get_flow('Upper Sacramento River', range(years))
  up_mid_sac_flow <- get_flow('Upper-mid Sacramento River', range(years))
  low_sac_flow <- get_flow('Lower Sacramento River', range(years))
  
  up_sac_fp <- DSMhabitat::set_floodplain_habitat('Upper Sacramento River', species, up_sac_flow)
  up_mid_sac_fp <- DSMhabitat::set_floodplain_habitat('Upper-mid Sacramento River', species, up_mid_sac_flow)
  low_sac_fp <- DSMhabitat::set_floodplain_habitat('Lower Sacramento River', species, low_sac_flow)
  
  # lower-mid sacramento
  low_mid_sac_flows1 <- get_flow("Lower-mid Sacramento River1", range(years))
  low_mid_sac_flows2 <- get_flow("Lower-mid Sacramento River2", range(years))
  low_mid_sac_fp <- DSMhabitat::set_floodplain_habitat('Lower-mid Sacramento River', species,
                                                       low_mid_sac_flows1, flow2 = low_mid_sac_flows2)
  
  # deer creek 
  deer_creek_flows <- get_flow("Deer Creek", range(years)) 
  deer_creek_fp <- DSMhabitat::set_floodplain_habitat("Deer Creek", species, deer_creek_flows)
  
  num_watersheds <- 5
  sac_and_deer <- tibble(
    year = rep(rep(years, each = 12), times = num_watersheds),
    month = rep(1:12, length(years) * num_watersheds),
    watershed = rep(c('Upper Sacramento River', 'Upper-mid Sacramento River',
                      'Lower-mid Sacramento River', 'Lower Sacramento River', 
                      'Deer Creek'), each = 12 * length(years)),
    hab_sq_m = c(up_sac_fp, up_mid_sac_fp, low_mid_sac_fp, low_sac_fp, deer_creek_fp))
  
  hab <- bind_rows(most, sac_and_deer) %>%
    spread(watershed, hab_sq_m) %>%
    bind_cols(tibble(`Sutter Bypass` = rep(NA, total_obs),
                     `Yolo Bypass` = rep(NA, total_obs))) %>%
    gather(watershed, habitat, -year, -month) %>%
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
    select(date, watershed, habitat) %>%
    spread(date, habitat) %>%
    left_join(watersheds_order) %>%
    arrange(order) %>%
    select(-watershed, -order) %>%
    create_SIT_array()
  
  
  return(hab)
  
}

# spawning---------------------
spawning_watersheds <- DSMhabitat::watershed_species_present %>%
  filter(!(watershed_name %in% c("Upper Sacramento River", "Upper Mid Sac Region")),
         spawn) %>%
  pull(watershed_name)

fr_spawn <- get_spawn_hab_all(spawning_watersheds, 'fr')
dimnames(fr_spawn) <- list(watersheds, month.abb, 1979:2000)
fr_spawn[which(is.na(fr_spawn))] <- 0
usethis::use_data(fr_spawn, overwrite = TRUE)

st_spawn <- get_spawn_hab_all(spawning_watersheds, 'st')
st_spawn[which(is.na(st_spawn))] <- 0
dimnames(st_spawn) <- list(watersheds, month.abb, 1979:2000)
usethis::use_data(st_spawn, overwrite = TRUE)

sr_spawn <- get_spawn_hab_all(spawning_watersheds, 'sr')
sr_spawn[which(is.na(sr_spawn))] <- 0
# Old stuff 
# # several watershed that do not have spring run populations but SIT wants to enable colonization
# # NOTE: here i create new fr and st habs that include year 2000 in order to fill in for the sr
# fr_spawn_filler <- get_spawn_hab_all(spawning_watersheds, 'fr', year = 1979:2000)
# st_spawn_filler <- get_spawn_hab_all(spawning_watersheds, 'st', year = 1979:2000)
# 
# sr_spawn[15, , ] <- st_spawn_filler[15, , ] # Thomes Creek
# sr_spawn[25, , ] <- fr_spawn_filler[25, , ] # Calaveras River
# sr_spawn[26, , ] <- fr_spawn_filler[26, , ] # Cosumnes River
# sr_spawn[28, , ] <- fr_spawn_filler[28, , ] # Merced River
dimnames(sr_spawn) <- list(watersheds, month.abb, 1979:2000)
usethis::use_data(sr_spawn, overwrite = TRUE)

# Winter Run 
# only in Sacramento and battle creek
# spawn just in Upper Sac
wr_spawn <- array(0, dim = c(31, 12, 22), dimnames = list(watersheds, month.abb, 1979:2000)) 
up_sac_flows <- get_flow('Upper Sacramento River', years = c(1979, 2000))
months <- rep(1:12, 22)
up_sac_hab <- map2_dbl(months, up_sac_flows, function(month, flow) {
  DSMhabitat::set_spawning_habitat('Upper Sacramento River',
                                   species = 'wr',
                                   flow = flow, month = month)
})

battle_flows <- get_flow('Battle Creek', years = c(1979, 2000))
battle_hab <- map_dbl(battle_flows, function(flow) {
  DSMhabitat::set_spawning_habitat('Battle Creek',
                                   species = 'wr',
                                   flow = flow)
})

wr_spawn["Upper Sacramento River",,] <- up_sac_hab
wr_spawn["Battle Creek",,] <- battle_hab
usethis::use_data(wr_spawn, overwrite = TRUE)


# Late Fall Run 
# only in sacramento clear creek and battle creek
# spawn just in Upper Sac
lfr_spawn <- array(0, dim = c(31, 12, 22), dimnames = list(watersheds, month.abb, 1979:2000))  
up_sac_flows <- get_flow('Upper Sacramento River', years = c(1979, 2000))
months <- rep(1:12, 22)
up_sac_hab <- map2_dbl(months, up_sac_flows, function(month, flow) {
  DSMhabitat::set_spawning_habitat('Upper Sacramento River',
                                   species = 'lfr',
                                   flow = flow, month = month)
})

battle_flows <- get_flow('Battle Creek', years = c(1979, 2000))
battle_hab <- map_dbl(battle_flows, function(flow) {
  DSMhabitat::set_spawning_habitat('Battle Creek',
                                   species = 'lfr',
                                   flow = flow)
})

clear_flows <- get_flow('Clear Creek', years = c(1979, 2000))
clear_hab <- map_dbl(battle_flows, function(flow) {
  DSMhabitat::set_spawning_habitat('Clear Creek',
                                   species = 'lfr',
                                   flow = flow)
})
lfr_spawn["Upper Sacramento River", , ] <- up_sac_hab
lfr_spawn["Battle Creek", , ] <- battle_hab
lfr_spawn["Clear Creek", , ] <- clear_hab
usethis::use_data(lfr_spawn, overwrite = TRUE)

# rearing--------------------
watersheds_in_order <- DSMhabitat::watershed_species_present %>%
  filter(!(watershed_name  %in% c('Sutter Bypass',
                             'Lower-mid Sacramento River', 'Yolo Bypass', 
                             'Upper Mid Sac Region'))) %>%
  pull(watershed_name)

#fry------
fr_fry <- get_rear_hab_all(watersheds_in_order, 'fr', 'fry', 1980:2000)
dimnames(fr_fry) <- list(watersheds, month.abb, 1980:2000)
fr_fry[which(is.na(fr_fry))] <- 0
usethis::use_data(fr_fry, overwrite = TRUE)

st_fry <- get_rear_hab_all(watersheds_in_order, 'st', 'fry')
dimnames(st_fry) <- list(watersheds, month.abb, 1980:1999)
st_fry[which(is.na(st_fry))] <- fr_fry[which(is.na(st_fry))]
usethis::use_data(st_fry, overwrite = TRUE)

sr_fry <- get_rear_hab_all(watersheds_in_order, 'sr', 'fry', years = 1980:2000)
# Old stuff
# several watershed that do not have spring run populations but SIT wants to enable colonization
# fr_fry_filler <- get_rear_hab_all(watersheds_in_order, 'fr', 'fry', years = 1980:2000)
# st_fry_filler <- get_rear_hab_all(watersheds_in_order, 'st', 'fry', years = 1980:2000)
# 
# sr_fry[15, , ] <- st_fry_filler[15, , ] # Thomes Creek
# sr_fry[25, , ] <- fr_fry_filler[25, , ] # Calaveras River
# sr_fry[26, , ] <- fr_fry_filler[26, , ] # Cosumnes River
# sr_fry[28, , ] <- fr_fry_filler[28, , ] # Merced River

dimnames(sr_fry) <- list(watersheds, month.abb, 1980:2000)
sr_fry[which(is.na(sr_fry))] <- fr_fry[which(is.na(sr_fry))]
usethis::use_data(sr_fry, overwrite = TRUE)

# winter run 
# fry and juv
wr_fry <- fr_fry # Set default values to fall run to allow for straying
wr_fry["Upper Sacramento River", , ] <- DSMhabitat::set_instream_habitat('Upper Sacramento River',
                                                  species = 'wr',
                                                  life_stage = 'fry',
                                                  flow = get_flow('Upper Sacramento River',
                                                                  years = c(1980, 2000)))
wr_fry['Upper-mid Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Upper-mid Sacramento River',
                                                   species = 'wr',
                                                   life_stage = 'fry',
                                                   flow = get_flow('Upper-mid Sacramento River',
                                                                   years = c(1980, 2000)))
# deal with sacramento special cases
# lower-mid sac
low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', years = c(1980, 2000))
low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', years = c(1980, 2000))

low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
  DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                   species = 'wr',
                                   life_stage = 'fry',
                                   flow = flow, flow2 = flow2)
})


wr_fry['Lower-mid Sacramento River', , ] <- low_mid_sac_hab

wr_fry['Lower Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Lower Sacramento River',
                                                   species = 'wr',
                                                   life_stage = 'fry',
                                                   flow = get_flow('Lower Sacramento River',
                                                                   years = c(1980, 2000)))


wr_fry['Battle Creek', , ] <- DSMhabitat::set_instream_habitat('Battle Creek',
                                                  species = 'wr',
                                                  life_stage = 'fry',
                                                  flow = get_flow('Battle Creek',
                                                                  years = c(1980, 2000)))


wr_fry[which(is.na(wr_fry))] <- 0
usethis::use_data(wr_fry, overwrite = TRUE)

# Late fall run fry 
lfr_fry <- fr_fry # Set default values to fall run to allow for straying
lfr_fry['Upper Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Upper Sacramento River',
                                                  species = 'lfr',
                                                  life_stage = 'fry',
                                                  flow = get_flow('Upper Sacramento River',
                                                                  years = c(1980, 2000)))
lfr_fry['Upper-mid Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Upper-mid Sacramento River',
                                                   species = 'lfr',
                                                   life_stage = 'fry',
                                                   flow = get_flow('Upper-mid Sacramento River',
                                                                   years = c(1980, 2000)))
# deal with sacramento special cases
# lower-mid sac
low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', years = c(1980, 2000))
low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', years = c(1980, 2000))

low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
  DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                   species = 'lfr',
                                   life_stage = 'fry',
                                   flow = flow, flow2 = flow2)
})
lfr_fry['Lower-mid Sacramento River', , ] <- low_mid_sac_hab

lfr_fry['Lower Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Lower Sacramento River',
                                                   species = 'lfr',
                                                   life_stage = 'fry',
                                                   flow = get_flow('Lower Sacramento River',
                                                                   years = c(1980, 2000)))

lfr_fry['Battle Creek', , ] <- DSMhabitat::set_instream_habitat('Battle Creek',
                                                  species = 'lfr',
                                                  life_stage = 'fry',
                                                  flow = get_flow('Battle Creek',
                                                                  years = c(1980, 2000)))
lfr_fry['Clear Creek', , ] <- DSMhabitat::set_instream_habitat('Clear Creek',
                                                   species = 'lfr',
                                                   life_stage = 'fry',
                                                   flow = get_flow('Clear Creek',
                                                                   years = c(1980, 2000)))
lfr_fry[which(is.na(lfr_fry))] <- 0

dimnames(lfr_fry) <- list(watersheds, month.abb, 1980:2000)
usethis::use_data(lfr_fry, overwrite = TRUE)

#juvenile------
fr_juv <- get_rear_hab_all(watersheds_in_order, 'fr', 'juv', 1980:2000)
dimnames(fr_juv) <- list(watersheds, month.abb, 1980:2000)
fr_juv[which(is.na(fr_juv))] <- 0
usethis::use_data(fr_juv, overwrite = TRUE)

st_juv <- get_rear_hab_all(watersheds_in_order, 'st', 'juv')
dimnames(st_juv) <- list(watersheds, month.abb, 1980:1999)
st_juv[which(is.na(st_juv))] <- fr_juv[which(is.na(st_juv))]
usethis::use_data(st_juv, overwrite = TRUE)

sr_juv <- get_rear_hab_all(watersheds_in_order, 'sr', 'juv', years = 1980:2000)
# old stuff
# several watershed that do not have spring run populations but SIT wants to enable colonization
# fr_juv_filler <- get_rear_hab_all(watersheds_in_order, 'fr', 'juv', years = 1980:2000)
# st_juv_filler <- get_rear_hab_all(watersheds_in_order, 'st', 'juv', years = 1980:2000)
# 
# sr_juv[15, , ] <- st_juv_filler[15, , ] # Thomes Creek
# sr_juv[25, , ] <- fr_juv_filler[25, , ] # Calaveras River
# sr_juv[26, , ] <- fr_juv_filler[26, , ] # Cosumnes River
# sr_juv[28, , ] <- fr_juv_filler[28, , ] # Merced River

dimnames(sr_juv) <- list(watersheds, month.abb, 1980:2000)
sr_juv[which(is.na(sr_juv))] <- fr_juv[which(is.na(sr_juv))]
usethis::use_data(sr_juv, overwrite = TRUE)

# winter run
wr_juv <- fr_juv # Set default values to fall run to allow for straying
wr_juv['Upper Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Upper Sacramento River',
                                                  species = 'wr',
                                                  life_stage = 'juv',
                                                  flow = get_flow('Upper Sacramento River',
                                                                  years = c(1980, 2000)))
wr_juv['Upper-mid Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Upper-mid Sacramento River',
                                                   species = 'wr',
                                                   life_stage = 'juv',
                                                   flow = get_flow('Upper-mid Sacramento River',
                                                                   years = c(1980, 2000)))
# deal with sacramento special cases
# lower-mid sac
low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', years = c(1980, 2000))
low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', years = c(1980, 2000))

low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
  DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                   species = 'wr',
                                   life_stage = 'juv',
                                   flow = flow, flow2 = flow2)
})


wr_juv['Lower-mid Sacramento River', , ] <- low_mid_sac_hab

wr_juv['Lower Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Lower Sacramento River',
                                                 species = 'wr',
                                                 life_stage = 'juv',
                                                 flow = get_flow('Lower Sacramento River',
                                                                 years = c(1980, 2000)))

wr_juv['Battle Creek', , ] <- DSMhabitat::set_instream_habitat('Battle Creek',
                                                species = 'wr',
                                                life_stage = 'juv',
                                                flow = get_flow('Battle Creek',
                                                                years = c(1980, 2000)))

dimnames(wr_juv) <- list(watersheds, month.abb, 1980:2000)
usethis::use_data(wr_juv, overwrite = TRUE)

# Late fall Run juvenile 
lfr_juv <- fr_juv # Set default values to fall run to allow for straying
lfr_juv['Upper Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Upper Sacramento River',
                                                  species = 'lfr',
                                                  life_stage = 'juv',
                                                  flow = get_flow('Upper Sacramento River',
                                                                  years = c(1980, 2000)))
lfr_juv['Upper-mid Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Upper-mid Sacramento River',
                                                   species = 'lfr',
                                                   life_stage = 'juv',
                                                   flow = get_flow('Upper-mid Sacramento River',
                                                                   years = c(1980, 2000)))
# deal with sacramento special cases
# lower-mid sac
low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', years = c(1980, 2000))
low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', years = c(1980, 2000))

low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
  DSMhabitat::set_instream_habitat('Lower-mid Sacramento River',
                                   species = 'lfr',
                                   life_stage = 'juv',
                                   flow = flow, flow2 = flow2)
})


lfr_juv['Lower-mid Sacramento River', , ] <- low_mid_sac_hab

lfr_juv['Lower Sacramento River', , ] <- DSMhabitat::set_instream_habitat('Lower Sacramento River',
                                                   species = 'lfr',
                                                   life_stage = 'juv',
                                                   flow = get_flow('Lower Sacramento River',
                                                                   years = c(1980, 2000)))

lfr_juv['Battle Creek', , ] <- DSMhabitat::set_instream_habitat('Battle Creek',
                                                  species = 'lfr',
                                                  life_stage = 'juv',
                                                  flow = get_flow('Battle Creek',
                                                                  years = c(1980, 2000)))
lfr_juv['Clear Creek', , ] <- DSMhabitat::set_instream_habitat('Clear Creek',
                                                   species = 'lfr',
                                                   life_stage = 'juv',
                                                   flow = get_flow('Clear Creek',
                                                                   years = c(1980, 2000)))

dimnames(lfr_juv) <- list(watersheds, month.abb, 1980:2000)
usethis::use_data(lfr_juv, overwrite = TRUE)

# floodplain------------------------
watersheds_fp <- DSMhabitat::watershed_species_present %>%
  filter(!(watershed_name  %in% c('Sutter Bypass','Yolo Bypass',
                             'Upper Sacramento River',
                             'Upper-mid Sacramento River', 
                             'Lower-mid Sacramento River', 
                             'Lower Sacramento River',
                             'Upper Mid Sac Region', 
                             'Deer Creek'))) %>%
  pull(watershed_name)

fr_fp <- get_floodplain_hab_all(watersheds_fp, 'fr', 1980:2000)
dimnames(fr_fp) <- list(watersheds, month.abb, 1980:2000)
fr_fp[which(is.na(fr_fp))] <- 0
usethis::use_data(fr_fp, overwrite = TRUE)

st_fp <- get_floodplain_hab_all(watersheds_fp, 'st', 1980:2000)
dimnames(st_fp) <- list(watersheds, month.abb, 1980:2000)
st_fp[which(is.na(st_fp))] <- fr_fp[which(is.na(st_fp))]
usethis::use_data(st_fp, overwrite = TRUE)

# TODO fix this warning!
sr_fp <- get_floodplain_hab_all(watersheds_fp, 'sr', years = 1980:2000)
# Old stuff
# several watershed that do not have spring run populations but SIT wants to enable colonization
# fr_fp_filler <- get_floodplain_hab_all(watersheds_fp, 'fr', years = 1980:2000)
# st_fp_filler <- get_floodplain_hab_all(watersheds_fp, 'st', years = 1980:2000)
# 
# sr_fp[15, , ] <- st_fp_filler[15, , ] # Thomes Creek
# sr_fp[25, , ] <- fr_fp_filler[25, , ] # Calaveras River
# sr_fp[26, , ] <- fr_fp_filler[26, , ] # Cosumnes River
# sr_fp[28, , ] <- fr_fp_filler[28, , ] # Merced River
dimnames(sr_fp) <- list(watersheds, month.abb, 1980:2000)
sr_fp[which(is.na(sr_fp))] <- fr_fp[which(is.na(sr_fp))]
usethis::use_data(sr_fp, overwrite = TRUE)

wr_fp <- fr_fp # Set default values to fall run to allow for straying
wr_fp['Upper Sacramento River', , ] <- DSMhabitat::set_floodplain_habitat('Upper Sacramento River', 'wr',
                                                 get_flow('Upper Sacramento River',
                                                          years = c(1980, 2000)))
wr_fp['Upper-mid Sacramento River', , ] <- DSMhabitat::set_floodplain_habitat('Upper-mid Sacramento River', 'wr',
                                                  get_flow('Upper-mid Sacramento River',
                                                           years = c(1980, 2000)))
wr_fp['Lower Sacramento River', , ] <- DSMhabitat::set_floodplain_habitat('Lower Sacramento River', 'wr',
                                                  get_flow('Lower Sacramento River',
                                                           years = c(1980, 2000)))

# lower-mid sacramento
low_mid_sac_flows1 <- get_flow("Lower-mid Sacramento River1", years = c(1980, 2000))
low_mid_sac_flows2 <- get_flow("Lower-mid Sacramento River2", years = c(1980, 2000))
low_mid_sac_fp <- DSMhabitat::set_floodplain_habitat('Lower-mid Sacramento River', 'wr',
                                                     low_mid_sac_flows1, flow2 = low_mid_sac_flows2)

wr_fp['Lower-mid Sacramento River',,] <- low_mid_sac_fp
dimnames(wr_fp) <- list(watersheds, month.abb, 1980:2000)
usethis::use_data(wr_fp, overwrite = TRUE)

# Late fall run floodplain 
lfr_fp <- fr_fp # Set default values to fall run to allow for straying
lfr_fp['Upper Sacramento River',,] <- DSMhabitat::set_floodplain_habitat('Upper Sacramento River', 'lfr',
                                                 get_flow('Upper Sacramento River',
                                                          years = c(1980, 2000)))
lfr_fp['Upper-mid Sacramento River',,] <- DSMhabitat::set_floodplain_habitat('Upper-mid Sacramento River', 'lfr',
                                                  get_flow('Upper-mid Sacramento River',
                                                           years = c(1980, 2000)))
lfr_fp['Lower Sacramento River',,] <- DSMhabitat::set_floodplain_habitat('Lower Sacramento River', 'lfr',
                                                  get_flow('Lower Sacramento River',
                                                           years = c(1980, 2000)))

# lower-mid sacramento
low_mid_sac_flows1 <- get_flow("Lower-mid Sacramento River1", years = c(1980, 2000))
low_mid_sac_flows2 <- get_flow("Lower-mid Sacramento River2", years = c(1980, 2000))
low_mid_sac_fp <- DSMhabitat::set_floodplain_habitat('Lower-mid Sacramento River', 'lfr',
                                                     low_mid_sac_flows1, flow2 = low_mid_sac_flows2)

lfr_fp['Lower-mid Sacramento River',,] <- low_mid_sac_fp
dimnames(lfr_fp) <- list(watersheds, month.abb, 1980:2000)
usethis::use_data(lfr_fp, overwrite = TRUE)

# bypass in stream ----------------
bpf <- DSMflow::bypass_flows %>%
  filter(between(year(date), 1980, 2000))

sutter_habitat <- bind_cols(
  'date' = pull(bpf, date),
  square_meters = DSMhabitat::set_bypass_habitat(bypass = 'sutter1', flow = pull(bpf, sutter1)) + 
    DSMhabitat::set_bypass_habitat(bypass = 'sutter2', flow = pull(bpf, sutter2)) + 
    DSMhabitat::set_bypass_habitat(bypass = 'sutter3', flow = pull(bpf, sutter3)) +
    DSMhabitat::set_bypass_habitat(bypass = 'sutter4', flow = pull(bpf, sutter4))
) %>% 
  mutate(year = year(date), 
         month = month(date)) %>%
  select(-date) %>% 
  spread(year, square_meters) %>% 
  select(-month) %>% 
  as.matrix()

rownames(sutter_habitat) <- month.abb

yolo_habitat <- bind_cols(
  'date' = pull(bpf, date),
  square_meters = DSMhabitat::set_bypass_habitat(bypass = 'yolo1', flow = pull(bpf, yolo1)) + 
    DSMhabitat::set_bypass_habitat(bypass = 'yolo2', flow = pull(bpf, yolo2)),
) %>%
  mutate(year = year(date), 
         month = month(date)) %>%
  select(-date) %>% 
  spread(year, square_meters) %>% 
  select(-month) %>% 
  as.matrix()

rownames(yolo_habitat) <- month.abb

usethis::use_data(sutter_habitat, overwrite = TRUE)
usethis::use_data(yolo_habitat, overwrite = TRUE)


# weeks flooded -----
weeks_flooded <- array(2, dim = c(31, 12, 21))

for (i in 1:31) {
  if (i %in% c(17, 21, 22)) next
  flow <- get_flow(watersheds[i], years = c(1980, 2000))
  flooded_weeks <- map_dbl(flow, ~get_weeks_flooded(watersheds[i], .))
  weeks_flooded[i,,] <- matrix(flooded_weeks, ncol = 12)
}

dimnames(weeks_flooded) <- list(watersheds, month.abb, 1980:2000)

usethis::use_data(weeks_flooded, overwrite = TRUE)

# delta -----
delta_habitat <- array(0, dim = c(12, 21, 2))
delta_rearing_habitat_filtered <- delta_rearing_habitat %>% 
  filter(year(date) < 2001)
delta_habitat[ , , 1] <- matrix(delta_rearing_habitat_filtered$`North Delta`, ncol = 21)
delta_habitat[ , , 2] <- matrix(delta_rearing_habitat_filtered$`South Delta`, ncol = 21)
dimnames(delta_habitat) <- list(month.abb, 1980:2000, c("North Delta", "South Delta"))
usethis::use_data(delta_habitat, overwrite = TRUE)

tisdale_bypass_watershed <- c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                              1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
names(tisdale_bypass_watershed) <- watersheds
usethis::use_data(tisdale_bypass_watershed, overwrite = TRUE)

yolo_bypass_watershed <- c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                              1L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
names(yolo_bypass_watershed) <- watersheds
usethis::use_data(yolo_bypass_watershed, overwrite = TRUE)

south_delta_routed_watersheds <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                                   0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 0L)
names(south_delta_routed_watersheds) <- watersheds
usethis::use_data(south_delta_routed_watersheds, overwrite = TRUE)

prop_high_predation <- c(0.3, 0.17, 0.1, 0.17, 0.07, 0.07, 1, 0.27, 0.17, 0.11, 0.27, 
                                   0.11, 0.17, 0, 0.27, 0.3, 0, 0.67, 0.72, 0.17, 1, 0, 0.2, 1, 
                                   0.5, 0.33, 0.33, 0.33, 0.83, 0.34, 0.83)
names(prop_high_predation) <- watersheds
usethis::use_data(prop_high_predation, overwrite = TRUE)

contact_points <- c(33L, 14L, 18L, 5L, 12L, 81L, 10L, 5L, 50L, 12L, 5L, 5L, 2L, 
                    23L, 7L, 121L, 0L, 16L, 123L, 71L, 352L, 0L, 27L, 101L, 179L, 
                    39L, 170L, 197L, 109L, 126L, 220L)
names(contact_points) <- watersheds
usethis::use_data(contact_points, overwrite = TRUE)

prob_strand_early <- c(0.32, 0.15, 0.09, 0.15, 0.19, 0.09, 0.1, 0.15, 0.23, 0.07, 
                       0.19, 0.17, 0.15, 0.08, 0.15, 0.32, 0, 0.16, 0.31, 0.12, 0, 0, 
                       0.02, 0, 0.05, 0.13, 0.02, 0.16, 0.04, 0.32, 0.5)
names(prob_strand_early) <- watersheds
usethis::use_data(prob_strand_early, overwrite = TRUE)

prob_strand_late <- c(0.36, 0.03, 0.01, 0.03, 0.01, 0.01, 0.03, 0.03, 0.05, 0.01, 
                      0.04, 0.27, 0.08, 0.01, 0.02, 0.36, 0, 0.15, 0.2, 0.37, 0.22, 
                      0, 0.01, 0.22, 0.01, 0.01, 0.3, 0.01, 0.5, 0.04, 0.03)
names(prob_strand_late) <- watersheds
usethis::use_data(prob_strand_late, overwrite = TRUE)

prob_nest_scoured <- c(0.18, 0.04, 0.04, 0.04, 0.03, 0.03, 0.05, 0.01, 0.04, 0.05, 
                       0, 0.03, 0.04, 0.05, 0.03, 0.18, 0, 0.04, 0.2, 0.02, 0.01, 0, 
                       0.01, 0.01, 0.05, 0.02, 0.04, 0.04, 0.04, 0.05, 0.01)
names(prob_nest_scoured) <- watersheds
usethis::use_data(prob_nest_scoured, overwrite = TRUE)

delta_contact_points <- c("North Delta" = 718, "South Delta" = 1437)
usethis::use_data(delta_contact_points, overwrite = TRUE)

delta_prop_high_predation <- c("North Delta" = 1, "South Delta" = 1)
usethis::use_data(delta_prop_high_predation, overwrite = TRUE)

