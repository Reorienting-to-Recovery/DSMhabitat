library(tidyverse)

# Add Upper San Joaquin Rearing flow to area curves 
flow_habitat_raw <- read_csv('data-raw/R2R_add_upper_san_joaquin/data-raw/tidy_rearing_data_sjrrp_fish_habitat_study_results.csv') |> 
  glimpse()

# TODO: research inundated acres and floodplain - can we use that column? remove the suitable acres and use? 
# Inundation mapping was conducted for Reach 5 from results of 1D HEC-RAS modeling. The existing conditions model 
# titled SJRRP07, documented in Mussetter Engineering, Inc (MEI, 2008) 
# was used in order to represent the existing levels of floodplain available along the San Joaquin River. 

unique(flow_habitat_raw$reach)

flow_habitat <- flow_habitat_raw |> 
  group_by(reach) |> 
  reframe(flow_cfs = flow_cfs,
          suitable_acres = suitable_acres, 
          total_inundated_acres = total_inundated_acres) |> 
  filter(reach != "5") # reach 5 does not have flow associated with suitable acres

# reach lengths 
reach_length <- tribble(
  ~reach, ~mile_post_num_high, ~mile_post_num_low,
  "4A", 182, 168,
  "1B", 243, 229,
  "2A", 229, 216,
  "3", 205, 182,
  "4B2", NA, NA # unable to find length info on 4B2
) |> 
  mutate(reach_length_miles = mile_post_num_high - mile_post_num_low) |> 
  select(-mile_post_num_high, -mile_post_num_low)

flow_habitat_length <- flow_habitat |> 
  left_join(reach_length)

reach_1B <- flow_habitat_length |> filter(reach == "1B")
reach_2A <- flow_habitat_length |> filter(reach == "2A")
reach_3 <- flow_habitat_length |> filter(reach == "3")
reach_4A <- flow_habitat_length |> filter(reach == "4A")

rearing <- tibble(
  flow_cfs = c(200, 500, 750, 1000, 1250, 1500, 1750, 2000, 2500, 3000, 3500, 
               4000, 4500, 5000, 5500, 6000, 6500, 7000),
  sj_fr_1B = approx(reach_1B$flow_cfs, reach_1B$suitable_acres, xout = flow_cfs, rule = 2)$y,
  sj_fr_2A = approx(reach_2A$flow_cfs, reach_2A$suitable_acres, xout = flow_cfs, rule = 2)$y,
  sj_fr_3 = approx(reach_3$flow_cfs, reach_3$suitable_acres, xout = flow_cfs, rule = 2)$y,
  sj_fr_4A = approx(reach_4A$flow_cfs, reach_4A$suitable_acres, xout = flow_cfs, rule = 2)$y,
)

rearing |> 
  pivot_longer(cols = c(sj_fr_1B, sj_fr_2A, sj_fr_3, sj_fr_4A), values_to = "suitable_acres") |> 
  ggplot(aes(x = flow_cfs, y = suitable_acres)) +
  geom_line(aes(color = name)) +
  ggtitle('Suitable Acres By Reach (prior to weighting by reach length): Rearing') +
  theme_minimal()

# total reach length:
total_reach_length_miles <- sum(flow_habitat_length |> 
                                  group_by(reach) |> 
                                  summarise(sum_miles = sum(unique(reach_length_miles), na.rm = TRUE)) |> 
                                  pull(sum_miles))

upper_san_joaquin_rearing <- rearing |> 
  mutate(sj_fr = sj_fr_1B * (reach_1B$reach_length_miles/total_reach_length_miles) + 
           sj_fr_2A * (reach_2A$reach_length_miles/total_reach_length_miles) +
           sj_fr_3 * (reach_3$reach_length_miles/total_reach_length_miles) +
           sj_fr_4A * (reach_4A$reach_length_miles/total_reach_length_miles) 
  ) |> 
  select(flow_cfs, FR_fry_acres = sj_fr, FR_juv_acres = sj_fr) |> # TODO: check that we want to use the same value for fry and juv
  mutate(watershed = "Upper San Joaquin")

options(scipen = 999)
upper_san_joaquin_rearing |> 
  ggplot(aes(x = flow_cfs, y = FR_fry_acres)) +
  geom_line() +
  ggtitle('Upper San Joaquin Suitable Rearing Acres') +
  theme_minimal() + 
  labs(x = 'Flow (cfs)', y = 'Suitable Acres') + 
  scale_y_continuous(labels = scales::comma)

# add Spawning to Upper San Joaquin  --------------------------------------

spawning_raw <- read_csv('data-raw/R2R_add_upper_san_joaquin/data-raw/san_joaquin_spawning.csv') |> 
  glimpse()

# reach lengths 
reach_length_spawn <- tribble(
  ~reach, ~mile_post_num_high, ~mile_post_num_low,
  "friant to 41", 41, 0, # TODO placeholder for these reach lengths - want to verify with GIS
  "41 to 99", 99, 41 # TODO placeholder for these reach lengths - want to verify with GIS
) |> 
  mutate(reach_length_miles = mile_post_num_high - mile_post_num_low) |> 
  select(-mile_post_num_high, -mile_post_num_low)

flow_habitat_length_spwn <- spawning_raw |> 
  left_join(reach_length_spawn) |> 
  rename(flow_cfs = flow, 
         suitable_acres = area)

reach_friant_to_41 <- flow_habitat_length_spwn |> filter(reach == "friant to 41")
reach_41_to_99 <- flow_habitat_length_spwn |> filter(reach == "41 to 99")

spawning <- tibble(
  flow_cfs = c(200, 500, 750, 1000, 1250, 1500, 1750, 2000, 2500, 3000, 3500, 
               4000, 4500, 5000, 5500, 6000, 6500, 7000),
  sj_fr_friant_to_41 = approx(reach_friant_to_41$flow_cfs, reach_friant_to_41$suitable_acres, xout = flow_cfs, rule = 2)$y,
  sj_fr_41_to_99 = approx(reach_41_to_99$flow_cfs, reach_41_to_99$suitable_acres, xout = flow_cfs, rule = 2)$y
)

spawning |> 
  pivot_longer(cols = c(sj_fr_friant_to_41, sj_fr_41_to_99), values_to = "suitable_acres") |> 
  ggplot(aes(x = flow_cfs, y = suitable_acres)) +
  geom_line(aes(color = name)) +
  ggtitle('Suitable Acres By Reach (prior to weighting by reach length): Spawning.') +
  theme_minimal()

# total reach length:
total_reach_length_miles_spwn <- sum(flow_habitat_length_spwn |> 
                                       group_by(reach) |> 
                                       summarise(sum_miles = sum(unique(reach_length_miles), na.rm = TRUE)) |> 
                                       pull(sum_miles))

upper_san_joaquin_spawning <- spawning |> 
  mutate(sj_fr = sj_fr_friant_to_41 * (unique(reach_friant_to_41$reach_length_miles/total_reach_length_miles_spwn)) + 
           sj_fr_41_to_99 * unique(reach_41_to_99$reach_length_miles/total_reach_length_miles_spwn) 
  ) |> 
  select(flow_cfs, FR_spawn_acres = sj_fr) |> 
  mutate(watershed = "Upper San Joaquin")

options(scipen = 999)
upper_san_joaquin_spawning |> 
  ggplot(aes(x = flow_cfs, y = FR_spawn_acres)) +
  geom_line() +
  ggtitle('Upper San Joaquin Suitable Spawning Acres') +
  theme_minimal() + 
  labs(x = 'Flow (cfs)', y = 'Suitable Acres') + 
  scale_y_continuous(labels = scales::comma)



# combine rearing and spawning into one instream object -------------------

upper_san_joaquin_instream <- upper_san_joaquin_rearing |> 
  full_join(upper_san_joaquin_spawning) |> 
  select(watershed, everything())

# update DSMhabitat san joaquin object as current hab + new hab
lower_san_joaquin_length <- dplyr::pull(dplyr::filter(DSMhabitat::watershed_lengths,
                                                      watershed == 'San Joaquin River',
                                                      species == 'fr',
                                                      lifestage == 'rearing'), feet)

DSMhabitat::wua_to_area(DSMhabitat::san_joaquin_river_instream$FR_fry_wua, "San Joaquin River", "rearing", "fr")

ggplot() +
  geom_line(data = upper_san_joaquin_instream, aes(y = FR_fry_acres, x = flow_cfs, color = "Upper San Joaquin")) +
  geom_line(data = DSMhabitat::san_joaquin_river_instream, 
            aes(y = DSMhabitat::wua_to_area(DSMhabitat::san_joaquin_river_instream$FR_juv_wua, "San Joaquin River", "rearing", "fr") * 0.000247105, 
                x = flow_cfs, color = "Lower San Joaquin")) +
  theme_minimal() +
  labs(x = 'Flow (cfs)', y = 'Rearing Acres') 

ggplot() +
  geom_line(data = upper_san_joaquin_instream, aes(y = FR_spawn_acres, x = flow_cfs, color = "Upper San Joaquin")) +
  theme_minimal() +
  labs(x = 'Flow (cfs)', y = 'Spawning Acres') 


# floodplain  -------------------------------------------------------------

floodplain <- tibble(
  flow_cfs = c(200, 500, 750, 1000, 1250, 1500, 1750, 2000, 2500, 3000, 3500, 
               4000, 4500, 5000, 5500, 6000, 6500, 7000),
  sj_fr_1B = approx(reach_1B$flow_cfs, reach_1B$total_inundated_acres, xout = flow_cfs, rule = 2)$y,
  sj_fr_2A = approx(reach_2A$flow_cfs, reach_2A$total_inundated_acres, xout = flow_cfs, rule = 2)$y,
  sj_fr_3 = approx(reach_3$flow_cfs, reach_3$total_inundated_acres, xout = flow_cfs, rule = 2)$y,
  sj_fr_4A = approx(reach_4A$flow_cfs, reach_4A$total_inundated_acres, xout = flow_cfs, rule = 2)$y,
)

floodplain |> 
  pivot_longer(cols = c(sj_fr_1B, sj_fr_2A, sj_fr_3, sj_fr_4A), values_to = "total_inundated_acres") |> 
  ggplot(aes(x = flow_cfs, y = total_inundated_acres)) +
  geom_line(aes(color = name)) +
  ggtitle('Inundated Acres By Reach (prior to weighting by reach length): Floodplain') +
  theme_minimal()

# total reach length:
total_reach_length_miles <- sum(flow_habitat_length |> 
                                  group_by(reach) |> 
                                  summarise(sum_miles = sum(unique(reach_length_miles), na.rm = TRUE)) |> 
                                  pull(sum_miles))

upper_san_joaquin_floodplain <- floodplain |> 
  mutate(sj_fr_fp = sj_fr_1B * (reach_1B$reach_length_miles/total_reach_length_miles) + 
           sj_fr_2A * (reach_2A$reach_length_miles/total_reach_length_miles) +
           sj_fr_3 * (reach_3$reach_length_miles/total_reach_length_miles) +
           sj_fr_4A * (reach_4A$reach_length_miles/total_reach_length_miles) 
  ) |> 
  select(flow_cfs, FR_floodplain_acres = sj_fr_fp) 

# TODO: do we need to scale floodplain? How?
# TODO: increase flows?

upper_san_joaquin_floodplain |> 
  ggplot(aes(x = flow_cfs, y = FR_floodplain_acres)) +
  geom_line() +
  ggtitle('Upper San Joaquin Suitable Floodplain Acres') +
  theme_minimal() + 
  labs(x = 'Flow (cfs)', y = 'Inundated Acres') + 
  scale_y_continuous(labels = scales::comma)

ggplot() +
  geom_line(data = upper_san_joaquin_floodplain, aes(y = FR_floodplain_acres, x = flow_cfs, color = "Upper San Joaquin")) +
  geom_line(data = DSMhabitat::san_joaquin_river_floodplain, 
            aes(y = DSMhabitat::wua_to_area(DSMhabitat::san_joaquin_river_floodplain$FR_floodplain_acres, "San Joaquin River", "rearing", "fr") * 0.000247105, 
                x = flow_cfs, color = "Lower San Joaquin")) +
  theme_minimal() +
  labs(x = 'Flow (cfs)', y = 'Flooplain Acres') 


# save new data objects ---------------------------------------------------
# TODO: save new instream object 
# TODO: save new flooplain object

# create instream object 

get_flow <- function(watershed, calsim_version, years = c(1980, 1999)) {
  
  # get the flow values at the dates
  dplyr::pull(dplyr::filter(dplyr::select(DSMflow::flows_cfs[[calsim_version]], date, watershed),
                            lubridate::year(date) >= years[1],
                            lubridate::year(date) <= years[2]), 2)
}

create_SIT_array <- function(input) {
  
  output <- array(NA, dim = c(nrow(input), 12, ncol(input) / 12))
  index <-  1
  for (i in seq(1, ncol(input), 12)) {
    output[ , , index] <- as.matrix(input[ , i:(i + 11)])
    index <- index + 1
  }
  return(output)
  
}

calsim_version = 'biop_itp_2018_2019'
years = 1980:2000
watersheds_order <- DSMhabitat::watershed_species_present %>% 
  select(order, watershed = watershed_name)

total_obs <- 12 * length(years)
flows <- get_flow('San Joaquin River', calsim_version, range(years))

hab_func <- approxfun(upper_san_joaquin_instream$flow_cfs, upper_san_joaquin_instream$FR_fry_acres , rule = 2)
habitat_area <- hab_func(flows)

sj_hab <- tibble(
  year = rep(years, each = 12),
  month = rep(1:12, length(years)),
  watershed = 'San Joaquin River',
  hab_sq_m = acres_to_square_meters(habitat_area)
)

watershed = "San Joaquin River"

hab <- sj_hab %>%
  spread(watershed, hab_sq_m) %>% 
  gather(watershed, habitat, -year, -month) %>%
  mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
  select(date, watershed, habitat) %>%
  spread(date, habitat) %>%
  left_join(watersheds_order) %>%
  arrange(order) %>%
  select(-watershed, -order) %>%
  create_SIT_array()

dimnames(hab) <- list('San Joaquin River', month.abb, 1980:2000)
hab[which(is.na(hab))] <- 0

# add habitats together 
# # Juveniles
# update instream juvenile object 
r_to_r_add_sj_fr_juv <- DSMhabitat::fr_juv$r_to_r_baseline
new_juv_hab <- DSMhabitat::fr_juv$r_to_r_baseline['San Joaquin River', , ] + hab['San Joaquin River', , ]
r_to_r_add_sj_fr_juv["San Joaquin River" , , ] <- new_juv_hab 

# check to make sure it works 
r_to_r_add_sj_fr_juv['San Joaquin River' , , ] == DSMhabitat::fr_juv$r_to_r_baseline['San Joaquin River' , , ]

baseline_fr_juv <- DSMhabitat::fr_juv
baseline_fr_juv$r_to_r_baseline <- r_to_r_add_sj_fr_juv
fr_juv <- baseline_fr_juv

# check to make sure it works 
fr_juv$r_to_r_baseline == DSMhabitat::fr_juv$r_to_r_baseline

# save new data object
usethis::use_data(fr_juv, overwrite = TRUE)


# Update Fry object -------------------------------------------------------
r_to_r_add_sj_fr_fry <- DSMhabitat::fr_fry$r_to_r_baseline
new_fry_hab <- DSMhabitat::fr_fry$r_to_r_baseline['San Joaquin River', , ] + hab['San Joaquin River', , ]
r_to_r_add_sj_fr_fry["San Joaquin River" , , ] <- new_fry_hab 

# check to make sure it works 
r_to_r_add_sj_fr_fry['San Joaquin River' , , ] == DSMhabitat::fr_fry$r_to_r_baseline['San Joaquin River' , , ]

baseline_fr_fry <- DSMhabitat::fr_fry
baseline_fr_fry$r_to_r_baseline <- r_to_r_add_sj_fr_fry
fr_fry <- baseline_fr_fry

# check to make sure it works 
fr_fry$r_to_r_baseline == DSMhabitat::fr_fry$r_to_r_baseline

# save new data object: 
usethis::use_data(fr_fry, overwrite = TRUE)




# Exploratory Plots:  -----------------------------------------------------


# Juve
juv <- expand_grid(
    watershed = as.factor('San Joaquin River'),
    month = 1:12,
    year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    low_sj = as.vector(DSMhabitat::fr_juv$biop_itp_2018_2019['San Joaquin River' , , ] |> 
                         DSMhabitat::square_meters_to_acres()),
    upper_sj = as.vector(hab['San Joaquin River', , ]) |>  
      DSMhabitat::square_meters_to_acres(),
    new_hab = as.vector(new_juv_hab) |>   DSMhabitat::square_meters_to_acres()) 

juv |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            low_sj, upper_sj, new_hab) |> 
  gather(reach, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = reach)) +
  geom_line(alpha = .75) + 
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal() + 
  theme(legend.position = "top") + 
  ggtitle('New San Joaquin Juvenile Habitat') 

# Fry: 

fry <- expand_grid(
  watershed = as.factor('San Joaquin River'),
  month = 1:12,
  year = 1980:2000) |> 
  arrange(year, month, watershed) |> 
  mutate(
    low_sj = as.vector(DSMhabitat::fr_fry$biop_itp_2018_2019['San Joaquin River' , , ] |> 
                         DSMhabitat::square_meters_to_acres()),
    upper_sj = as.vector(hab['San Joaquin River', , ]) |>  
      DSMhabitat::square_meters_to_acres(),
    new_hab = as.vector(new_fry_hab) |>   DSMhabitat::square_meters_to_acres()) 

fry |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            low_sj, upper_sj, new_hab) |> 
  gather(reach, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = reach)) +
  geom_line(alpha = .75) + 
  facet_wrap(~watershed, scales = 'free_y') + 
  ggtitle('New San Joaquin Fry Habitat') + 
  theme_minimal() + 
  theme(legend.position = "top")
  


# TODO: markdown for gitbook site 


