# for rearing habitat update:
# 
# create a flow to suitable area curve from each reach 

flow_habitat_raw <- read_csv('data-raw/R2R_add_upper_san_joaquin/data-raw/tidy_rearing_data_sjrrp_fish_habitat_study_results.csv') |> 
  glimpse()

unique(flow_habitat_raw$reach)

flow_habitat <- flow_habitat_raw |> 
  group_by(reach) |> 
  reframe(flow_cfs = flow_cfs,
            suitable_acres = suitable_acres) |> 
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
  sj_wua_fr_1B = approx(reach_1B$flow_cfs, reach_1B$suitable_acres, xout = flow_cfs, rule = 2)$y,
  sj_wua_fr_2A = approx(reach_2A$flow_cfs, reach_2A$suitable_acres, xout = flow_cfs, rule = 2)$y,
  sj_wua_fr_3 = approx(reach_3$flow_cfs, reach_3$suitable_acres, xout = flow_cfs, rule = 2)$y,
  sj_wua_fr_4A = approx(reach_4A$flow_cfs, reach_4A$suitable_acres, xout = flow_cfs, rule = 2)$y,
  )
  
rearing |> 
  pivot_longer(cols = c(sj_wua_fr_1B, sj_wua_fr_2A, sj_wua_fr_3, sj_wua_fr_4A), values_to = "wua") |> 
  ggplot(aes(x = flow_cfs, y = wua)) +
  geom_line(aes(color = name)) +
  ggtitle('WUAs: by reach. Prior to weighting by length') +
  theme_minimal()

# total reach length:
total_reach_length_miles <- sum(flow_habitat_length |> 
  group_by(reach) |> 
  summarise(sum_miles = sum(unique(reach_length_miles), na.rm = TRUE)) |> 
  pull(sum_miles))
  
upper_san_joaquin_instream <- rearing |> 
  mutate(sj_wua_fr = sj_wua_fr_1B * (reach_1B$reach_length_miles/total_reach_length_miles) + 
           sj_wua_fr_2A * (reach_2A$reach_length_miles/total_reach_length_miles) +
           sj_wua_fr_3 * (reach_3$reach_length_miles/total_reach_length_miles) +
           sj_wua_fr_4A * (reach_4A$reach_length_miles/total_reach_length_miles) 
  ) |> 
  select(flow_cfs, FR_fry_wua = sj_wua_fr, FR_juv_wua = sj_wua_fr) |> # TODO: check that we want to use the same value for fry and juv
  mutate(
    FR_fry_wua = DSMhabitat::acres_to_square_meters(FR_fry_wua),
    FR_juv_wua = DSMhabitat::acres_to_square_meters(FR_juv_wua),
    watershed = "Upper San Joaquin")

options(scipen = 999)
upper_san_joaquin_instream |> 
  ggplot(aes(x = flow_cfs, y = FR_fry_wua)) +
  geom_line() +
  ggtitle('Upper San Joaquin WUA') +
  theme_minimal() + 
  labs(x = 'Flow (cfs)', y = 'WUA (sqft/1000ft)') + 
  scale_y_continuous(labels = scales::comma)

# usethis::use_data(upper_san_joaquin_instream, overwrite = TRUE)



