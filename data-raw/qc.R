library(tidyverse)
library(lubridate)
library(DSMhabitat)

# spawning -----
spawning <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1979:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    fr = as.vector(fr_spawn),
    lfr = as.vector(lfr_spawn),
    sr = as.vector(sr_spawn),
    wr = as.vector(wr_spawn),
    st = as.vector(st_spawn))

max_spawn_length <- watershed_lengths %>% 
  filter(lifestage == "spawning", species == "fr") %>% 
  transmute(watershed, relative_length = miles/max(miles)) 

spawn_summary <- spawning %>% 
  gather(species, sqm, -watershed, -month, -year) %>% 
  mutate(acres = square_meters_to_acres(sqm)) %>% 
  group_by(watershed, species) %>% 
  summarise(mean = mean(acres),
            min = min(acres),
            max = max(acres),
            median = median(acres)) %>% 
  ungroup()

spawn_summary %>% 
  left_join(max_spawn_length) %>%
  group_by(species) %>% 
  mutate(scaled_mean = mean/max(mean)) %>% 
  filter(species == "fr") %>% View


spawning %>% 
  gather(species, sqm, -watershed, -month, -year) %>% 
  mutate(acres = square_meters_to_acres(sqm),
         date = ymd(paste(year, month, 1))) %>% 
  filter(acres > 0) %>% 
  ggplot(aes(date, acres, color = species)) +
  geom_line() +
  facet_wrap(~watershed, scales = "free_y")

# fry rearing -----
fry_rearing <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    fr = as.vector(fr_fry),
    lfr = as.vector(lfr_fry),
    sr = as.vector(sr_fry),
    wr = as.vector(wr_fry),
    st = as.vector(st_fry))

max_rear_length <- watershed_lengths %>% 
  filter(lifestage == "rearing", species == "fr") %>% 
  transmute(watershed, relative_length = miles/max(miles)) 

fry_rear_summary <- fry_rearing %>% 
  gather(species, sqm, -watershed, -month, -year) %>% 
  mutate(acres = square_meters_to_acres(sqm)) %>% 
  group_by(watershed, species) %>% 
  summarise(mean = mean(acres),
            min = min(acres),
            max = max(acres),
            median = median(acres)) %>% 
  ungroup()

fry_rear_summary %>% 
  left_join(max_rear_length) %>%
  group_by(species) %>% 
  mutate(scaled_mean = mean/max(mean)) %>% 
  filter(species == "fr") %>% View

fry_rearing %>% 
  gather(species, sqm, -watershed, -month, -year) %>% 
  mutate(acres = square_meters_to_acres(sqm),
         date = ymd(paste(year, month, 1))) %>% 
  filter(acres > 0) %>% 
  ggplot(aes(date, acres, color = species)) +
  geom_line() +
  facet_wrap(~watershed, scales = "free_y")

# juv rearing ----

juv_rearing <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    fr = as.vector(fr_juv),
    lfr = as.vector(lfr_juv),
    sr = as.vector(sr_juv),
    wr = as.vector(wr_juv),
    st = as.vector(st_juv))

juv_rear_summary <- juv_rearing %>% 
  gather(species, sqm, -watershed, -month, -year) %>% 
  mutate(acres = square_meters_to_acres(sqm)) %>% 
  group_by(watershed, species) %>% 
  summarise(mean = mean(acres),
            min = min(acres),
            max = max(acres),
            median = median(acres)) %>% 
  ungroup()

juv_rear_summary %>% 
  left_join(max_rear_length) %>%
  group_by(species) %>% 
  mutate(scaled_mean = mean/max(mean)) %>% 
  filter(species == "fr") %>% View

juv_rearing %>% 
  gather(species, sqm, -watershed, -month, -year) %>% 
  mutate(acres = square_meters_to_acres(sqm),
         date = ymd(paste(year, month, 1))) %>% 
  filter(acres > 0) %>% 
  ggplot(aes(date, acres, color = species)) +
  geom_line() +
  facet_wrap(~watershed, scales = "free_y")

# fp rearing ----------
fp_rearing <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    fr = as.vector(fr_fp),
    lfr = as.vector(lfr_fp),
    sr = as.vector(sr_fp),
    wr = as.vector(wr_fp),
    st = as.vector(st_fp))

fp_rear_summary <- fp_rearing %>% 
  gather(species, sqm, -watershed, -month, -year) %>% 
  mutate(acres = square_meters_to_acres(sqm)) %>% 
  group_by(watershed, species) %>% 
  summarise(mean = mean(acres),
            min = min(acres),
            max = max(acres),
            median = median(acres)) %>% 
  ungroup()

fp_rear_summary %>% 
  left_join(max_rear_length) %>%
  group_by(species) %>% 
  mutate(scaled_mean = mean/max(mean)) %>% 
  filter(species == "fr") %>% View

fp_rearing %>% 
  gather(species, sqm, -watershed, -month, -year) %>% 
  mutate(acres = square_meters_to_acres(sqm),
         date = ymd(paste(year, month, 1))) %>% 
  filter(acres > 0) %>% 
  ggplot(aes(date, acres, color = species)) +
  geom_line() +
  facet_wrap(~watershed, scales = "free_y")
