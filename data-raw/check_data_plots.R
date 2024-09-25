library(tidyverse)
# CHECK SPAWN PLOTS
r2r_baseline <- DSMhabitat::fr_spawn$r_to_r_baseline %>% DSMhabitat::square_meters_to_acres()
biop <- DSMhabitat::fr_spawn$biop_itp_2018_2019 %>% DSMhabitat::square_meters_to_acres()
hrl <- DSMhabitat::fr_spawn$r_to_r_hrl %>% DSMhabitat::square_meters_to_acres()
max_hab <- DSMhabitat::fr_spawn$r_to_r_tmh %>% DSMhabitat::square_meters_to_acres()
eff_max_hab <- DSMhabitat::fr_spawn$r_to_r_tmh_eff %>% DSMhabitat::square_meters_to_acres()
eff_baseline <- DSMhabitat::fr_spawn$r_to_r_eff_baseline %>% DSMhabitat::square_meters_to_acres()

spawn <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1979:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    biop = as.vector(biop),
    r2r_baseline = as.vector(r2r_baseline),
    hrl = as.vector(hrl), 
    max_hab = as.vector(max_hab),
    eff_max_hab = as.vector(eff_max_hab),
    eff_baseline = as.vector(eff_baseline))

spawn %>% 
  mutate(date = ymd(paste(year, month, 1))) %>% 
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass',
                            "Lower-mid Sacramento River", "Upper-mid Sacramento River", "Lower Sacramento River", "San Joaquin River"))) %>% 
  # filter(watershed == "Paynes Creek") |> # Intersting that HRL water creates different hab on paynes creek even though it is not a HRL stream
  gather(version, acres, -watershed, -date, -year, -month)  %>% 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# CHECK REAR PLOTS
r2r_baseline <- DSMhabitat::fr_fry$r_to_r_baseline %>% DSMhabitat::square_meters_to_acres()
biop <- DSMhabitat::fr_fry$biop_itp_2018_2019 %>% DSMhabitat::square_meters_to_acres()
hrl <- DSMhabitat::fr_fry$r_to_r_hrl %>% DSMhabitat::square_meters_to_acres()
max_hab <- DSMhabitat::fr_fry$r_to_r_tmh %>% DSMhabitat::square_meters_to_acres()
eff_max_hab <- DSMhabitat::fr_fry$r_to_r_tmh_eff %>% DSMhabitat::square_meters_to_acres()
eff_baseline <- DSMhabitat::fr_fry$r_to_r_eff_baseline %>% DSMhabitat::square_meters_to_acres()

fry_rear <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    biop = as.vector(biop),
    r2r_baseline = as.vector(r2r_baseline),
    hrl = as.vector(hrl), 
    max_hab = as.vector(max_hab),
    eff_max_hab = as.vector(eff_max_hab),
    eff_baseline = as.vector(eff_baseline))

fry_rear %>% 
  mutate(date = ymd(paste(year, month, 1))) %>% 
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'
                           ))) %>% 
  # filter(watershed == "Paynes Creek") |> # Intersting that HRL water creates different hab on paynes creek even though it is not a HRL stream
  gather(version, acres, -watershed, -date, -year, -month)  %>% 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# CHECK REAR PLOTS
r2r_baseline <- DSMhabitat::fr_juv$r_to_r_baseline %>% DSMhabitat::square_meters_to_acres()
biop <- DSMhabitat::fr_juv$biop_itp_2018_2019 %>% DSMhabitat::square_meters_to_acres()
hrl <- DSMhabitat::fr_juv$r_to_r_hrl %>% DSMhabitat::square_meters_to_acres()
max_hab <- DSMhabitat::fr_juv$r_to_r_tmh %>% DSMhabitat::square_meters_to_acres()
eff_max_hab <- DSMhabitat::fr_juv$r_to_r_tmh_eff %>% DSMhabitat::square_meters_to_acres()
eff_baseline <- DSMhabitat::fr_juv$r_to_r_eff_baseline %>% DSMhabitat::square_meters_to_acres()

juv_rear <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    biop = as.vector(biop),
    r2r_baseline = as.vector(r2r_baseline),
    hrl = as.vector(hrl), 
    max_hab = as.vector(max_hab),
    eff_max_hab = as.vector(eff_max_hab),
    eff_baseline = as.vector(eff_baseline))

juv_rear %>% 
  mutate(date = ymd(paste(year, month, 1))) %>% 
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'
  ))) %>% 
  # filter(watershed == "Paynes Creek") |> # Intersting that HRL water creates different hab on paynes creek even though it is not a HRL stream
  gather(version, acres, -watershed, -date, -year, -month)  %>% 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# CHECK FP Plots
# CHECK REAR PLOTS
r2r_baseline <- DSMhabitat::fr_fp$r_to_r_baseline %>% DSMhabitat::square_meters_to_acres()
biop <- DSMhabitat::fr_fp$biop_itp_2018_2019 %>% DSMhabitat::square_meters_to_acres()
hrl <- DSMhabitat::fr_fp$r_to_r_hrl %>% DSMhabitat::square_meters_to_acres()
max_hab <- DSMhabitat::fr_fp$r_to_r_tmh %>% DSMhabitat::square_meters_to_acres()
eff_max_hab <- DSMhabitat::fr_fp$r_to_r_tmh_eff %>% DSMhabitat::square_meters_to_acres()
eff_baseline <- DSMhabitat::fr_fp$r_to_r_eff_baseline %>% DSMhabitat::square_meters_to_acres()

fp <- expand_grid(
  watershed = factor(DSMscenario::watershed_labels, 
                     levels = DSMscenario::watershed_labels),
  month = 1:12,
  year = 1980:2000) %>% 
  arrange(year, month, watershed) %>% 
  mutate(
    biop = as.vector(biop),
    r2r_baseline = as.vector(r2r_baseline),
    hrl = as.vector(hrl), 
    max_hab = as.vector(max_hab),
    eff_max_hab = as.vector(eff_max_hab),
    eff_baseline = as.vector(eff_baseline))

fp %>% 
  mutate(date = ymd(paste(year, month, 1))) %>% 
  filter(!(watershed %in% c('Sutter Bypass', 'Yolo Bypass'
  ))) %>% 
  # filter(watershed == "Paynes Creek") |> # Intersting that HRL water creates different hab on paynes creek even though it is not a HRL stream
  gather(version, acres, -watershed, -date, -year, -month)  %>% 
  ggplot(aes(date, acres, color = version)) +
  geom_line() + 
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal()

# flows -------------------------------------------------------------------

DSMflow::flows_cfs$LTO_12a |> 
  pivot_longer(`Antelope Creek`:`San Joaquin River`,
               names_to = "watershed",
               values_to = "flow_cfs") |> 
  mutate(scenario = "LTO_12a") |> 
  bind_rows(DSMflow::flows_cfs$biop_itp_2018_2019 |> 
              pivot_longer(`Antelope Creek`:`San Joaquin River`,
                           names_to = "watershed",
                           values_to = "flow_cfs") |> 
              mutate(scenario = "biop_18_19")) |> 
  ggplot(aes(x = date, y = flow_cfs, color = scenario)) + 
  geom_line(alpha = 0.8) +
  facet_wrap(~watershed, scales = "free_y") +
  scale_x_date(breaks = "1 month") +
  theme(legend.position = "bottom") +
  labs(x = "Date", y = "Flow (cfs)")

