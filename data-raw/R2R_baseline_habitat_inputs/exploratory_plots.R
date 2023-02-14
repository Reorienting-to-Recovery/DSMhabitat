library(tidyverse)
library(DSMhabitat)
source("data-raw/R2R_baseline_habitat_inputs/generate_hab_helper_functions.R")
DSMhabitat::american_river_instream

american_river_instream %>% 
  select(flow_cfs, Fry = FR_fry_wua, Juvenile = FR_juv_wua) %>% 
  gather(Lifestage, WUA, -flow_cfs) %>%
  mutate(Updated_WUA = WUA + WUA * hab_prop_change_from_projects("inchannel rearing", "American River", "fr", "juv", "biop_itp_2018_2019")) |> 
  pivot_longer(cols = WUA:Updated_WUA, names_to = "scenario", values_to = "WUAS") |> 
  filter(!is.na(WUAS)) %>% 
  ggplot(aes(x = flow_cfs, y = WUAS, color = Lifestage, linetype = scenario)) +
  geom_line() +
  labs(title = "American River Instream Habitat", x = 'Flow (cfs)', y = 'WUA (sqft/1000ft)') + 
  theme_minimal() + 
  scale_color_manual(values = c('#d95f02', '#7570b3')) +
  theme(legend.justification = c(1, 0), legend.position = c(.9, .7))


sit_habitat <- DSMhabitat::set_spawning_habitat("American River", "fr", existing_cfs_median_comparison_point("inchannel rearing", "American River", "fr", 'biop_itp_2018_2019'))


ic_juv |> 
  filter(watershed == "American River") |> 
  transmute(watershed, date = ymd(paste(year, month, 1)), 
            sit_habitat, r_to_r_baseline_habitat) |> 
  gather(version, acres, -watershed, -date)  |> 
  ggplot(aes(date, acres, color = version)) +
  geom_line(alpha = .75, weight = 1.5) + 
  # geom_col(position = 'dodge') +
  facet_wrap(~watershed, scales = 'free_y') + 
  theme_minimal() 


# yuba floodplain 
yuba_river_floodplain %>% 
  select(FR_floodplain_acres, watershed, flow_cfs) %>%
  mutate(Updated_FR_floodplain_acres = FR_floodplain_acres + FR_floodplain_acres * hab_prop_change_from_projects("floodplain rearing", "Yuba River", "fr", "juv", "biop_itp_2018_2019")) |> 
  pivot_longer(cols = c(FR_floodplain_acres, Updated_FR_floodplain_acres), names_to = "scenario", values_to = "Acres") |>  
  ggplot(aes(flow_cfs, Acres, linetype = scenario)) +
  geom_line() +
  theme_minimal() +
  scale_color_brewer(palette = 'Dark2') +
  labs(x = 'Flow (cfs)', y = 'Total Inundated Acres') +
  theme(legend.justification = c(1, 0), legend.position = c(.95, .1))
