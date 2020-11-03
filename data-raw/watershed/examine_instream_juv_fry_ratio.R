library(tidyverse)

me <- DSMhabitat::modeling_exist
me %>% glimpse()
me$Region %>% unique

# upper mid sac----------------------
me %>%
  filter(Region == "Upper-mid Sacramento River") %>%
  select(Watershed, starts_with('FR'), Region) %>%
  filter(FR_fry, FR_juv)

ba <- DSMhabitat::battle_creek_instream %>%
  mutate(ratio = FR_fry_wua/FR_juv_wua) %>%
  select(flow_cfs, ratio) %>%
  mutate(creek = 'battle')

bu <- DSMhabitat::butte_creek_instream %>%
  mutate(ratio = FR_fry_wua/FR_juv_wua) %>%
  select(flow_cfs, ratio) %>%
  filter(!is.na(ratio)) %>%
  mutate(creek = 'butte')

cl <- DSMhabitat::clear_creek_instream %>%
  mutate(ratio = FR_fry_wua/FR_juv_wua) %>%
  select(flow_cfs, ratio) %>%
  mutate(creek = 'clear')

cot <- DSMhabitat::cottonwood_creek_instream %>%
  mutate(ratio = FR_fry_wua/FR_juv_wua) %>%
  select(flow_cfs, ratio) %>%
  filter(!is.na(ratio), flow_cfs < 1000) %>%
  mutate(creek = 'cottonwood')

cow <- DSMhabitat::cow_creek_instream %>%
  mutate(ratio = FR_fry_wua/FR_juv_wua) %>%
  select(flow_cfs, ratio) %>%
  mutate(creek = 'cow')

# use region approximation for big chico
flows_cfs <- cl$flow_cfs
fry <- purrr::map_dbl(flows_cfs, set_instream_habitat, watershed = 'Big Chico Creek', species = 'fr', life_stage = 'fry')
juv <- purrr::map_dbl(flows_cfs, set_instream_habitat, watershed = 'Big Chico Creek', species = 'fr', life_stage = 'juv')
ratio <- fry/juv

bc <- tibble(flow_cfs = flows_cfs, ratio = ratio, creek = 'big chico*')


bind_rows(ba, bu, cl, cot, cow, bc) %>%
  ggplot(aes(x = flow_cfs, y = ratio, color = creek)) +
  geom_line() +
  theme_minimal() +
  geom_hline(yintercept = 1) +
  labs(y = 'ratio fryWUA to juvWUA', x = 'flow (cfs)', caption = "*calculated using regional approximation")


# south delta------------
me %>%
  filter(Region == "South Delta") %>%
  select(Watershed, starts_with('FR'), Region) %>%
  filter(FR_juv)

cal <- DSMhabitat::calaveras_river_instream %>%
  mutate(ratio = FR_fry_wua/FR_juv_wua) %>%
  select(flow_cfs, ratio) %>%
  mutate(river = 'calaveras')

DSMhabitat::mokelumne_river_instream
moke_fry <- approxfun(x = DSMhabitat::mokelumne_river_instream$flow_cfs,
                      y = DSMhabitat::mokelumne_river_instream$FR_fry_wua, rule = 2)

moke_juv <- approxfun(x = DSMhabitat::mokelumne_river_instream$flow_cfs,
                      y = DSMhabitat::mokelumne_river_instream$FR_juv_wua, rule = 2)
ratio <- moke_fry(DSMhabitat::mokelumne_river_instream$flow_cfs) / moke_juv(DSMhabitat::mokelumne_river_instream$flow_cfs)
flows_cfs <- DSMhabitat::mokelumne_river_instream$flow_cfs
moke <- tibble(flow_cfs = flows_cfs, ratio = ratio, river = 'moke') %>%
  filter(flows_cfs < 500)

# region approximation is doing a good job, no need to scale fry or juv not a
# consistant ratio fry/juv to use as scaling factor for tribs with only juv
# modeled to estimate fry value. just use juv for fry

