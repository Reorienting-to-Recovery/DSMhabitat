library(tidyverse)
library(purrr)
library(lubridate)

DSMhabitat::modeling_exist %>%
  group_by(Region) %>%
  summarise(n())

# create cache for Upper-mid Sacramento River --------
watersheds_with_modeling <- dplyr::pull(dplyr::filter(DSMhabitat::modeling_exist,
                                                      Region == 'Upper-mid Sacramento River',
                                                      FR_fry, Watershed != 'Cottonwood Creek',
                                                      Watershed != 'Upper-mid Sacramento River'), Watershed)

watersheds_without_modeling <- dplyr::pull(dplyr::filter(DSMhabitat::modeling_exist,
                                                         Region == 'Upper-mid Sacramento River',
                                                         !FR_juv), Watershed)

watersheds_with_spawn <- dplyr::pull(dplyr::filter(DSMhabitat::modeling_exist,
                                                      Region == 'Upper-mid Sacramento River',
                                                      FR_spawn, Watershed != 'Cottonwood Creek',
                                                      Watershed != 'Upper-mid Sacramento River'), Watershed)

# explore flow range of modeling
model_flow_summary <- function(df) {summary(pull(df, flow_cfs))}

model_flow_summary(DSMhabitat::battle_creek_instream)
model_flow_summary(DSMhabitat::butte_creek_instream)
model_flow_summary(DSMhabitat::clear_creek_instream)
model_flow_summary(DSMhabitat::cow_creek_instream)


# create list of wua (sq ft/1000 ft) approximators for each watershed in region
get_approx_spwn <- function(df) {approxfun(df$flow_cfs, df$FR_spawn_wua, rule = 2)}
get_approx_fry <- function(df) {approxfun(df$flow_cfs, df$FR_fry_wua, rule = 2)}
get_approx_juv <- function(df) {approxfun(df$flow_cfs, df$FR_juv_wua, rule = 2)}

upmidsac_spwn <- list(get_approx_spwn(DSMhabitat::battle_creek_instream),
                     get_approx_spwn(DSMhabitat::butte_creek_instream),
                     get_approx_spwn(DSMhabitat::clear_creek_instream))

upmidsac_juv <- list(get_approx_juv(DSMhabitat::battle_creek_instream),
                     get_approx_juv(DSMhabitat::butte_creek_instream),
                     get_approx_juv(DSMhabitat::clear_creek_instream),
                     get_approx_juv(DSMhabitat::cow_creek_instream))

upmidsac_fry <- list(get_approx_fry(DSMhabitat::battle_creek_instream),
                     get_approx_fry(DSMhabitat::butte_creek_instream),
                     get_approx_fry(DSMhabitat::clear_creek_instream),
                     get_approx_fry(DSMhabitat::cow_creek_instream))

cvpiaFlow::flows_cfs %>%
  gather(watershed, flow, -date) %>%
  filter(watershed %in% watersheds_without_modeling, month(date) %in% 1:8) %>%
  group_by(watershed) %>%
  summarise(min = min(flow), median = median(flow), mean = mean(flow),
            q90 = quantile(flow, .9), max = max(flow))

cvpiaFlow::flows_cfs %>%
  gather(watershed, flow, -date) %>%
  filter(watershed %in% watersheds_with_modeling, month(date) %in% 1:8) %>%
  group_by(watershed) %>%
  summarise(min = min(flow), median = median(flow), mean = mean(flow),
            q90 = quantile(flow, .9), max = max(flow))

flows <- DSMhabitat::clear_creek_instream$flow_cfs

upper_mid_sac_region_instream <- purrr::map_df(flows, function(flow) {
  wua_spn <- mean(purrr::map_dbl(1:length(upmidsac_spwn), function(i){upmidsac_spwn[[i]](flow)}))
  wua_fry <- mean(purrr::map_dbl(1:length(upmidsac_fry), function(i){upmidsac_fry[[i]](flow)}))
  wua_juv <- mean(purrr::map_dbl(1:length(upmidsac_juv), function(i){upmidsac_juv[[i]](flow)}))
  tibble(flow_cfs = flow, FR_spawn_wua = wua_spn, FR_fry_wua = wua_fry, FR_juv_wua = wua_juv,
         watershed = 'Upper-mid Sacramento River Region')
})

devtools::use_data(upper_mid_sac_region_instream, overwrite = TRUE)

ggplot(upper_mid_sac_region_instream, aes(x = flow, y = mean_wua)) +
  geom_line()

bind_rows(DSMhabitat::battle_creek_instream,
     DSMhabitat::butte_creek_instream,
     DSMhabitat::clear_creek_instream,
     DSMhabitat::cow_creek_instream,
     upper_mid_sac_region_instream) %>%
  ggplot(aes(x = flow_cfs, y= FR_fry_wua, color = watershed)) +
  geom_line() +
  theme_minimal()



