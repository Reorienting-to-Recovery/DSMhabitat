library(tidyverse)
library(lubridate)
library(CDECRetrieve)
library(dataRetrieval)
library(DSMhabitat)

DSMhabitat::apply_suitability(
  DSMhabitat::square_meters_to_acres(
    DSMhabitat::set_floodplain_habitat(watershed = 'Tuolumne River', species = 'fr', flow = 2000)))

# TUOLUMNE R A MODESTO CA-------------------
tuolumne <- dataRetrieval::readNWISdv(siteNumbers = '11290000', parameterCd = '00060',
                                        startDate = '1980-01-01', endDate = '2000-12-31')

fp_threshold_flow <- DSMhabitat::tuolumne_river_floodplain$flow_cfs[which(cumsum(DSMhabitat::tuolumne_river_floodplain$FR_floodplain_acres != 0) == 1)-1]


tuolumne %>%
  select(date = Date, flow_cfs = X_00060_00003) %>%
  # group_by(year = year(date)) %>%
  # summarise(n())
  # arrange(desc(flow_cfs))
  ggplot(aes(x = date, y = flow_cfs)) +
  geom_line()

days_inundated <- tuolumne %>%
  select(date = Date, flow_cfs = X_00060_00003) %>%
  mutate(fp_active = flow_cfs >= fp_threshold_flow) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(days_inundated = sum(fp_active),
            monthly_mean_flow = mean(flow_cfs, na.rm = TRUE)) %>%
  mutate(fp_area_acres = DSMhabitat::apply_suitability(
    DSMhabitat::square_meters_to_acres(
      DSMhabitat::set_floodplain_habitat(watershed = 'Tuolumne River', species = 'fr', flow = monthly_mean_flow))))


days_inundated %>%
  filter(monthly_mean_flow > fp_threshold_flow) %>%
  # filter(days_inundated > 0) %>%
  ggplot(aes(x = monthly_mean_flow, y = days_inundated)) +
  geom_jitter(pch = 1, width = .2) +
  geom_vline(xintercept = fp_threshold_flow) +
  theme_minimal() +
  geom_hline(yintercept = 7) +
  geom_hline(yintercept = 14) +
  geom_hline(yintercept = 21) +
  geom_hline(yintercept = 28)
# geom_smooth(method = 'lm', se = FALSE)
# non linear
cor(days_inundated$days_inundated, days_inundated$monthly_mean_flow)

days_inundated %>%
  ungroup() %>%
  # filter(days_inundated > 0) %>%
  filter(monthly_mean_flow >= fp_threshold_flow) %>%
  pull(days_inundated) %>% summary()


data.frame(
  watershed = rep(c('Tuolumne River'), 5),
  weeks_inundated = 0:4,
  flow_threshhold = c(0, NA, NA, NA, 1000)
) %>% write_rds('data-raw/floodplain_inundation_thresholds/tuolumne_river_inundated.rds')
