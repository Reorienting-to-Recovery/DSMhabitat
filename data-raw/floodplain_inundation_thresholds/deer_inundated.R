library(tidyverse)
library(lubridate)
library(CDECRetrieve)
library(dataRetrieval)
library(DSMhabitat)

DSMhabitat::apply_suitability(
  DSMhabitat::square_meters_to_acres(
    DSMhabitat::set_floodplain_habitat(watershed = 'Deer Creek', species = 'fr', flow = 2000)))

# DEER C NR VINA CA-------------------
deer <- dataRetrieval::readNWISdv(siteNumbers = '11383500', parameterCd = '00060',
                                  startDate = '1984-01-01', endDate = '2003-12-31')

fp_threshold_flow <- DSMhabitat::deer_creek_floodplain$flow_cfs[which(cumsum(DSMhabitat::deer_creek_floodplain$FR_floodplain_acres != 0) == 1) - 1]


deer %>%
  select(date = Date, flow_cfs = X_00060_00003) %>%
  # group_by(year = year(date)) %>%
  # summarise(n())
  arrange(desc(flow_cfs))
ggplot(aes(x = date, y = flow_cfs)) +
  geom_line()

days_inundated <- deer %>%
  select(date = Date, flow_cfs = X_00060_00003) %>%
  mutate(fp_active = flow_cfs >= fp_threshold_flow) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(days_inundated = sum(fp_active),
            monthly_mean_flow = mean(flow_cfs, na.rm = TRUE)) %>%
  mutate(fp_area_acres = DSMhabitat::apply_suitability(
    DSMhabitat::square_meters_to_acres(
      DSMhabitat::set_floodplain_habitat(watershed = 'Deer Creek', species = 'fr', flow = monthly_mean_flow))))


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
# non linear
cor(days_inundated$days_inundated, days_inundated$monthly_mean_flow)

days_inundated %>%
  ungroup() %>%
  filter(monthly_mean_flow >= fp_threshold_flow) %>%
  pull(days_inundated) %>% summary()


data.frame(
  watershed = rep(c('Deer Creek'), 5),
  weeks_inundated = 0:4,
  flow_threshhold = c(0, NA, 370, NA, 750)
) %>% write_rds('data-raw/floodplain_inundation_thresholds/deer_inundated.rds')
