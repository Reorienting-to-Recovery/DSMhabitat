# file to cache Run of River theoretical Max Habitat data objects 
# and some exploratory plots to compare SIT existing 
# to TMH. 

library(tidyverse)
library(DSMhabitat)
library(lubridate)

source('data-raw/R2R_TMH_habitat_inputs/tmh_helper_functions.R')


# WINTER RUN --------------------------------------------------------------
# for now, only generate different values for winter run
# and only in the Upper Sacramento River


# FALL RUN: does not extend past reservoirs -------------------------------
# Winter and Spring Run: extend past reservoirs ---------------------------

# update DSMhabitat values ------------------------------------------------
action_5_tmh_wr_spawn <- spawn_tmh_processing(watersheds = "Upper Sacramento River", species = "wr", calsim_run = "action_5")

## inchannel habitat to both fry and juvenile habitat objects ---------------

action_5_tmh_wr_fry <- rearing_tmh_processing(watersheds = "Upper Sacramento River", species = "wr", calsim_run = "action_5")$fry
action_5_tmh_wr_juv <- rearing_tmh_processing(watersheds = "Upper Sacramento River", species = "wr", calsim_run = "action_5")$juv

##floodplain: -------------------------------------------------------------

action_5_tmh_wr_flood <- floodplain_tmh_processing(watersheds = "Upper Sacramento River", species = "wr", calsim_run = "action_5")

action_5_tmh_wr_flood["Upper Sacramento River",,] == DSMhabitat::wr_fp$action_5["Upper Sacramento River",,]
action_5_tmh_wr_flood["Upper Sacramento River",,] == DSMhabitat::wr_fp$action_5_baseline["Upper Sacramento River",,]

##delta: -------------------------------------------------------------------
action_5_tmh_delta <- delta_tmh_processing()

# save data objects -------------------------------------------------------

# Save as data object to DSMhabitat
## FLOODPLAIN:
#### Winter Run:
wr_fp <- modifyList(DSMhabitat::wr_fp, list(action_5_tmh = action_5_tmh_wr_flood))
usethis::use_data(wr_fp, overwrite = TRUE)

## IN CHANNEL REARING:
### Winter Run: 
wr_fry <- modifyList(DSMhabitat::wr_fry, list(action_5_tmh = action_5_tmh_wr_fry))
usethis::use_data(wr_fry, overwrite = TRUE)

wr_juv <- modifyList(DSMhabitat::wr_juv, list(action_5_tmh = action_5_tmh_wr_juv))
usethis::use_data(wr_juv, overwrite = TRUE)

## SPAWNING: 
### Winter Run:
wr_spawn <- modifyList(DSMhabitat::wr_spawn, list(action_5_tmh = action_5_tmh_wr_spawn))
usethis::use_data(wr_spawn, overwrite = TRUE)

delta_habitat <- modifyList(DSMhabitat::delta_habitat, list(action_5_tmh = action_5_tmh_delta))
usethis::use_data(delta_habitat, overwrite = TRUE)
# do some checks, but make sure you build library first 

# commented this out because it doesn't work unless you build, and so it causes an error in sourcing update_data.R
# table(DSMhabitat::fr_spawn$action_5 == DSMhabitat::fr_spawn$action_5_tmh)
# table(DSMhabitat::fr_fp$action_5 == DSMhabitat::fr_fp$action_5_tmh)
# table(DSMhabitat::sr_juv$action_5 == DSMhabitat::sr_juv$action_5_tmh)
# 
# # Exploratory Plots:  -----------------------------------------------------
# ## spawning plot:  ---------------------------------------------------------
# ### fall run: 
# tmh_comparison_plot(tmh_data = DSMhabitat::fr_spawn$action_5_tmh, 
#                     sit_habitat = DSMhabitat::fr_spawn$action_5, "spawn")
# 
# tmh_comparison_plot(tmh_data = DSMhabitat::fr_spawn$action_5_tmh, 
#                     sit_habitat = DSMhabitat::fr_spawn$r_to_r_tmh, "spawn")
# 
# tmh_comparison_plot(tmh_data =DSMhabitat::wr_spawn$action_5_tmh, 
#                     sit_habitat = DSMhabitat::wr_spawn$r_to_r_tmh, "spawn")
# 
# tmh_comparison_plot(tmh_data = DSMhabitat::sr_spawn$action_5_tmh, 
#                     sit_habitat = DSMhabitat::sr_spawn$r_to_r_tmh, "spawn")
# 
# ## fry and juv plots:  -----------------------------------------------------
# tmh_comparison_plot(tmh_data = DSMhabitat::fr_fry$action_5_tmh, 
#                     sit_habitat = DSMhabitat::fr_fry$r_to_r_tmh, "fry")
# 
# # winter run
# tmh_comparison_plot(tmh_data = DSMhabitat::wr_fry$action_5_tmh, 
#                     sit_habitat = DSMhabitat::wr_fry$r_to_r_tmh, "fry")
# # spring run 
# tmh_comparison_plot(tmh_data = DSMhabitat::sr_fry$action_5_tmh, 
#                     sit_habitat = DSMhabitat::sr_fry$r_to_r_tmh, "fry")
# 
# ## floodplain exploratory plot:  -------------------------------------------
# # fall run: 
# tmh_comparison_plot(tmh_data = DSMhabitat::fr_fp$action_5_tmh, 
#                     sit_habitat = DSMhabitat::fr_fp$r_to_r_tmh, "flood")
# # winter run:
# tmh_comparison_plot(tmh_data = DSMhabitat::wr_fp$action_5_tmh, 
#                     sit_habitat = DSMhabitat::wr_fp$r_to_r_tmh, "flood")
# # spring run: 
# tmh_comparison_plot(tmh_data = DSMhabitat::sr_fp$action_5_tmh, 
#                     sit_habitat = DSMhabitat::sr_fp$r_to_r_tmh, "flood")

