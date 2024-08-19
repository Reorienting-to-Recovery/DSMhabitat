library(tidyverse)

# create input data for streams for scaling
# using HRL commitments

all_habitat_data_for_hrl_inputs_all_runs <- read_csv(here::here("data-raw", "R2R_HRL_habitat_inputs", "hrl_commitments_by_watershed.csv")) |> 
  filter(!is.na(total_acres_HRL)) |> 
  #pivot_wider(names_from = habitat_type, values_from = total_acres_HRL) |> 
  # rename(max_spawning_acres_hrl = spawning,
  #        max_rearing_acres_hrl = `inchannel rearing`,
  #        max_floodplain_acres_hrl = `floodplain rearing`) |> 
  # TODO initially set for fall run
  # TODO can do spawning on Sacramento for fall and winter
  # TODO rearing can be for all runs on all watersheds
  # TODO then update once meet with Rene and HRL
  rename(total_acres = total_acres_HRL) |> 
  mutate(run = case_when(run == "fall + spring" ~ "Fall and Spring",
                         run == "all" ~ "Fall and Winter and Spring",
                         run == "unknown" ~ "Fall and Winter and Spring", # TODO: assumption, need to get clarification from tech team
                         TRUE ~ NA)) |> 
  separate_rows(run, sep = " and ") |>  
  mutate(run = tolower(run)) |> 
  filter(proposed_approach_for_incorporation == "Scale existing flow-area curve") |> 
  select(-c(total_acres_R2R_baseline, questions, proposed_approach_for_incorporation)) |> 
  mutate(watershed = case_when(watershed == "Mokelumne" ~"Mokelumne River", 
                               watershed == "Sacramento River" ~ "Upper Sacramento River", # TODO: flagging this as we aren't 100% 
                                                                                           # sure we want to add all acreage to Upper Sac 
                               .default = as.character(watershed)))  


View(all_habitat_data_for_hrl_inputs_all_runs)
# TODO assign runs

saveRDS(all_habitat_data_for_hrl_inputs_all_runs,
        "data-raw/R2R_HRL_habitat_inputs/all_habitat_data_for_hrl_inputs_all_runs.rdata")


