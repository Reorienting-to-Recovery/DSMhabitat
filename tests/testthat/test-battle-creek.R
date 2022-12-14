library(DSMhabitat)
context('Battle Creek Habitat')

test_that("modeling of species coverage hasn't changed - Battle", {
  modeling <- subset(DSMhabitat::modeling_exist, Watershed == 'Battle Creek')

  expect_equal(modeling$FR_spawn, TRUE)
  expect_equal(modeling$FR_fry, TRUE)
  expect_equal(modeling$FR_juv, TRUE)
  expect_equal(modeling$FR_floodplain, FALSE)

  expect_equal(modeling$SR_spawn, TRUE)
  expect_equal(modeling$SR_fry, TRUE)
  expect_equal(modeling$SR_juv, TRUE)
  expect_equal(modeling$SR_floodplain, FALSE)
  
  expect_equal(modeling$WR_spawn, TRUE)
  expect_equal(modeling$WR_fry, TRUE)
  expect_equal(modeling$WR_juv, TRUE)
  expect_equal(modeling$WR_floodplain, FALSE)

  expect_equal(modeling$ST_spawn, TRUE)
  expect_equal(modeling$ST_fry, TRUE)
  expect_equal(modeling$ST_juv, TRUE)
  expect_equal(modeling$ST_floodplain, FALSE)
  expect_equal(modeling$ST_adult, TRUE)
})

test_that('FR instream Battle Creek works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$FR_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$FR_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$FR_spawn_wua))[1]

  fry_wua <- DSMhabitat::battle_creek_instream$FR_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::battle_creek_instream$FR_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::battle_creek_instream$FR_spawn_wua[spawn_not_na_index]

  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Battle Creek' & lifestage == 'rearing'
                                  & species == 'fr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Battle Creek' & lifestage == 'spawning'
                                   & species == 'fr')$feet

  fry_m2 <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juv_m2 <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawn_m2 <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::battle_creek_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::battle_creek_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::battle_creek_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Battle Creek', 'fr', 'fry', fry_flow), fry_m2)
  expect_equal(
    set_instream_habitat('Battle Creek', 'fr', 'juv', juv_flow), juv_m2)
  expect_equal(
    set_spawning_habitat('Battle Creek', 'fr', spawn_flow), spawn_m2)
})

test_that('SR instream Battle Creek works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$SR_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$SR_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$SR_spawn_wua))[1]

  fry_wua <- DSMhabitat::battle_creek_instream$SR_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::battle_creek_instream$SR_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::battle_creek_instream$SR_spawn_wua[spawn_not_na_index]

  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Battle Creek' & lifestage == 'rearing'
                                  & species == 'sr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Battle Creek' & lifestage == 'spawning'
                                   & species == 'sr')$feet

  fry_m2 <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juv_m2 <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawn_m2 <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::battle_creek_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::battle_creek_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::battle_creek_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Battle Creek', 'sr', 'fry', fry_flow), fry_m2)
  expect_equal(
    set_instream_habitat('Battle Creek', 'sr', 'juv', juv_flow), juv_m2)
  expect_equal(
    set_spawning_habitat('Battle Creek', 'sr', spawn_flow), spawn_m2)
})

test_that('ST instream Battle Creek works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$ST_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$ST_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$ST_spawn_wua))[1]

  fry_wua <- DSMhabitat::battle_creek_instream$ST_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::battle_creek_instream$ST_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::battle_creek_instream$ST_spawn_wua[spawn_not_na_index]

  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Battle Creek' & lifestage == 'rearing'
                                  & species == 'st')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Battle Creek' & lifestage == 'spawning'
                                   & species == 'st')$feet

  fry_m2 <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juv_m2 <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawn_m2 <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::battle_creek_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::battle_creek_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::battle_creek_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Battle Creek', 'st', 'fry', fry_flow), fry_m2)
  expect_equal(
    set_instream_habitat('Battle Creek', 'st', 'juv', juv_flow), juv_m2)
  expect_equal(
    set_spawning_habitat('Battle Creek', 'st', spawn_flow), spawn_m2)
})

#this test should fail for now (10/01/20) until stream length & set-instream-habitat.R code updated
test_that('ST adult Battle Creek works', {

  adult_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$ST_adult_wua))[1]
  adult_wua <- DSMhabitat::battle_creek_instream$ST_adult_wua[adult_not_na_index]

  adult_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Battle Creek' & lifestage == 'rearing'
                                  & species == 'st')$feet

  adult_m2 <- (((adult_stream_length/1000) * adult_wua)/10.7639)

  flow <- DSMhabitat::battle_creek_instream$flow_cfs[adult_not_na_index]

  expect_equal(
    set_instream_habitat('Battle Creek', 'st', 'adult', flow), adult_m2)

})



# winter run  -----

test_that('WR instream Battle Creek works', {
  
  fry_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$WR_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$WR_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::battle_creek_instream$WR_spawn_wua))[1]
  
  fry_wua <- DSMhabitat::battle_creek_instream$WR_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::battle_creek_instream$WR_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::battle_creek_instream$WR_spawn_wua[spawn_not_na_index]
  
  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Battle Creek' & lifestage == 'rearing'
                                  & species == 'wr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Battle Creek' & lifestage == 'spawning'
                                   & species == 'wr')$feet
  
  fry_m2 <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juv_m2 <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawn_m2 <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)
  
  fry_flow <- DSMhabitat::battle_creek_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::battle_creek_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::battle_creek_instream$flow_cfs[spawn_not_na_index]
  
  expect_equal(
    set_instream_habitat('Battle Creek', 'wr', 'fry', fry_flow), fry_m2)
  expect_equal(
    set_instream_habitat('Battle Creek', 'wr', 'juv', juv_flow), juv_m2)
  expect_equal(
    set_spawning_habitat('Battle Creek', 'wr', spawn_flow), spawn_m2)
})


# late fall run  -----

test_that('LFR instream Battle Creek works', {
  
  fry_index <- 6
  juv_index <- 6
  spawn_index <- 6
  
  fry_wua <- DSMhabitat::battle_creek_instream$FR_fry_wua[fry_index]
  juv_wua <- DSMhabitat::battle_creek_instream$FR_juv_wua[juv_index]
  spawn_wua <- DSMhabitat::battle_creek_instream$FR_spawn_wua[spawn_index]
  
  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Battle Creek' & lifestage == 'rearing'
                                  & species == 'lfr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Battle Creek' & lifestage == 'spawning'
                                   & species == 'lfr')$feet
  
  fry_m2 <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juv_m2 <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawn_m2 <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)
  
  fry_flow <- DSMhabitat::battle_creek_instream$flow_cfs[fry_index]
  juv_flow <- DSMhabitat::battle_creek_instream$flow_cfs[juv_index]
  spawn_flow <- DSMhabitat::battle_creek_instream$flow_cfs[spawn_index]
  
  expect_equal(
    set_instream_habitat('Battle Creek', 'lfr', 'fry', fry_flow), fry_m2)
  expect_equal(
    set_instream_habitat('Battle Creek', 'lfr', 'juv', juv_flow), juv_m2)
  expect_equal(
    set_spawning_habitat('Battle Creek', 'lfr', spawn_flow), spawn_m2)
})

# late fall run  -----

test_that('LFR floodplain Battle Creek works', {
  first_flood_index <-  which(DSMhabitat::battle_creek_floodplain$LFR_floodplain_acres > 0)[1]
  
  flow <- DSMhabitat::battle_creek_floodplain$flow_cfs[first_flood_index]
  floodplain <- subset(DSMhabitat::battle_creek_floodplain,flow_cfs == flow)$LFR_floodplain_acres
  
  expect_equal(
    square_meters_to_acres(set_floodplain_habitat('Battle Creek', 'lfr', flow)),
    floodplain,
    tolerance = .01)
})
