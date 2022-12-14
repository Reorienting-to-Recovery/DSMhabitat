library(DSMhabitat)
context('Clear Creek Habitat')

test_that("modeling of species coverage hasn't changed - Clear", {
  modeling <- subset(DSMhabitat::modeling_exist, Watershed == 'Clear Creek')

  expect_equal(modeling$FR_spawn, TRUE)
  expect_equal(modeling$FR_fry, TRUE)
  expect_equal(modeling$FR_juv, TRUE)
  expect_equal(modeling$FR_floodplain, FALSE)

  expect_equal(modeling$SR_spawn, TRUE)
  expect_equal(modeling$SR_fry, TRUE)
  expect_equal(modeling$SR_juv, TRUE)
  expect_equal(modeling$SR_floodplain, FALSE)

  expect_equal(modeling$ST_spawn, TRUE)
  expect_equal(modeling$ST_fry, TRUE)
  expect_equal(modeling$ST_juv, TRUE)
  expect_equal(modeling$ST_floodplain, FALSE)
  expect_equal(modeling$ST_adult, FALSE)
})

test_that('FR instream Clear Creek works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::clear_creek_instream$FR_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::clear_creek_instream$FR_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::clear_creek_instream$FR_spawn_wua))[1]

  fry_wua <- DSMhabitat::clear_creek_instream$FR_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::clear_creek_instream$FR_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::clear_creek_instream$FR_spawn_wua[spawn_not_na_index]

  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Clear Creek' & lifestage == 'rearing'
                                  & species == 'fr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Clear Creek' & lifestage == 'spawning'
                                   & species == 'fr')$feet

  fryx <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juvx <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawnx <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::clear_creek_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::clear_creek_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::clear_creek_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Clear Creek', 'fr', 'fry', fry_flow), fryx)
  expect_equal(
    set_instream_habitat('Clear Creek', 'fr', 'juv', juv_flow), juvx)
  expect_equal(
    set_spawning_habitat('Clear Creek', 'fr', spawn_flow), spawnx)
})


test_that('SR instream Clear Creek works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::clear_creek_instream$SR_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::clear_creek_instream$SR_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::clear_creek_instream$SR_spawn_wua))[1]

  fry_wua <- DSMhabitat::clear_creek_instream$SR_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::clear_creek_instream$SR_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::clear_creek_instream$SR_spawn_wua[spawn_not_na_index]

  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Clear Creek' & lifestage == 'rearing'
                                  & species == 'sr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Clear Creek' & lifestage == 'spawning'
                                   & species == 'sr')$feet

  fryx <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juvx <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawnx <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::clear_creek_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::clear_creek_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::clear_creek_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Clear Creek', 'sr', 'fry', fry_flow), fryx)
  expect_equal(
    set_instream_habitat('Clear Creek', 'sr', 'juv', juv_flow), juvx)
  expect_equal(
    set_spawning_habitat('Clear Creek', 'sr', spawn_flow), spawnx)
})

test_that('ST instream Clear Creek works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::clear_creek_instream$ST_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::clear_creek_instream$ST_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::clear_creek_instream$ST_spawn_wua))[1]

  fry_wua <- DSMhabitat::clear_creek_instream$ST_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::clear_creek_instream$ST_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::clear_creek_instream$ST_spawn_wua[spawn_not_na_index]

  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Clear Creek' & lifestage == 'rearing'
                                  & species == 'st')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Clear Creek' & lifestage == 'spawning'
                                   & species == 'st')$feet

  fryx <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juvx <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawnx <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::clear_creek_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::clear_creek_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::clear_creek_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Clear Creek', 'st', 'fry', fry_flow), fryx)
  expect_equal(
    set_instream_habitat('Clear Creek', 'st', 'juv', juv_flow), juvx)
  expect_equal(
    set_spawning_habitat('Clear Creek', 'st', spawn_flow), spawnx)
})

# Tests for species/habitat without modeling (FALSE modeling_exists) ----

test_that('FR floodplain Clear Creek works', {
  first_flood_index <-  which(DSMhabitat::clear_creek_floodplain$FR_floodplain_acres > 0)[1]

  flow <- DSMhabitat::clear_creek_floodplain$flow_cfs[first_flood_index]
  floodplain <- subset(DSMhabitat::clear_creek_floodplain,flow_cfs == flow)$FR_floodplain_acres

  expect_equal(
    square_meters_to_acres(set_floodplain_habitat('Clear Creek', 'fr', flow)),
    floodplain,
    tolerance = .01)
})

test_that('SR floodplain Clear Creek works', {
  first_flood_index <-  which(DSMhabitat::clear_creek_floodplain$SR_floodplain_acres > 0)[1]

  flow <- DSMhabitat::clear_creek_floodplain$flow_cfs[first_flood_index]
  floodplain <- subset(DSMhabitat::clear_creek_floodplain,flow_cfs == flow)$SR_floodplain_acres

  expect_equal(
    square_meters_to_acres(set_floodplain_habitat('Clear Creek', 'sr', flow)),
    floodplain,
    tolerance = .01)
})
# LRF ----

test_that('LFR instream Clear Creek works', {
  
  fry_index <- 6
  juv_index <- 6
  spawn_index <- 6
  
  fry_wua <- DSMhabitat::clear_creek_instream$FR_fry_wua[fry_index]
  juv_wua <- DSMhabitat::clear_creek_instream$FR_juv_wua[juv_index]
  spawn_wua <- DSMhabitat::clear_creek_instream$FR_spawn_wua[spawn_index]
  
  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Clear Creek' & lifestage == 'rearing'
                                  & species == 'lfr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Clear Creek' & lifestage == 'spawning'
                                   & species == 'lfr')$feet
  
  fry_m2 <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juv_m2 <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawn_m2 <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)
  
  fry_flow <- DSMhabitat::clear_creek_instream$flow_cfs[fry_index]
  juv_flow <- DSMhabitat::clear_creek_instream$flow_cfs[juv_index]
  spawn_flow <- DSMhabitat::clear_creek_instream$flow_cfs[spawn_index]
  
  expect_equal(
    set_instream_habitat('Clear Creek', 'lfr', 'fry', fry_flow), fry_m2)
  expect_equal(
    set_instream_habitat('Clear Creek', 'lfr', 'juv', juv_flow), juv_m2)
  expect_equal(
    set_spawning_habitat('Clear Creek', 'lfr', spawn_flow), spawn_m2)
})

test_that('LFR floodplain Clear Creek works', {
  first_flood_index <-  which(DSMhabitat::clear_creek_floodplain$LFR_floodplain_acres > 0)[1]
  
  flow <- DSMhabitat::clear_creek_floodplain$flow_cfs[first_flood_index]
  floodplain <- subset(DSMhabitat::clear_creek_floodplain,flow_cfs == flow)$LFR_floodplain_acres
  
  expect_equal(
    square_meters_to_acres(set_floodplain_habitat('Clear Creek', 'lfr', flow)),
    floodplain,
    tolerance = .01)
})

test_that('No WR on Clear Creek', {
  expect_true(is.na(set_instream_habitat('Clear Creek', 'wr', 'fry', 200)))
  expect_true(is.na(set_spawning_habitat('Clear Creek', 'wr', 100)))
  expect_true(is.na(set_floodplain_habitat('Clear Creek', 'wr', 2000)))
})