library(DSMhabitat)
context('Yuba River Habitat')

test_that("modeling of species coverage hasn't changed - Yuba", {
  modeling <- subset(DSMhabitat::modeling_exist, Watershed == 'Yuba River')

  expect_equal(modeling$FR_spawn, TRUE)
  expect_equal(modeling$FR_fry, TRUE)
  expect_equal(modeling$FR_juv, TRUE)
  expect_equal(modeling$FR_floodplain, TRUE)

  expect_equal(modeling$SR_spawn, TRUE)
  expect_equal(modeling$SR_fry, FALSE)
  expect_equal(modeling$SR_juv, FALSE)
  expect_equal(modeling$SR_floodplain, TRUE)

  expect_equal(modeling$ST_spawn, TRUE)
  expect_equal(modeling$ST_fry, TRUE)
  expect_equal(modeling$ST_juv, TRUE)
  expect_equal(modeling$ST_floodplain, TRUE)
  expect_equal(modeling$ST_adult, TRUE)
})

test_that('FR instream Yuba River works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::yuba_river_instream$FR_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::yuba_river_instream$FR_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::yuba_river_instream$FR_spawn_wua))[1]

  fry_wua <- DSMhabitat::yuba_river_instream$FR_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::yuba_river_instream$FR_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::yuba_river_instream$FR_spawn_wua[spawn_not_na_index]

  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Yuba River' & lifestage == 'rearing'
                                  & species == 'fr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Yuba River' & lifestage == 'spawning'
                                   & species == 'fr')$feet

  fryx <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juvx <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawnx <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::yuba_river_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::yuba_river_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::yuba_river_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Yuba River', 'fr', 'fry', fry_flow), fryx)
  expect_equal(
    set_instream_habitat('Yuba River', 'fr', 'juv', juv_flow), juvx)
  expect_equal(
    set_spawning_habitat('Yuba River', 'fr', spawn_flow), spawnx)
})

test_that('SR spawn Yuba River works', {

  spawn_not_na_index <- which(!is.na(DSMhabitat::yuba_river_instream$FR_spawn_wua))[1]

  spawn_wua <- DSMhabitat::yuba_river_instream$FR_spawn_wua[spawn_not_na_index]
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Yuba River' & lifestage == 'spawning'
                                   & species == 'fr')$feet

  spawnx <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  spawn_flow <- DSMhabitat::yuba_river_instream$flow_cfs[spawn_not_na_index]
  expect_equal(
    set_spawning_habitat('Yuba River', 'fr', spawn_flow), spawnx)

})

test_that('ST instream Yuba River works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::yuba_river_instream$ST_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::yuba_river_instream$ST_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::yuba_river_instream$ST_spawn_wua))[1]

  fry_wua <- DSMhabitat::yuba_river_instream$ST_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::yuba_river_instream$ST_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::yuba_river_instream$ST_spawn_wua[spawn_not_na_index]

  #TODO - there is no steelhead extent for yuba river - is fall run ok? (Issue #205)
  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Yuba River' & lifestage == 'rearing'
                                  & species == 'fr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Yuba River' & lifestage == 'spawning'
                                   & species == 'fr')$feet

  fryx <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juvx <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawnx <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::yuba_river_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::yuba_river_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::yuba_river_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Yuba River', 'st', 'fry', fry_flow), fryx)
  expect_equal(
    set_instream_habitat('Yuba River', 'st', 'juv', juv_flow), juvx)
  expect_equal(
    set_spawning_habitat('Yuba River', 'st', spawn_flow), spawnx)
})

test_that('FR floodplain Yuba River works', {
  first_flood_index <-  which(DSMhabitat::yuba_river_floodplain$FR_floodplain_acres > 0)[1]

  flow <- DSMhabitat::yuba_river_floodplain$flow_cfs[first_flood_index]
  floodplain <- subset(DSMhabitat::yuba_river_floodplain,flow_cfs == flow)$FR_floodplain_acres

  expect_equal(
    square_meters_to_acres(set_floodplain_habitat('Yuba River', 'fr', flow)),
    floodplain,
    tolerance = .01)
})

test_that('SR rearing are same as FR', {

  expect_equal(
    set_instream_habitat('Yuba River', 'fr', 'fry', 2500),
    set_instream_habitat('Yuba River', 'sr', 'fry', 2500))

  expect_equal(
    set_instream_habitat('Yuba River', 'fr', 'juv', 2500),
    set_instream_habitat('Yuba River', 'sr', 'juv', 2500))

})

# TODO
#this test should fail for now (10/01/20) until stream length & set-instream-habitat.R code updated
test_that('ST adult Yuba River works', {

  adult_not_na_index <- which(!is.na(DSMhabitat::yuba_river_instream$ST_adult_wua))[1]
  adult_wua <- DSMhabitat::yuba_river_instream$ST_adult_wua[adult_not_na_index]

  adult_stream_length <- subset(DSMhabitat::watershed_lengths,
                                watershed == 'Yuba River' & lifestage == 'adult'
                                & species == 'st')$feet

  adultx <- (((adult_stream_length/1000) * adult_wua)/10.7639)

  adult_flow <- DSMhabitat::yuba_river_instream$flow_cfs[adult_not_na_index]

  expect_equal(
    set_instream_habitat('Yuba River', 'st', 'adult', adult_flow), adultx)

})

test_that('No WR or LFR on Yuba River', {
  expect_true(is.na(set_instream_habitat('Yuba River', 'wr', 'fry', 200)))
  expect_true(is.na(set_instream_habitat('Yuba River', 'lfr', 'fry', 200)))
  expect_true(is.na(set_spawning_habitat('Yuba River', 'wr', 100)))
  expect_true(is.na(set_spawning_habitat('Yuba River', 'lfr', 100)))
  expect_true(is.na(set_floodplain_habitat('Yuba River', 'wr', 2000)))
  expect_true(is.na(set_floodplain_habitat('Yuba River', 'lfr', 2000)))
})