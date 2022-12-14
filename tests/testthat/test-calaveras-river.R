library(DSMhabitat)
context('Calaveras River Habitat')

test_that("modeling of species coverage hasn't changed - Calaveras", {
  modeling <- subset(DSMhabitat::modeling_exist, Watershed == 'Calaveras River')

  expect_equal(modeling$FR_spawn, FALSE)
  expect_equal(modeling$FR_fry, FALSE)
  expect_equal(modeling$FR_juv, FALSE)
  expect_equal(modeling$FR_floodplain, FALSE)

  expect_equal(is.na(modeling$SR_spawn), TRUE)
  expect_equal(is.na(modeling$SR_fry), TRUE)
  expect_equal(is.na(modeling$SR_juv), TRUE)
  expect_equal(is.na(modeling$SR_floodplain), TRUE)

  expect_equal(modeling$ST_spawn, TRUE)
  expect_equal(modeling$ST_fry, TRUE)
  expect_equal(modeling$ST_juv, TRUE)
  expect_equal(modeling$ST_floodplain, FALSE)
  expect_equal(modeling$ST_adult, FALSE)
})

test_that('ST instream Calaveras River works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::calaveras_river_instream$ST_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::calaveras_river_instream$ST_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::calaveras_river_instream$ST_spawn_wua))[1]

  fry_wua <- DSMhabitat::calaveras_river_instream$ST_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::calaveras_river_instream$ST_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::calaveras_river_instream$ST_spawn_wua[spawn_not_na_index]

  #TODO - there is no steelhead extent for calaveras river - is fall run ok? (Issue #205)
  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Calaveras River' & lifestage == 'rearing'
                                  & species == 'fr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Calaveras River' & lifestage == 'spawning'
                                   & species == 'fr')$feet

  fryx <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juvx <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawnx <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::calaveras_river_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::calaveras_river_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::calaveras_river_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Calaveras River', 'st', 'fry', fry_flow), fryx)
  expect_equal(
    set_instream_habitat('Calaveras River', 'st', 'juv', juv_flow), juvx)
  expect_equal(
    set_spawning_habitat('Calaveras River', 'st', spawn_flow), spawnx)
})

# Tests for species/habitat without modeling (FALSE modeling_exists) ----

test_that('FR instream Calaveras River works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::calaveras_river_instream$ST_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::calaveras_river_instream$ST_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::calaveras_river_instream$ST_spawn_wua))[1]

  fry_wua <- DSMhabitat::calaveras_river_instream$ST_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::calaveras_river_instream$ST_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::calaveras_river_instream$ST_spawn_wua[spawn_not_na_index]

  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Calaveras River' & lifestage == 'rearing'
                                  & species == 'fr')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Calaveras River' & lifestage == 'spawning'
                                   & species == 'fr')$feet

  fryx <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juvx <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawnx <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::calaveras_river_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::calaveras_river_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::calaveras_river_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Calaveras River', 'fr', 'fry', fry_flow), fryx)
  expect_equal(
    set_instream_habitat('Calaveras River', 'fr', 'juv', juv_flow), juvx)
  expect_equal(
    set_spawning_habitat('Calaveras River', 'fr', spawn_flow), spawnx)
})

test_that('No WR, SR, or LFR on Calaveras River', {
  expect_true(is.na(set_instream_habitat('Calaveras River', 'wr', 'fry', 200)))
  expect_true(is.na(set_instream_habitat('Calaveras River', 'sr', 'fry', 200)))
  expect_true(is.na(set_instream_habitat('Calaveras River', 'lfr', 'fry', 200)))
  expect_true(is.na(set_spawning_habitat('Calaveras River', 'wr', 100)))
  expect_true(is.na(set_spawning_habitat('Calaveras River', 'sr', 100)))
  expect_true(is.na(set_spawning_habitat('Calaveras River', 'lfr', 100)))
  expect_true(is.na(set_floodplain_habitat('Calaveras River', 'wr', 2000)))
  expect_true(is.na(set_floodplain_habitat('Calaveras River', 'sr', 2000)))
  expect_true(is.na(set_floodplain_habitat('Calaveras River', 'lfr', 2000)))
})