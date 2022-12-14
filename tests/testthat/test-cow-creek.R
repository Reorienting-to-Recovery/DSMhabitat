library(DSMhabitat)
context('Cow Creek Habitat')

test_that("modeling of species coverage hasn't changed - Cow", {
  modeling <- subset(DSMhabitat::modeling_exist, Watershed == 'Cow Creek')

  expect_equal(modeling$FR_spawn, FALSE)
  expect_equal(modeling$FR_fry, TRUE)
  expect_equal(modeling$FR_juv, TRUE)
  expect_equal(modeling$FR_floodplain, FALSE)

  expect_equal(is.na(modeling$SR_spawn), TRUE)
  expect_equal(is.na(modeling$SR_fry), TRUE)
  expect_equal(is.na(modeling$SR_juv), TRUE)
  expect_equal(is.na(modeling$SR_floodplain), TRUE)

  expect_equal(modeling$ST_spawn, FALSE)
  expect_equal(modeling$ST_fry, FALSE)
  expect_equal(modeling$ST_juv, FALSE)
  expect_equal(modeling$ST_floodplain, FALSE)
  expect_equal(modeling$ST_adult, FALSE)
})

test_that('FR rearing Cow Creek works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::cow_creek_instream$FR_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::cow_creek_instream$FR_juv_wua))[1]


  fry_wua <- DSMhabitat::cow_creek_instream$FR_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::cow_creek_instream$FR_juv_wua[juv_not_na_index]


  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Cow Creek' & lifestage == 'rearing'
                                  & species == 'fr')$feet

  fryx <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juvx <- (((rearing_stream_length/1000) * juv_wua)/10.7639)

  fry_flow <- DSMhabitat::cow_creek_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::cow_creek_instream$flow_cfs[juv_not_na_index]

  expect_equal(
    set_instream_habitat('Cow Creek', 'fr', 'fry', fry_flow), fryx)
  expect_equal(
    set_instream_habitat('Cow Creek', 'fr', 'juv', juv_flow), juvx)
})

# Tests for species/habitat without modeling (FALSE modeling_exists) ----


test_that('ST instream Cow Creek works', {

  fry_not_na_index <- which(!is.na(DSMhabitat::cow_creek_instream$FR_fry_wua))[1]
  juv_not_na_index <- which(!is.na(DSMhabitat::cow_creek_instream$FR_juv_wua))[1]
  spawn_not_na_index <- which(!is.na(DSMhabitat::upper_mid_sac_region_instream$FR_spawn_wua))[1]

  fry_wua <- DSMhabitat::cow_creek_instream$FR_fry_wua[fry_not_na_index]
  juv_wua <- DSMhabitat::cow_creek_instream$FR_juv_wua[juv_not_na_index]
  spawn_wua <- DSMhabitat::upper_mid_sac_region_instream$FR_spawn_wua[spawn_not_na_index]

  rearing_stream_length <- subset(DSMhabitat::watershed_lengths,
                                  watershed == 'Cow Creek' & lifestage == 'rearing'
                                  & species == 'st')$feet
  spawning_stream_length <- subset(DSMhabitat::watershed_lengths,
                                   watershed == 'Cow Creek' & lifestage == 'spawning'
                                   & species == 'st')$feet

  fryx <- (((rearing_stream_length/1000) * fry_wua)/10.7639)
  juvx <- (((rearing_stream_length/1000) * juv_wua)/10.7639)
  spawnx <- (((spawning_stream_length/1000) * spawn_wua)/10.7639)

  fry_flow <- DSMhabitat::cow_creek_instream$flow_cfs[fry_not_na_index]
  juv_flow <- DSMhabitat::cow_creek_instream$flow_cfs[juv_not_na_index]
  spawn_flow <- DSMhabitat::upper_mid_sac_region_instream$flow_cfs[spawn_not_na_index]

  expect_equal(
    set_instream_habitat('Cow Creek', 'st', 'fry', fry_flow), fryx)
  expect_equal(
    set_instream_habitat('Cow Creek', 'st', 'juv', juv_flow), juvx)
  expect_equal(
    set_spawning_habitat('Cow Creek', 'st', spawn_flow), spawnx)
})

test_that('No WR, SR, or LFR on Cow Creek', {
  expect_true(is.na(set_instream_habitat('Cow Creek', 'wr', 'fry', 200)))
  expect_true(is.na(set_instream_habitat('Cow Creek', 'sr', 'fry', 200)))
  expect_true(is.na(set_instream_habitat('Cow Creek', 'lfr', 'fry', 200)))
  expect_true(is.na(set_spawning_habitat('Cow Creek', 'wr', 100)))
  expect_true(is.na(set_spawning_habitat('Cow Creek', 'sr', 100)))
  expect_true(is.na(set_spawning_habitat('Cow Creek', 'lfr', 100)))
  expect_true(is.na(set_floodplain_habitat('Cow Creek', 'wr', 2000)))
  expect_true(is.na(set_floodplain_habitat('Cow Creek', 'sr', 2000)))
  expect_true(is.na(set_floodplain_habitat('Cow Creek', 'lfr', 2000)))
})