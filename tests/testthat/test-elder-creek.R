library(DSMhabitat)
context('Elder Creek Habitat')

test_that("modeling of species coverage hasn't changed - Elder", {
  modeling <- subset(DSMhabitat::modeling_exist, Watershed == 'Elder Creek')

  expect_equal(modeling$FR_spawn, FALSE)
  expect_equal(modeling$FR_fry, FALSE)
  expect_equal(modeling$FR_juv, FALSE)
  expect_equal(modeling$FR_floodplain, TRUE)

  expect_equal(is.na(modeling$SR_spawn), TRUE)
  expect_equal(is.na(modeling$SR_fry), TRUE)
  expect_equal(is.na(modeling$SR_juv), TRUE)
  expect_equal(is.na(modeling$SR_floodplain), TRUE)

  expect_equal(modeling$ST_spawn, FALSE)
  expect_equal(modeling$ST_fry, FALSE)
  expect_equal(modeling$ST_juv, FALSE)
  expect_equal(modeling$ST_floodplain, TRUE)
  expect_equal(modeling$ST_adult, FALSE)
})

test_that('FR floodplain Elder Creek works', {
  first_flood_index <-  which(DSMhabitat::elder_creek_floodplain$FR_floodplain_acres > 0)[1]

  flow <- DSMhabitat::elder_creek_floodplain$flow_cfs[first_flood_index]
  floodplain <- subset(DSMhabitat::elder_creek_floodplain,flow_cfs == flow)$FR_floodplain_acres

  expect_equal(
    square_meters_to_acres(set_floodplain_habitat('Elder Creek', 'fr', flow)),
    floodplain,
    tolerance = .01)
})

test_that('No WR, SR, or LFR on Elder Creek', {
  expect_true(is.na(set_instream_habitat('Elder Creek', 'wr', 200)))
  expect_true(is.na(set_instream_habitat('Elder Creek', 'sr', 200)))
  expect_true(is.na(set_instream_habitat('Elder Creek', 'lfr', 200)))
  expect_true(is.na(set_spawning_habitat('Elder Creek', 'wr', 100)))
  expect_true(is.na(set_spawning_habitat('Elder Creek', 'sr', 100)))
  expect_true(is.na(set_spawning_habitat('Elder Creek', 'lfr', 100)))
  expect_true(is.na(set_floodplain_habitat('Elder Creek', 'wr', 2000)))
  expect_true(is.na(set_floodplain_habitat('Elder Creek', 'sr', 2000)))
  expect_true(is.na(set_floodplain_habitat('Elder Creek', 'lfr', 2000)))
})