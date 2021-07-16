library(DSMhabitat)
context('Deer Creek Habitat')

test_that("modeling of species coverage hasn't changed - Deer", {
  modeling <- subset(DSMhabitat::modeling_exist, Watershed == 'Deer Creek')

  expect_equal(modeling$FR_spawn, FALSE)
  expect_equal(modeling$FR_fry, FALSE)
  expect_equal(modeling$FR_juv, FALSE)
  expect_equal(modeling$FR_floodplain, TRUE)

  expect_equal(modeling$SR_spawn, FALSE)
  expect_equal(modeling$SR_fry, FALSE)
  expect_equal(modeling$SR_juv, FALSE)
  expect_equal(modeling$SR_floodplain, TRUE)

  expect_equal(modeling$ST_spawn, FALSE)
  expect_equal(modeling$ST_fry, FALSE)
  expect_equal(modeling$ST_juv, FALSE)
  expect_equal(modeling$ST_floodplain, TRUE)
  expect_equal(modeling$ST_adult, FALSE)
})

# rearing tests ---------------------
test_that('set rearing at Deer Creek works', {
  
  flow_test_val <- 7000
  
  fr_juv_result <- deer_creek_instream[deer_creek_instream$flow_cfs == flow_test_val,]$FR_juv_sqm
  lfr_juv_result <- NA
  sr_juv_result <- deer_creek_instream[deer_creek_instream$flow_cfs == flow_test_val,]$SR_juv_sqm
  st_juv_result <- deer_creek_instream[deer_creek_instream$flow_cfs == flow_test_val,]$ST_juv_sqm
  
  expect_equal(set_instream_habitat('Deer Creek', 'fr', 'juv', flow_test_val), fr_juv_result)
  expect_equal(set_instream_habitat('Deer Creek', 'lfr', 'juv', flow_test_val), lfr_juv_result)
  expect_equal(set_instream_habitat('Deer Creek', 'sr', 'juv', flow_test_val), sr_juv_result)
  expect_equal(set_instream_habitat('Deer Creek', 'st', 'juv', flow_test_val), st_juv_result)
})

# spawning tests -----------------

test_that("set spawning works at deer creek", {
  flow_test_val <- 7000
  
  fr_spawn_result <- deer_creek_instream[deer_creek_instream$flow_cfs == flow_test_val,]$FR_spawn_sqm
  lfr_spawn_result <- NA
  
  expect_equal(set_spawning_habitat('Deer Creek', 'fr', flow_test_val), fr_spawn_result)
  expect_equal(set_spawning_habitat('Deer Creek', 'lfr', flow_test_val), lfr_spawn_result)
})


# floodplain tests --------------------

test_that("set floodplain works at deer creek", {
  flow_test_val <- 7000
  
  fr_fp_result <- deer_creek_floodplain[deer_creek_floodplain$flow_cfs == flow_test_val,]$FR_floodplain_acres
  lfr_fp_result <- NA
  sr_fp_result <- deer_creek_floodplain[deer_creek_floodplain$flow_cfs == flow_test_val,]$SR_floodplain_acres
  st_fp_result <- deer_creek_floodplain[deer_creek_floodplain$flow_cfs == flow_test_val,]$ST_floodplain_acres
  
  expect_equal(set_floodplain_habitat('Deer Creek', 'fr', flow_test_val), acres_to_square_meters(fr_fp_result))
  expect_equal(set_floodplain_habitat('Deer Creek', 'lfr', flow_test_val), lfr_fp_result)
  expect_equal(set_floodplain_habitat('Deer Creek', 'sr', flow_test_val), acres_to_square_meters(sr_fp_result))
  expect_equal(set_floodplain_habitat('Deer Creek', 'st', flow_test_val), acres_to_square_meters(st_fp_result))
})

test_that('No WR or LFR on Deer Creek', {
  expect_true(is.na(set_instream_habitat('Deer Creek', 'wr', 'fry', 200)))
  expect_true(is.na(set_instream_habitat('Deer Creek', 'lfr', 'fry', 200)))
  expect_true(is.na(set_spawning_habitat('Deer Creek', 'wr', 100)))
  expect_true(is.na(set_spawning_habitat('Deer Creek', 'lfr', 100)))
  expect_true(is.na(set_floodplain_habitat('Deer Creek', 'wr', 2000)))
  expect_true(is.na(set_floodplain_habitat('Deer Creek', 'lfr', 2000)))
})


