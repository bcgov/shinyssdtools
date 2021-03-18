# tests need to be run manually because not a pkg anymore
# run script from top to bottom to run tests

library(testthat)
library(ssdtools)
source("R/functions.R")

test_that("ssd_hp and ssd_hc output table includes all dists", {
  x <- boron_data

  ### complete output table issue #33
  y <- ssd_fit_dists(x)

  pred_hc <- ssd_hc_ave(y, 5, 500L)
  expect_identical(pred_hc$dist, c("average", "llogis", "gamma", "lnorm"))
  expect_true(all(pred_hc$percent == 5))

  pred_hp <- ssd_hp_ave(y, 5, 500L)
  expect_identical(pred_hp$dist, c("average", "llogis", "gamma", "lnorm"))
  expect_true(all(pred_hp$conc == 5))
})
