test_that("ssd_hp and ssd_hc output table includes all dists", {
  ### complete output table issue #33
  x <- ssdtools::ssd_fit_dists(ssddata::ccme_boron)

  pred_hc <- ssd_hc_ave(x, 5, nboot = 10L)
  expect_identical(pred_hc$dist, c("average", ssdtools::ssd_dists_bcanz()))
  expect_true(all(pred_hc$proportion == 0.05))
  
  pred_hp <- ssd_hp_ave(x, 5, nboot = 10L)
  expect_identical(pred_hp$dist, c("average", ssdtools::ssd_dists_bcanz()))
  expect_true(all(pred_hp$conc == 5))
})
