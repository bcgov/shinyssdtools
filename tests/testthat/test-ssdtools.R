test_that("ssd_hp and ssd_hc output table includes all dists", {
  ### complete output table issue #33
  y <- ssdtools::ssd_fit_dists(ssdtools::boron_data)

  pred_hc <- ssd_hc_ave(y, 5, 10L)
  expect_identical(pred_hc$dist, c("average", "llogis", "gamma", "lnorm"))
  expect_true(all(pred_hc$percent == 5))

  pred_hp <- ssd_hp_ave(y, 5, 10L)
  expect_identical(pred_hp$dist, c("average", "llogis", "gamma", "lnorm"))
  expect_true(all(pred_hp$conc == 5))
})
