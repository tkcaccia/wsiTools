test_that("wsi_open errors on missing files", {
  expect_error(wsi_open("definitely-not-a-slide.svs"), "does not exist")
})

test_that("mock slides support metadata helpers", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  expect_s3_class(slide, "wsi_slide")
  expect_equal(wsi_levels(slide)$level, c(0L, 1L))
  expect_equal(wsi_info(slide)$dimensions[["width"]], 1000)
  expect_equal(wsi_mpp(slide), c(x = 0.25, y = 0.25))
  expect_equal(wsi_objective_power(slide), 40)
})
