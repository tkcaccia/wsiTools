test_that("region validation catches invalid coordinates", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  expect_error(
    wsi_read_region(slide, x = 900, y = 700, width = 200, height = 200, level = 0),
    "outside slide bounds"
  )
  expect_error(
    wsi_read_region(slide, x = -1, y = 0, width = 10, height = 10, level = 0),
    "greater than or equal to zero"
  )
})

test_that("mock region reads return arrays and rasters", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  patch <- wsi_read_region(slide, x = 10, y = 20, width = 64, height = 32, level = 0)
  expect_equal(dim(patch), c(32, 64, 4))

  raster <- wsi_read_region(slide, x = 10, y = 20, width = 64, height = 32, level = 0, format = "raster")
  expect_s3_class(raster, "raster")
})
