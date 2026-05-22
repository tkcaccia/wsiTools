test_that("ImageMagick fallback opens ordinary images when available", {
  skip_if_not(wsi_has_imagemagick())
  skip_if_not_installed("magick")

  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 80, height = 60)
  on.exit(if (!identical(names(grDevices::dev.cur()), "null device")) grDevices::dev.off(), add = TRUE)
  graphics::par(mar = rep(0, 4))
  graphics::plot.new()
  graphics::plot.window(c(0, 1), c(0, 1))
  graphics::rect(0, 0, 1, 1, col = "white", border = NA)
  graphics::rect(0.2, 0.2, 0.8, 0.8, col = "red", border = NA)
  grDevices::dev.off()

  slide <- wsi_open(path, backend = "imagemagick")
  expect_s3_class(slide, "wsi_slide")
  expect_equal(slide$backend, "imagemagick")
  expect_equal(unname(slide$dimensions), c(80, 60))

  thumb <- wsi_thumbnail(slide, width = 40, format = "array")
  expect_equal(dim(thumb)[[2L]], 40)

  region <- wsi_read_region(slide, x = 0, y = 0, width = 20, height = 20, format = "array")
  expect_equal(dim(region)[1:2], c(20, 20))
})
