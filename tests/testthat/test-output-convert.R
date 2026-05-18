test_that("output paths are not silently overwritten", {
  input <- tempfile(fileext = ".tif")
  output <- tempfile(fileext = ".tif")
  file.create(input)
  file.create(output)
  expect_error(
    wsi_convert(input, output, format = "tiff", overwrite = FALSE),
    "already exists"
  )
})

test_that("conversion reports missing libvips clearly", {
  skip_if(wsi_has_vips(), "libvips is installed")
  input <- tempfile(fileext = ".tif")
  output <- tempfile(fileext = ".ome.tiff")
  file.create(input)
  expect_error(
    wsi_convert(input, output, format = "ome-tiff"),
    "libvips backend is not installed"
  )
})
