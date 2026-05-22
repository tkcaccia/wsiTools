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

test_that("OME-TIFF libvips target includes pyramidal export options", {
  target <- wsi_vips_tiff_target(
    output = "out.ome.tiff",
    tile_size = 256,
    compression = "jpeg",
    pyramid = TRUE,
    bigtiff = TRUE,
    ome = TRUE,
    subifd = TRUE,
    properties = TRUE,
    depth = "onetile",
    region_shrink = "mean",
    predictor = NULL,
    quality = 90
  )
  expect_true(grepl("out.ome.tiff[", target, fixed = TRUE))
  expect_true(grepl("tile-width=256", target, fixed = TRUE))
  expect_true(grepl("tile-height=256", target, fixed = TRUE))
  expect_true(grepl("pyramid", target, fixed = TRUE))
  expect_true(grepl("bigtiff", target, fixed = TRUE))
  expect_true(grepl("subifd", target, fixed = TRUE))
  expect_true(grepl("properties", target, fixed = TRUE))
  expect_true(grepl("compression=jpeg", target, fixed = TRUE))
  expect_true(grepl("depth=onetile", target, fixed = TRUE))
  expect_true(grepl("region-shrink=mean", target, fixed = TRUE))
  expect_true(grepl("Q=90", target, fixed = TRUE))
})

test_that("OME-TIFF export wrapper is available", {
  expect_true(is.function(wsi_export_ome_tiff))
})
