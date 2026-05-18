test_that("OME-Zarr metadata and open functions read pyramid metadata only", {
  root <- tempfile("sample.ome.zarr")
  dir.create(root)
  dir.create(file.path(root, "0"))
  dir.create(file.path(root, "1"))

  jsonlite::write_json(
    list(
      multiscales = list(list(
        version = "0.4",
        axes = list(
          list(name = "c", type = "channel"),
          list(name = "y", type = "space"),
          list(name = "x", type = "space")
        ),
        datasets = list(
          list(path = "0", coordinateTransformations = list(list(type = "scale", scale = c(1, 1, 1)))),
          list(path = "1", coordinateTransformations = list(list(type = "scale", scale = c(1, 4, 4))))
        )
      ))
    ),
    file.path(root, ".zattrs"),
    auto_unbox = TRUE
  )
  jsonlite::write_json(list(shape = c(3, 100, 200), chunks = c(1, 64, 64)), file.path(root, "0", ".zarray"), auto_unbox = TRUE)
  jsonlite::write_json(list(shape = c(3, 25, 50), chunks = c(1, 64, 64)), file.path(root, "1", ".zarray"), auto_unbox = TRUE)

  meta <- omezarr_metadata(root)
  expect_equal(meta$ngff_version, "0.4")
  expect_equal(nrow(meta$levels), 2)
  expect_equal(meta$levels$width, c(200, 50))
  expect_equal(meta$levels$height, c(100, 25))
  expect_equal(meta$levels$downsample, c(1, 4))

  slide <- open_omezarr(root)
  expect_s3_class(slide, "wsi_slide")
  expect_equal(slide$backend, "omezarr")
  expect_equal(slide$dimensions, c(width = 200, height = 100))

  auto <- wsi_open(root)
  expect_equal(auto$backend, "omezarr")
})
