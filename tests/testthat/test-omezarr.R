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

test_that("OME-Zarr metadata reads v3 axes, chunks, transforms, and multiple multiscales", {
  root <- tempfile("sample-v3.ome.zarr")
  dir.create(root)
  dir.create(file.path(root, "0"))
  dir.create(file.path(root, "1"))

  axes <- list(
    list(name = "t", type = "time", unit = "second"),
    list(name = "c", type = "channel"),
    list(name = "y", type = "space", unit = "micrometer"),
    list(name = "x", type = "space", unit = "micrometer")
  )
  jsonlite::write_json(
    list(
      zarr_format = 3,
      node_type = "group",
      attributes = list(
        ome = list(
          version = "0.5",
          multiscales = list(
            list(
              name = "image",
              axes = axes,
              datasets = list(
                list(
                  path = "0",
                  coordinateTransformations = list(
                    list(type = "scale", scale = c(1, 1, 0.5, 0.5)),
                    list(type = "translation", translation = c(0, 0, 10, 20))
                  )
                ),
                list(
                  path = "1",
                  coordinateTransformations = list(
                    list(type = "scale", scale = c(1, 1, 2, 2))
                  )
                )
              )
            ),
            list(
              name = "label-image",
              axes = axes,
              datasets = list(
                list(path = "0", coordinateTransformations = list(list(type = "scale", scale = c(1, 1, 1, 1))))
              )
            )
          )
        )
      )
    ),
    file.path(root, "zarr.json"),
    auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(
      zarr_format = 3,
      node_type = "array",
      shape = c(1, 3, 100, 200),
      data_type = "uint16",
      chunk_grid = list(name = "regular", configuration = list(chunk_shape = c(1, 1, 64, 64))),
      dimension_names = c("t", "c", "y", "x")
    ),
    file.path(root, "0", "zarr.json"),
    auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(
      zarr_format = 3,
      node_type = "array",
      shape = c(1, 3, 25, 50),
      data_type = "uint16",
      chunk_grid = list(name = "regular", configuration = list(chunk_shape = c(1, 1, 32, 32))),
      dimension_names = c("t", "c", "y", "x")
    ),
    file.path(root, "1", "zarr.json"),
    auto_unbox = TRUE
  )

  meta <- omezarr_metadata(root)
  expect_equal(meta$ngff_version, "0.5")
  expect_equal(meta$image_name, "image")
  expect_equal(meta$zarr_format, 3)
  expect_equal(meta$axis_table$name, c("t", "c", "y", "x"))
  expect_equal(meta$levels$width, c(200, 50))
  expect_equal(meta$levels$height, c(100, 25))
  expect_equal(meta$levels$downsample, c(1, 4))
  expect_equal(meta$levels$scale_x, c(0.5, 2))
  expect_equal(meta$levels$scale_y, c(0.5, 2))
  expect_equal(meta$levels$translation_x[[1L]], 20)
  expect_equal(meta$levels$translation_y[[1L]], 10)
  expect_equal(meta$levels$unit_x, c("micrometer", "micrometer"))
  expect_equal(meta$levels$chunks[[1L]], c(1, 1, 64, 64))
  expect_equal(meta$levels$dimension_names[[1L]], c("t", "c", "y", "x"))
  expect_equal(meta$levels$dtype, c("uint16", "uint16"))

  second <- omezarr_metadata(root, multiscale = 2)
  expect_equal(second$image_name, "label-image")
  expect_equal(nrow(second$levels), 1)
})
