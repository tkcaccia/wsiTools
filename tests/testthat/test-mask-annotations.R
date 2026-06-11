test_that("mask matrices are converted to ROI polygons", {
  mask <- matrix(0, nrow = 6, ncol = 8)
  mask[2:4, 3:5] <- 1

  rois <- wsi_mask_to_rois(
    mask,
    origin = c(x = 10, y = 20),
    scale = c(x = 2, y = 2),
    class_map = c("1" = "tumour")
  )

  expect_s3_class(rois, "wsi_roi")
  expect_equal(nrow(rois), 1)
  expect_equal(rois$class, "tumour")
  expect_equal(rois$xmin, 14)
  expect_equal(rois$ymin, 22)
  expect_equal(rois$xmax, 20)
  expect_equal(rois$ymax, 28)
  expect_equal(rois$geometry_type, "Polygon")
  expect_true(length(rois$coordinates[[1]][[1]]) >= 5)
})

test_that("labelled masks produce one ROI per connected label component", {
  mask <- matrix(0, nrow = 6, ncol = 8)
  mask[2:3, 2:3] <- 1
  mask[4:5, 6:7] <- 2
  mask[1, 8] <- 2

  rois <- wsi_mask_to_rois(
    mask,
    class_map = c("1" = "tumour", "2" = "stroma"),
    color_map = c("1" = "red", "2" = "blue")
  )

  expect_equal(nrow(rois), 3)
  expect_equal(sort(rois$class), c("stroma", "stroma", "tumour"))
  expect_true(all(rois$object_type == "annotation"))
  expect_true(all(grepl("^#[0-9A-F]{6}$", rois$color)))
})

test_that("thresholded masks can be written as GeoJSON annotations", {
  mask <- matrix(0, nrow = 4, ncol = 4)
  mask[2:3, 2:3] <- 0.9
  rois <- wsi_mask_to_rois(mask, threshold = 0.5, prefix = "threshold")

  output <- tempfile(fileext = ".geojson")
  write_geojson(rois, output)
  roundtrip <- read_geojson(output)

  expect_equal(nrow(roundtrip), 1)
  expect_equal(roundtrip$class, "mask")
  expect_equal(roundtrip$properties[[1]]$source, "mask")
})

test_that("mask image files can be imported as annotations when magick is available", {
  skip_if_not_installed("magick")

  raster <- as.raster(matrix(c(
    "#000000", "#000000", "#000000", "#000000",
    "#000000", "#FFFFFF", "#FFFFFF", "#000000",
    "#000000", "#FFFFFF", "#FFFFFF", "#000000",
    "#000000", "#000000", "#000000", "#000000"
  ), nrow = 4, byrow = TRUE))
  path <- tempfile(fileext = ".png")
  magick::image_write(magick::image_read(raster), path)

  rois <- read_mask_annotations(path, threshold = 0.5)

  expect_s3_class(rois, "wsi_roi")
  expect_equal(nrow(rois), 1)
  expect_equal(rois$xmin, 1)
  expect_equal(rois$ymin, 1)
  expect_equal(rois$xmax, 3)
  expect_equal(rois$ymax, 3)
})

test_that("segmentation importer can vectorise mask files on request", {
  skip_if_not_installed("magick")

  raster <- as.raster(matrix(c(
    "#000000", "#000000", "#000000",
    "#000000", "#FFFFFF", "#FFFFFF",
    "#000000", "#FFFFFF", "#FFFFFF"
  ), nrow = 3, byrow = TRUE))
  path <- tempfile(fileext = ".png")
  magick::image_write(magick::image_read(raster), path)

  rois <- import_segmentation(path, mask_as_rois = TRUE, threshold = 0.5)

  expect_s3_class(rois, "wsi_segmentation")
  expect_s3_class(rois, "wsi_roi")
  expect_equal(nrow(rois), 1)
})

test_that("ROI polygons can be rasterised back into masks", {
  seed <- matrix(0, nrow = 6, ncol = 8)
  seed[2:4, 3:5] <- 1

  rois <- wsi_mask_to_rois(
    seed,
    origin = c(x = 10, y = 20),
    scale = c(x = 2, y = 2),
    class_map = c("1" = "tumour")
  )
  mask <- rois_to_mask(
    rois,
    width = 8,
    height = 6,
    origin = c(x = 10, y = 20),
    scale = c(x = 2, y = 2),
    label_by = "class"
  )

  expect_s3_class(mask, "wsi_roi_mask")
  expect_equal(dim(mask), c(6, 8))
  expect_equal(unclass(mask)[2:4, 3:5], matrix(1, nrow = 3, ncol = 3))
  expect_equal(sum(unclass(mask) != 0), 9)
  expect_equal(attr(mask, "labels")$key, "tumour")
  expect_equal(attr(mask, "origin"), c(x = 10, y = 20))
})

test_that("ROI rasterisation respects polygon holes and overlap modes", {
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "donut",
        properties = list(name = "Donut", classification = list(name = "tumour")),
        geometry = list(
          type = "Polygon",
          coordinates = list(
            list(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0)),
            list(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2))
          )
        )
      ),
      list(
        type = "Feature",
        id = "small",
        properties = list(name = "Small", classification = list(name = "stroma")),
        geometry = list(
          type = "Polygon",
          coordinates = list(list(c(1, 1), c(4, 1), c(4, 4), c(1, 4), c(1, 1)))
        )
      )
    )
  ))

  mask <- rois_to_mask(rois, width = 5, height = 5, label_by = "roi_id")
  expect_equal(unclass(mask)[3, 3], 2)
  expect_equal(unclass(mask)[1, 1], 1)

  first <- rois_to_mask(rois, width = 5, height = 5, label_by = "roi_id", overlap = "first")
  expect_equal(unclass(first)[2, 2], 1)
  expect_equal(unclass(first)[3, 3], 2)

  expect_error(
    rois_to_mask(rois, width = 5, height = 5, overlap = "error"),
    "overlaps"
  )
})

test_that("ROI smoothing densifies boundaries while preserving labels", {
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "cell-1",
        properties = list(name = "Cell 1", classification = list(name = "epithelial")),
        geometry = list(
          type = "Polygon",
          coordinates = list(list(c(1, 1), c(9, 1), c(9, 9), c(1, 9), c(1, 1)))
        )
      )
    )
  ))

  smoothed <- wsiTools:::wsi_smooth_rois_for_mask(
    rois,
    smooth = TRUE,
    smooth_iterations = 1,
    smooth_preserve_area = TRUE
  )
  original_ring <- wsiTools:::wsi_ring_matrix(rois$coordinates[[1L]][[1L]])
  smoothed_ring <- wsiTools:::wsi_ring_matrix(smoothed$coordinates[[1L]][[1L]])

  expect_gt(nrow(smoothed_ring), nrow(original_ring))
  expect_equal(
    wsiTools:::wsi_ring_area(smoothed_ring),
    wsiTools:::wsi_ring_area(original_ring),
    tolerance = 1e-8
  )
  expect_equal(smoothed$class, rois$class)

  mask <- rois_to_mask(smoothed, width = 12, height = 12, label_by = "class")
  expect_equal(attr(mask, "labels")$key, "epithelial")
  expect_true(any(unclass(mask) == 1))
})

test_that("ROI masks can be exported without optional image dependencies", {
  seed <- matrix(0, nrow = 4, ncol = 4)
  seed[2:3, 2:3] <- 1
  rois <- mask_to_rois(seed)
  output <- tempfile(fileext = ".rds")

  mask <- rois_to_mask(rois, width = 4, height = 4, output = output)
  roundtrip <- readRDS(output)

  expect_true(file.exists(output))
  expect_s3_class(mask, "wsi_roi_mask")
  expect_equal(dim(roundtrip), dim(mask))
  expect_equal(as.vector(roundtrip), as.vector(mask))
  expect_equal(attr(roundtrip, "labels")$value, 1)
})

test_that("GeoJSON annotations can be exported directly as TIFF masks", {
  skip_if_not(wsi_has_vips() || requireNamespace("magick", quietly = TRUE))

  geojson <- tempfile(fileext = ".geojson")
  jsonlite::write_json(
    list(
      type = "FeatureCollection",
      features = list(
        list(
          type = "Feature",
          id = "tumour-1",
          properties = list(name = "Tumour 1", classification = list(name = "tumour")),
          geometry = list(
            type = "Polygon",
            coordinates = list(list(c(2, 2), c(8, 2), c(8, 8), c(2, 8), c(2, 2)))
          )
        )
      )
    ),
    geojson,
    auto_unbox = TRUE
  )
  output <- tempfile(fileext = ".tif")
  legend <- tempfile(fileext = ".csv")

  result <- wsi_geojson_to_mask_tiff(
    geojson,
    output,
    width = 12,
    height = 12,
    downsample = 2,
    label_by = "class",
    smooth = TRUE,
    smooth_iterations = 1,
    legend_output = legend,
    return_mask = TRUE
  )

  expect_s3_class(result, "wsi_geojson_mask_tiff")
  expect_true(file.exists(output))
  expect_true(file.exists(legend))
  expect_equal(result$mask_width, 6)
  expect_equal(result$mask_height, 6)
  expect_equal(result$labels$key, "tumour")
  expect_true(all(c("colour", "color") %in% names(result$labels)))
  expect_equal(dim(result$mask), c(6, 6))
  expect_true(any(unclass(result$mask) == 1))
})

test_that("GeoJSON masks can be written as coloured TIFF layers", {
  skip_if_not(wsi_has_vips())

  geojson <- tempfile(fileext = ".geojson")
  jsonlite::write_json(
    list(
      type = "FeatureCollection",
      features = list(
        list(
          type = "Feature",
          id = "tumour-1",
          properties = list(name = "Tumour 1", classification = list(name = "tumour"), color = "#ff0000"),
          geometry = list(
            type = "Polygon",
            coordinates = list(list(c(0, 0), c(512, 0), c(512, 256), c(0, 256), c(0, 0)))
          )
        ),
        list(
          type = "Feature",
          id = "stroma-1",
          properties = list(name = "Stroma 1", classification = list(name = "stroma"), color = "#00ff00"),
          geometry = list(
            type = "Polygon",
            coordinates = list(list(c(512, 0), c(1024, 0), c(1024, 256), c(512, 256), c(512, 0)))
          )
        )
      )
    ),
    geojson,
    auto_unbox = TRUE
  )
  output <- tempfile(fileext = ".ome.tif")

  result <- wsi_geojson_to_mask_tiff(
    geojson,
    output,
    width = 1024,
    height = 512,
    label_by = "class",
    format = "ome-tiff",
    colour = TRUE,
    overwrite = TRUE
  )

  expect_true(file.exists(output))
  expect_equal(sort(result$labels$key), c("stroma", "tumour"))
  expect_true(all(grepl("^#[0-9A-F]{6}$", result$labels$colour)))
  expect_match(paste(system2("vipsheader", output, stdout = TRUE), collapse = " "), "3 bands")
})
