test_that("segmentation importer reads GeoJSON, centroids, and mask paths", {
  geojson <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "seg-1",
          "properties": {"name": "Segmentation", "classification": {"name": "cells"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [4, 0], [4, 4], [0, 4], [0, 0]]]
          }
        }
      ]
    }',
    geojson
  )
  rois <- import_segmentation(geojson)
  expect_s3_class(rois, "wsi_segmentation")
  expect_s3_class(rois, "wsi_roi")

  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(cell_id = 1:2, centroid_x = c(1, 2), centroid_y = c(3, 4)), csv, row.names = FALSE)
  centroids <- import_segmentation(csv)
  expect_s3_class(centroids, "wsi_segmentation_centroids")
  expect_equal(centroids$x, c(1, 2))
  expect_equal(centroids$y, c(3, 4))

  mask <- tempfile(fileext = ".png")
  writeBin(charToRaw("placeholder mask"), mask)
  mask_seg <- import_segmentation(mask)
  expect_s3_class(mask_seg, "wsi_segmentation_mask")
  expect_equal(mask_seg$type, "mask")
})

test_that("segmentation viewer bridges ROI overlays through the existing viewer", {
  geojson <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "seg-1",
          "properties": {"name": "Segmentation", "classification": {"name": "tumour"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[10, 10], [40, 10], [40, 40], [10, 40], [10, 10]]]
          }
        }
      ]
    }',
    geojson
  )

  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 100, levels = c(1))
  output <- tempfile(fileext = ".html")
  result <- viewer_add_segmentation(slide, import_segmentation(geojson), output = output, open = FALSE)

  expect_identical(result, output)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "seg-1", fixed = TRUE)
  expect_match(html, "tumour", fixed = TRUE)
})

test_that("segmentation viewer draws centroid tables as cell overlays", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(cell_id = c("cell-a", "cell-b"), centroid_x = c(20, 50), centroid_y = c(30, 60)),
    csv,
    row.names = FALSE
  )

  segmentation <- import_segmentation(csv)
  rois <- segmentation_to_rois(segmentation, radius = 5)
  expect_s3_class(rois, "wsi_roi")
  expect_equal(nrow(rois), 2)
  expect_equal(rois$roi_id, c("cell-a", "cell-b"))
  expect_equal(rois$xmin, c(15, 45))
  expect_equal(rois$ymax, c(35, 65))

  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 100, levels = c(1))
  output <- tempfile(fileext = ".html")
  result <- viewer_add_segmentation(slide, segmentation, output = output, open = FALSE, cell_radius = 5)

  expect_identical(result, output)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "cell-a", fixed = TRUE)
  expect_match(html, "cell-b", fixed = TRUE)
  expect_match(html, "wsiTools viewer", fixed = TRUE)
})

test_that("ROI translation maps crop-local segmentation coordinates to slide coordinates", {
  geojson <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "cell-1",
          "properties": {"name": "cell"},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [5, 0], [5, 5], [0, 5], [0, 0]]]
          }
        }
      ]
    }',
    geojson
  )

  rois <- read_geojson(geojson)
  shifted <- translate_rois(rois, dx = 100, dy = 200)

  expect_equal(shifted$xmin, 100)
  expect_equal(shifted$ymin, 200)
  expect_equal(shifted$xmax, 105)
  expect_equal(shifted$ymax, 205)
  expect_equal(unlist(shifted$coordinates[[1]][[1]][[1]], use.names = FALSE), c(100, 200))
})

test_that("centroid segmentation coordinates can be shifted to slide coordinates", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(cell_id = "cell-1", centroid_x = 2, centroid_y = 3), csv, row.names = FALSE)
  centroids <- import_segmentation(csv)
  shifted <- wsiTools:::wsi_offset_centroids(centroids, dx = 100, dy = 200)

  expect_s3_class(shifted, "wsi_segmentation_centroids")
  expect_equal(shifted$x, 102)
  expect_equal(shifted$y, 203)
})

test_that("CellPhenotyper-style centroid tables can become viewer cell overlays", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(cell_id = "cell-1", centroid_x = 2, centroid_y = 3), csv, row.names = FALSE)

  centroids <- import_segmentation(csv)
  rois <- segmentation_to_rois(centroids, radius = 5)

  expect_s3_class(centroids, "wsi_segmentation_centroids")
  expect_s3_class(rois, "wsi_roi")
  expect_equal(rois$roi_id, "cell-1")
  expect_equal(rois$class, "cell")
})

test_that("cell segmentation engine presets are optional runtime capabilities", {
  engines <- wsi_cell_segmentation_engines()

  expect_s3_class(engines, "data.frame")
  expect_true(all(c("engine", "label", "backend", "installed", "default_model", "notes") %in% names(engines)))
  expect_true(all(c("stardist_he", "stardist_ihc", "mesmer_dapi") %in% engines$engine))
  expect_type(wsi_has_stardist(), "logical")
  expect_type(wsi_has_mesmer(), "logical")
})

test_that("cell segmentation command plans support tiled low-memory placeholders", {
  input <- tempfile(fileext = ".png")
  output <- tempfile(fileext = ".geojson")
  writeBin(charToRaw("mock crop"), input)

  plan <- wsiTools:::stardist_segment_image(
    input = input,
    output = output,
    command = "stardist-predict2d",
    args = c("--input", "{input}", "--output", "{output}", "--model", "{model}", "--tiles", "{tiles_y}", "{tiles_x}", "--min-area", "{min_area}"),
    model = "2D_versatile_he",
    template_values = list(tiles_x = 32, tiles_y = 32, min_area = 120),
    run = FALSE
  )

  expect_s3_class(plan, "wsi_cell_segmentation_result")
  expect_equal(plan$status, "planned")
  expect_true("--tiles" %in% plan$args)
  expect_true("32" %in% plan$args)
  expect_true("120" %in% plan$args)

  mesmer_output <- tempfile(fileext = ".tif")
  mesmer_plan <- wsiTools:::wsi_cell_segment_image(
    input = input,
    output = mesmer_output,
    engine = "mesmer_dapi",
    command = "mesmer",
    args = c("--nuclear-channel", "{nuclear_channel}", "{input}", "{output}"),
    template_values = list(nuclear_channel = "DAPI"),
    run = FALSE
  )

  expect_s3_class(mesmer_plan, "wsi_cell_segmentation_result")
  expect_equal(mesmer_plan$engine, "mesmer_dapi")
  expect_true("DAPI" %in% mesmer_plan$args)
})
