wsi_test_roi <- function() {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "tumour-1",
          "properties": {"name": "Tumour", "classification": {"name": "tumour"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [10, 0], [10, 10], [0, 10], [0, 0]]]
          }
        },
        {
          "type": "Feature",
          "id": "stroma-1",
          "properties": {"name": "Stroma", "classification": {"name": "stroma"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[20, 0], [30, 0], [30, 10], [20, 10], [20, 0]]]
          }
        }
      ]
    }',
    path
  )
  wsi_read_geojson(path)
}

test_that("measurement helpers report pixel and micron distances", {
  distance <- measure_distance(c(0, 0), c(3, 4), pixel_size = 0.5)

  expect_equal(distance$distance_px, 5)
  expect_equal(distance$distance_um, 2.5)

  nearest <- measure_nearest_cells(data.frame(x = c(0, 3, 100), y = c(0, 4, 100)))
  expect_equal(nearest$nearest_cell_id[1], 2)
  expect_equal(nearest$distance_px[1], 5)
})

test_that("ROI measurements and summaries use polygon area", {
  rois <- wsi_test_roi()
  cells <- data.frame(x = c(2, 5, 25, 50), y = c(2, 5, 5, 50))

  boundary <- measure_cells_to_roi(cells, rois, pixel_size = 1)
  expect_equal(nrow(boundary), 4)
  expect_true(boundary$inside[1])
  expect_equal(boundary$roi_id[1], "tumour-1")

  density <- measure_cell_density(cells, rois, pixel_size = 1)
  expect_equal(density$cell_count, c(2, 1))
  expect_equal(density$area_px2, c(100, 100))

  summary <- summarise_rois(rois, cells = cells, pixel_size = 1)
  expect_equal(summary$area_px2, c(100, 100))
  expect_equal(sum(summary$percent_area), 100)
  expect_true(all(c("class", "cell_count", "cells_per_mm2") %in% names(summary)))
})

test_that("affine registration transforms ROI coordinates", {
  rois <- wsi_test_roi()
  from <- data.frame(x = c(0, 10, 0), y = c(0, 0, 10))
  to <- data.frame(x = c(5, 15, 5), y = c(7, 7, 17))

  transform <- estimate_transform(from, to)
  expect_s3_class(transform, "wsi_affine_transform")
  expect_equal(round(transform$matrix, 8), matrix(c(1, 0, 5, 0, 1, 7, 0, 0, 1), nrow = 3, byrow = TRUE))

  shifted <- transform_rois(rois, transform)
  expect_equal(shifted$xmin[1], 5)
  expect_equal(shifted$ymin[1], 7)
  expect_equal(shifted$xmax[1], 15)
  expect_equal(shifted$ymax[1], 17)
})
