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

test_that("measurement reports include cell, class, boundary, stain, and CSV tables", {
  rois <- wsi_test_roi()
  cells <- data.frame(x = c(2, 5, 25, 50), y = c(2, 5, 5, 50))
  image <- array(0.8, dim = c(12, 32, 3))
  image[, , 1] <- 0.65
  image[, , 2] <- 0.55
  image[, , 3] <- 0.45
  stains <- wsi_deconvolve_ihc(image)

  stain_summary <- measure_stain_intensity(stains, rois = rois, positive_threshold = 0)
  expect_equal(nrow(stain_summary), 4)
  expect_true(all(c("hematoxylin", "hrp_dab") %in% stain_summary$channel_id))
  expect_true(all(stain_summary$n_pixels == 100))
  expect_true(all(is.finite(stain_summary$mean_intensity)))

  ihc <- measure_ihc_intensity(stains, rois = rois, dab_threshold = 0, pixel_size = 1)
  expect_equal(nrow(ihc), 2)
  expect_true(all(c(
    "ihc_dab_mean",
    "ihc_dab_positive_area_px2",
    "ihc_hematoxylin_density",
    "ihc_dab_h_ratio"
  ) %in% names(ihc)))
  expect_true(all(ihc$ihc_dab_positive_area_px2 == 100))
  expect_true(all(is.finite(ihc$ihc_dab_h_ratio)))

  ihc_class <- summarise_ihc_intensity(ihc)
  expect_equal(nrow(ihc_class), 2)
  expect_true(all(c("class", "ihc_dab_mean", "ihc_dab_positive_area_px2") %in% names(ihc_class)))

  output_dir <- tempfile("measurement-report")
  report <- measurement_report(
    rois,
    cells = cells,
    stains = stains,
    pixel_size = 1,
    output_dir = output_dir,
    prefix = "sample",
    overwrite = TRUE
  )
  expect_s3_class(report, "wsi_measurement_report")
  expect_true(all(c("roi_summary", "class_summary", "nearest_cells", "cell_boundary", "stain_summary") %in% names(report)))
  expect_equal(nrow(report$roi_summary), 2)
  expect_equal(nrow(report$nearest_cells), 4)
  expect_equal(nrow(report$cell_boundary), 4)
  expect_equal(nrow(report$stain_summary), 4)
  expect_equal(nrow(report$ihc_summary), 2)
  expect_equal(nrow(report$ihc_class_summary), 2)
  expect_true(all(file.exists(report$files)))
  expect_true(file.exists(file.path(output_dir, "sample_class_summary.csv")))
  expect_true(file.exists(file.path(output_dir, "sample_ihc_summary.csv")))

  round_trip <- utils::read.csv(file.path(output_dir, "sample_class_summary.csv"))
  expect_true(all(c("class", "area_px2", "cell_count", "cells_per_mm2") %in% names(round_trip)))
})

test_that("IHC ROI intensity reports expected DAB and hematoxylin values", {
  rois <- wsi_test_roi()
  channels <- structure(
    list(
      hematoxylin = matrix(0.5, nrow = 12, ncol = 32),
      hrp_dab = matrix(0.05, nrow = 12, ncol = 32),
      channel_metadata = list(
        list(id = "hematoxylin", name = "Hematoxylin"),
        list(id = "hrp_dab", name = "HRP/DAB")
      )
    ),
    class = "wsi_ihc_channels"
  )
  channels$hematoxylin[1:10, 1:10] <- 0.25
  channels$hrp_dab[1:10, 1:10] <- 0.50
  channels$hematoxylin[1:10, 21:30] <- 0.50
  channels$hrp_dab[1:10, 21:30] <- 0.10

  ihc <- measure_ihc_intensity(channels, rois = rois, dab_threshold = 0.3, pixel_size = 2)

  expect_equal(ihc$ihc_dab_mean, c(0.5, 0.1))
  expect_equal(ihc$ihc_hematoxylin_density, c(0.25, 0.5))
  expect_equal(ihc$ihc_dab_positive_area_px2, c(100, 0))
  expect_equal(ihc$ihc_dab_positive_area_um2, c(400, 0))
  expect_equal(ihc$ihc_dab_h_ratio, c(2, 0.2))

  both <- measure_ihc_intensity(channels, rois = rois, dab_threshold = 0.3, by = "both")
  expect_s3_class(both, "wsi_ihc_intensity_report")
  expect_equal(both$class_summary$ihc_dab_positive_pixels, c(100L, 0L))
})

test_that("multipolygon ROI measurements sum separate polygon areas", {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "multi-1",
          "properties": {"name": "Multi", "classification": {"name": "tumour"}},
          "geometry": {
            "type": "MultiPolygon",
            "coordinates": [
              [[[0, 0], [10, 0], [10, 10], [0, 10], [0, 0]]],
              [[[20, 0], [30, 0], [30, 10], [20, 10], [20, 0]]]
            ]
          }
        }
      ]
    }',
    path
  )
  rois <- wsi_read_geojson(path)
  density <- measure_cell_density(data.frame(x = c(5, 25, 15), y = c(5, 5, 5)), rois)

  expect_equal(density$area_px2, 200)
  expect_equal(density$cell_count, 2)
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

test_that("orientation transform mirrors ROI coordinates in slide space", {
  rois <- wsi_test_roi()

  transform <- wsi_orientation_transform(width = 100, height = 80, flip = "both")
  expect_s3_class(transform, "wsi_affine_transform")
  expect_equal(transform$matrix, matrix(c(-1, 0, 100, 0, -1, 80, 0, 0, 1), nrow = 3, byrow = TRUE))

  flipped <- transform_rois(rois, transform)
  expect_equal(flipped$xmin[1], 90)
  expect_equal(flipped$ymin[1], 70)
  expect_equal(flipped$xmax[1], 100)
  expect_equal(flipped$ymax[1], 80)
})

test_that("tissue translation estimator recovers global centroid offsets", {
  tissue <- matrix(FALSE, nrow = 100, ncol = 100)
  tissue[35:70, 45:80] <- TRUE
  grid <- expand.grid(
    x = seq(48, 76, by = 7),
    y = seq(38, 66, by = 7)
  )
  points <- data.frame(
    x = grid$x * 10 - 100,
    y = grid$y * 10 + 100
  )

  fit <- wsi_estimate_tissue_translation(
    thumbnail = tissue,
    points = points,
    slide_width = 1000,
    slide_height = 1000,
    sample_n = nrow(points),
    max_shift = 25,
    coarse_step = 5,
    refine_radius = 6,
    refine_step = 1,
    seed = NULL
  )

  expect_s3_class(fit, "wsi_annotation_translation")
  expect_equal(round(fit$dx), 100)
  expect_equal(round(fit$dy), -100)
  expect_s3_class(fit$transform, "wsi_affine_transform")
  expect_equal(round(fit$transform$matrix[1, 3]), 100)
  expect_equal(round(fit$transform$matrix[2, 3]), -100)
})
