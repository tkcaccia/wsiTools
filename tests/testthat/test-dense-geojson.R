test_that("dense GeoJSON display caps vertices and supports bounds LOD", {
  theta <- seq(0, 2 * pi, length.out = 5000)
  x <- 100 + 40 * cos(theta)
  y <- 120 + 25 * sin(theta)
  coords <- paste(sprintf("[%.4f,%.4f]", x, y), collapse = ",")
  path <- tempfile(fileext = ".geojson")
  writeLines(sprintf(
    '{"type":"FeatureCollection","features":[{"type":"Feature","id":"dense_1","properties":{"name":"dense","classification":{"name":"tissue"}},"geometry":{"type":"Polygon","coordinates":[[%s]]}}]}',
    coords
  ), path, useBytes = TRUE)

  rois <- read_geojson(path)
  detailed <- wsiTools:::wsi_viewer_dense_roi_features(
    rois,
    source_name = "Tissue annotation",
    bounds_only = FALSE,
    max_points_per_roi = 120
  )
  detailed_points <- sum(vapply(detailed[[1]]$rings, length, integer(1)))
  expect_lte(detailed_points, 120)
  expect_gt(detailed_points, 20)

  full <- wsiTools:::wsi_viewer_dense_roi_features(
    rois,
    source_name = "Tissue annotation",
    bounds_only = FALSE,
    max_points_per_roi = Inf
  )
  full_points <- sum(vapply(full[[1]]$rings, length, integer(1)))
  expect_equal(full_points, wsiTools:::wsi_viewer_point_count(rois$coordinates[[1]]))

  clipped <- wsiTools:::wsi_viewer_dense_roi_features(
    rois,
    source_name = "Tissue annotation",
    bounds_only = FALSE,
    max_points_per_roi = Inf,
    clip_bounds = c(xmin = 130, ymin = 100, xmax = 145, ymax = 140)
  )
  clipped_points <- sum(vapply(clipped[[1]]$rings, length, integer(1)))
  expect_gt(clipped_points, 20)
  expect_lt(clipped_points, full_points)

  bounds <- wsiTools:::wsi_viewer_dense_roi_features(
    rois,
    source_name = "Tissue annotation",
    bounds_only = TRUE
  )
  expect_equal(length(bounds[[1]]$rings[[1]]), 5)
  expect_equal(bounds[[1]]$geometry_type, "Polygon")
})
