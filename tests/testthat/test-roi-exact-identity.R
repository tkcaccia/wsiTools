test_that("GeoJSON identity metadata cannot become a partially matched ROI id", {
  geometry <- list(type = "Polygon", coordinates = list(list(
    c(0, 0), c(10, 0), c(10, 10), c(0, 0))))
  feature <- list(type = "Feature", geometry = geometry,
    properties = list(name = "stroma", classification = list(name = "stroma"),
      identity_prediction_unit = "whole_source_group"))
  collection <- list(type = "FeatureCollection", features = list(feature, feature))
  rois <- wsi_roi_from_geojson(collection)
  expect_identical(rois$roi_id, c("1", "2"))
  expect_identical(rois$class, c("stroma", "stroma"))
  collection$features[[1]]$properties$id <- "explicit-property-id"
  collection$features[[2]]$id <- "explicit-feature-id"
  expect_identical(wsi_roi_from_geojson(collection)$roi_id,
                   c("explicit-property-id", "explicit-feature-id"))
})
