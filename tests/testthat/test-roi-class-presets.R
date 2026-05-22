test_that("ROI class presets expose editable pathology defaults", {
  presets <- wsi_roi_class_presets()

  expect_s3_class(presets, "wsi_roi_class_presets")
  expect_equal(
    presets$class,
    c("tumour", "stroma", "necrosis", "normal", "artefact", "exclusion", "invasive front")
  )
  expect_equal(presets$color[presets$class == "tumour"], "#D73027")
  expect_false(presets$export[presets$class == "artefact"])
  expect_false(presets$export[presets$class == "exclusion"])
})

test_that("ROI class presets can be updated and applied to GeoJSON ROIs", {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "roi-tumour",
          "properties": {"name": "Tumour", "classification": {"name": "tumour"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [10, 0], [10, 10], [0, 10], [0, 0]]]
          }
        },
        {
          "type": "Feature",
          "id": "roi-exclusion",
          "properties": {"name": "Exclusion", "classification": {"name": "exclusion"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[20, 20], [30, 20], [30, 30], [20, 30], [20, 20]]]
          }
        }
      ]
    }',
    path
  )

  rois <- read_geojson(path)
  presets <- wsi_update_roi_class_preset(
    wsi_roi_class_presets(),
    "tumour",
    color = "#FF00AA"
  )
  applied <- wsi_apply_roi_class_presets(rois, presets)

  expect_equal(applied$color, c("#FF00AA", "#000000"))
  expect_equal(applied$classification_color, c("#FF00AA", "#000000"))

  output <- tempfile(fileext = ".geojson")
  write_geojson(
    rois,
    output,
    class_presets = presets,
    respect_export_rules = TRUE
  )
  written <- jsonlite::fromJSON(output, simplifyVector = FALSE)

  expect_length(written$features, 1)
  expect_equal(written$features[[1]]$id, "roi-tumour")
  expect_equal(written$features[[1]]$properties$classification$name, "tumour")
  expect_equal(written$features[[1]]$properties$classification$color, "#FF00AA")
})

