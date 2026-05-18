test_that("GeoJSON parser reads a small QuPath-style polygon", {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "roi-1",
          "properties": {
            "name": "Tumor",
            "classification": {"name": "tumor"}
          },
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [10, 0], [10, 20], [0, 20], [0, 0]]]
          }
        }
      ]
    }',
    path
  )

  roi <- wsi_read_geojson(path)
  expect_s3_class(roi, "wsi_roi")
  expect_equal(nrow(roi), 1)
  expect_equal(roi$roi_id, "roi-1")
  expect_equal(roi$name, "Tumor")
  expect_equal(roi$class, "tumor")
  expect_equal(roi$xmax, 10)
  expect_equal(roi$ymax, 20)
  expect_true(is.list(roi$coordinates))
})
