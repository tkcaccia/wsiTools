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

test_that("GeoJSON writer preserves ROI classes and blocks accidental overwrite", {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "roi-1",
          "properties": {"name": "Tumor", "classification": {"name": "tumour"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [10, 0], [10, 20], [0, 20], [0, 0]]]
          }
        }
      ]
    }',
    path
  )

  roi <- read_geojson(path)
  roi <- wsi_set_roi_class(roi, "necrosis")
  output <- tempfile(fileext = ".geojson")

  expect_invisible(write_geojson(roi, output))
  expect_error(write_geojson(roi, output), "overwrite = FALSE")

  roundtrip <- wsi_read_geojson(output)
  expect_equal(roundtrip$class, "necrosis")
  expect_equal(roundtrip$geometry_type, "Polygon")
  written <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(written, '"label": "Tumor"', fixed = TRUE)
})

test_that("GeoJSON parser uses label property when name is absent", {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "roi-1",
          "properties": {"label": "User drawn region", "classification": {"name": "tumour"}},
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

  expect_equal(roi$name, "User drawn region")
  expect_equal(roi$class, "tumour")
})
