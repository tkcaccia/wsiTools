test_that("wsi_cli prints help and backend information", {
  expect_equal(wsi_cli(c("help")), 0L)
  expect_equal(wsi_cli(c("backends")), 0L)
})

test_that("wsi_cli translates ROI GeoJSON from crop to slide coordinates", {
  input <- tempfile(fileext = ".geojson")
  output <- tempfile(fileext = ".geojson")
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
    input
  )

  expect_equal(
    wsi_cli(c(
      "translate-rois",
      "--input", input,
      "--output", output,
      "--dx", "10",
      "--dy", "20"
    )),
    0L
  )
  shifted <- read_geojson(output)
  expect_equal(shifted$xmin, 10)
  expect_equal(shifted$ymin, 20)
})

test_that("exec script is installed with the package", {
  script <- system.file("exec", "wsitools", package = "wsiTools")
  expect_true(nzchar(script))
  expect_true(file.exists(script))
  expect_match(readLines(script, n = 1L, warn = FALSE), "Rscript", fixed = TRUE)
})
