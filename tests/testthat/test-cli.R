test_that("wsi_cli prints help and backend information", {
  expect_equal(wsi_cli(c("help")), 0L)
  expect_equal(wsi_cli(c("backends")), 0L)
})

test_that("wsi_cli plans and runs StarDist image commands", {
  input <- tempfile(fileext = ".png")
  writeBin(charToRaw("placeholder image"), input)
  output <- tempfile(fileext = ".geojson")

  expect_equal(
    wsi_cli(c(
      "stardist-image",
      "--input", input,
      "--output", output,
      "--command", "missing-stardist-command",
      "--arg", "--input",
      "--arg", "{input}",
      "--arg", "--output",
      "--arg", "{output}",
      "--plan"
    )),
    0L
  )

  script <- tempfile(fileext = ".R")
  writeLines(
    c(
      "args <- commandArgs(TRUE)",
      "writeLines('{",
      "  \"type\": \"FeatureCollection\",",
      "  \"features\": [{",
      "    \"type\": \"Feature\",",
      "    \"id\": \"cli-cell-1\",",
      "    \"properties\": {\"name\": \"CLI StarDist cell\", \"classification\": {\"name\": \"cell\"}},",
      "    \"geometry\": {\"type\": \"Polygon\", \"coordinates\": [[[1,1],[3,1],[3,3],[1,3],[1,1]]] }",
      "  }]",
      "}', args[[2]])"
    ),
    script
  )

  expect_equal(
    wsi_cli(c(
      "stardist-image",
      "--input", input,
      "--output", output,
      "--command", file.path(R.home("bin"), "Rscript"),
      "--arg", script,
      "--arg", "{input}",
      "--arg", "{output}",
      "--overwrite"
    )),
    0L
  )
  expect_true(file.exists(output))
  cells <- read_geojson(output)
  expect_equal(cells$roi_id, "cli-cell-1")
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
