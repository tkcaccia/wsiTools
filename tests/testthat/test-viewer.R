test_that("interactive viewer writes a self-contained HTML file for mock slides", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- wsi_viewer(slide, width = 256, output = output, open = FALSE)

  expect_identical(result, output)
  expect_true(file.exists(output))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "wsiTools viewer", fixed = TRUE)
  expect_match(html, "data:image/svg\\+xml;base64,")
  expect_match(html, "thumbnail preview, full slide not loaded into R", fixed = TRUE)
  expect_match(html, "toolPan", fixed = TRUE)
  expect_match(html, "toolSelect", fixed = TRUE)
  expect_match(html, "toolDraw", fixed = TRUE)
  expect_match(html, "saveGeojson", fixed = TRUE)
  expect_match(html, "FeatureCollection", fixed = TRUE)
  expect_match(html, "crosshairToggle", fixed = TRUE)
  expect_match(html, "copyCoord", fixed = TRUE)
  expect_match(html, "GeoJSON Geometries", fixed = TRUE)
})

test_that("interactive viewer overlays GeoJSON ROI polygons", {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "roi-1",
          "properties": {"name": "Tumor", "classification": {"name": "tumor"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[100, 100], [500, 100], [500, 300], [100, 300], [100, 100]]]
          }
        },
        {
          "type": "Feature",
          "id": "line-1",
          "properties": {"name": "Margin"},
          "geometry": {
            "type": "LineString",
            "coordinates": [[100, 100], [200, 250], [320, 260]]
          }
        }
      ]
    }',
    path
  )

  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")
  result <- wsi_viewer_roi(
    slide,
    path,
    mode = "thumbnail",
    width = 256,
    output = output,
    open = FALSE
  )

  expect_identical(result, output)
  expect_true(file.exists(output))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "roi-1", fixed = TRUE)
  expect_match(html, "Tumor", fixed = TRUE)
  expect_match(html, "line-1", fixed = TRUE)
  expect_match(html, "LineString", fixed = TRUE)
  expect_match(html, "roiToggle", fixed = TRUE)
  expect_match(html, "layersToggle", fixed = TRUE)
  expect_match(html, "roiOpacity", fixed = TRUE)
  expect_match(html, "GeoJSON Geometries", fixed = TRUE)
  expect_match(html, "geometry_type", fixed = TRUE)
  expect_match(html, "point_count", fixed = TRUE)
  expect_match(html, "formatBounds", fixed = TRUE)
  expect_match(html, "Drawn ROI", fixed = TRUE)
  expect_match(html, "ROIs", fixed = TRUE)
})

test_that("interactive tiled viewer writes Deep Zoom HTML when libvips is available", {
  skip_if_not(wsi_has_vips())

  input <- tempfile(fileext = ".ppm")
  pixels <- paste(rep("255 0 0", 48 * 32), collapse = " ")
  writeLines(c("P3", "48 32", "255", pixels), input)

  slide <- wsi_open(input, backend = "vips")
  on.exit(wsi_close(slide), add = TRUE)

  output <- tempfile(fileext = ".html")
  tile_dir <- tempfile("wsi-viewer-tiles-")
  result <- wsi_viewer(
    slide,
    output = output,
    open = FALSE,
    mode = "tiles",
    tile_dir = tile_dir,
    tile_size = 16,
    tile_format = "png"
  )

  expect_identical(result, output)
  expect_true(file.exists(output))
  expect_true(file.exists(file.path(tile_dir, "slide.dzi")))
  expect_true(dir.exists(file.path(tile_dir, "slide_files")))

  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "full-resolution tiled viewer", fixed = TRUE)
  expect_match(html, "slide_files", fixed = TRUE)
  expect_match(html, "drawAncestorTile", fixed = TRUE)
  expect_match(html, "drawBleed", fixed = TRUE)
  expect_match(html, "requestDraw", fixed = TRUE)
  expect_match(html, "loadingTiles", fixed = TRUE)
  expect_match(html, "saveGeojson", fixed = TRUE)
})

test_that("interactive IHC viewer writes stain deconvolution controls", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- wsi_viewer_ihc(
    slide,
    mode = "thumbnail",
    width = 256,
    output = output,
    open = FALSE
  )

  expect_identical(result, output)
  expect_true(file.exists(output))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "stainToggle", fixed = TRUE)
  expect_match(html, "hemaColor", fixed = TRUE)
  expect_match(html, "hrpColor", fixed = TRUE)
  expect_match(html, "applyStainToCanvas", fixed = TRUE)
  expect_match(html, "IHC H-DAB", fixed = TRUE)
})
