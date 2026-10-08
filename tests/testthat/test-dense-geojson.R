test_that("dense GeoJSON display keeps every source vertex", {
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
  full <- wsiTools:::wsi_viewer_dense_roi_features(
    rois,
    source_name = "Tissue annotation"
  )
  full_points <- sum(vapply(full[[1]]$rings, length, integer(1)))
  expect_equal(full_points, wsiTools:::wsi_viewer_point_count(rois$coordinates[[1]]))
  expect_gt(full_points, 4900)
  expect_equal(full[[1]]$rings[[1L]][[1L]],
               full[[1]]$rings[[1L]][[full_points]])
  rois$class[[1L]] <- "cell"
  cell <- wsiTools:::wsi_viewer_dense_roi_features(rois, source_name = "Cell annotation")
  expect_equal(sum(vapply(cell[[1L]]$rings, length, integer(1))), full_points)
})

test_that("static GeoJSON rings keep every point at overview zoom", {
  skip_if_not_installed("V8")
  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 80, levels = c(1, 4))
  path <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = path, open = FALSE, overwrite = TRUE)
  lines <- readLines(path, warn = FALSE)
  js <- V8::v8()
  for (name in c("pointFromGeojsonCoord", "ringFromGeojsonCoords",
                 "denseStaticRing", "denseStaticRawGroups")) {
    definition <- grep(paste0("^function ", name, "\\("), lines, value = TRUE)
    expect_length(definition, 1L)
    js$eval(definition)
  }
  coords <- lapply(seq_len(5000L), function(i) c(i, i %% 17L))
  coords[[5000L]] <- coords[[1L]]
  js$assign("coords", coords)
  js$eval("var item={geometry_type:'Polygon',raw_coordinates:[coords]}; var scale=0.001;")
  expect_equal(as.integer(js$eval("denseStaticRawGroups(item)[0][0].length")), 5000L)
  js$eval("delete item._dense_full_groups; scale=100;")
  expect_equal(as.integer(js$eval("denseStaticRawGroups(item)[0][0].length")), 5000L)
})

test_that("imported GeoJSON ROIs use detailed boundaries at overview zoom", {
  skip_if_not_installed("V8")
  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 80, levels = c(1, 4))
  path <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = path, open = FALSE, overwrite = TRUE)
  lines <- readLines(path, warn = FALSE)
  js <- V8::v8()
  js$eval("var selectedRoi=-1, rois=new Array(1000), scale=0.001;")
  for (name in c("geojsonAnnotationRoi", "denseGeometryVisible", "roiLodMode")) {
    definition <- grep(paste0("^function ", name, "\\("), lines, value = TRUE)
    expect_gt(length(definition), 0L)
    if (name != "geojsonAnnotationRoi") {
      expect_true(all(grepl("geojsonAnnotationRoi(roi)", definition, fixed = TRUE)))
    }
    for (entry in definition) js$eval(entry)
  }
  js$eval("var imported={imported:true,source:'imported: cells.geojson'};")
  expect_identical(js$eval("denseGeometryVisible(imported)"), "true")
  expect_identical(js$eval("roiLodMode(imported,1,null,null,false)"), "detail")
})
