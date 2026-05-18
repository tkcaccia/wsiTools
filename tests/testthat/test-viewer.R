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
  expect_match(html, "toolBrush", fixed = TRUE)
  expect_match(html, "toolEdit", fixed = TRUE)
  expect_match(html, "Brush size", fixed = TRUE)
  expect_match(html, "brushSizeValue", fixed = TRUE)
  expect_match(html, "startBrush", fixed = TRUE)
  expect_match(html, "finishBrush", fixed = TRUE)
  expect_match(html, "brushRingFromPoints", fixed = TRUE)
  expect_match(html, "brushOperation", fixed = TRUE)
  expect_match(html, "extendSelectedRoiWithBrush", fixed = TRUE)
  expect_match(html, "subtractSelectedRoiWithBrush", fixed = TRUE)
  expect_match(html, "roi_brush_edited", fixed = TRUE)
  expect_match(html, "Alt brush", fixed = TRUE)
  expect_match(html, "roiCompositeGeometry", fixed = TRUE)
  expect_match(html, "clippedHoleForGroup", fixed = TRUE)
  expect_match(html, "roiContainsPoint", fixed = TRUE)
  expect_match(html, "drawBrushPreview", fixed = TRUE)
  expect_match(html, "findVertexAt", fixed = TRUE)
  expect_match(html, "moveActiveVertex", fixed = TRUE)
  expect_match(html, "insertVertexAt", fixed = TRUE)
  expect_match(html, "deleteSelectedVertex", fixed = TRUE)
  expect_match(html, "deleteRoi", fixed = TRUE)
  expect_match(html, "Delete selected", fixed = TRUE)
  expect_match(html, "Segmentation", fixed = TRUE)
  expect_match(html, "exportSelectedRoi", fixed = TRUE)
  expect_match(html, "startSegmentation", fixed = TRUE)
  expect_match(html, "Start StarDist", fixed = TRUE)
  expect_match(html, "loadSegmentation", fixed = TRUE)
  expect_match(html, "loadSegmentationCsv", fixed = TRUE)
  expect_match(html, "segmentationTableFile", fixed = TRUE)
  expect_match(html, "segLocalCoords", fixed = TRUE)
  expect_match(html, "segCellRadius", fixed = TRUE)
  expect_match(html, "addSegmentationCentroidTable", fixed = TRUE)
  expect_match(html, "parseDelimitedTable", fixed = TRUE)
  expect_match(html, "centroid_x", fixed = TRUE)
  expect_match(html, "bindSegmentationControls", fixed = TRUE)
  expect_match(html, "startSegmentationForSelectedRoi", fixed = TRUE)
  expect_match(html, "segmentation_run_url", fixed = TRUE)
  expect_match(html, "addSegmentationGeojson", fixed = TRUE)
  expect_match(html, "stardist", fixed = TRUE)
  expect_match(html, "Measure", fixed = TRUE)
  expect_match(html, "toolMeasure", fixed = TRUE)
  expect_match(html, "clearMeasures", fixed = TRUE)
  expect_match(html, "measureSummary", fixed = TRUE)
  expect_match(html, "measureList", fixed = TRUE)
  expect_match(html, "measurePixelSize", fixed = TRUE)
  expect_match(html, "measurementRecord", fixed = TRUE)
  expect_match(html, "drawMeasurements", fixed = TRUE)
  expect_match(html, "bindMeasureControls", fixed = TRUE)
  expect_match(html, "mpp", fixed = TRUE)
  expect_match(html, "saveGeojson", fixed = TRUE)
  expect_match(html, "FeatureCollection", fixed = TRUE)
  expect_match(html, "crosshairToggle", fixed = TRUE)
  expect_match(html, "copyCoord", fixed = TRUE)
  expect_match(html, "GeoJSON Geometries", fixed = TRUE)
  expect_match(html, "#roiPanel{position:fixed;left:12px", fixed = TRUE)
  expect_match(html, "toolMenu", fixed = TRUE)
  expect_match(html, "bindExclusiveMenus", fixed = TRUE)
  expect_match(html, "syncViewerState", fixed = TRUE)
  expect_match(html, "scheduleViewerStateSync", fixed = TRUE)
  expect_match(html, "viewer_state_url", fixed = TRUE)
  expect_match(html, "R sync", fixed = TRUE)
  expect_match(html, "querySelectorAll('.toolMenu')", fixed = TRUE)
  expect_match(html, "closeOtherMenus", fixed = TRUE)
  expect_match(html, "pointerdown", fixed = TRUE)
  expect_match(html, "closest('.toolMenu')", fixed = TRUE)
  expect_match(html, "menu!==active", fixed = TRUE)
  expect_match(html, "Navigate", fixed = TRUE)
  expect_match(html, "Annotations", fixed = TRUE)
  expect_match(html, "Geometry list", fixed = TRUE)
  expect_match(html, "Import GeoJSON", fixed = TRUE)
  expect_match(html, "importGeojson", fixed = TRUE)
  expect_match(html, "geojsonImportFile", fixed = TRUE)
  expect_match(html, "geojsonImportSummary", fixed = TRUE)
  expect_match(html, "bindGeojsonImportControls", fixed = TRUE)
  expect_match(html, "addImportedGeojson", fixed = TRUE)
  expect_match(html, "importedRoiFromFeature", fixed = TRUE)
  expect_match(html, "geojsonGeometryParts", fixed = TRUE)
  expect_match(html, "roiLabelInput", fixed = TRUE)
  expect_match(html, "annotation label", fixed = TRUE)
  expect_match(html, "roiClassSelect", fixed = TRUE)
  expect_match(html, "tumour", fixed = TRUE)
  expect_match(html, "applyRoiClass", fixed = TRUE)
  expect_match(html, "Apply to selected", fixed = TRUE)
  expect_match(html, "annotationLabelValue", fixed = TRUE)
  expect_match(html, "activeRoiName", fixed = TRUE)
  expect_match(html, "label:name", fixed = TRUE)
})

test_that("interactive viewer can be configured with a live R state endpoint", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- wsi_viewer(
    slide,
    width = 256,
    output = output,
    open = FALSE,
    viewer_state_url = "http://127.0.0.1:8788/viewer-state"
  )

  expect_identical(result, output)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "http://127.0.0.1:8788/viewer-state", fixed = TRUE)
  expect_match(html, "viewerStatePayload", fixed = TRUE)
  expect_match(html, "roiGeojsonObject", fixed = TRUE)
  expect_match(html, "measurements:measures", fixed = TRUE)
  expect_match(html, "segmentationGeojsonObject", fixed = TRUE)
  expect_match(html, "geojson_imported", fixed = TRUE)
  expect_match(html, "measurement_added", fixed = TRUE)
  expect_match(html, "segmentation_added", fixed = TRUE)
})

test_that("live viewer state payloads update R objects", {
  env <- new.env(parent = emptyenv())
  state <- wsiTools:::wsi_new_viewer_state(name = "live", envir = env)
  feature <- list(
    type = "Feature",
    id = "roi-1",
    properties = list(
      name = "Tumour region",
      classification = list(name = "tumour")
    ),
    geometry = list(
      type = "Polygon",
      coordinates = list(list(
        c(0, 0), c(10, 0), c(10, 8), c(0, 8), c(0, 0)
      ))
    )
  )
  payload <- list(
    event = "roi_added",
    time = "2026-05-18T12:00:00Z",
    selected_index = 0,
    selected_roi = feature,
    rois = list(type = "FeatureCollection", features = list(feature)),
    segmentation = list(type = "FeatureCollection", features = list()),
    measurements = list(list(
      id = "measure_1",
      start = list(x = 0, y = 0),
      end = list(x = 3, y = 4),
      distance_px = 5,
      distance_um = 2.5
    )),
    view = list(mode = "measure", scale = 1),
    stain = list(enabled = FALSE)
  )

  wsiTools:::wsi_viewer_state_apply(state, payload)
  snapshot <- wsi_viewer_state(state)

  expect_s3_class(snapshot$rois, "wsi_roi")
  expect_equal(snapshot$rois$roi_id, "roi-1")
  expect_equal(snapshot$rois$class, "tumour")
  expect_equal(snapshot$measurements$distance_px, 5)
  expect_equal(snapshot$selected_roi$name, "Tumour region")
  expect_identical(env$live, state)
  expect_s3_class(env$live_rois, "wsi_roi")
  expect_equal(env$live_measurements$id, "measure_1")
  expect_equal(env$live_last_event$event, "roi_added")
})

test_that("interactive viewer can be configured with a segmentation run endpoint", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- wsi_viewer(
    slide,
    width = 256,
    output = output,
    open = FALSE,
    segmentation_run_url = "http://127.0.0.1:8787/segment"
  )

  expect_identical(result, output)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "http://127.0.0.1:8787/segment", fixed = TRUE)
  expect_match(html, "fetch(url", fixed = TRUE)
  expect_match(html, "Running StarDist on selected ROI", fixed = TRUE)
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
          "properties": {"label": "Margin label"},
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
  expect_match(html, "Margin label", fixed = TRUE)
  expect_match(html, "LineString", fixed = TRUE)
  expect_match(html, "roiToggle", fixed = TRUE)
  expect_match(html, "layersToggle", fixed = TRUE)
  expect_match(html, "roiOpacity", fixed = TRUE)
  expect_match(html, "GeoJSON Geometries", fixed = TRUE)
  expect_match(html, "id=\"roiPanel\" class=\"panel open\"", fixed = TRUE)
  expect_match(html, "aria-label=\"GeoJSON geometry list\"", fixed = TRUE)
  expect_match(html, "setRoiPanelOpen", fixed = TRUE)
  expect_match(html, "toggleRoiPanel", fixed = TRUE)
  expect_match(html, "geometry_type", fixed = TRUE)
  expect_match(html, "point_count", fixed = TRUE)
  expect_match(html, "formatBounds", fixed = TRUE)
  expect_match(html, "roiLabelText", fixed = TRUE)
  expect_match(html, "roiLabelPoint", fixed = TRUE)
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
  expect_match(html, "stainChannelControls", fixed = TRUE)
  expect_match(html, "stainColor_hematoxylin", fixed = TRUE)
  expect_match(html, "stainColor_hrp_dab", fixed = TRUE)
  expect_match(html, "Stains", fixed = TRUE)
  expect_match(html, "stainShowOriginal", fixed = TRUE)
  expect_match(html, "stainShowAll", fixed = TRUE)
  expect_match(html, "class=\"stainOnly\"", fixed = TRUE)
  expect_match(html, "activeStainNames", fixed = TRUE)
  expect_match(html, "syncStainStateFromControls", fixed = TRUE)
  expect_match(html, "setStainVisible", fixed = TRUE)
  expect_match(html, "addEventListener('input',redraw)", fixed = TRUE)
  expect_match(html, "addEventListener('change',redraw)", fixed = TRUE)
  expect_match(html, "applyStainToCanvas", fixed = TRUE)
  expect_match(html, "open the viewer through localhost/http", fixed = TRUE)
  expect_match(html, "IHC H-DAB", fixed = TRUE)
})

test_that("interactive multi-IHC viewer writes selectable channel controls", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")
  channels <- wsi_stain_channels(
    name = c("Hematoxylin", "HRP/DAB", "Fast Red"),
    vector = list(
      c(0.650, 0.704, 0.286),
      c(0.268, 0.570, 0.776),
      c(0.213, 0.851, 0.477)
    ),
    colour = c("#4b3f99", "#8b5a2b", "#d73027"),
    visible = c(TRUE, TRUE, FALSE)
  )

  result <- wsi_viewer_multi_ihc(
    slide,
    channels = channels,
    mode = "thumbnail",
    width = 256,
    output = output,
    open = FALSE
  )

  expect_identical(result, output)
  expect_true(file.exists(output))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "mIHC", fixed = TRUE)
  expect_match(html, "IHC channels", fixed = TRUE)
  expect_match(html, "stainVisible_fast_red", fixed = TRUE)
  expect_match(html, "stainStrength_fast_red", fixed = TRUE)
  expect_match(html, '"visible":false', fixed = TRUE)
})

test_that("side-by-side comparison viewer writes synchronized controls", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- viewer_compare(slide, slide, sync = TRUE, output = output, open = FALSE)

  expect_identical(result, output)
  expect_true(file.exists(output))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "wsiTools comparison viewer", fixed = TRUE)
  expect_match(html, "syncToggle", fixed = TRUE)
  expect_match(html, "linked cursor", fixed = TRUE)
  expect_match(html, "canvas0", fixed = TRUE)
  expect_match(html, "canvas1", fixed = TRUE)
})
