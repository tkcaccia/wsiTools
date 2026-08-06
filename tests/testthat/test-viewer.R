wsi_skip_if_no_httpuv_server <- function(port = 8788L) {
  skip_if_not_installed("httpuv")
  app <- list(call = function(req) {
    list(status = 200L, headers = list("Content-Type" = "text/plain"), body = "ok")
  })
  server <- NULL
  last_error <- NULL
  for (candidate in seq.int(as.integer(port), as.integer(port) + 20L)) {
    server <- try(httpuv::startServer("127.0.0.1", candidate, app), silent = TRUE)
    if (!inherits(server, "try-error")) {
      httpuv::stopServer(server)
      return(invisible(TRUE))
    }
    last_error <- conditionMessage(attr(server, "condition"))
  }
  if (is.null(last_error) || !nzchar(last_error)) {
    last_error <- "unknown error"
  }
  skip(sprintf("httpuv cannot start a local test server: %s", last_error))
}

test_that("wsi_open_viewer opens a static viewer with one command", {
  slide <- wsiTools:::wsi_mock_slide(width = 800, height = 400, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- wsi_open_viewer(
    slide,
    live = "no",
    open = FALSE,
    output = output,
    quiet = TRUE
  )

  expect_identical(result, normalizePath(output, winslash = "/", mustWork = FALSE))
  expect_true(file.exists(output))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "wsiTools viewer", fixed = TRUE)
  expect_true(
    grepl("thumbnail preview, full slide not loaded into R", html, fixed = TRUE) ||
      grepl("tile_url_base", html, fixed = TRUE)
  )
})

test_that("wsi_open_viewer accepts friendly flag aliases", {
  expect_equal(wsiTools:::wsi_open_viewer_flag(TRUE, "live"), "yes")
  expect_equal(wsiTools:::wsi_open_viewer_flag(FALSE, "live"), "no")
  expect_equal(wsiTools:::wsi_open_viewer_flag("static", "live", no_alias = "static"), "no")
  expect_equal(wsiTools:::wsi_open_viewer_flag("tiles", "tiled", yes_alias = "tiles"), "yes")
})

test_that("wsi_open_viewer detects large files for progressive dynamic tiling", {
  tmp <- tempfile(fileext = ".svs")
  writeBin(as.raw(c(1, 2)), tmp)
  slide <- list(path = tmp, backend = "vips")
  old <- Sys.getenv("WSITOOLS_LARGE_IMAGE_BYTES", unset = NA_character_)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("WSITOOLS_LARGE_IMAGE_BYTES")
    } else {
      Sys.setenv(WSITOOLS_LARGE_IMAGE_BYTES = old)
    }
  }, add = TRUE)

  expect_equal(wsiTools:::wsi_open_viewer_file_size(slide), 2)
  expect_true(wsiTools:::wsi_open_viewer_is_large_file(slide, threshold = 1))
  expect_false(wsiTools:::wsi_open_viewer_is_large_file(slide, threshold = 10))
  Sys.setenv(WSITOOLS_LARGE_IMAGE_BYTES = "1")
  expect_true(wsiTools:::wsi_open_viewer_is_large_file(slide))
  expect_true("wsi_open_fast_viewer" %in% getNamespaceExports("wsiTools"))
})

test_that("wsi_viewer accepts tiled as a logical mode shortcut", {
  slide <- wsiTools:::wsi_mock_slide(width = 800, height = 400, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  wsi_viewer(slide, output = output, open = FALSE, tiled = FALSE)

  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, '"viewer_mode":"thumbnail"', fixed = TRUE)
})

test_that("wsi_open_viewer explains missing httpuv for forced live mode", {
  expect_error(
    wsiTools:::wsi_open_viewer_use_live("yes", httpuv_ready = FALSE),
    "install.packages\\(\"httpuv\"\\)"
  )
})

test_that("interactive viewer writes a self-contained HTML file for mock slides", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- wsi_viewer(slide, width = 256, output = output, open = FALSE)

  expect_identical(result, output)
  expect_true(file.exists(output))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "wsiTools viewer", fixed = TRUE)
  expect_match(html, "data:image/svg\\+xml;base64,")
  expect_true(
    grepl("thumbnail preview, full slide not loaded into R", html, fixed = TRUE) ||
      grepl("tile_url_base", html, fixed = TRUE)
  )
  expect_match(html, "toolPan", fixed = TRUE)
  expect_match(html, "navPanButton", fixed = TRUE)
  expect_match(html, "iconMove", fixed = TRUE)
  expect_match(html, "navDock", fixed = TRUE)
  expect_match(html, "Navigation controls", fixed = TRUE)
  expect_match(html, "Arrow keys", fixed = TRUE)
  expect_match(html, "keyboardPanStep", fixed = TRUE)
  expect_match(html, "panByKeyboard(e.key,e.shiftKey)", fixed = TRUE)
  expect_match(html, "key==='ArrowLeft'", fixed = TRUE)
  expect_match(html, "Highlight labels", fixed = TRUE)
  expect_match(html, "annotationLabelHighlightList", fixed = TRUE)
  expect_match(html, "annotationHighlightAll", fixed = TRUE)
  expect_match(html, "annotationHighlightNone", fixed = TRUE)
  expect_match(html, "roiClassHighlighted", fixed = TRUE)
  expect_match(html, "roiLodMode", fixed = TRUE)
  expect_match(html, "roiVisibleSlideBounds", fixed = TRUE)
  expect_match(html, "roiIntersectsViewport", fixed = TRUE)
  expect_match(html, "denseGeometryVisible", fixed = TRUE)
  expect_match(html, "denseGeometryMinZoom", fixed = TRUE)
  expect_match(html, "Annotation level-of-detail active", fixed = TRUE)
  expect_match(html, "screenshotTool", fixed = TRUE)
  expect_match(html, "Select screenshot area", fixed = TRUE)
  expect_match(html, "PNG, JPEG, SVG, or PDF screenshot", fixed = TRUE)
  expect_match(html, "screenshotDialog", fixed = TRUE)
  expect_match(html, "screenshotDialogFormat", fixed = TRUE)
  expect_match(html, "<option value=\"jpeg\">JPEG</option>", fixed = TRUE)
  expect_match(html, "<option value=\"pdf\">PDF</option>", fixed = TRUE)
  expect_match(html, "screenshotIncludeTissue", fixed = TRUE)
  expect_match(html, "screenshotIncludeLayers", fixed = TRUE)
  expect_match(html, "screenshotIncludeAnnotations", fixed = TRUE)
  expect_match(html, "#proximityLegend{position:fixed", fixed = TRUE)
  expect_match(html, "box-sizing:border-box;display:none;z-index:27", fixed = TRUE)
  expect_match(html, "proximityLegendDragState={startX:e.clientX,startY:e.clientY,left:rect.left,top:rect.top,width:width,moved:false}", fixed = TRUE)
  expect_match(html, "setProximityLegendPosition(proximityLegendDragState.left+dx,proximityLegendDragState.top+dy,false,proximityLegendDragState.width)", fixed = TRUE)
  expect_match(html, "zoomIn", fixed = TRUE)
  expect_match(html, "zoomOut", fixed = TRUE)
  expect_match(html, "oneToOne", fixed = TRUE)
  expect_false(grepl("if(e.key==='1')oneToOne();", html, fixed = TRUE))
  expect_false(grepl("<button id=\"toolSelect\"", html, fixed = TRUE))
  expect_match(html, "toolDraw", fixed = TRUE)
  expect_match(html, "toolBrush", fixed = TRUE)
  expect_match(html, "setMode('draw');closeMenuAfterToolAction", fixed = TRUE)
  expect_match(html, "setMode('brush');closeMenuAfterToolAction", fixed = TRUE)
  expect_false(grepl("<button id=\"newRoi\"", html, fixed = TRUE))
  expect_match(html, "toolEdit", fixed = TRUE)
  expect_false(grepl("<button id=\"finishRoi\"", html, fixed = TRUE))
  expect_false(grepl("<button id=\"undoPoint\"", html, fixed = TRUE))
  expect_false(grepl("Zoom spots", html, fixed = TRUE))
  expect_match(html, "layerPanelToggle", fixed = TRUE)
  expect_match(html, "Layer panel", fixed = TRUE)
  expect_match(html, "layerItemMatchesActiveProject", fixed = TRUE)
  expect_match(html, "layerItemHasProjectScope", fixed = TRUE)
  expect_false(grepl("seuratSpotZoom", html, fixed = TRUE))
  expect_false(grepl("seuratLayerPanel", html, fixed = TRUE))
  expect_false(grepl("cellPanelToggle", html, fixed = TRUE))
  expect_false(grepl("<summary title=\"CellPhenotyper cell overlays\">Cells</summary>", html, fixed = TRUE))
  expect_false(grepl("id=\"cellToggle\"", html, fixed = TRUE))
  expect_false(grepl("No CellPhenotyper cells loaded.", html, fixed = TRUE))
  expect_false(grepl("seuratSpotRadius", html, fixed = TRUE))
  expect_false(grepl("seuratSpotRadiusValue", html, fixed = TRUE))
  expect_false(grepl("setSeuratSpotRadius", html, fixed = TRUE))
  expect_false(grepl("<summary title=\"Spatial transcriptomics spot overlays and reduction plots\">", html, fixed = TRUE))
  expect_false(grepl("id=\"seuratSpotToggle\"", html, fixed = TRUE))
  expect_false(grepl("id=\"seuratSpotOpacityHelp\"", html, fixed = TRUE))
  expect_false(grepl("id=\"seuratTileWindowOpen\"", html, fixed = TRUE))
  expect_false(grepl("id=\"seuratGeneInput\"", html, fixed = TRUE))
  expect_false(grepl("<div class=\"menuHint\">Spot size is fixed by the spatial transcriptomics platform metadata.", html, fixed = TRUE))
  expect_false(grepl("id=\"seuratTileHelp\"", html, fixed = TRUE))
  expect_false(grepl("<div class=\"menuHint\">Preview tile boxes on the slide before export.", html, fixed = TRUE))
  expect_match(html, "spot_index_file", fixed = TRUE)
  expect_match(html, "spot index CSV", fixed = TRUE)
  expect_match(html, "Stains", fixed = TRUE)
  expect_match(html, "Overlay layer", fixed = TRUE)
  expect_match(html, "data-overlay-focus=\"coordinates\"", fixed = TRUE)
  expect_match(html, "this can be active together with annotations", fixed = TRUE)
  expect_match(html, "data-overlay-focus=\"cell_segmentation\"", fixed = TRUE)
  expect_match(html, "data-overlay-focus=\"annotation\"", fixed = TRUE)
  expect_match(html, "this can be active together with coordinates", fixed = TRUE)
  expect_match(html, "data-overlay-focus=\"trajectory\"", fixed = TRUE)
  expect_match(html, "overlayFocusMode", fixed = TRUE)
  expect_match(html, "overlayFocusLayerAllowed", fixed = TRUE)
  expect_match(html, "overlayFocusRoiAllowed", fixed = TRUE)
  expect_match(html, "overlayFocusChannelAllowed", fixed = TRUE)
  expect_match(html, "overlayFocusTrajectoryVisible", fixed = TRUE)
  expect_match(html, "spots?|proximity|distance[_ -]?to[_ -]?reference", fixed = TRUE)
  expect_match(html, "function revealCoordinateOverlays()", fixed = TRUE)
  expect_match(html, "meta.coordinate_overlay===true", fixed = TRUE)
  expect_match(html, "function proximityPayload(){return {point_source:", fixed = TRUE)
  expect_false(grepl("function proximityPayload\\(\\).*rois:roiGeojsonObject", html))
  expect_match(html, "mode==='coordinates_annotation'&&(wanted==='coordinates'||wanted==='annotation')", fixed = TRUE)
  expect_match(html, "current===other)overlayFocusMode='coordinates_annotation'", fixed = TRUE)
  expect_match(html, "Showing coordinates and annotations together.", fixed = TRUE)
  expect_match(html, "if(typeof revealCoordinateOverlays==='function')revealCoordinateOverlays();updateSeuratControls()", fixed = TRUE)
  expect_match(html, "handleViewerCommands(body);if(typeof revealCoordinateOverlays==='function')revealCoordinateOverlays();if(typeof invalidateSpatialWebglCache==='function')invalidateSpatialWebglCache()", fixed = TRUE)
  pos_annotations_menu <- regexpr("<summary title=\"Draw, select, import, export, and manage annotations\">Annotations</summary>", html, fixed = TRUE)[[1]]
  pos_segmentation_section <- regexpr("StarDist segmentation", html, fixed = TRUE)[[1]]
  expect_gt(pos_annotations_menu, 0)
  expect_identical(pos_segmentation_section, -1L)
  pos_stains_menu <- regexpr("<summary title=\"Stain deconvolution display options\">Stains</summary>", html, fixed = TRUE)[[1]]
  pos_help_menu <- regexpr("<summary title=\"Viewer guide and shortcuts\">Help</summary>", html, fixed = TRUE)[[1]]
  expect_true(all(c(pos_stains_menu, pos_help_menu) > 0))
  expect_lt(pos_stains_menu, pos_help_menu)
  expect_match(html, "No stain channels are configured for this viewer.", fixed = TRUE)
  expect_false(grepl("stainChannelControls", html, fixed = TRUE))
  expect_false(grepl("<div class=\"menuTitle\">Channels</div>", html, fixed = TRUE))
  expect_match(html, "Image channels", fixed = TRUE)
  expect_match(html, "Brush size", fixed = TRUE)
  expect_match(html, "value=\"32\"", fixed = TRUE)
  expect_match(html, "brushRadius=32,brushScreenRadius=32", fixed = TRUE)
  expect_match(html, "effective 32 slide px", fixed = TRUE)
  expect_match(html, "annotationBrushControls", fixed = TRUE)
  expect_match(html, "brushSizeValue", fixed = TRUE)
  expect_match(html, "brushZoomHint", fixed = TRUE)
  expect_match(html, "brushScreenRadius", fixed = TRUE)
  expect_match(html, "syncBrushRadiusToZoom", fixed = TRUE)
  expect_match(html, "effective slide-pixel size adapts automatically with zoom", fixed = TRUE)
  expect_match(html, "m==='brush'&&typeof setRoiPanelOpen", fixed = TRUE)
  pos_brush <- regexpr("annotationBrushControls", html, fixed = TRUE)[[1]]
  pos_layers <- regexpr("<div class=\"sideTitle\">Layers</div>", html, fixed = TRUE)[[1]]
  pos_rois <- regexpr("<div class=\"sideTitle\">ROIs</div>", html, fixed = TRUE)[[1]]
  expect_true(all(c(pos_brush, pos_layers, pos_rois) > 0))
  expect_lt(pos_brush, pos_layers)
  expect_lt(pos_layers, pos_rois)
  expect_false(grepl("<div id=\"classPresetEditor\"", html, fixed = TRUE))
  expect_match(html, "viewerPreferenceKey", fixed = TRUE)
  expect_match(html, "wsiTools.viewer.preferences.v1", fixed = TRUE)
  expect_match(html, "applyViewerPreferences", fixed = TRUE)
  expect_match(html, "return validToolMode(prefs.tool_mode)||'pan';", fixed = TRUE)
  expect_match(html, "savePreferences", fixed = TRUE)
  expect_match(html, "Save prefs", fixed = TRUE)
  expect_match(html, "saveCurrentViewerPreferences", fixed = TRUE)
  expect_match(html, "currentViewerPreferencePatch", fixed = TRUE)
  expect_match(html, "currentAnnotationPreferencePatch", fixed = TRUE)
  expect_match(html, "currentDisplayPreferencePatch", fixed = TRUE)
  expect_match(html, "show_rois:!!showRois", fixed = TRUE)
  expect_match(html, "show_labels:!!showLabels", fixed = TRUE)
  expect_match(html, "show_crosshair:!!showCrosshair", fixed = TRUE)
  expect_match(html, "screenshot_format", fixed = TRUE)
  expect_false(grepl("simplify_tolerance", html, fixed = TRUE))
  expect_match(html, "trajectory_area_width", fixed = TRUE)
  expect_match(html, "Viewer preferences saved in this browser", fixed = TRUE)
  expect_match(html, "stableClassColour", fixed = TRUE)
  expect_match(html, "roiClassKey", fixed = TRUE)
  expect_match(html, "annotationIndicesShareClass", fixed = TRUE)
  expect_match(html, "ensureClassPreset", fixed = TRUE)
  expect_match(html, "classColour", fixed = TRUE)
  expect_match(html, "setClassColour", fixed = TRUE)
  expect_match(html, "applyClassPresetColoursToRois(false)", fixed = TRUE)
  expect_match(html, "Class color updated", fixed = TRUE)
  expect_match(html, "saveBrushPreference", fixed = TRUE)
  expect_match(html, "saveToolPreference", fixed = TRUE)
  expect_match(html, "resetPreferences", fixed = TRUE)
  expect_match(html, '"viewer_mode":"thumbnail"', fixed = TRUE)
  expect_false(grepl("simplifyTolerance", html, fixed = TRUE))
  expect_false(grepl("simplifyToleranceValue", html, fixed = TRUE))
  expect_false(grepl("Simplification tolerance", html, fixed = TRUE))
  expect_match(html, "Brush refinement", fixed = TRUE)
  expect_false(grepl("<button id=\"smoothRoi\"", html, fixed = TRUE))
  expect_false(grepl("<button id=\"simplifyRoi\"", html, fixed = TRUE))
  expect_match(html, "fillRoiHoles", fixed = TRUE)
  expect_false(grepl("<button id=\"mergeRois\"", html, fixed = TRUE))
  expect_false(grepl("<button id=\"splitRoi\"", html, fixed = TRUE))
  expect_match(html, "startBrush", fixed = TRUE)
  expect_match(html, "finishBrush", fixed = TRUE)
  expect_match(html, "if(typeof closeAllToolMenus==='function')closeAllToolMenus();draw();return;", fixed = TRUE)
  expect_match(html, "startNewAnnotation", fixed = TRUE)
  expect_match(html, "deselectAnnotation", fixed = TRUE)
  expect_match(html, "pointNearRoi", fixed = TRUE)
  expect_match(html, "roi_deselected", fixed = TRUE)
  expect_match(html, "Ready for new ROI", fixed = TRUE)
  expect_false(grepl("Brush creates a new automatically named annotation", html, fixed = TRUE))
  expect_false(grepl("If it touches an existing annotation with the same label", html, fixed = TRUE))
  expect_match(html, "automaticAnnotationName", fixed = TRUE)
  expect_match(html, "return raw.replace", fixed = TRUE)
  expect_match(html, "Pathology class for the next drawn or painted annotation", fixed = TRUE)
  expect_false(grepl("Next class <select id=\"roiClassSelect\"", html, fixed = TRUE))
  expect_false(grepl("id=\"roiClassSelect\"", html, fixed = TRUE))
  expect_false(grepl("id=\"roiClassCustom\"", html, fixed = TRUE))
  expect_match(html, "id=\"newAnnotationEditor\"", fixed = TRUE)
  expect_match(html, "New ROI annotation settings", fixed = TRUE)
  expect_match(html, "New class <select id=\"panelRoiClassSelect\"", fixed = TRUE)
  expect_match(html, "panelRoiClassCustom", fixed = TRUE)
  expect_match(html, "panelApplyRoiClass", fixed = TRUE)
  expect_match(html, "id=\"selectedAnnotationEditor\" class=\"annotationEditor\" aria-label=\"Selected annotation editor\" hidden", fixed = TRUE)
  expect_match(html, "selectedEditor.hidden=!has", fixed = TRUE)
  expect_match(html, "Selected class <select id=\"annotationClassSelect\"", fixed = TRUE)
  expect_match(html, "focusRoiCategoryEditor", fixed = TRUE)
  expect_match(html, "category.textContent='Category'", fixed = TRUE)
  expect_match(html, "Modify annotation category", fixed = TRUE)
  expect_match(html, "Choose a category and click Apply", fixed = TRUE)
  expect_false(grepl("Selected category <select id=\"selectedRoiClassSelect\"", html, fixed = TRUE))
  expect_false(grepl("id=\"selectedRoiClassSelect\"", html, fixed = TRUE))
  expect_false(grepl("id=\"selectedRoiClassCustom\"", html, fixed = TRUE))
  expect_false(grepl("Apply selected category", html, fixed = TRUE))
  expect_false(grepl("Set next class", html, fixed = TRUE))
  expect_match(html, "Set new ROI category", fixed = TRUE)
  expect_match(html, "Next annotation class:", fixed = TRUE)
  expect_match(html, "Click Apply to update the selected annotation category", fixed = TRUE)
  expect_false(grepl("setRoiNameForClassAssignment", html, fixed = TRUE))
  expect_false(grepl("selectedRoiRelabelClassValue", html, fixed = TRUE))
  expect_false(grepl("applySelectedRoiLabelSwap", html, fixed = TRUE))
  expect_match(html, "roi_label_updated", fixed = TRUE)
  expect_match(html, "viewerTypingTarget", fixed = TRUE)
  expect_match(
    html,
    "document.addEventListener('keydown',e=>{if(viewerTypingTarget(e.target))e.stopPropagation();});",
    fixed = TRUE
  )
  expect_match(html, "classPresetColour(cls,stableClassColour(cls))", fixed = TRUE)
  expect_match(html, "if(cls)syncPanelColour(cls)", fixed = TRUE)
  expect_false(grepl("if(cls){ensureRoiClassOption(cls);syncPanelColour(cls);}", html, fixed = TRUE))
  expect_match(html, "Annotation category changed:", fixed = TRUE)
  expect_match(html, "automaticAnnotationName(selectedClass,'ROI',selectedRoi)", fixed = TRUE)
  expect_match(html, "consistent_category_label:true", fixed = TRUE)
  expect_false(grepl("unique_name:true", html, fixed = TRUE))
  expect_false(grepl("name_preserved:true", html, fixed = TRUE))
  expect_false(grepl("annotationNameLooksAutomatic", html, fixed = TRUE))
  expect_match(html, "automatic_name:!!roi.automatic_name", fixed = TRUE)
  expect_match(html, "category_label:roiName", fixed = TRUE)
  expect_match(html, "nextRoiNameDirty=false", fixed = TRUE)
  expect_match(html, "clearNextAnnotationName", fixed = TRUE)
  expect_false(grepl("Next ROI will use automatic class name", html, fixed = TRUE))
  expect_match(html, "nextRoiClass='tumour'", fixed = TRUE)
  expect_match(html, "setNextRoiClass", fixed = TRUE)
  expect_match(html, "panel&&selectedRoi<0", fixed = TRUE)
  expect_match(html, "if(panelClass)setSelectValue(panelClass,cls)", fixed = TRUE)
  expect_false(grepl("panel&&(selectedRoi<0||mode==='brush'||brushing)", html, fixed = TRUE))
  expect_false(grepl("displayCls=(brushing&&brushOperation!=='subtract')?(brushClass||nextRoiClass||activeRoiClass||cls):cls", html, fixed = TRUE))
  expect_match(html, "ROI metadata updated; shape remains locked", fixed = TRUE)
  expect_match(html, "input.disabled=!selected;", fixed = TRUE)
  expect_false(grepl("input.disabled=!selected||!editable", html, fixed = TRUE))
  expect_match(html, "Click Apply to update selected annotation", fixed = TRUE)
  expect_false(grepl("const nextClassButton=el('applyRoiClass');if(nextClassButton)nextClassButton.disabled=false;", html, fixed = TRUE))
  expect_false(grepl("if(menuName)menuName.value=name", html, fixed = TRUE))
  expect_false(grepl("roiClassSelects().forEach(s=>setSelectValue(s,cls))", html, fixed = TRUE))
  expect_false(grepl("activeRoiClass=cls;}", html, fixed = TRUE))
  expect_false(grepl("if(selectedRoi>=0)applySelectedRoiClass();", html, fixed = TRUE))
  expect_false(grepl("['applyRoiClass','selectionCardToggle'", html, fixed = TRUE))
  expect_false(grepl("['roiClassSelect','annotationClassSelect'].forEach(id=>setPreferenceInput(id,cls))", html, fixed = TRUE))
  expect_false(grepl("panelColor.onchange=()=>{if(selectedRoi>=0)applyAnnotationPanelMetadata();}", html, fixed = TRUE))
  expect_match(html, "brushRingFromPoints", fixed = TRUE)
  expect_match(html, "additiveBrushRingsFromPoints", fixed = TRUE)
  expect_match(html, "brushCapsuleRing", fixed = TRUE)
  expect_match(html, "brushGeometryPathPoints", fixed = TRUE)
  expect_match(html, "brushMaskGeometry", fixed = TRUE)
  expect_match(html, "brushProtectionForClass", fixed = TRUE)
  expect_match(html, "annotationProtectionRois", fixed = TRUE)
  expect_match(html, "sameLabelAnnotationEntries", fixed = TRUE)
  expect_match(html, "sameLabelTouchedAnnotationEntries", fixed = TRUE)
  expect_match(html, "brushGroupsTouchRoi", fixed = TRUE)
  expect_match(html, "unionBrushGroupsWithSameLabel", fixed = TRUE)
  expect_match(html, "removeMergedSameLabelAnnotations", fixed = TRUE)
  expect_match(html, "clipBrushGroupsAgainstAnnotations", fixed = TRUE)
  expect_match(html, "enforceRoiNonOverlap", fixed = TRUE)
  expect_match(html, "drawProtectedRoisOnMask", fixed = TRUE)
  expect_match(html, "protected_class_boundaries", fixed = TRUE)
  expect_match(html, "non_overlapping", fixed = TRUE)
  expect_match(html, "overlap_clipped", fixed = TRUE)
  expect_match(html, "same_label_merged", fixed = TRUE)
  expect_match(html, "roi_same_label_merged", fixed = TRUE)
  expect_match(html, "Overlapping annotation area clipped", fixed = TRUE)
  expect_match(html, "ROI merged with same label", fixed = TRUE)
  expect_match(html, "different-label overlap clipped", fixed = TRUE)
  expect_match(html, "maskContoursFromCanvas", fixed = TRUE)
  expect_match(html, "brushContourTolerance", fixed = TRUE)
  expect_match(html, "brushContourMaxPoints", fixed = TRUE)
  expect_match(html, "simplifyBrushContourRing", fixed = TRUE)
  expect_match(html, "drawBrushStrokeOnMask", fixed = TRUE)
  expect_match(html, "brush_mask_contour", fixed = TRUE)
  expect_match(html, "addRoiFromBrushRings", fixed = TRUE)
  expect_match(html, "addRoiFromBrushGroups", fixed = TRUE)
  expect_match(html, "brush_ring_count", fixed = TRUE)
  expect_match(html, "brushPointSpacing", fixed = TRUE)
  expect_match(html, "densifyBrushPoints", fixed = TRUE)
  expect_match(html, "steps=192", fixed = TRUE)
  expect_match(html, "radius*.04", fixed = TRUE)
  expect_match(html, "brushOperation", fixed = TRUE)
  expect_match(html, "brushTargetRoi", fixed = TRUE)
  expect_match(html, "brushClass=''", fixed = TRUE)
  expect_match(html, "brushClass=currentRoiClass()", fixed = TRUE)
  expect_match(html, "const pts=brushPoints.slice(),op=brushOperation,target=brushTargetRoi,cls=brushClass||currentRoiClass()", fixed = TRUE)
  expect_match(html, "brushProtectionForClass(cls,-1)", fixed = TRUE)
  expect_match(html, "addRoiFromBrushGroups(geometry.groups,'brush','Painted ROI',cls)", fixed = TRUE)
  expect_match(html, "brushTouchedSelection", fixed = TRUE)
  expect_match(html, "brushAdditiveSelection", fixed = TRUE)
  expect_match(html, "brushSelectionIsAdditive", fixed = TRUE)
  expect_match(html, "clearBrushSelection", fixed = TRUE)
  expect_match(html, "selectRoiForBrush", fixed = TRUE)
  expect_match(html, "bufferedBrushGeometry", fixed = TRUE)
  expect_match(html, "brushStrokeIntersectsRoi", fixed = TRUE)
  expect_match(html, "brushRingIntersectsRoiGroup", fixed = TRUE)
  expect_match(html, "updateBrushSelectionFromStroke", fixed = TRUE)
  expect_match(html, "boundsOverlap", fixed = TRUE)
  expect_match(html, "segmentsIntersect", fixed = TRUE)
  expect_match(html, "ringsHaveSegmentIntersection", fixed = TRUE)
  expect_match(html, "brush_selection_updated", fixed = TRUE)
  expect_match(html, "selected_indices", fixed = TRUE)
  expect_match(html, "extendSelectedRoiWithBrush", fixed = TRUE)
  expect_match(html, "subtractSelectedRoiWithBrush", fixed = TRUE)
  expect_match(html, "roi_brush_edited", fixed = TRUE)
  expect_match(html, "brushOperation==='subtract'", fixed = TRUE)
  expect_match(html, "brush-add", fixed = TRUE)
  expect_match(html, "brush-subtract", fixed = TRUE)
  expect_match(html, "cursor-blocked", fixed = TRUE)
  expect_match(html, "brushAltDown", fixed = TRUE)
  expect_match(html, "viewerIsMac", fixed = TRUE)
  expect_match(html, "brushSubtractModifier", fixed = TRUE)
  expect_match(html, "brushSubtractKeyEvent", fixed = TRUE)
  expect_match(html, "e.shiftKey||e.ctrlKey", fixed = TRUE)
  expect_match(html, "Alt on Windows/Linux or Command on Mac", fixed = TRUE)
  expect_match(html, "brushCursorState", fixed = TRUE)
  expect_match(html, "updateCursorFeedback", fixed = TRUE)
  expect_match(html, "drawBrushGlyph", fixed = TRUE)
  expect_match(html, "lastCanvasPointer", fixed = TRUE)
  expect_match(html, "finiteCanvasPoint", fixed = TRUE)
  expect_match(html, "brushPreviewCanvasPoint", fixed = TRUE)
  expect_match(html, "cursor:none", fixed = TRUE)
  expect_match(html, "toastStack", fixed = TRUE)
  expect_match(html, "showToast", fixed = TRUE)
  expect_match(html, "toastDisplayMessage", fixed = TRUE)
  expect_match(html, "Full message saved in History and Logs", fixed = TRUE)
  expect_match(html, "openViewerLogPanel", fixed = TRUE)
  expect_match(html, "overflow-wrap:anywhere", fixed = TRUE)
  expect_match(html, "dismissToast", fixed = TRUE)
  expect_match(html, "toast-success", fixed = TRUE)
  expect_match(html, "toast-actionable", fixed = TRUE)
  expect_match(html, "toastAction", fixed = TRUE)
  expect_match(html, "notifyAction", fixed = TRUE)
  expect_match(html, "Deleted '+deleteLabel+'.", fixed = TRUE)
  expect_match(html, "'Undo',()=>restoreAnnotationUndo()", fixed = TRUE)
  expect_false(grepl("<summary title=\"Background task progress and live R synchronization status\">Jobs / Sync</summary>", html, fixed = TRUE))
  expect_false(grepl("id=\"jobSummary\"", html, fixed = TRUE))
  expect_false(grepl("id=\"jobList\"", html, fixed = TRUE))
  expect_match(html, "jobSyncIndicator", fixed = TRUE)
  expect_match(html, "jobSyncLabel", fixed = TRUE)
  expect_match(html, "jobSyncDetail", fixed = TRUE)
  expect_match(html, "syncIndicator", fixed = TRUE)
  expect_match(html, "jobProgress", fixed = TRUE)
  expect_match(html, "jobProgressText", fixed = TRUE)
  expect_match(html, "jobMessage", fixed = TRUE)
  expect_match(html, "jobMeta", fixed = TRUE)
  expect_match(html, "jobError", fixed = TRUE)
  expect_match(html, "jobLogDetails", fixed = TRUE)
  expect_match(html, "jobCountsObject", fixed = TRUE)
  expect_match(html, "jobCountsText", fixed = TRUE)
  expect_match(html, "jobErrorText", fixed = TRUE)
  expect_match(html, "jobPrimaryStatus", fixed = TRUE)
  expect_match(html, "updateJobSyncIndicator", fixed = TRUE)
  expect_false(grepl("openJobSyncMenu", html, fixed = TRUE))
  expect_match(html, "viewerJobs", fixed = TRUE)
  expect_match(html, "upsertViewerJob", fixed = TRUE)
  expect_match(html, "updateViewerJob", fixed = TRUE)
  expect_match(html, "handleViewerJobs", fixed = TRUE)
  expect_match(html, "job_update", fixed = TRUE)
  expect_match(html, "Sync off", fixed = TRUE)
  expect_match(html, "#jobSyncIndicator{display:inline-flex;align-items:center;justify-content:flex-start;gap:6px;flex:0 0 158px;width:158px", fixed = TRUE)
  expect_match(html, "#jobSyncIndicator{flex-basis:118px;width:118px", fixed = TRUE)
  help_menu_pos <- regexpr("<summary title=\"Viewer guide and shortcuts\">Help</summary>", html, fixed = TRUE)
  sync_indicator_pos <- regexpr("id=\"jobSyncIndicator\"", html, fixed = TRUE)
  nav_dock_pos <- regexpr("<div id=\"navDock\"", html, fixed = TRUE)
  expect_gt(sync_indicator_pos[[1]], help_menu_pos[[1]])
  expect_lt(sync_indicator_pos[[1]], nav_dock_pos[[1]])
  expect_match(html, "pending", fixed = TRUE)
  expect_match(html, "running", fixed = TRUE)
  expect_match(html, "completed", fixed = TRUE)
  expect_match(html, "failed", fixed = TRUE)
  expect_match(html, "miniNavigator", fixed = TRUE)
  expect_match(html, "miniNavigatorCanvas", fixed = TRUE)
  expect_match(html, "miniNavigatorViewport", fixed = TRUE)
  expect_match(html, "miniNavigatorDensity", fixed = TRUE)
  expect_match(html, "navigatorImage", fixed = TRUE)
  expect_match(html, "miniNavigatorOverviewImage", fixed = TRUE)
  expect_match(html, "setNavigatorImageSource", fixed = TRUE)
  expect_match(html, "navigatorCacheBustedSource", fixed = TRUE)
  expect_match(html, "navigatorImageToken", fixed = TRUE)
  expect_match(html, "navigatorDisplayImage", fixed = TRUE)
  expect_match(html, "loader=new Image", fixed = TRUE)
  expect_match(html, "navigatorDisplayImage=loader", fixed = TRUE)
  expect_match(html, "projectItemUniqueKey", fixed = TRUE)
  expect_match(html, "__item_", fixed = TRUE)
  expect_match(html, "drawMiniNavigatorOverview", fixed = TRUE)
  expect_match(html, "drawMiniNavigator", fixed = TRUE)
  expect_match(html, "drawMiniNavigatorViewport", fixed = TRUE)
  expect_match(html, "drawMiniNavigatorMarkers", fixed = TRUE)
  expect_match(html, "drawMiniNavigatorRoiDensity", fixed = TRUE)
  expect_match(html, "drawMiniNavigatorLayerDensity", fixed = TRUE)
  expect_match(html, "navigatorDensitySummary", fixed = TRUE)
  expect_match(html, "bindMiniNavigator", fixed = TRUE)
  expect_match(html, "panMiniNavigatorTo", fixed = TRUE)
  expect_match(html, "annotationDirtyIndicator", fixed = TRUE)
  expect_match(html, "unsavedIndicator", fixed = TRUE)
  expect_match(html, "role=\"button\" tabindex=\"0\" aria-label=\"Save unsaved project changes\"", fixed = TRUE)
  expect_match(html, "Click to save the current project changes", fixed = TRUE)
  expect_match(html, "annotationsDirty", fixed = TRUE)
  expect_match(html, "markAnnotationsDirty", fixed = TRUE)
  expect_match(html, "markAnnotationsSaved", fixed = TRUE)
  expect_match(html, "updateAnnotationDirtyIndicator", fixed = TRUE)
  expect_match(html, "saveFromUnsavedIndicator", fixed = TRUE)
  expect_match(html, "bindUnsavedIndicator", fixed = TRUE)
  expect_match(html, "node.onclick=saveFromUnsavedIndicator", fixed = TRUE)
  expect_match(html, "bindUnsavedRefreshGuard", fixed = TRUE)
  expect_match(html, "beforeunload", fixed = TRUE)
  expect_match(html, "You have unsaved annotations or project changes", fixed = TRUE)
  expect_match(html, "event.returnValue=message", fixed = TRUE)
  expect_match(html, "saveProjectFile", fixed = TRUE)
  expect_match(html, "annotations:{dirty:!!(annotationsDirty||projectDirty)", fixed = TRUE)
  expect_match(html, "annotations_saved", fixed = TRUE)
  expect_match(html, "autosave_enabled", fixed = TRUE)
  expect_match(html, "startViewerAutosave", fixed = TRUE)
  expect_match(html, "autosave_tick", fixed = TRUE)
  expect_match(html, "commandPalette", fixed = TRUE)
  expect_match(html, "Command Palette", fixed = TRUE)
  expect_match(html, "Ctrl+K", fixed = TRUE)
  expect_match(html, "bindCommandPalette", fixed = TRUE)
  expect_match(html, "toggleCommandPalette", fixed = TRUE)
  expect_match(html, "commandPaletteDefinitions", fixed = TRUE)
  expect_match(html, "Export selected ROIs", fixed = TRUE)
  expect_false(grepl("Run StarDist", html, fixed = TRUE))
  expect_match(html, "Show tile grid", fixed = TRUE)
  expect_match(html, "Save project", fixed = TRUE)
  expect_match(html, "tileGridVisible", fixed = TRUE)
  expect_match(html, "drawTileGrid", fixed = TRUE)
  expect_match(html, "tile_grid_toggled", fixed = TRUE)
  expect_match(html, "project_save_requested", fixed = TRUE)
  expect_match(html, "Multi-view tissue display", fixed = TRUE)
  expect_match(html, "multiViewGrid", fixed = TRUE)
  expect_match(html, "multiView2", fixed = TRUE)
  expect_match(html, "multiView4", fixed = TRUE)
  expect_match(html, "multiView6", fixed = TRUE)
  expect_match(html, "multiViewCustomCount", fixed = TRUE)
  expect_match(html, "multiViewCustom", fixed = TRUE)
  expect_match(html, "multiViewPaneBlank", fixed = TRUE)
  expect_match(html, "Drag project images or sections onto a pane", fixed = TRUE)
  expect_match(html, "link zoom/pan", fixed = TRUE)
  expect_match(html, "Multi-view panes are independent by default", fixed = TRUE)
  expect_match(html, "Synchronize zoom and pan across all multi-view panes for this viewer session", fixed = TRUE)
  expect_match(html, "bindMultiViewControls", fixed = TRUE)
  expect_match(html, "setMultiViewLayout", fixed = TRUE)
  expect_match(html, "multiViewFocusPane(index,true)", fixed = TRUE)
  expect_match(html, "if(typeof updateMagnificationControls==='function')updateMagnificationControls();", fixed = TRUE)
  expect_match(html, "shortcutHelp", fixed = TRUE)
  expect_match(html, "Viewer Help", fixed = TRUE)
  expect_match(html, "shortcutHelpTabs", fixed = TRUE)
  expect_match(html, "role=\"tablist\"", fixed = TRUE)
  expect_match(html, "shortcutHelpTabKeyboard", fixed = TRUE)
  expect_match(html, "shortcutHelpTabFull", fixed = TRUE)
  expect_match(html, "Quick Recommendations", fixed = TRUE)
  expect_match(html, "Full Guide", fixed = TRUE)
  expect_match(html, "Keyboard Shortcuts", fixed = TRUE)
  pos_help_keyboard_menu <- regexpr("<button id=\"shortcutHelpKeyboard\"", html, fixed = TRUE)[[1]]
  pos_help_button <- regexpr("<button id=\"shortcutHelpButton\"", html, fixed = TRUE)[[1]]
  pos_help_quick_dialog <- regexpr("id=\"helpQuickRecommendations\"", html, fixed = TRUE)[[1]]
  pos_help_full_dialog <- regexpr("id=\"helpFullGuide\"", html, fixed = TRUE)[[1]]
  pos_help_shortcuts_dialog <- regexpr("id=\"helpKeyboardShortcuts\"", html, fixed = TRUE)[[1]]
  expect_true(all(c(
    pos_help_keyboard_menu,
    pos_help_button,
    pos_help_quick_dialog,
    pos_help_full_dialog,
    pos_help_shortcuts_dialog
  ) > 0))
  expect_lt(pos_help_keyboard_menu, pos_help_button)
  expect_lt(pos_help_shortcuts_dialog, pos_help_full_dialog)
  expect_lt(pos_help_full_dialog, pos_help_quick_dialog)
  expect_match(html, "helpQuickRecommendations", fixed = TRUE)
  expect_match(html, "helpFullGuide", fixed = TRUE)
  expect_match(html, "helpKeyboardShortcuts", fixed = TRUE)
  expect_match(html, "setShortcutHelpSection", fixed = TRUE)
  expect_match(html, "part.hidden=!show", fixed = TRUE)
  expect_match(html, "sectionId==='helpFullGuide'&&id==='helpQuickRecommendations'", fixed = TRUE)
  expect_match(html, "Keyboard shortcuts only", fixed = TRUE)
  expect_match(html, "Full viewer guide and quick recommendations", fixed = TRUE)
  expect_match(html, "openShortcutHelp('helpKeyboardShortcuts')", fixed = TRUE)
  expect_match(html, "openShortcutHelp('helpFullGuide')", fixed = TRUE)
  expect_match(html, "quickRecommendationList", fixed = TRUE)
  expect_match(html, "Open images", fixed = TRUE)
  expect_match(html, "Zoom and pan", fixed = TRUE)
  expect_match(html, "2, 4, or 6 side-by-side panes", fixed = TRUE)
  expect_match(html, "ROIs and annotations", fixed = TRUE)
  expect_match(html, "Troubleshooting", fixed = TRUE)
  expect_match(html, "Images or tiles do not load", fixed = TRUE)
  expect_match(html, "Slow rendering", fixed = TRUE)
  expect_match(html, "Saved outputs", fixed = TRUE)
  expect_match(html, "shortcutHelpButton", fixed = TRUE)
  expect_match(html, "bindShortcutHelp", fixed = TRUE)
  expect_match(html, "openShortcutHelp", fixed = TRUE)
  expect_match(html, "Press ? anytime to open the organized help dialog", fixed = TRUE)
  expect_match(html, "Pan mode", fixed = TRUE)
  expect_match(html, "Draw polygon ROI", fixed = TRUE)
  expect_match(html, "Brush annotation editing", fixed = TRUE)
  expect_match(html, "Edit ROI vertices or redraw smooth boundary curves", fixed = TRUE)
  expect_match(html, "Measure distance between two points", fixed = TRUE)
  expect_match(html, "Undo annotation, trajectory, registration, or closed-image edit", fixed = TRUE)
  expect_match(html, "Redo annotation, trajectory, or closed-image edit", fixed = TRUE)
  expect_match(html, "Ctrl+S", fixed = TRUE)
  expect_match(html, "Ctrl+I", fixed = TRUE)
  expect_match(html, "Ctrl+E", fixed = TRUE)
  expect_match(html, "shortcutImportGeojson", fixed = TRUE)
  expect_match(html, "ROI locked", fixed = TRUE)
  expect_match(html, "ROI saved", fixed = TRUE)
  expect_match(html, "GeoJSON exported", fixed = TRUE)
  expect_false(grepl("Run StarDist", html, fixed = TRUE))
  expect_match(html, "curveEditStroke", fixed = TRUE)
  expect_match(html, "nearestBoundarySegmentAtCanvas", fixed = TRUE)
  expect_match(html, "startCurveEditStroke", fixed = TRUE)
  expect_match(html, "addCurveEditPoint", fixed = TRUE)
  expect_match(html, "chaikinSmoothOpenPoints", fixed = TRUE)
  expect_match(html, "catmullRomSmoothOpenPoints", fixed = TRUE)
  expect_match(html, "smoothCurveEditPoints", fixed = TRUE)
  expect_match(html, "replaceRingForwardArc", fixed = TRUE)
  expect_match(html, "finishCurveEditStroke", fixed = TRUE)
  expect_match(html, "smooth_curve_boundary", fixed = TRUE)
  expect_match(html, "drawCurveEditPreview", fixed = TRUE)
  expect_match(html, "drawSmoothOpenCurvePath", fixed = TRUE)
  expect_match(html, "roi_curve_edited", fixed = TRUE)
  expect_match(html, "startCurveEditStroke(lastCanvasPointer,lastPointer,slideToCanvas)", fixed = TRUE)
  expect_match(html, "roiCompositeGeometry", fixed = TRUE)
  expect_match(html, "clippedHoleForGroup", fixed = TRUE)
  expect_match(html, "roiContainsPoint", fixed = TRUE)
  expect_match(html, "annotationUndo", fixed = TRUE)
  expect_match(html, "annotationRedo", fixed = TRUE)
  expect_match(html, "undoAnnotation", fixed = TRUE)
  expect_match(html, "state&&state.project&&typeof restoreProjectUndoSnapshot", fixed = TRUE)
  expect_match(html, "current.project=projectUndoSnapshot('project_redo')", fixed = TRUE)
  expect_match(html, "current.project=projectUndoSnapshot('project_undo')", fixed = TRUE)
  expect_match(html, "redoAnnotation", fixed = TRUE)
  expect_match(html, "pushAnnotationUndo", fixed = TRUE)
  expect_match(html, "restoreAnnotationUndo", fixed = TRUE)
  expect_match(html, "restoreAnnotationRedo", fixed = TRUE)
  expect_match(html, "pushHistory", fixed = TRUE)
  expect_match(html, "stack.length>10", fixed = TRUE)
  expect_match(html, "trajectories:JSON.parse", fixed = TRUE)
  expect_match(html, "selectedTrajectory:typeof selectedTrajectory", fixed = TRUE)
  expect_match(html, "trajectory_count:typeof trajectories", fixed = TRUE)
  expect_match(html, "selectedMeasure=-1", fixed = TRUE)
  expect_match(html, "selectedLayerIndex=-1,selectedLayerItemIndex=-1", fixed = TRUE)
  expect_match(html, "clearSelectedTrajectory", fixed = TRUE)
  expect_match(html, "clearSelectedAnnotation", fixed = TRUE)
  expect_match(html, "clearSelectedLayerObject", fixed = TRUE)
  expect_match(html, "clearSelectedMeasure", fixed = TRUE)
  expect_match(html, "selectAnnotation", fixed = TRUE)
  expect_match(html, "selectTrajectory", fixed = TRUE)
  expect_match(html, "selectLayerObject", fixed = TRUE)
  expect_match(html, "selectMeasure", fixed = TRUE)
  expect_match(html, "selectedObjectPayload", fixed = TRUE)
  expect_match(html, "enforceSingleObjectSelection", fixed = TRUE)
  expect_match(html, "clearSelectionAndPan", fixed = TRUE)
  expect_match(html, "selectObjectAtPoint", fixed = TRUE)
  expect_match(html, "function selectObjectAtPoint(p,prefer='trajectory')", fixed = TRUE)
  expect_match(html, "layerObjectAt", fixed = TRUE)
  expect_match(html, "layerPointHit", fixed = TRUE)
  expect_match(html, "measurementAt", fixed = TRUE)
  expect_match(html, "e.preventDefault();clearSelectionAndPan();", fixed = TRUE)
  expect_match(html, "trajectoryAt", fixed = TRUE)
  expect_match(html, "dragStartX=0,dragStartY=0,dragMoved=false", fixed = TRUE)
  expect_match(html, "wasClick&&mode==='pan'", fixed = TRUE)
  expect_match(html, "selectObjectAtPoint(p,'trajectory')", fixed = TRUE)
  expect_match(html, "selectTrajectory(i,true);zoomToTrajectory", fixed = TRUE)
  expect_match(html, "selectTrajectory(trajectories.length-1,true)", fixed = TRUE)
  expect_match(html, "pushAnnotationUndo('trajectory_added')", fixed = TRUE)
  expect_match(html, "pushAnnotationUndo('trajectories_cleared')", fixed = TRUE)
  expect_match(html, "mode==='trajectory'&&trajectoryDraft.length", fixed = TRUE)
  expect_match(html, "seuratRegistrationActive()&&restoreSeuratRegistrationUndo()", fixed = TRUE)
  expect_match(html, "seuratRegistrationActive()&&restoreSeuratRegistrationRedo()", fixed = TRUE)
  expect_match(html, "annotation_undo", fixed = TRUE)
  expect_match(html, "annotation_redo", fixed = TRUE)
  expect_match(html, "Nothing to undo", fixed = TRUE)
  expect_match(html, "Nothing to redo", fixed = TRUE)
  expect_match(html, "smoothSelectedRoi", fixed = TRUE)
  expect_false(grepl("simplifySelectedRoi", html, fixed = TRUE))
  expect_match(html, "fillSelectedRoiHoles", fixed = TRUE)
  expect_match(html, "mergeSelectedAnnotations", fixed = TRUE)
  expect_match(html, "Only annotations with the same class can be merged", fixed = TRUE)
  expect_match(html, "splitSelectedAnnotation", fixed = TRUE)
  expect_match(html, "chaikinSmoothPoints", fixed = TRUE)
  expect_match(html, "rdpSimplify", fixed = TRUE)
  expect_match(html, "checkedAnnotationIndices", fixed = TRUE)
  expect_match(html, "drawBrushPreview", fixed = TRUE)
  expect_match(html, "findVertexAt", fixed = TRUE)
  expect_match(html, "moveActiveVertex", fixed = TRUE)
  expect_match(html, "insertVertexAt", fixed = TRUE)
  expect_match(html, "deleteSelectedVertex", fixed = TRUE)
  expect_match(html, "deleteRoi", fixed = TRUE)
  expect_match(html, "deleteSelectedObject", fixed = TRUE)
  expect_match(html, "deleteSelectedLayerObject", fixed = TRUE)
  expect_match(html, "deleteSelectedMeasure", fixed = TRUE)
  expect_match(html, "layer_object_deleted", fixed = TRUE)
  expect_match(html, "measurement_deleted", fixed = TRUE)
  expect_match(html, "deleteSelectedTrajectory", fixed = TRUE)
  expect_match(html, "trajectory_deleted", fixed = TRUE)
  expect_match(html, "Delete the selected annotation, trajectory, marker, layer object, or measurement", fixed = TRUE)
  expect_match(html, "Only one annotation, trajectory, marker, layer object, or measurement is selected at a time.", fixed = TRUE)
  expect_match(html, "Click a saved trajectory path to select it, then press Delete", fixed = TRUE)
  expect_match(html, "e.preventDefault();deleteSelectedObject();return;", fixed = TRUE)
  expect_match(html, "Delete selected", fixed = TRUE)
  expect_false(grepl("StarDist segmentation", html, fixed = TRUE))
  expect_false(grepl("id=\"exportSelectedRoi\"", html, fixed = TRUE))
  expect_false(grepl("id=\"startSegmentation\"", html, fixed = TRUE))
  expect_false(grepl("Run segmentation", html, fixed = TRUE))
  expect_false(grepl("id=\"loadSegmentation\"", html, fixed = TRUE))
  expect_false(grepl("id=\"loadSegmentationCsv\"", html, fixed = TRUE))
  expect_false(grepl("id=\"segmentationTableFile\"", html, fixed = TRUE))
  expect_false(grepl("id=\"segLocalCoords\"", html, fixed = TRUE))
  expect_false(grepl("id=\"segCellRadius\"", html, fixed = TRUE))
  expect_false(grepl("run_stardist", html, fixed = TRUE))
  expect_false(grepl("<summary title=\"Visualize GrandQC artifact GeoJSON annotations\">Artifacts</summary>", html, fixed = TRUE))
  expect_false(grepl("GrandQC GeoJSON</div>", html, fixed = TRUE))
  expect_match(html, "grandqcLoadAll", fixed = TRUE)
  expect_match(html, "loadAllGrandqcGeojsons", fixed = TRUE)
  expect_match(html, "clearGrandqcRois", fixed = TRUE)
  expect_match(html, "artifactPayload", fixed = TRUE)
  expect_match(html, "drawArtifactOverlays", fixed = TRUE)
  expect_match(html, "bindArtifactControls", fixed = TRUE)
  expect_match(html, "Load GrandQC artifacts", fixed = TRUE)
  expect_false(grepl("detectVisibleArtifacts", html, fixed = TRUE))
  expect_false(grepl("artifactPixelMetrics", html, fixed = TRUE))
  expect_match(html, "Measure", fixed = TRUE)
  expect_match(html, "toolMeasure", fixed = TRUE)
  expect_match(html, "clearMeasures", fixed = TRUE)
  expect_match(html, "setMode('measure');closeMenuAfterToolAction", fixed = TRUE)
  expect_match(html, "measureSummary", fixed = TRUE)
  expect_match(html, "measureList", fixed = TRUE)
  expect_match(html, "measurePixelSize", fixed = TRUE)
  expect_match(html, "measurementRecord", fixed = TRUE)
  expect_match(html, "drawMeasurements", fixed = TRUE)
  expect_match(html, "bindMeasureControls", fixed = TRUE)
  expect_match(html, "Trajectories", fixed = TRUE)
  expect_match(html, "toolTrajectory", fixed = TRUE)
  expect_match(html, "finishTrajectory", fixed = TRUE)
  expect_match(html, "finishTrajectory(true);closeMenuAfterToolAction", fixed = TRUE)
  expect_match(html, "trajectoryHelp", fixed = TRUE)
  expect_match(html, "showTrajectoryHelp", fixed = TRUE)
  expect_match(html, "trajectoryProfileHelp", fixed = TRUE)
  expect_match(html, "showTrajectoryProfileHelp", fixed = TRUE)
  expect_match(html, "proximityHelp", fixed = TRUE)
  expect_match(html, "showProximityHelp", fixed = TRUE)
  expect_match(html, "saved; trajectory drawing off", fixed = TRUE)
  expect_match(html, "if(e.detail===1)addTrajectoryPoint", fixed = TRUE)
  expect_match(html, "returns to pan mode", fixed = TRUE)
  expect_false(grepl("<div class=\"menuHint\">Click points on the slide to sketch a trajectory.", html, fixed = TRUE))
  expect_false(grepl("id=\"trajectoryProfileSummary\" class=\"menuHint\">Select a trajectory", html, fixed = TRUE))
  expect_false(grepl("id=\"proximitySummary\" class=\"menuHint\">Select query", html, fixed = TRUE))
  expect_match(html, "function trajectoryResolution(){return 200;}", fixed = TRUE)
  expect_false(grepl("id=\"trajectoryResolution\"", html, fixed = TRUE))
  expect_false(grepl("trajectoryResolutionValue", html, fixed = TRUE))
  expect_match(html, "trajectoryAreaWidth", fixed = TRUE)
  expect_false(grepl("id=\"trajectoryAreaRoi\"", html, fixed = TRUE))
  expect_false(grepl("id:'trajectory_area'", html, fixed = TRUE))
  expect_false(grepl("label:'Create trajectory area'", html, fixed = TRUE))
  expect_match(html, "editTrajectoryArea", fixed = TRUE)
  expect_match(html, "Edit border", fixed = TRUE)
  expect_match(html, "Trajectory border ready for editing", fixed = TRUE)
  expect_match(html, "canEditBorder=trajectoryDraft.length>=2", fixed = TRUE)
  expect_match(html, "Use Edit border first", fixed = TRUE)
  expect_match(html, "updateTrajectoryArea", fixed = TRUE)
  expect_match(html, "selectTrajectoryAreaRoi", fixed = TRUE)
  expect_match(html, "updateTrajectoryAreaRoi", fixed = TRUE)
  expect_match(html, "createTrajectoryAreaRoi", fixed = TRUE)
  expect_match(html, "applyTrajectoryAreaGeometry", fixed = TRUE)
  expect_match(html, "trajectoryAreaGeometry", fixed = TRUE)
  expect_match(html, "drag a border dot and nearby dots move smoothly", fixed = TRUE)
  expect_match(html, "flat-ended trajectory width preview", fixed = TRUE)
  expect_match(html, "trajectoryFlatCapRing", fixed = TRUE)
  expect_match(html, "trajectory_flat_caps", fixed = TRUE)
  expect_match(html, "isTrajectoryAreaRoi", fixed = TRUE)
  expect_match(html, "prepareVertexDrag", fixed = TRUE)
  expect_match(html, "softMoveTrajectoryBorderVertex", fixed = TRUE)
  expect_match(html, "trajectory_border_vertex_moved", fixed = TRUE)
  expect_match(html, "trajectoryPayload", fixed = TRUE)
  expect_match(html, "drawTrajectories", fixed = TRUE)
  expect_match(html, "bindTrajectoryControls", fixed = TRUE)
  expect_match(html, "Gradient profile", fixed = TRUE)
  expect_match(html, "trajectoryProfileSource", fixed = TRUE)
  expect_match(html, "trajectoryProfileFeature", fixed = TRUE)
  expect_match(html, "runTrajectoryProfile", fixed = TRUE)
  expect_match(html, "trajectoryProfileResultRows", fixed = TRUE)
  expect_match(html, "trajectory_profile_finished", fixed = TRUE)
  expect_match(html, "viewer$get_trajectory_profile()", fixed = TRUE)
  expect_match(html, "Project", fixed = TRUE)
  expect_false(grepl("projectOpenPanel", html, fixed = TRUE))
  expect_match(html, "projectPanelToggle", fixed = TRUE)
  expect_match(html, "annotationPanelToggle", fixed = TRUE)
  expect_match(html, "Annotation panel", fixed = TRUE)
  expect_match(html, "openRoiPanel", fixed = TRUE)
  expect_match(html, "['layersToggle','annotationPanelToggle','layerPanelToggle']", fixed = TRUE)
  expect_match(html, "if(projectPanelIsClosed())openProjectPanel();else closeProjectPanel();", fixed = TRUE)
  expect_match(html, "projectOpenImage", fixed = TRUE)
  expect_match(html, "Add image", fixed = TRUE)
  expect_match(html, "add_project_image", fixed = TRUE)
  expect_match(html, "\"managed_analysis_project\":false", fixed = TRUE)
  expect_match(html, "item.id!=='add_project_image'", fixed = TRUE)
  expect_match(html, "projectSaveFile", fixed = TRUE)
  expect_match(html, "projectOpenFile", fixed = TRUE)
  expect_match(html, "projectImageFile", fixed = TRUE)
  expect_match(html, "multiple", fixed = TRUE)
  expect_match(html, ".czi", fixed = TRUE)
  expect_match(html, ".svs", fixed = TRUE)
  expect_match(html, ".ndpi", fixed = TRUE)
  expect_match(html, ".ome.tiff", fixed = TRUE)
  expect_match(html, ".qptiff", fixed = TRUE)
  expect_match(html, "addProjectFileReference", fixed = TRUE)
  expect_match(html, "projectBrowserReadableExtension", fixed = TRUE)
  expect_match(html, "browser-reference", fixed = TRUE)
  expect_match(html, "needs backend", fixed = TRUE)
  expect_match(html, "CZI, SVS, NDPI and OME-TIFF", fixed = TRUE)
  expect_match(html, "without loading the whole file into memory", fixed = TRUE)
  expect_match(html, "projectFile", fixed = TRUE)
  expect_match(html, "openProjectPanel", fixed = TRUE)
  expect_match(html, "ensureProjectWorkspaceVisible", fixed = TRUE)
  expect_match(html, "ensureProjectWorkspaceVisible({preserve_state:true})", fixed = TRUE)
  expect_match(html, "setProjectPanelClosed(closed,save=true)", fixed = TRUE)
  expect_match(html, "setProjectPanelMinimized(minimized,save=true)", fixed = TRUE)
  expect_match(html, "loadProjectImageFile", fixed = TRUE)
  expect_match(html, "loadProjectImageFiles", fixed = TRUE)
  expect_match(html, "moveProjectItem", fixed = TRUE)
  expect_match(html, "bindProjectItemDrag", fixed = TRUE)
  expect_match(html, "projectDragIndex", fixed = TRUE)
  expect_match(html, "project_image_reordered", fixed = TRUE)
  expect_match(html, "Drag images to reorder", fixed = TRUE)
  expect_match(html, "projectItemClose", fixed = TRUE)
  expect_match(html, "removeProjectItem", fixed = TRUE)
  expect_match(html, "project_image_closed", fixed = TRUE)
  expect_match(html, "projectUndoSnapshot", fixed = TRUE)
  expect_match(html, "restoreProjectUndoSnapshot", fixed = TRUE)
  expect_match(html, "undoSnapshot.project=projectUndoSnapshot('project_image_closed')", fixed = TRUE)
  expect_match(html, "Restored closed image", fixed = TRUE)
  expect_match(html, "Press Ctrl+Z to undo", fixed = TRUE)
  expect_match(html, "Close this project image", fixed = TRUE)
  expect_match(html, "saveProjectFile", fixed = TRUE)
  expect_match(html, "openProjectFile", fixed = TRUE)
  expect_match(html, "projectBrowserSnapshot", fixed = TRUE)
  expect_match(html, "projectAnnotationSetsFull", fixed = TRUE)
  expect_match(html, "wsiTools-viewer-project", fixed = TRUE)
  expect_match(html, "projectHasUnsavedChanges", fixed = TRUE)
  expect_match(html, "confirmProjectReplacement", fixed = TRUE)
  expect_match(html, "You have an unsaved project. Do you want to save it before", fixed = TRUE)
  expect_match(html, "Open the new project without saving current changes?", fixed = TRUE)
  expect_match(html, "const control=e.currentTarget;if(!(await confirmProjectReplacement('opening a new project')))return;projectFile.value='';projectFile.click();closeContainingToolMenu(control);", fixed = TRUE)
  expect_match(html, "trajectory_count", fixed = TRUE)
  expect_match(html, "addProjectImageDataUri", fixed = TRUE)
  expect_match(html, "project_image_added", fixed = TRUE)
  expect_false(grepl("Non-destructive image display transforms", html, fixed = TRUE))
  expect_match(html, "rotateImageLeft", fixed = TRUE)
  expect_match(html, "rotateImageRight", fixed = TRUE)
  expect_match(html, "flipImageHorizontal", fixed = TRUE)
  expect_match(html, "flipImageVertical", fixed = TRUE)
  expect_match(html, "imageTransformButton", fixed = TRUE)
  expect_match(html, "imageTransformRefreshing .openseadragon-canvas canvas", fixed = TRUE)
  expect_match(html, "navIcon", fixed = TRUE)
  expect_false(grepl("id=\"resetImageTransform\"", html, fixed = TRUE))
  expect_match(html, "imageTransformPayload", fixed = TRUE)
  expect_match(html, "imageTransformHasDisplayTransform", fixed = TRUE)
  expect_match(html, "refreshProgressivePreviewForImageTransform", fixed = TRUE)
  expect_match(html, "clearOpenSeadragonTransformArtifacts", fixed = TRUE)
  expect_match(html, "setImageTransformRefreshing", fixed = TRUE)
  expect_match(html, "scheduleOpenSeadragonTransformCleanup", fixed = TRUE)
  expect_match(html, "viewerEl.querySelectorAll('canvas')", fixed = TRUE)
  expect_match(html, "viewerEl.querySelectorAll('.openseadragon-canvas canvas')", fixed = TRUE)
  expect_match(html, "osdViewer.viewport.stop", fixed = TRUE)
  expect_match(html, "osdViewer.viewport.setRotation(t.rotation,true)", fixed = TRUE)
  expect_match(html, "osdViewer.viewport.setFlip(t.flip)", fixed = TRUE)
  expect_match(html, "requestAnimationFrame(()=>setImageTransformRefreshing(false))", fixed = TRUE)
  expect_match(html, "baseImageDirty=true", fixed = TRUE)
  expect_match(html, "stainOverlayCanvas", fixed = TRUE)
  expect_match(html, "imageTransformHasDisplayTransform()){viewerEl.style.backgroundImage=''", fixed = TRUE)
  expect_match(html, "drawTransformedImage", fixed = TRUE)
  expect_match(html, "osdDisplayPixelForOverlayInRect", fixed = TRUE)
  expect_match(html, "osdDisplayPixelForOverlay", fixed = TRUE)
  expect_match(html, "overlayPixelForOsdDisplay", fixed = TRUE)
  expect_match(html, "const q=slideToViewImagePoint(p);return {x:offsetX+q.x*scale,y:offsetY+q.y*scale};", fixed = TRUE)
  expect_match(html, "multiViewOverlayPixelForOsdDisplay({x:px.x,y:px.y},pane)", fixed = TRUE)
  expect_match(html, "multiViewOverlayPixelForOsdDisplay({x:px,y:py},pane)", fixed = TRUE)
  expect_match(html, "multiViewOverlayPixelForOsdDisplay({x:xy[0],y:xy[1]},pane)", fixed = TRUE)
  expect_match(html, "multiViewOverlayPixelForOsdDisplay({x:e.clientX-rect.left,y:e.clientY-rect.top},pane)", fixed = TRUE)
  expect_match(html, "annotationSpotLayer", fixed = TRUE)
  expect_match(html, "source_type||'')==='seurat_spots'", fixed = TRUE)
  expect_match(html, "spatialPointLayer(layer)?2.25:1", fixed = TRUE)
  expect_match(html, "zoom>=5)return 1", fixed = TRUE)
  expect_match(html, "zoom>=2)return 2", fixed = TRUE)
  expect_match(html, "pointZoom>=5)?1", fixed = TRUE)
  expect_match(html, "pointZoom>=2)?2", fixed = TRUE)
  expect_match(html, "bindImageTransformControls", fixed = TRUE)
  expect_match(html, "beginScreenshotMode", fixed = TRUE)
  expect_match(html, "drawScreenshotSelection", fixed = TRUE)
  expect_match(html, "screenshotFormat", fixed = TRUE)
  expect_match(html, "openScreenshotDialog", fixed = TRUE)
  expect_match(html, "screenshotOptionsFromDialog", fixed = TRUE)
  expect_match(html, "screenshotRenderOptions", fixed = TRUE)
  expect_match(html, "screenshotIncludeComponent", fixed = TRUE)
  expect_match(html, "saveScreenshotFromDialog", fixed = TRUE)
  expect_match(html, "screenshotIncludeMeasurements", fixed = TRUE)
  expect_match(html, "screenshotIncludeTrajectories", fixed = TRUE)
  expect_match(html, "screenshotIncludeTileGrid", fixed = TRUE)
  expect_match(html, "screenshotIncludeArtifacts", fixed = TRUE)
  expect_match(html, "Choose format, filename, and visible content", fixed = TRUE)
  expect_false(grepl("id=\"screenshotFormat\"", html, fixed = TRUE))
  expect_false(grepl("<div class=\"menuTitle\">Screenshot</div>", html, fixed = TRUE))
  expect_false(grepl("id=\"screenshotSummary\"", html, fixed = TRUE))
  expect_match(html, "saveScreenshot", fixed = TRUE)
  expect_match(html, "saveScreenshotPng", fixed = TRUE)
  expect_match(html, "jpegBlobFromCanvas", fixed = TRUE)
  expect_match(html, "svgBlobFromCanvas", fixed = TRUE)
  expect_match(html, "pdfBlobFromCanvas", fixed = TRUE)
  expect_match(html, "application/pdf", fixed = TRUE)
  expect_match(html, "image/jpeg", fixed = TRUE)
  expect_match(html, "screenshotFormatExtension", fixed = TRUE)
  expect_match(html, "Save As dialog", fixed = TRUE)
  expect_match(html, "choose the folder and filename", fixed = TRUE)
  expect_match(html, "screenshotBlobFromCanvas", fixed = TRUE)
  expect_match(html, "screenshotOsdInternalCanvases", fixed = TRUE)
  expect_match(html, "screenshotBaseElements", fixed = TRUE)
  expect_match(html, "drawPreviewBaseIntoScreenshot", fixed = TRUE)
  expect_match(html, "_wsiScreenshotBaseIncluded", fixed = TRUE)
  expect_match(html, "_wsiScreenshotPreviewFallback", fixed = TRUE)
  expect_match(html, "using preview-resolution tissue plus overlays", fixed = TRUE)
  expect_match(html, "tissue image could not be captured", fixed = TRUE)
  expect_match(html, "canvasIsReadable", fixed = TRUE)
  expect_match(html, "without base image", fixed = TRUE)
  expect_match(html, "image/png", fixed = TRUE)
  expect_match(html, "image/svg+xml", fixed = TRUE)
  expect_match(html, ".svg", fixed = TRUE)
  expect_match(html, "Screenshot saved as '+label", fixed = TRUE)
  expect_match(html, "exportImageRegion", fixed = TRUE)
  expect_match(html, "imageExportUrl", fixed = TRUE)
  expect_match(html, "exportViewTiff", fixed = TRUE)
  expect_match(html, "exportSelectedRoiTiff", fixed = TRUE)
  expect_false(grepl("id=\"exportViewTiff\"", html, fixed = TRUE))
  expect_false(grepl("Save view TIFF", html, fixed = TRUE))
  expect_false(grepl("<div class=\"menuTitle\">Image export</div>", html, fixed = TRUE))
  expect_false(grepl("id=\"imageExportDir\"", html, fixed = TRUE))
  expect_false(grepl("placeholder=\"R working directory\"", html, fixed = TRUE))
  expect_false(grepl("id=\"imageExportSummary\"", html, fixed = TRUE))
  expect_false(grepl("id=\"exportSelectedRoiTiff\"", html, fixed = TRUE))
  expect_false(grepl("Save ROI TIFF", html, fixed = TRUE))
  expect_match(html, "TIFF export needs a live R viewer", fixed = TRUE)
  expect_match(html, "mpp", fixed = TRUE)
  expect_match(html, "id=\"scaleBar\"", fixed = TRUE)
  expect_match(html, "Micron scale bar", fixed = TRUE)
  expect_match(html, "updateScaleBar", fixed = TRUE)
  expect_match(html, "showUnavailableScaleBar", fixed = TRUE)
  expect_match(html, "scale unavailable", fixed = TRUE)
  expect_match(html, "multiViewDrawScaleBar", fixed = TRUE)
  expect_match(html, "multiViewPaneMpp", fixed = TRUE)
  expect_match(html, "multiViewMppFromValue", fixed = TRUE)
  expect_match(html, "multiViewCanvasUnitScale(pane)", fixed = TRUE)
  expect_match(html, "deltaPointsFromPixels(new OpenSeadragon.Point(1,0),true)", fixed = TRUE)
  expect_match(html, "refreshMultiViewOverlaysSoon", fixed = TRUE)
  expect_match(html, "window.requestAnimationFrame(requestMultiViewOverlayDraw)", fixed = TRUE)
  expect_match(html, "multiViewPaneSpatialOverlay", fixed = TRUE)
  expect_match(html, "multiViewLayersForPane", fixed = TRUE)
  expect_match(html, "const paneLayers=multiViewLayersForPane(pane)", fixed = TRUE)
  expect_match(html, "multiViewDrawSpatialWebglLayers", fixed = TRUE)
  expect_match(html, "multiViewSpatialLayerData", fixed = TRUE)
  expect_match(html, "webglLayers.has(layer)", fixed = TRUE)
  expect_match(html, "multiViewDrawScaleBar(pack.ctx,pane,pack.rect)", fixed = TRUE)
  expect_match(html, "if(typeof multiViewLayout!=='undefined'&&multiViewLayout>1){bar.style.display='none'", fixed = TRUE)
  expect_match(html, "if(typeof updateScaleBar==='function')updateScaleBar();if(typeof requestDraw==='function')requestDraw();", fixed = TRUE)
  expect_match(html, "normalizeProjectMpp", fixed = TRUE)
  expect_match(html, "applyProjectScaleMetadata", fixed = TRUE)
  expect_match(html, "cfg.mpp=projectMppValue", fixed = TRUE)
  expect_match(html, "#scaleBar.unavailable{opacity:.68;}", fixed = TRUE)
  expect_false(grepl("#scaleBar.unavailable{display:none;}", html, fixed = TRUE))
  expect_match(html, "niceScaleLength", fixed = TRUE)
  expect_match(html, "Magnification", fixed = TRUE)
  expect_match(html, "magnificationSummary", fixed = TRUE)
  expect_match(html, "magnification5", fixed = TRUE)
  expect_match(html, "magnification10", fixed = TRUE)
  expect_match(html, "magnification20", fixed = TRUE)
  expect_match(html, "magnification40", fixed = TRUE)
  expect_match(html, "magnificationInitial", fixed = TRUE)
  expect_match(html, "currentMagnification", fixed = TRUE)
  expect_match(html, "pane&&typeof multiViewCanvasUnitScale==='function'?multiViewCanvasUnitScale(pane):scaleBarSlideUnitScale()", fixed = TRUE)
  expect_match(html, "setMagnificationPower", fixed = TRUE)
  expect_match(html, "resetInitialMagnification", fixed = TRUE)
  expect_match(html, "Returned to initial magnification", fixed = TRUE)
  expect_match(html, "bindMagnificationControls", fixed = TRUE)
  expect_match(html, "defaultObjectivePower", fixed = TRUE)
  expect_match(html, "objectivePowerEstimated", fixed = TRUE)
  expect_match(html, "40x full-resolution fallback", fixed = TRUE)
  expect_false(grepl("Magnification unavailable: no MPP/objective metadata.", html, fixed = TRUE))
  expect_match(html, "objective_power", fixed = TRUE)
  expect_match(html, "\\u00b5m", fixed = TRUE)
  expect_match(html, "saveGeojson", fixed = TRUE)
  expect_match(html, "annotationExportDialog", fixed = TRUE)
  expect_match(html, "annotationExportFormat", fixed = TRUE)
  expect_match(html, "annotationExportScope", fixed = TRUE)
  expect_match(html, "Save annotations", fixed = TRUE)
  expect_match(html, "annotationExportDialogDownload", fixed = TRUE)
  expect_match(html, "Choose location and save", fixed = TRUE)
  expect_match(html, "aria-label=\"Choose annotation save location\"", fixed = TRUE)
  expect_match(html, "aria-label=\"Download annotations using the browser download folder\"", fixed = TRUE)
  expect_match(html, "GeoJSON</option><option value=\"json\">JSON</option><option value=\"csv\">CSV summary", fixed = TRUE)
  expect_match(html, "All exportable annotations", fixed = TRUE)
  expect_match(html, "Selected or checked annotations", fixed = TRUE)
  expect_match(html, "showSaveFilePicker", fixed = TRUE)
  expect_match(html, "openAnnotationExportDialog('all')", fixed = TRUE)
  expect_match(html, "openAnnotationExportDialog('selected')", fixed = TRUE)
  expect_match(html, "saveAnnotationExportFromDialog", fixed = TRUE)
  expect_match(html, "saveAnnotationExportBlob", fixed = TRUE)
  expect_match(html, "save_mode", fixed = TRUE)
  expect_match(html, "saveAnnotationSpotsCsv", fixed = TRUE)
  expect_match(html, "annotationSpotAssociations", fixed = TRUE)
  expect_match(html, "annotation_spots", fixed = TRUE)
  expect_match(html, "FeatureCollection", fixed = TRUE)
  expect_match(html, "normaliseSlidePoint", fixed = TRUE)
  expect_match(html, "normaliseGeojsonGeometry", fixed = TRUE)
  expect_match(html, "normaliseSlideRingCoordinates", fixed = TRUE)
  expect_match(html, "coordinate_space:'level0_slide_pixels'", fixed = TRUE)
  expect_match(html, "display_transform_applied:false", fixed = TRUE)
  expect_match(html, "geometry=normaliseGeojsonGeometry({type:geometryType(roi),coordinates:roi.coordinates})", fixed = TRUE)
  expect_match(html, "if(isDrawable(roi)){geometry=roiCompositeGeometry(roi);}", fixed = TRUE)
  expect_false(grepl("roi.edited||roi.drawn||roi.brushed||roi.brush_edited", html, fixed = TRUE))
  expect_false(grepl("id=\"crosshairToggle\"", html, fixed = TRUE))
  expect_false(grepl("Copy XY", html, fixed = TRUE))
  expect_false(grepl("copyCoord", html, fixed = TRUE))
  expect_match(html, "GeoJSON Geometries", fixed = TRUE)
  expect_match(html, "#workspacePanel{position:fixed;left:12px;top:72px", fixed = TRUE)
  expect_match(html, "workspaceResizeHandle", fixed = TRUE)
  expect_match(html, "Drag to resize the left panels", fixed = TRUE)
  expect_match(html, "startWorkspacePanelResize", fixed = TRUE)
  expect_match(html, "setWorkspacePanelWidth", fixed = TRUE)
  expect_match(html, "workspacePanelResizing", fixed = TRUE)
  expect_match(html, "width:Math.round(rect.width)", fixed = TRUE)
  expect_match(html, "panelPrefs.width", fixed = TRUE)
  expect_match(html, "panelPrefs.open", fixed = TRUE)
  expect_match(html, "setRoiPanelOpen(panelPrefs.open,{save:false})", fixed = TRUE)
  expect_match(html, "setRoiPanelMinimized(panelPrefs.minimized,false)", fixed = TRUE)
  expect_match(html, "function workspacePanelBaseTop(){return 72;}", fixed = TRUE)
  expect_match(html, "function workspacePanelSafeTop(){const base=workspacePanelBaseTop()", fixed = TRUE)
  expect_match(html, "Math.max(base,barBottom)", fixed = TRUE)
  expect_match(html, "sidePanelResizeHandle", fixed = TRUE)
  expect_match(html, "Drag to resize Project vertically", fixed = TRUE)
  expect_match(html, "#projectPanel{display:flex;flex-direction:column", fixed = TRUE)
  expect_match(html, "#projectPanelBody{padding-top:4px;padding-bottom:12px;overflow:auto", fixed = TRUE)
  expect_match(html, "startSidePanelResize", fixed = TRUE)
  expect_match(html, "setSidePanelHeight", fixed = TRUE)
  expect_match(html, "project_height", fixed = TRUE)
  expect_match(html, "roi_height", fixed = TRUE)
  expect_match(html, "history_height", fixed = TRUE)
  expect_match(html, "--wsi-side-panel-bg:rgba(18,18,18,.94);", fixed = TRUE)
  expect_match(html, ".projectPanel{margin:0;padding:8px 10px;border:1px solid rgba(255,255,255,.16);border-radius:6px;background:var(--wsi-side-panel-bg);}", fixed = TRUE)
  expect_match(html, "#annotationHistory{display:block;flex:0 0 auto;max-height:min(30vh,280px);overflow:auto;margin:0;padding:8px;border:1px solid rgba(255,255,255,.16);border-radius:6px;background:var(--wsi-side-panel-bg);}", fixed = TRUE)
  expect_match(html, "#viewerLogPanel{display:block;flex:0 0 auto;max-height:min(30vh,280px);overflow:auto;margin:0;padding:8px;border:1px solid rgba(255,255,255,.16);border-radius:6px;background:var(--wsi-side-panel-bg);}", fixed = TRUE)
  expect_match(html, "id=\"viewerLogPanel\" class=\"panel projectPanel logPanel closed\"", fixed = TRUE)
  expect_match(html, "#roiPanel{position:relative;width:auto", fixed = TRUE)
  expect_match(html, "roiPanelHeader", fixed = TRUE)
  expect_match(html, "roiPanelBody", fixed = TRUE)
  expect_match(html, "roiPanelMinimizeState", fixed = TRUE)
  expect_match(html, "Double-click to minimize or restore the annotation manager", fixed = TRUE)
  expect_match(html, "#roiPanel.minimized #roiPanelBody{display:none;}", fixed = TRUE)
  expect_match(html, "Project and annotation panels", fixed = TRUE)
  expect_match(html, "closeProjectPanel", fixed = TRUE)
  expect_match(html, "onclick=\"event.preventDefault();event.stopPropagation();if(typeof closeProjectPanel==='function')", fixed = TRUE)
  expect_match(html, "projectPanelIsClosed", fixed = TRUE)
  expect_match(html, "ensureProjectWorkspaceVisible({preserve_state:true})", fixed = TRUE)
  expect_match(html, "setProjectPanelClosed(panelPrefs.project_closed,false)", fixed = TRUE)
  expect_match(html, "setProjectPanelMinimized(panelPrefs.project_minimized,false)", fixed = TRUE)
  expect_match(html, "panel.style.display=closed?'none':''", fixed = TRUE)
  expect_match(html, "close.onclick=e=>closeProjectPanel(e)", fixed = TRUE)
  expect_false(grepl("setProjectPanelClosed(false);if(typeof setProjectPanelMinimized==='function')setProjectPanelMinimized(false);", html, fixed = TRUE))
  expect_match(html, "projectSectionIsPyramidLevel", fixed = TRUE)
  expect_false(grepl("Sections / pyramid levels", html, fixed = TRUE))
  expect_true(regexpr("id=\"projectPanel\"", html, fixed = TRUE) < regexpr("id=\"roiPanel\"", html, fixed = TRUE))
  expect_match(html, "roiPanelUserHidden", fixed = TRUE)
  expect_match(html, "if(open&&automatic&&roiPanelUserHidden())return false", fixed = TRUE)
  expect_match(html, "setRoiPanelOpen(true,{automatic:true})", fixed = TRUE)
  expect_match(html, "toggleRoiPanelMinimized", fixed = TRUE)
  expect_match(html, "bindRoiPanelControls", fixed = TRUE)
  expect_match(html, "selectionCard", fixed = TRUE)
  expect_match(html, "Selected ROI summary", fixed = TRUE)
  expect_false(grepl("id=\"selectionCardToggle\"", html, fixed = TRUE))
  expect_match(html, "selectionCardVisible=false", fixed = TRUE)
  expect_match(html, "toggleSelectionCard", fixed = TRUE)
  expect_match(html, "setSelectionCardVisible", fixed = TRUE)
  expect_match(html, "selectionCardArea", fixed = TRUE)
  expect_match(html, "selectionCardCells", fixed = TRUE)
  expect_match(html, "selectionCardDensity", fixed = TRUE)
  expect_match(html, "selectionZoom", fixed = TRUE)
  expect_match(html, "selectionEdit", fixed = TRUE)
  expect_match(html, "selectionDelete", fixed = TRUE)
  expect_match(html, "selectionClose", fixed = TRUE)
  expect_match(html, "updateSelectionCard", fixed = TRUE)
  expect_match(html, "selectionCellCount", fixed = TRUE)
  expect_match(html, "bindSelectionCardControls", fixed = TRUE)
  expect_match(html, ".bar{position:fixed;left:12px;right:12px;top:12px", fixed = TRUE)
  expect_match(html, "z-index:30", fixed = TRUE)
  expect_match(html, "#workspacePanel{position:fixed;left:12px;top:72px", fixed = TRUE)
  expect_match(html, "#roiPanel{position:relative;width:auto", fixed = TRUE)
  expect_match(html, "z-index:29", fixed = TRUE)
  expect_match(html, "Annotation manager", fixed = TRUE)
  expect_match(html, "annotationSearchInput", fixed = TRUE)
  expect_match(html, "search name/category", fixed = TRUE)
  expect_match(html, "annotationFilter", fixed = TRUE)
  expect_match(html, "annotationSort", fixed = TRUE)
  expect_match(html, "annotationFilterClear", fixed = TRUE)
  expect_match(html, "annotationListTools\"><button id=\"prevRoi\"", fixed = TRUE)
  expect_match(html, "<button id=\"nextRoi\" title=\"Next ROI\">Next</button><button id=\"annotationSelectAll\"", fixed = TRUE)
  expect_match(html, "bindAnnotationListControls", fixed = TRUE)
  expect_match(html, "currentRoiListEntries", fixed = TRUE)
  expect_match(html, "function unpackViewerLayerPoints", fixed = TRUE)
  expect_match(html, "delete layer.packed_points", fixed = TRUE)
  expect_match(html, "function revealSelectedAnnotationInList", fixed = TRUE)
  expect_match(html, "requestAnimationFrame(revealSelectedAnnotationInList)", fixed = TRUE)
  expect_match(html, "item.dataset.index=String(i)", fixed = TRUE)
  expect_match(html, "roiMatchesAnnotationSearch", fixed = TRUE)
  expect_match(html, "roiMatchesAnnotationFilter", fixed = TRUE)
  expect_match(html, "annotationListSummary", fixed = TRUE)
  expect_match(html, "roiListEmpty", fixed = TRUE)
  expect_match(html, "area_desc", fixed = TRUE)
  expect_match(html, "selectedRoiAreaValue", fixed = TRUE)
  expect_false(grepl("annotationNameInput", html, fixed = TRUE))
  expect_match(html, "annotationClassSelect", fixed = TRUE)
  expect_false(grepl("id=\"selectedRoiClassSelect\"", html, fixed = TRUE))
  expect_false(grepl("id=\"applySelectedRoiLabel\"", html, fixed = TRUE))
  expect_match(html, "annotationColorInput", fixed = TRUE)
  expect_false(grepl("annotationVisible", html, fixed = TRUE))
  expect_false(grepl("annotationLock", html, fixed = TRUE))
  expect_false(grepl("annotationZoom", html, fixed = TRUE))
  expect_false(grepl("annotationDuplicate", html, fixed = TRUE))
  expect_match(html, "annotationDelete", fixed = TRUE)
  expect_match(html, "annotationExportSelected", fixed = TRUE)
  expect_match(html, "Export selected ROIs", fixed = TRUE)
  expect_match(html, "annotationSelectAll", fixed = TRUE)
  expect_match(html, "annotationSelectNone", fixed = TRUE)
  expect_match(html, "Annotations deselected", fixed = TRUE)
  expect_match(html, "toggleRoiVisibility", fixed = TRUE)
  expect_match(html, "toggleRoiLock", fixed = TRUE)
  expect_match(html, "updateRoiColor", fixed = TRUE)
  expect_match(html, "duplicateRoi", fixed = TRUE)
  expect_match(html, "exportSelectedAnnotations", fixed = TRUE)
  expect_match(html, "roiExportIndices", fixed = TRUE)
  expect_match(html, "visibleRoi", fixed = TRUE)
  expect_match(html, "lockedRoi", fixed = TRUE)
  expect_match(html, "roiSelect", fixed = TRUE)
  expect_match(html, "toolMenu", fixed = TRUE)
  expect_false(grepl("content:'v'", html, fixed = TRUE))
  expect_match(html, "border-right:1.6px solid", fixed = TRUE)
  expect_match(html, ".toolMenu[open] summary::after", fixed = TRUE)
  expect_match(html, "closeAllToolMenus", fixed = TRUE)
  expect_match(html, "document.querySelectorAll('.toolMenu[open]')", fixed = TRUE)
  expect_match(html, "closeMenuAfterToolAction", fixed = TRUE)
  expect_match(html, "bindExclusiveMenus", fixed = TRUE)
  expect_match(html, "syncViewerState", fixed = TRUE)
  expect_match(html, "scheduleViewerStateSync", fixed = TRUE)
  expect_match(html, "syncRoiSelection", fixed = TRUE)
  expect_match(html, "roi_selected", fixed = TRUE)
  expect_match(html, "R-controlled overlays", fixed = TRUE)
  expect_match(html, "layerList", fixed = TRUE)
  expect_match(html, "drawLayers", fixed = TRUE)
  expect_match(html, "layerStatePayload", fixed = TRUE)
  expect_match(html, "upsertViewerLayer", fixed = TRUE)
  expect_match(html, "setViewerLayerVisible", fixed = TRUE)
  expect_match(html, "removeViewerLayer", fixed = TRUE)
  expect_match(html, "layer_visibility_updated", fixed = TRUE)
  expect_match(html, "handleViewerCommand", fixed = TRUE)
  expect_match(html, "handleViewerCommands", fixed = TRUE)
  expect_match(html, "pollViewerCommands", fixed = TRUE)
  expect_match(html, "startViewerCommandPolling", fixed = TRUE)
  expect_match(html, "startViewerStateSocket", fixed = TRUE)
  expect_match(html, "viewer_state_ws_url", fixed = TRUE)
  expect_match(html, "WebSocket unavailable; polling fallback active", fixed = TRUE)
  expect_match(html, "R command: added ROIs", fixed = TRUE)
  expect_match(html, "R command: added segmentation", fixed = TRUE)
  expect_match(html, "R command: added layer", fixed = TRUE)
  expect_match(html, "viewer_state_url", fixed = TRUE)
  expect_match(html, "R sync", fixed = TRUE)
  expect_match(html, "querySelectorAll('.toolMenu')", fixed = TRUE)
  expect_match(html, "closeOtherMenus", fixed = TRUE)
  expect_match(html, "setTimeout(closeAllToolMenus,0)", fixed = TRUE)
  expect_match(html, "pointerdown", fixed = TRUE)
  expect_match(html, "closest('.toolMenu')", fixed = TRUE)
  expect_match(html, "menu!==active", fixed = TRUE)
  expect_false(grepl("<summary title=\"Pan and zoom controls\">Navigate</summary>", html, fixed = TRUE))
  expect_match(html, "navDock", fixed = TRUE)
  expect_match(html, "navPanButton", fixed = TRUE)
  expect_match(html, "screenshotButton", fixed = TRUE)
  expect_match(html, "wsiTools_screenshot_", fixed = TRUE)
  expect_match(html, "Annotations", fixed = TRUE)
  expect_match(html, "Draw, select, import, export, and manage annotations", fixed = TRUE)
  expect_match(html, "GeoJSON and display", fixed = TRUE)
  expect_false(grepl("StarDist segmentation", html, fixed = TRUE))
  expect_false(grepl("<summary title=\"StarDist segmentation import for selected ROIs\">Segmentation</summary>", html, fixed = TRUE))
  expect_false(grepl("<summary title=\"ROI overlay and GeoJSON geometry list\">GeoJSON</summary>", html, fixed = TRUE))
  expect_false(grepl("id=\"layersToggle\"", html, fixed = TRUE))
  expect_match(html, "Import GeoJSON", fixed = TRUE)
  expect_match(html, "Rasterize ROIs", fixed = TRUE)
  expect_match(html, "importGeojson", fixed = TRUE)
  expect_match(html, "rasterizeAnnotations", fixed = TRUE)
  expect_match(html, "geojsonImportFile", fixed = TRUE)
  expect_match(html, "geojsonImportSummary", fixed = TRUE)
  expect_match(html, "bindGeojsonImportControls", fixed = TRUE)
  expect_match(html, "addImportedGeojson", fixed = TRUE)
  expect_match(html, "geojsonMaskUrl", fixed = TRUE)
  expect_match(html, "importGeojsonObject(obj,fileName){return addImportedGeojson(obj,fileName);}", fixed = TRUE)
  expect_match(html, "rasterizeCurrentAnnotationsAsMask", fixed = TRUE)
  expect_match(html, "Vector ROIs are hidden", fixed = TRUE)
  expect_match(html, "annotationMaskBrushEnabled", fixed = TRUE)
  expect_match(html, "paintAnnotationMaskStroke", fixed = TRUE)
  expect_match(html, "drawAnnotationMasks", fixed = TRUE)
  expect_match(html, "annotationMaskPayload", fixed = TRUE)
  expect_match(html, "annotation_mask_updated", fixed = TRUE)
  expect_match(html, "importedRoiFromFeature", fixed = TRUE)
  expect_match(html, "geojsonGeometryParts", fixed = TRUE)
  expect_false(grepl("roiLabelInput", html, fixed = TRUE))
  expect_false(grepl("annotation label", html, fixed = TRUE))
  expect_match(html, "panelRoiClassSelect", fixed = TRUE)
  expect_match(html, "panelRoiClassCustom", fixed = TRUE)
  expect_match(html, "custom category", fixed = TRUE)
  expect_match(html, "customCategoryValue", fixed = TRUE)
  expect_match(html, "tumour", fixed = TRUE)
  expect_match(html, "stroma", fixed = TRUE)
  expect_match(html, "necrosis", fixed = TRUE)
  expect_match(html, "invasive front", fixed = TRUE)
  expect_match(html, "#D73027", fixed = TRUE)
  expect_false(grepl("Class Presets", html, fixed = TRUE))
  expect_false(grepl("maximizeClassPresets", html, fixed = TRUE))
  expect_false(grepl("id=\"addClassPreset\"", html, fixed = TRUE))
  expect_false(grepl("id=\"resetClassPresets\"", html, fixed = TRUE))
  expect_false(grepl("id=\"respectClassExportRules\"", html, fixed = TRUE))
  expect_match(html, "roiClassPresetDefaults", fixed = TRUE)
  expect_match(html, "populateRoiClassSelects", fixed = TRUE)
  expect_false(grepl("classPresetList", html, fixed = TRUE))
  expect_match(html, "classPresetColour", fixed = TRUE)
  expect_match(html, "applyClassPresetColoursToRois", fixed = TRUE)
  expect_match(html, "roiAllowedByExportRules", fixed = TRUE)
  expect_match(html, "exportableRoiFeatures", fixed = TRUE)
  expect_false(grepl("id=\"applyRoiClass\"", html, fixed = TRUE))
  expect_false(grepl("Set next class", html, fixed = TRUE))
  expect_match(html, "ROI updated", fixed = TRUE)
  expect_match(html, "annotationLabelValue", fixed = TRUE)
  expect_match(html, "activeRoiName", fixed = TRUE)
  expect_match(html, "label:name", fixed = TRUE)
})

test_that("Artifact menu appears only for CellPhenotyper project viewers", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")
  grandqc <- list(
    enabled = TRUE,
    geojsons = list(list(
      id = "grandqc_artifacts",
      label = "GrandQC artifacts",
      path = "grandqc.geojson",
      feature_count = 2L,
      geojson = list(type = "FeatureCollection", features = list())
    ))
  )

  result <- wsi_viewer(
    slide,
    width = 256,
    output = output,
    open = FALSE,
    cellphenotyper = list(enabled = TRUE, grandqc = grandqc)
  )
  html <- paste(readLines(result, warn = FALSE), collapse = "\n")

  expect_false(grepl("<summary title=\"Visualize GrandQC artifact GeoJSON annotations\">Artifacts</summary>", html, fixed = TRUE))

  output_project <- tempfile(fileext = ".html")
  result_project <- wsi_viewer(
    slide,
    width = 256,
    output = output_project,
    open = FALSE,
    cellphenotyper = list(
      enabled = TRUE,
      project_type = "cellphenotyper",
      is_project = TRUE,
      grandqc = grandqc
    )
  )
  html <- paste(readLines(result_project, warn = FALSE), collapse = "\n")

  expect_match(html, "<summary title=\"Visualize GrandQC artifact GeoJSON annotations\">Artifacts</summary>", fixed = TRUE)
  expect_match(html, "GrandQC GeoJSON", fixed = TRUE)
  expect_match(html, "grandqcLoadAll", fixed = TRUE)
  expect_match(html, "GrandQC artifacts", fixed = TRUE)
})

test_that("non-interactive viewer wrapper writes HTML without opening a browser", {
  slide <- wsiTools:::wsi_mock_slide(width = 800, height = 400, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- wsi_viewer_noninteractive(
    slide,
    output = output,
    width = 256,
    quiet = TRUE
  )

  expect_identical(result, normalizePath(output, winslash = "/", mustWork = FALSE))
  expect_true(file.exists(output))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "wsiTools viewer", fixed = TRUE)
  expect_true(
    grepl("thumbnail preview, full slide not loaded into R", html, fixed = TRUE) ||
      grepl("tile_url_base", html, fixed = TRUE)
  )
})

test_that("project panel does not list slide pyramid levels as sections", {
  slide <- wsiTools:::wsi_mock_slide(width = 800, height = 400, levels = c(1, 4, 16))

  item <- wsiTools:::wsi_viewer_project_item_from_slide(
    slide,
    width = 256,
    include_preview = FALSE
  )

  expect_length(item$sections, 0)
  expect_length(wsiTools:::wsi_viewer_project_sections_from_slide(slide), 0)
})

test_that("viewer ROI colours are consistent within label categories", {
  geojson <- list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "tumour-red",
        properties = list(name = "Tumour red", classification = list(name = "tumour", color = "#FF0000")),
        geometry = list(type = "Polygon", coordinates = list(list(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))))
      ),
      list(
        type = "Feature",
        id = "tumour-blue",
        properties = list(name = "Tumour blue", classification = list(name = "tumour", color = "#0000FF")),
        geometry = list(type = "Polygon", coordinates = list(list(c(20, 0), c(30, 0), c(30, 10), c(20, 10), c(20, 0))))
      ),
      list(
        type = "Feature",
        id = "custom-a",
        properties = list(name = "Custom A", classification = list(name = "immune niche", color = "#123456")),
        geometry = list(type = "Polygon", coordinates = list(list(c(0, 20), c(10, 20), c(10, 30), c(0, 30), c(0, 20))))
      ),
      list(
        type = "Feature",
        id = "custom-b",
        properties = list(name = "Custom B", classification = list(name = "immune niche", color = "#654321")),
        geometry = list(type = "Polygon", coordinates = list(list(c(20, 20), c(30, 20), c(30, 30), c(20, 30), c(20, 20))))
      )
    )
  )
  rois <- wsiTools:::wsi_roi_from_geojson(geojson)
  features <- wsiTools:::wsi_viewer_roi_features(rois, class_presets = wsi_roi_class_presets())
  colours <- vapply(features, `[[`, character(1), "colour")

  expect_equal(colours[1:2], c("#D73027", "#D73027"))
  expect_equal(colours[[3]], colours[[4]])
  expect_match(colours[[3]], "^#[0-9A-F]{6}$")
})

test_that("interactive viewer can be configured with a live R state endpoint", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- wsi_viewer(
    slide,
    width = 256,
    output = output,
    open = FALSE,
    viewer_state_url = "http://127.0.0.1:8788/viewer-state",
    image_export_url = "http://127.0.0.1:8788/image-export"
  )

  expect_identical(result, output)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "http://127.0.0.1:8788/viewer-state", fixed = TRUE)
  expect_match(html, "http://127.0.0.1:8788/image-export", fixed = TRUE)
  expect_match(html, "viewerStatePayload", fixed = TRUE)
  expect_match(html, "roiGeojsonObject", fixed = TRUE)
  expect_match(html, "measurements:measures", fixed = TRUE)
  expect_match(html, "segmentationGeojsonObject", fixed = TRUE)
  expect_match(html, "layers:layerStatePayload()", fixed = TRUE)
  expect_match(html, "artifacts:(typeof artifactPayload==='function'?artifactPayload():[])", fixed = TRUE)
  expect_match(html, "syncRoiSelection", fixed = TRUE)
  expect_match(html, "roi_selected", fixed = TRUE)
  expect_match(html, "selected_rois", fixed = TRUE)
  expect_match(html, "roi.properties", fixed = TRUE)
  expect_match(html, "clonePlain", fixed = TRUE)
  expect_match(html, "add_groups", fixed = TRUE)
  expect_match(html, "MultiPolygon", fixed = TRUE)
  expect_match(html, "startViewerCommandPolling", fixed = TRUE)
  expect_match(html, "startViewerStateSocket", fixed = TRUE)
  expect_match(html, "viewerSocketSend", fixed = TRUE)
  expect_match(html, "viewer_state_ws_url", fixed = TRUE)
  expect_match(html, "commands", fixed = TRUE)
  expect_match(html, "add_layer", fixed = TRUE)
  expect_match(html, "set_layer_visible", fixed = TRUE)
  expect_match(html, "colour_spots_by_gene", fixed = TRUE)
  expect_match(html, "geojson_imported", fixed = TRUE)
  expect_match(html, "measurement_added", fixed = TRUE)
  expect_match(html, "segmentation_added", fixed = TRUE)
  expect_match(html, "annotationHistory", fixed = TRUE)
  expect_match(html, "historyPanelToggle", fixed = TRUE)
  expect_match(html, "historyPanelMinimizeState", fixed = TRUE)
  expect_match(html, "historyPanelIsClosed", fixed = TRUE)
  expect_match(html, "ensureHistoryWorkspaceVisible", fixed = TRUE)
  expect_match(html, "openHistoryPanel", fixed = TRUE)
  expect_match(html, "closeHistoryPanel", fixed = TRUE)
  expect_match(html, "copyAnnotationHistoryAll", fixed = TRUE)
  expect_match(html, "wsiTools viewer history and R sync report", fixed = TRUE)
  expect_match(html, "R/live sync commands and events", fixed = TRUE)
  expect_match(html, "viewerSyncHistory", fixed = TRUE)
  expect_match(html, "recordViewerSyncHistory('from_R'", fixed = TRUE)
  expect_match(html, "recordViewerSyncHistory('to_R'", fixed = TRUE)
  expect_match(html, "panel.style.display=closed?'none':''", fixed = TRUE)
  expect_match(html, "viewerLogPanelToggle", fixed = TRUE)
  expect_match(html, "Troubleshooting logs", fixed = TRUE)
  expect_match(html, "downloadViewerLogFile", fixed = TRUE)
  expect_match(html, "copyViewerLogText", fixed = TRUE)
  expect_match(html, "viewerLogPayload", fixed = TRUE)
  expect_match(html, "bindViewerLogCapture", fixed = TRUE)
  expect_match(html, "window.addEventListener('unhandledrejection'", fixed = TRUE)
  expect_match(html, "logs:(typeof viewerLogPayload==='function'?viewerLogPayload():[])", fixed = TRUE)
  expect_match(html, "maximizeAnnotationHistory", fixed = TRUE)
  expect_match(html, "annotationSectionBackdrop", fixed = TRUE)
  expect_match(html, "setAnnotationSectionMaximized", fixed = TRUE)
  expect_match(html, "toggleAnnotationSectionMaximized", fixed = TRUE)
  expect_match(html, "bindAnnotationSectionMaximizeControls", fixed = TRUE)
  expect_match(html, "#annotationHistory.maximized", fixed = TRUE)
  expect_match(html, "recordAnnotationHistory", fixed = TRUE)
  expect_match(html, "annotationHistoryPayload", fixed = TRUE)
  expect_match(html, "recordAnnotationHistory('viewer_'+level", fixed = TRUE)
  expect_match(html, "Viewer warning", fixed = TRUE)
  expect_match(html, "Viewer error", fixed = TRUE)
  expect_match(html, "Brush subtract", fixed = TRUE)
  expect_match(html, "Imported GeoJSON", fixed = TRUE)
  expect_match(html, "Imported cell segmentation", fixed = TRUE)
})

test_that("live viewer image export payloads are validated", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  payload <- list(
    scope = "selected_roi",
    format = "tiff",
    region = list(x = 10, y = 20, width = 128, height = 64, level = 0)
  )

  region <- wsiTools:::wsi_viewer_image_export_region(payload, slide)
  expect_equal(region$x, 10L)
  expect_equal(region$y, 20L)
  expect_equal(region$width, 128L)
  expect_equal(region$height, 64L)
  expect_equal(region$level, 0L)
  expect_match(
    wsiTools:::wsi_viewer_image_export_filename(payload, image_format = "tiff", scope = "selected_roi"),
    "[.]tif$"
  )
  expect_error(
    wsiTools:::wsi_viewer_image_export_region(c(payload, list(code = "rm -rf /")), slide),
    "Unsupported image export field"
  )
  expect_error(
    wsiTools:::wsi_viewer_image_export_response(
      slide,
      payload,
      output_dir = tempdir(),
      max_pixels = 100
    ),
    "too large",
    class = "wsi_error"
  )
})

test_that("live viewer sessions expose R-native helper methods and command queue", {
  env <- new.env(parent = emptyenv())
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  state <- wsiTools:::wsi_new_viewer_state(name = "live", envir = env)
  session <- wsiTools:::wsi_attach_viewer_session_methods(structure(
    list(
      server = NULL,
      url = "http://127.0.0.1:8788/viewer-state",
      state = state,
      slide = slide,
      html = tempfile(fileext = ".html"),
      name = "live",
      envir = env,
      stardist_server = NULL
    ),
    class = "wsi_viewer_session"
  ))

  expect_true(is.function(session$get_rois))
  expect_true(is.function(session$get_selected_roi))
  expect_true(is.function(session$get_selected_rois))
  expect_true(is.function(session$get_selected_object))
  expect_true(is.function(session$get_selected_spots))
  expect_true(is.function(session$get_performance))
  expect_true(is.function(session$get_spot_annotation_table))
  expect_true(is.function(session$get_measurements))
  expect_true(is.function(session$get_trajectories))
  expect_true(is.function(session$get_roi_summary))
  expect_true(is.function(session$get_cell_summary))
  expect_true(is.function(session$get_class_summary))
  expect_true(is.function(session$get_ihc_summary))
  expect_true(is.function(session$get_ihc_class_summary))
  expect_true(is.function(session$get_segmentation))
  expect_true(is.function(session$get_layers))
  expect_true(is.function(session$get_annotation_masks))
  expect_true(is.function(session$get_annotation_spots))
  expect_true(is.function(session$get_spatial_registration))
  expect_true(is.function(session$get_history))
  expect_true(is.function(session$get_logs))
  expect_true(is.function(session$get_tile_preview))
  expect_true(is.function(session$get_prediction))
  expect_true(is.function(session$get_proximity))
  expect_true(is.function(session$get_trajectory_profile))
  expect_true(is.function(session$list_layers))
  expect_true(is.function(session$on))
  expect_true(is.function(session$off))
  expect_true(is.function(session$list_callbacks))
  expect_true(is.function(session$capabilities))
  expect_true(is.function(session$colour_spots_by_gene))
  expect_true(is.function(session$color_spots_by_gene))
  expect_true(is.function(session$add_rois))
  expect_true(is.function(session$add_segmentation))
  expect_true(is.function(session$add_layer))
  expect_true(is.function(session$set_layer_visible))
  expect_true(is.function(session$remove_layer))
  expect_true(is.function(session$measure_ihc_intensity))
  expect_true(is.function(session$preview_tiles))
  expect_true(is.function(session$clear_tile_preview))
  expect_true(is.function(session$extract_tile_preview))
  expect_true(is.function(session$extract_preview_tiles))
  expect_true(is.function(session$run_preview_tiles))
  expect_true(is.function(session$get_jobs))
  expect_true(is.function(session$list_jobs))
  expect_true(is.function(session$get_job))
  expect_true(is.function(session$get_job_progress))
  expect_true(is.function(session$get_job_log))
  expect_true(is.function(session$run_segmentation_async))
  expect_true(is.function(session$run_tiles_async))
  expect_true(is.function(session$run_conversion_async))
  expect_true(is.function(session$run_pyramid_async))
  expect_true(is.function(session$save_project))
  expect_true(is.function(session$autosave_start))
  expect_true(is.function(session$autosave_stop))
  expect_true(is.function(session$autosave_now))
  expect_true(is.function(session$autosave_status))
  expect_s3_class(session$list_jobs(), "data.frame")
  capabilities <- session$capabilities()
  expect_s3_class(capabilities, "wsi_viewer_capabilities")
  expect_named(capabilities, c("capability", "available", "backend", "notes"))
  expect_true(all(c(
    "session_api", "live_r_sync", "geojson_roundtrip",
    "autosave_project", "r_controlled_layers", "measurements", "async_jobs"
  ) %in% capabilities$capability))
  expect_true(capabilities$available[match("session_api", capabilities$capability)])
  expect_identical(
    capabilities$available[match("async_jobs", capabilities$capability)],
    wsi_has_callr()
  )

  state$seurat_selection <- list(labels = c("spot_1", "spot_2"), count = 2L, matched_count = 1L)
  selected_spots <- session$get_selected_spots(service = FALSE)
  expect_s3_class(selected_spots, "wsi_selected_spots")
  expect_equal(selected_spots$spot_id, c("spot_1", "spot_2"))
  expect_equal(attr(selected_spots, "matched_count"), 1L)
  expect_s3_class(session$get_spot_annotation_table(service = FALSE), "wsi_annotation_spots")

  expect_error(session$colour_spots_by_gene("", service = FALSE), "`gene`")
  session$colour_spots_by_gene("Mbp", service = FALSE)
  response <- wsiTools:::wsi_viewer_state_response(state)
  expect_equal(response$commands[[1]]$type, "colour_spots_by_gene")
  expect_equal(response$commands[[1]]$payload$gene, "Mbp")

  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(list(
      type = "Feature",
      id = "roi-1",
      properties = list(name = "Tumour", classification = list(name = "tumour")),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          c(0, 0), c(10, 0), c(10, 8), c(0, 8), c(0, 0)
        ))
      )
    ))
  ))

  session$add_rois(rois, service = FALSE)
  expect_s3_class(session$get_rois(service = FALSE), "wsi_roi")
  expect_equal(nrow(session$get_rois(service = FALSE)), 1)
  expect_s3_class(session$get_selected_roi(service = FALSE), "wsi_roi")
  expect_s3_class(session$get_selected_rois(service = FALSE), "wsi_roi")
  expect_equal(nrow(session$get_selected_rois(service = FALSE)), 1)
  expect_equal(env$live_rois$roi_id, "roi-1")
  expect_equal(env$live_selected_rois$roi_id, "roi-1")

  response <- wsiTools:::wsi_viewer_state_response(state)
  expect_equal(length(response$commands), 1)
  expect_equal(response$commands[[1]]$type, "add_rois")
  expect_equal(length(wsiTools:::wsi_viewer_state_response(state)$commands), 0)

  cells <- data.frame(cell_id = "cell-1", centroid_x = 5, centroid_y = 6)
  session$add_segmentation(cells, service = FALSE)
  expect_s3_class(session$get_segmentation(service = FALSE), "wsi_roi")
  expect_equal(nrow(session$get_segmentation(service = FALSE)), 1)
  expect_equal(nrow(session$get_roi_summary(service = FALSE)), 1)
  expect_equal(session$get_roi_summary(service = FALSE)$area_px2, 80)
  expect_equal(session$get_roi_summary(service = FALSE)$cell_count, 1)
  expect_equal(nrow(session$get_cell_summary(service = FALSE)), 1)
  expect_equal(session$get_cell_summary(service = FALSE)$roi_id, "roi-1")
  expect_true(session$get_cell_summary(service = FALSE)$inside_roi)
  expect_equal(session$get_class_summary(service = FALSE)$class, "tumour")
  expect_equal(session$get_class_summary(service = FALSE)$cell_count, 1)
  expect_s3_class(env$live_roi_summary, "data.frame")
  expect_s3_class(env$live_cell_summary, "data.frame")
  expect_s3_class(env$live_history, "data.frame")
  response <- wsiTools:::wsi_viewer_state_response(state)
  expect_equal(response$commands[[1]]$type, "add_segmentation")

  session$add_layer("tumour ROIs", rois, service = FALSE)
  layers <- session$list_layers(service = FALSE)
  expect_equal(layers$name, "tumour ROIs")
  expect_true(layers$visible)
  response <- wsiTools:::wsi_viewer_state_response(state)
  expect_equal(response$commands[[1]]$type, "add_layer")

  session$set_layer_visible("tumour ROIs", FALSE, service = FALSE)
  expect_false(session$list_layers(service = FALSE)$visible)
  response <- wsiTools:::wsi_viewer_state_response(state)
  expect_equal(response$commands[[1]]$type, "set_layer_visible")

  session$add_layer("DAB intensity", matrix(seq(0, 1, length.out = 9), nrow = 3), opacity = 0.4, service = FALSE)
  heatmap <- session$get_layers(service = FALSE)$dab_intensity
  expect_equal(heatmap$type, "heatmap")
  expect_equal(heatmap$nrow, 3)
  expect_equal(heatmap$extent$xmax, 1000)
  expect_equal(wsiTools:::wsi_viewer_state_response(state)$commands[[1]]$type, "add_layer")

  session$remove_layer("tumour ROIs", service = FALSE)
  expect_false("tumour_rois" %in% names(session$get_layers(service = FALSE)))
  expect_equal(wsiTools:::wsi_viewer_state_response(state)$commands[[1]]$type, "remove_layer")

  state$selected_roi <- NULL
  state$selected_rois <- wsiTools:::wsi_empty_roi()
  preview <- session$preview_tiles(tile_size = 250, stride = 250, max_tiles = 3, seed = 1, service = FALSE)
  expect_s3_class(preview, "wsi_tile_preview")
  expect_equal(nrow(preview), 3)
  expect_equal(nrow(session$get_tile_preview(service = FALSE)), 3)
  expect_equal(nrow(env$live_tile_preview), 3)
  response <- wsiTools:::wsi_viewer_state_response(state)
  expect_equal(response$tile_preview_count, 3)
  expect_true(any(vapply(response$commands, function(x) identical(x$type, "add_layer"), logical(1))))
  expect_true("tile_preview" %in% names(session$get_layers(service = FALSE)))

  session$clear_tile_preview(service = FALSE)
  expect_equal(nrow(session$get_tile_preview(service = FALSE)), 0)
  expect_equal(nrow(env$live_tile_preview), 0)
  response <- wsiTools:::wsi_viewer_state_response(state)
  expect_equal(response$tile_preview_count, 0)
  expect_true(any(vapply(response$commands, function(x) identical(x$type, "remove_layer"), logical(1))))

  fake_job <- list(
    id = "job_1",
    name = "WSI conversion",
    started = Sys.time(),
    metadata = function() list(
      id = "job_1",
      name = "WSI conversion",
      pid = NA_integer_,
      status = "running",
      display_status = "running",
      progress = 42,
      progress_available = TRUE,
      message = "progress 42%",
      started = Sys.time(),
      finished = NA,
      log = "progress 42%"
    )
  )
  wsiTools:::wsi_viewer_state_set_job(state, fake_job, status = "running", progress = 42)
  expect_equal(env$live_jobs$status, "running")
  expect_equal(env$live_jobs$progress, 42)
  expect_equal(session$get_job_progress("job_1", service = FALSE)$status, "running")
  response <- wsiTools:::wsi_viewer_state_response(state)
  expect_equal(response$jobs[[1]]$id, "job_1")
  expect_equal(response$jobs[[1]]$progress, 42)
  expect_equal(response$commands[[1]]$type, "job_update")

  dir <- tempfile("viewer-session.wsiproject")
  state$annotations <- list(dirty = TRUE, dirty_reason = "roi_updated")
  project <- session$save_project(dir, service = FALSE)
  expect_s3_class(project, "wsi_project")
  expect_true(file.exists(file.path(dir, "project.json")))
  expect_false(session$get_state(service = FALSE)$annotations$dirty)
  response <- wsiTools:::wsi_viewer_state_response(state)
  expect_equal(response$commands[[1]]$type, "annotations_saved")

  autosave_dir <- tempfile(fileext = ".wsiproject")
  autosaved <- session$autosave_start(autosave_dir, interval = 1, service = FALSE)
  expect_s3_class(autosaved, "wsi_project")
  expect_true(file.exists(file.path(autosave_dir, "project.json")))
  expect_true(file.exists(file.path(autosave_dir, "rois.geojson")))
  expect_true(file.exists(file.path(autosave_dir, "segmentation.geojson")))
  expect_true(session$autosave_status()$enabled)
  expect_null(session$autosave_status()$last_error)
  expect_equal(session$autosave_status()$count, 1L)
  reopened <- wsi_read_project(autosave_dir)
  expect_equal(reopened$metadata$autosave$reason, "autosave_started")
  expect_equal(nrow(reopened$rois), 1)
  expect_equal(nrow(reopened$segmentation), 1)
  session$autosave_stop(service = FALSE)
  expect_false(session$autosave_status()$enabled)
})

test_that("live viewer ROI injection tolerates mixed polygon and line GeoJSON", {
  env <- new.env(parent = emptyenv())
  state <- wsiTools:::wsi_new_viewer_state(name = "live", envir = env)
  session <- wsiTools:::wsi_attach_viewer_session_methods(structure(
    list(
      server = NULL,
      url = "http://127.0.0.1:8788/viewer-state",
      state = state,
      slide = wsiTools:::wsi_mock_slide(width = 100, height = 100, levels = c(1, 4)),
      html = tempfile(fileext = ".html"),
      name = "live",
      envir = env,
      stardist_server = NULL
    ),
    class = "wsi_viewer_session"
  ))

  mixed_rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "poly-1",
        properties = list(name = "Dysplasia", classification = list(name = "dysplasia")),
        geometry = list(
          type = "Polygon",
          coordinates = list(list(
            c(20, 20), c(30, 20), c(30, 30), c(20, 30), c(20, 20)
          ))
        )
      ),
      list(
        type = "Feature",
        id = "line-1",
        properties = list(name = "Mitotic figure", classification = list(name = "mitotic figure")),
        geometry = list(
          type = "LineString",
          coordinates = list(c(22, 22), c(28, 28))
        )
      )
    )
  ))

  expect_silent(session$add_rois(mixed_rois, service = FALSE))
  expect_equal(nrow(session$get_rois(service = FALSE)), 2)
  roi_summary <- session$get_roi_summary(service = FALSE)
  expect_equal(nrow(roi_summary), 2)
  expect_equal(roi_summary$area_px2[match("poly-1", roi_summary$roi_id)], 100)
  expect_true(is.na(roi_summary$area_px2[match("line-1", roi_summary$roi_id)]))
  response <- wsiTools:::wsi_viewer_state_response(state)
  expect_equal(response$commands[[1]]$type, "add_rois")
  expect_equal(length(response$commands[[1]]$payload$geojson$features), 2)
})

test_that("live viewer sessions dispatch event callbacks", {
  env <- new.env(parent = emptyenv())
  state <- wsiTools:::wsi_new_viewer_state(name = "live", envir = env)
  session <- wsiTools:::wsi_attach_viewer_session_methods(structure(
    list(
      server = NULL,
      url = "http://127.0.0.1:8788/viewer-state",
      state = state,
      slide = wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4)),
      html = tempfile(fileext = ".html"),
      name = "live",
      envir = env,
      stardist_server = NULL
    ),
    class = "wsi_viewer_session"
  ))

  feature <- list(
    type = "Feature",
    id = "roi-1",
    properties = list(name = "Tumour", classification = list(name = "tumour")),
    geometry = list(
      type = "Polygon",
      coordinates = list(list(c(0, 0), c(10, 0), c(10, 8), c(0, 8), c(0, 0)))
    )
  )
  payload <- list(
    event = "roi_added",
    time = "2026-05-18T12:00:00Z",
    selected_index = 0,
    selected_roi = feature,
    selected_rois = list(type = "FeatureCollection", features = list(feature)),
    rois = list(type = "FeatureCollection", features = list(feature)),
    segmentation = list(type = "FeatureCollection", features = list()),
    measurements = list(),
    view = list(mode = "select"),
    stain = NULL,
    detail = list()
  )

  created <- selected <- finished <- NULL
  created_id <- session$on("roi_created", function(roi) created <<- roi)
  session$on("roi_selected", function(roi, event) selected <<- list(roi = roi, event = event))
  session$on("segmentation_finished", function(cells) finished <<- cells)
  expect_type(created_id, "character")
  expect_equal(nrow(session$list_callbacks()), 3)

  wsiTools:::wsi_viewer_state_apply(state, payload)
  expect_s3_class(created, "wsi_roi")
  expect_equal(created$roi_id, "roi-1")

  payload$event <- "roi_selected"
  wsiTools:::wsi_viewer_state_apply(state, payload)
  expect_s3_class(selected$roi, "wsi_roi")
  expect_equal(selected$event, "roi_selected")

  payload$event <- "segmentation_added"
  payload$segmentation <- list(type = "FeatureCollection", features = list(feature))
  wsiTools:::wsi_viewer_state_apply(state, payload)
  expect_s3_class(finished, "wsi_roi")
  expect_equal(finished$roi_id, "roi-1")

  once_count <- 0L
  session$on("roi_selected", function(roi) once_count <<- once_count + 1L, once = TRUE)
  payload$event <- "roi_selected"
  wsiTools:::wsi_viewer_state_apply(state, payload)
  wsiTools:::wsi_viewer_state_apply(state, payload)
  expect_equal(once_count, 1L)

  session$off(id = created_id)
  expect_false(created_id %in% session$list_callbacks()$id)
})

test_that("live viewer state validates events and payload fields strictly", {
  env <- new.env(parent = emptyenv())
  state <- wsiTools:::wsi_new_viewer_state(name = "live", envir = env)
  payload <- list(
    event = "viewer_state",
    time = "2026-05-18T12:00:00Z",
    sequence = 1,
    rois = list(type = "FeatureCollection", features = list()),
    selected_rois = list(type = "FeatureCollection", features = list()),
    selected_object = list(type = "trajectory", index = 0, id = "trajectory_1", name = "Trajectory 1"),
    segmentation = list(type = "FeatureCollection", features = list()),
    measurements = list(),
    trajectories = list(),
    annotation_spots = list(),
    view = list(mode = "pan"),
    annotations = list(dirty = FALSE),
    history = list(),
    logs = list(),
    performance = list(tile_loaded = 12L, render_average_ms = 4.25, renderer = "webgl"),
    detail = list()
  )

  expect_silent(wsiTools:::wsi_viewer_state_apply(state, payload))
  expect_equal(state$performance$tile_loaded, 12L)
  expect_equal(state$performance$renderer, "webgl")

  renamed <- payload
  renamed$event <- "roi_label_updated"
  renamed$detail <- list(
    id = "brush_roi_7",
    old_name = "Sample",
    name = "Sample7",
    old_class = "Sample",
    class = "Sample7"
  )
  expect_silent(wsiTools:::wsi_viewer_state_apply(state, renamed))
  expect_equal(state$last_event, "roi_label_updated")

  bad_event <- payload
  bad_event$event <- "not_a_real_event"
  expect_error(
    wsiTools:::wsi_viewer_state_apply(state, bad_event),
    "Unsupported viewer event"
  )

  bad_field <- payload
  bad_field$unexpected <- TRUE
  expect_error(
    wsiTools:::wsi_viewer_state_apply(state, bad_field),
    "unsupported field"
  )

  bad_geojson <- payload
  bad_geojson$rois <- list(type = "Feature", geometry = list())
  expect_error(
    wsiTools:::wsi_viewer_state_apply(state, bad_geojson),
    "FeatureCollection"
  )

  bad_selected_object <- payload
  bad_selected_object$selected_object <- "roi-1"
  expect_error(
    wsiTools:::wsi_viewer_state_apply(state, bad_selected_object),
    "selected_object"
  )

  bad_selected_object_type <- payload
  bad_selected_object_type$selected_object <- list(type = "r_code", index = 0)
  expect_error(
    wsiTools:::wsi_viewer_state_apply(state, bad_selected_object_type),
    "selected_object\\$type"
  )
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
  cell_feature <- list(
    type = "Feature",
    id = "cell-1",
    properties = list(
      name = "Cell 1",
      classification = list(name = "cell"),
      class = "cell",
      objectType = "detection",
      source = "cellphenotyper",
      measurements = list(
        list(name = "DAB mean", value = 0.42),
        list(name = "Hematoxylin mean", value = 0.23)
      )
    ),
    geometry = list(
      type = "Polygon",
      coordinates = list(list(
        c(4, 4), c(6, 4), c(6, 6), c(4, 6), c(4, 4)
      ))
    )
  )
  payload <- list(
    event = "roi_added",
    time = "2026-05-18T12:00:00Z",
    selected_index = 0,
    selected_roi = feature,
    selected_object = list(type = "annotation", index = 0, id = "roi-1", name = "Tumour region"),
    rois = list(type = "FeatureCollection", features = list(feature)),
    segmentation = list(type = "FeatureCollection", features = list(cell_feature)),
    measurements = list(list(
      id = "measure_1",
      start = list(x = 0, y = 0),
      end = list(x = 3, y = 4),
      distance_px = 5,
      distance_um = 2.5
    )),
    trajectories = list(list(
      id = "trajectory_1",
      name = "Trajectory 1",
      n = 5,
      length_px = 10,
      area_width_px = 256,
      area_roi_id = "trajectory_roi_1",
      control_points = list(
        list(x = 0, y = 0),
        list(x = 10, y = 0)
      ),
      points = list(
        list(x = 0, y = 0),
        list(x = 2.5, y = 0),
        list(x = 5, y = 0),
        list(x = 7.5, y = 0),
        list(x = 10, y = 0)
      )
    )),
    view = list(mode = "measure", scale = 1),
    annotations = list(dirty = TRUE, dirty_reason = "roi_updated"),
    history = list(list(
      id = "history_1",
      time = "2026-05-18T12:00:01Z",
      action = "roi_added",
      label = "Created ROI",
      detail = list(id = "roi-1", name = "Tumour region")
    )),
    logs = list(list(
      id = "log_1",
      time = "2026-05-18T12:00:02Z",
      level = "warning",
      message = "Tiles did not load",
      source = "toast",
      detail = list(timeout = 7200)
    )),
    stain = list(enabled = FALSE),
    annotation_spots = list(list(
      annotation_index = 1,
      annotation_id = "roi-1",
      annotation_name = "Tumour region",
      annotation_class = "tumour",
      spot_id = "AAAC-1",
      spot_label = "AAAC-1",
      spot_x = 5,
      spot_y = 4,
      spot_layer_id = "seurat_spots",
      spot_layer_name = "Spatial spots",
      project_image = "section 1",
      project_section = "anterior"
    )),
    detail = list()
  )

  wsiTools:::wsi_viewer_state_apply(state, payload)
  snapshot <- wsi_viewer_state(state)

  expect_s3_class(snapshot$rois, "wsi_roi")
  expect_equal(snapshot$rois$roi_id, "roi-1")
  expect_equal(snapshot$rois$class, "tumour")
  expect_equal(snapshot$measurements$distance_px, 5)
  expect_s3_class(snapshot$trajectories, "wsi_trajectories")
  expect_equal(snapshot$trajectories$id, "trajectory_1")
  expect_equal(snapshot$trajectories$point_count, 5)
  expect_equal(snapshot$trajectories$area_width_px, 256)
  expect_equal(snapshot$trajectories$area_roi_id, "trajectory_roi_1")
  expect_equal(nrow(snapshot$trajectories$points[[1]]), 5)
  expect_true(snapshot$annotations$dirty)
  expect_equal(snapshot$annotations$dirty_reason, "roi_updated")
  expect_s3_class(snapshot$history, "data.frame")
  expect_equal(snapshot$history$label, "Created ROI")
  expect_equal(snapshot$history$detail[[1]]$id, "roi-1")
  expect_s3_class(snapshot$logs, "wsi_viewer_logs")
  expect_equal(snapshot$logs$level, "warning")
  expect_equal(snapshot$logs$message, "Tiles did not load")
  expect_equal(snapshot$logs$detail[[1]]$timeout, 7200)
  expect_s3_class(snapshot$annotation_spots, "wsi_annotation_spots")
  expect_equal(snapshot$annotation_spots$annotation_id, "roi-1")
  expect_equal(snapshot$annotation_spots$spot_id, "AAAC-1")
  expect_equal(snapshot$roi_summary$area_px2, 80)
  expect_equal(snapshot$roi_summary$cell_count, 1)
  expect_equal(snapshot$class_summary$class, "tumour")
  expect_equal(snapshot$class_summary$cell_count, 1)
  expect_equal(snapshot$cell_summary$roi_id, "roi-1")
  expect_equal(snapshot$cell_summary$measurement_DAB_mean, 0.42)
  expect_equal(snapshot$cell_summary$measurement_Hematoxylin_mean, 0.23)
  expect_equal(snapshot$selected_roi$name, "Tumour region")
  expect_s3_class(snapshot$selected_rois, "wsi_roi")
  expect_equal(snapshot$selected_rois$roi_id, "roi-1")
  expect_equal(snapshot$selected_object$type, "annotation")
  expect_equal(snapshot$selected_object$id, "roi-1")
  expect_identical(env$live, state)
  expect_s3_class(env$live_rois, "wsi_roi")
  expect_s3_class(env$live_selected_rois, "wsi_roi")
  expect_equal(env$live_selected_object$type, "annotation")
  expect_equal(env$live_selected_object$id, "roi-1")
  expect_equal(env$live_measurements$id, "measure_1")
  expect_equal(env$live_trajectories$id, "trajectory_1")
  expect_s3_class(env$live_roi_summary, "data.frame")
  expect_s3_class(env$live_cell_summary, "data.frame")
  expect_s3_class(env$live_class_summary, "data.frame")
  expect_equal(env$live_history$action, "roi_added")
  expect_s3_class(env$live_logs, "wsi_viewer_logs")
  expect_equal(env$live_logs$message, "Tiles did not load")
  expect_s3_class(env$live_annotation_spots, "wsi_annotation_spots")
  expect_equal(env$live_annotation_spots$spot_label, "AAAC-1")
  expect_equal(env$live_last_event$event, "roi_added")

  payload$event <- "trajectory_profile_finished"
  payload$detail <- list(
    trajectory_profile = list(
      list(
        trajectory_id = "trajectory_1",
        trajectory_name = "Trajectory 1",
        source_id = "seurat_spots",
        source_name = "Spatial spots",
        feature = "CD8A",
        feature_type = "numeric",
        bin = 1,
        distance_px = 5,
        distance_fraction = 0.25,
        width_px = 512,
        total_length_px = 20,
        count = 2,
        mean = 1.5,
        median = 1.5,
        min = 1,
        max = 2,
        sd = 0.7,
        project_image = "section 1"
      ),
      list(
        trajectory_id = "trajectory_1",
        trajectory_name = "Trajectory 1",
        source_id = "seurat_spots",
        source_name = "Spatial spots",
        feature = "cluster",
        feature_type = "categorical",
        category = "tumour",
        bin = 2,
        distance_px = 15,
        distance_fraction = 0.75,
        width_px = 512,
        total_length_px = 20,
        count = 3,
        dominant = "tumour",
        dominant_count = 2,
        fraction = 2 / 3,
        project_image = "section 1"
      )
    )
  )
  wsiTools:::wsi_viewer_state_apply(state, payload)
  snapshot <- wsi_viewer_state(state)
  expect_s3_class(snapshot$trajectory_profile, "wsi_trajectory_profile")
  expect_equal(nrow(snapshot$trajectory_profile), 2)
  expect_equal(snapshot$trajectory_profile$feature[[1]], "CD8A")
  expect_equal(snapshot$trajectory_profile$mean[[1]], 1.5)
  expect_equal(snapshot$trajectory_profile$dominant[[2]], "tumour")
  expect_equal(env$live_trajectory_profile$source_id[[1]], "seurat_spots")

  payload$event <- "trajectory_profile_cleared"
  payload$detail <- list()
  wsiTools:::wsi_viewer_state_apply(state, payload)
  expect_equal(nrow(wsi_viewer_state(state)$trajectory_profile), 0)

  payload$event <- "segmentation_added"
  payload$detail <- list(
    added = 1,
    crop = "roi_crop.png",
    output = "cellphenotyper_cells.geojson",
    status = "complete"
  )
  wsiTools:::wsi_viewer_state_apply(state, payload)
  snapshot <- wsi_viewer_state(state)

  expect_equal(snapshot$last_segmentation$crop, "roi_crop.png")
  expect_equal(env$live_last_segmentation$output, "cellphenotyper_cells.geojson")
})

test_that("live viewer sessions sync IHC intensity measurements to R", {
  env <- new.env(parent = emptyenv())
  slide <- wsiTools:::wsi_mock_slide(width = 120, height = 80, levels = c(1, 4))
  state <- wsiTools:::wsi_new_viewer_state(name = "live", envir = env)
  session <- wsiTools:::wsi_attach_viewer_session_methods(structure(
    list(
      server = NULL,
      url = NULL,
      state = state,
      slide = slide,
      html = tempfile(fileext = ".html"),
      name = "live",
      envir = env,
      stardist_server = NULL
    ),
    class = "wsi_viewer_session"
  ))
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(list(
      type = "Feature",
      id = "tumour-1",
      properties = list(name = "Tumour", classification = list(name = "tumour")),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)))
      )
    ))
  ))
  channels <- structure(
    list(
      hematoxylin = matrix(0.25, nrow = 12, ncol = 12),
      hrp_dab = matrix(0.5, nrow = 12, ncol = 12),
      channel_metadata = list(
        list(id = "hematoxylin", name = "Hematoxylin"),
        list(id = "hrp_dab", name = "HRP/DAB")
      )
    ),
    class = "wsi_ihc_channels"
  )

  session$add_rois(rois, service = FALSE)
  report <- session$measure_ihc_intensity(channels, dab_threshold = 0.3, service = FALSE)
  snapshot <- session$get_state(service = FALSE)

  expect_s3_class(report, "wsi_ihc_intensity_report")
  expect_equal(snapshot$ihc_summary$ihc_dab_mean, 0.5)
  expect_equal(snapshot$ihc_summary$ihc_hematoxylin_density, 0.25)
  expect_equal(snapshot$ihc_summary$ihc_dab_h_ratio, 2)
  expect_equal(snapshot$roi_summary$ihc_dab_positive_area_px2, 100)
  expect_equal(snapshot$class_summary$ihc_dab_mean, 0.5)
  expect_s3_class(env$live_ihc_summary, "data.frame")
  expect_equal(env$live_ihc_summary$ihc_dab_positive_pixels, 100L)
  expect_equal(env$live_last_event$event, "ihc_intensity_measured")
})

test_that("interactive viewer exposes Cells controls when a segmentation endpoint is supplied", {
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
  expect_match(html, "id=\"segmentationEngine\"", fixed = TRUE)
  expect_match(html, "StarDist H&amp;E", fixed = TRUE)
  expect_match(html, "StarDist IHC", fixed = TRUE)
  expect_match(html, "Mesmer DAPI", fixed = TRUE)
  expect_match(html, "id=\"startSegmentation\"", fixed = TRUE)
  expect_match(html, "id=\"loadSegmentationMask\"", fixed = TRUE)
  expect_match(html, "segmentationRunUrl", fixed = TRUE)
  expect_match(html, "fetch(url", fixed = TRUE)
})

test_that("ROI-aware segmentation writes results into the live viewer state", {
  env <- new.env(parent = emptyenv())
  state <- wsiTools:::wsi_new_viewer_state(name = "live", envir = env)
  roi <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(list(
      type = "Feature",
      id = "roi-1",
      properties = list(name = "Tumour", classification = list(name = "tumour")),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(c(10, 20), c(60, 20), c(60, 80), c(10, 80), c(10, 20)))
      )
    ))
  ))
  centroids <- data.frame(cell_id = "cell-1", x = 25, y = 35)
  class(centroids) <- c("wsi_segmentation_centroids", "wsi_segmentation", class(centroids))
  result <- list(
    input = "roi_crop.png",
    output = "cells.csv",
    segmentation = centroids,
    crop = "roi_crop.png",
    roi_id = "roi-1",
    bbox = c(x = 10, y = 20, width = 50, height = 60),
    status = "complete"
  )

  wsiTools:::wsi_viewer_state_set_selected_roi(
    state,
    roi,
    event = "segmentation_requested",
    detail = list(engine = "cellphenotyper")
  )
  wsiTools:::wsi_viewer_state_add_segmentation_result(state, result, cell_radius = 4)
  snapshot <- wsi_viewer_state(state)

  expect_s3_class(snapshot$selected_roi, "wsi_roi")
  expect_equal(snapshot$selected_roi$roi_id, "roi-1")
  expect_s3_class(snapshot$segmentation, "wsi_roi")
  expect_equal(nrow(snapshot$segmentation), 1)
  expect_equal(snapshot$last_segmentation$crop, "roi_crop.png")
  expect_equal(snapshot$last_segmentation$roi_id, "roi-1")
  expect_equal(snapshot$last_event, "segmentation_finished")
  expect_s3_class(env$live_segmentation, "wsi_roi")
  expect_equal(env$live_last_segmentation$output, "cells.csv")
})

test_that("live viewer keeps selected-ROI cell segmentation arguments", {
  args <- names(formals(wsi_viewer_session))

  expect_true("stardist" %in% args)
	  expect_true("transport" %in% args)
	  expect_true("dynamic_tiles" %in% args)
	  expect_true("spatial_tile_path" %in% args)
	  expect_true("stardist_command" %in% args)
  expect_true("stardist_args" %in% args)
  expect_true("stardist_output_dir" %in% args)
  expect_true("segmentation_engines" %in% args)
  expect_true("mesmer_command" %in% args)
  expect_true("segmentation_tiles_x" %in% args)
  expect_true("autosave" %in% args)
  expect_true("autosave_path" %in% args)
  expect_true("autosave_interval" %in% args)
  expect_false("wsi_viewer_stardist" %in% getNamespaceExports("wsiTools"))
  expect_false("wsi_stardist_server" %in% getNamespaceExports("wsiTools"))
})

test_that("dynamic tile sources create OpenSeadragon-compatible URLs and cache files", {
  slide <- wsiTools:::wsi_mock_slide(width = 1024, height = 768, levels = c(1, 4, 16))
  source <- wsi_dynamic_tile_source(slide, slide_id = "case 1", tile_size = 256)
  on.exit(wsiTools:::wsi_dynamic_tile_cleanup(source), add = TRUE)
  metadata <- wsiTools:::wsi_dynamic_tile_metadata(source, base_url = "http://127.0.0.1:8788")
  request <- wsiTools:::wsi_dynamic_tile_parse("/tiles/case_1/10/0/0.png")
  file <- wsiTools:::wsi_dynamic_tile_file(source, level = source$max_level, col = 0, row = 0)

  expect_s3_class(source, "wsi_dynamic_tile_source")
  expect_equal(source$id, "case_1")
  expect_equal(source$tile_overlap, 1L)
  expect_match(metadata$tile_url_template, "/tiles/case_1/{level}/{x}/{y}.{format}", fixed = TRUE)
  expect_equal(metadata$tile_overlap, 1L)
  expect_equal(request$slide_id, "case_1")
  expect_equal(request$format, "png")
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
  expect_false(any(grepl("\\.lock$", list.files(source$cache_dir, recursive = TRUE))))
})

test_that("vips-backed dynamic tiles cache low-resolution fit levels", {
  skip_if_not(wsi_has_vips())

  input <- tempfile(fileext = ".png")
  grDevices::png(input, width = 1024, height = 512, units = "px", bg = "white")
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::rect(0, 0, 1, 1, col = "#dbeafe", border = NA)
  graphics::rect(0.25, 0.25, 0.75, 0.75, col = "#2563eb", border = NA)
  grDevices::dev.off()

  slide <- wsi_open(input, backend = "vips")
  on.exit(wsi_close(slide), add = TRUE)
  source <- wsi_dynamic_tile_source(slide, slide_id = "vips fit", tile_size = 64, format = "jpg")
  on.exit(wsiTools:::wsi_dynamic_tile_cleanup(source), add = TRUE)
  level <- max(0L, source$max_level - 6L)
  region <- wsiTools:::wsi_dynamic_tile_region(source, level = level, col = 0, row = 0)
  file <- wsiTools:::wsi_dynamic_tile_file(source, level = level, col = 0, row = 0, format = "jpg")
  level_cache <- file.path(
    source$cache_dir,
    source$cache_namespace,
    "_levels",
    sprintf("level_%d.tif", level)
  )

  expect_true(file.exists(level_cache))
  expect_true(file.exists(file))
  expect_equal(as.integer(wsiTools:::wsi_vips_field(file, "width")), region$desired_width)
  expect_equal(as.integer(wsiTools:::wsi_vips_field(file, "height")), region$desired_height)
})

test_that("persistent dynamic tiles use stable fingerprints and HTTP validation", {
  slide <- wsiTools:::wsi_mock_slide(width = 640, height = 480, levels = c(1, 4))
  cache_dir <- tempfile("persistent_tile_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  first <- wsi_dynamic_tile_source(
    slide,
    slide_id = "persistent case",
    tile_size = 128,
    cache_dir = cache_dir,
    persistent_cache = TRUE
  )
  second <- wsi_dynamic_tile_source(
    slide,
    slide_id = "persistent case",
    tile_size = 128,
    cache_dir = cache_dir,
    persistent_cache = TRUE
  )
  changed <- wsi_dynamic_tile_source(
    slide,
    slide_id = "persistent case",
    tile_size = 256,
    cache_dir = cache_dir,
    persistent_cache = TRUE
  )

  expect_true(first$cache_persistent)
  expect_equal(first$cache_key, second$cache_key)
  expect_equal(first$cache_namespace, second$cache_namespace)
  expect_false(identical(first$cache_key, changed$cache_key))

  response <- wsiTools:::wsi_dynamic_tile_response(
    first,
    level = first$max_level,
    col = 0,
    row = 0
  )
  expect_equal(response$status, 200L)
  expect_equal(response$headers[["Cache-Control"]], "public, max-age=31536000, immutable")
  expect_equal(response$headers[["X-wsiTools-Tile-Cache"]], "miss")
  expect_match(response$headers[["ETag"]], '^"v2_')

  validated <- wsiTools:::wsi_dynamic_tile_response(
    second,
    level = second$max_level,
    col = 0,
    row = 0,
    request_etag = response$headers[["ETag"]]
  )
  expect_equal(validated$status, 304L)
  expect_length(validated$body, 0L)
  expect_equal(validated$headers[["X-wsiTools-Tile-Cache"]], "hit")

  namespace <- file.path(cache_dir, first$cache_namespace)
  expect_true(dir.exists(namespace))
  wsiTools:::wsi_dynamic_tile_cleanup(first)
  expect_true(dir.exists(namespace))
  wsiTools:::wsi_dynamic_tile_cleanup(first, force = TRUE)
  expect_false(dir.exists(namespace))
})

test_that("mIHC channel sources use registration extent and dynamic tiles", {
  skip_if_not(wsi_has_vips())

  input <- tempfile(fileext = ".png")
  grDevices::png(input, width = 32, height = 24, units = "px", bg = "black")
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::rect(0, 0, 1, 1, col = "white", border = NA)
  grDevices::dev.off()
  registration <- list(
    crop_bbox_xyxy = list(x0 = 10, y0 = 20, x1 = 42, y1 = 44),
    crop_size = list(width = 32, height = 24),
    offset_crop_to_original = list(dx = 10, dy = 20)
  )

  sources <- wsi_mihc_channel_sources(
    input,
    pages = 0,
    channel_names = "Marker A",
    colours = "#ff00ff",
    registration = registration,
    tile_size = 16
  )
  dynamic <- sources$dynamic_sources[[1L]]
  metadata <- wsiTools:::wsi_dynamic_tile_metadata(dynamic, base_url = "http://127.0.0.1:8788")
  channel <- wsiTools:::wsi_channel_source_from_dynamic(dynamic, base_url = "http://127.0.0.1:8788")
  file <- wsiTools:::wsi_dynamic_tile_file(dynamic, level = dynamic$max_level, col = 0, row = 0)
  on.exit(wsiTools:::wsi_dynamic_tile_cleanup(dynamic), add = TRUE)

  expect_s3_class(sources, "wsi_mihc_channel_sources")
  expect_equal(unlist(dynamic$extent), c(x = 10, y = 20, width = 32, height = 24))
  expect_match(metadata$tile_url_template, "/tiles/", fixed = TRUE)
  expect_equal(metadata$tile_overlap, 1L)
  expect_equal(channel$type, "dynamic")
  expect_equal(channel$tile_overlap, 1L)
  expect_equal(channel$metadata$extent$x, 10)
  expect_true(isTRUE(channel$metadata$server_colourized))
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})

test_that("dynamic mIHC channel tiles are colourized with alpha", {
  skip_if_not(wsi_has_vips())

  input <- tempfile(fileext = ".tif")
  bright <- tempfile(fileext = ".tif")
  wsiTools:::wsi_run_command(
    "vips",
    c("black", input, "32", "32", "--bands", "1"),
    error_message = "libvips failed to create a test channel image."
  )
  wsiTools:::wsi_run_command(
    "vips",
    c("linear", input, bright, "0", "180"),
    error_message = "libvips failed to create a test channel image."
  )

  source <- wsi_dynamic_image_tile_source(
    bright,
    source_id = "colour_test",
    colour = "#ff3366",
    tile_size = 16
  )
  on.exit(wsiTools:::wsi_dynamic_tile_cleanup(source), add = TRUE)

  file <- wsiTools:::wsi_dynamic_tile_file(
    source,
    level = source$max_level,
    col = 0,
    row = 0,
    format = "png",
    settings = list(colour = "#ff3366")
  )
  expect_equal(as.integer(wsiTools:::wsi_vips_field(file, "bands")), 4L)

  band_avg <- function(index) {
    band <- tempfile(fileext = ".tif")
    wsiTools:::wsi_run_command(
      "vips",
      c("extract_band", file, band, as.character(index), "--n", "1"),
      error_message = "libvips failed to inspect a colourized test tile."
    )
    as.numeric(system2("vips", c("avg", band), stdout = TRUE))
  }

  expect_equal(round(band_avg(0)), 180)
  expect_equal(round(band_avg(1)), 36)
  expect_equal(round(band_avg(2)), 72)
  expect_equal(round(band_avg(3)), 180)
})

test_that("H&E stain deconvolution can be served as dynamic channel tiles", {
  slide <- wsiTools:::wsi_mock_slide(width = 512, height = 384, levels = c(1, 4))
  slide$path <- "mock_he_slide.svs"
  sources <- wsi_stain_channel_sources(slide, tile_size = 128)
  on.exit(lapply(sources$dynamic_sources, wsiTools:::wsi_dynamic_tile_cleanup), add = TRUE)

  metadata <- lapply(
    sources$dynamic_sources,
    wsiTools:::wsi_dynamic_tile_metadata,
    base_url = "http://127.0.0.1:8788"
  )
  channels <- lapply(
    sources$dynamic_sources,
    wsiTools:::wsi_channel_source_from_dynamic,
    base_url = "http://127.0.0.1:8788"
  )
  file <- wsiTools:::wsi_dynamic_tile_file(
    sources$dynamic_sources[[1L]],
    level = sources$dynamic_sources[[1L]]$max_level,
    col = 0,
    row = 0,
    format = "png",
    settings = list(colour = "#4b3f99")
  )

  expect_s3_class(sources, "wsi_stain_channel_sources")
  expect_equal(length(sources$dynamic_sources), 3L)
  expect_equal(vapply(sources$dynamic_sources, `[[`, character(1), "kind"), rep("stain_channel", 3))
  expect_equal(vapply(metadata, function(x) x$metadata$stain_channel_id, character(1)), c("hematoxylin", "eosin", "residual"))
  expect_true(all(vapply(metadata, function(x) identical(x$metadata$target_path, slide$path), logical(1))))
  expect_true(all(vapply(metadata, function(x) identical(x$metadata$slide_path, slide$path), logical(1))))
  expect_true(all(vapply(channels, function(x) identical(x$type, "dynamic"), logical(1))))
  expect_true(all(vapply(channels, function(x) isTRUE(x$metadata$server_colourized), logical(1))))
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})

test_that("live static channel sources beside the viewer use relative URLs", {
  root <- tempfile("viewer-root-")
  output <- file.path(root, "viewer.html")
  tile_files <- file.path(root, "viewer_spatial_masks", "seurat_spots_deepzoom", "slide_files")
  dir.create(tile_files, recursive = TRUE)

  source <- wsi_channel_source(
    name = "Seurat spots",
    id = "seurat_spots",
    type = "deepzoom",
    tile_url_base = wsiTools:::wsi_file_url(tile_files),
    width = 128,
    height = 128,
    tile_size = 64,
    tile_format = "png",
    max_level = 7,
    metadata = list(kind = "mask", transparent_background = TRUE)
  )
  live_sources <- wsiTools:::wsi_live_channel_sources(list(source), output = output)

  expect_length(live_sources, 1L)
  expect_false(grepl("^file://", live_sources[[1L]]$tile_url_base))
  expect_equal(
    live_sources[[1L]]$tile_url_base,
    "viewer_spatial_masks/seurat_spots_deepzoom/slide_files"
  )
  expect_true(isTRUE(live_sources[[1L]]$metadata$served_relative_to_viewer))
  expect_null(live_sources[[1L]]$metadata$tile_url_base_original)
})

test_that("H&E mIHC live wrapper defaults the dynamic base image to JPEG tiles", {
  wsi_skip_if_no_httpuv_server()
  skip_if_not(wsi_has_vips())

  input <- tempfile(fileext = ".png")
  grDevices::png(input, width = 32, height = 24, units = "px", bg = "white")
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::rect(0, 0, 1, 1, col = "#d8b5c8", border = NA)
  grDevices::dev.off()

  session <- wsi_viewer_he_mihc(
    he = input,
    mihc = input,
    pages = 0,
    dynamic_tiles = TRUE,
    open = FALSE,
    wait = FALSE,
    transport = "polling",
    name = "he_mihc_default_jpg"
  )
  on.exit(if (inherits(session, "wsi_viewer_session")) wsi_viewer_stop(session), add = TRUE)

  expect_equal(session$dynamic_tile_source$tile_format, "jpg")
  expect_equal(session$state$tile_sources$dynamic$tile_format, "jpg")
  mihc_sources <- Filter(function(x) identical(x$metadata$source_type, "mihc"), session$state$channel_sources)
  expect_length(mihc_sources, 1L)
  expect_true(all(vapply(mihc_sources, function(x) identical(x$metadata$target_path, normalizePath(input, winslash = "/", mustWork = FALSE)), logical(1))))
  expect_true(all(vapply(mihc_sources, function(x) identical(x$metadata$target_role, "base"), logical(1))))
  html <- paste(readLines(session$html, warn = FALSE), collapse = "\n")
  expect_match(html, "H&E", fixed = TRUE)
  expect_match(html, "H&E H/E/residual", fixed = TRUE)
  expect_false(grepl("<input id=\"stainVisible_hematoxylin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainVisible_eosin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainVisible_residual\"", html, fixed = TRUE))
  expect_match(html, "stainShowOriginal", fixed = TRUE)
  expect_match(html, "baseImageVisible", fixed = TRUE)
  settings <- session$get_channel_settings(service = FALSE)$id
  expect_true(any(grepl("stain_hematoxylin", settings, fixed = TRUE)))
})

test_that("live viewer can use dynamic tiles with polling transport", {
  wsi_skip_if_no_httpuv_server()
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  session <- wsi_viewer_live(
    slide,
    mode = "tiles",
    dynamic_tiles = TRUE,
    transport = "polling",
    open = FALSE,
    wait = FALSE
  )
  cache_dir <- session$dynamic_tile_cache_dir
  on.exit(if (inherits(session, "wsi_viewer_session")) wsi_viewer_stop(session), add = TRUE)
  html <- paste(readLines(session$html, warn = FALSE), collapse = "\n")

  expect_s3_class(session, "wsi_viewer_session")
  expect_equal(session$transport, "polling")
  expect_false(grepl(session$ws_url, html, fixed = TRUE))
  expect_match(html, "/tiles/wsi_viewer_live_state/{level}/{x}/{y}.{format}", fixed = TRUE)
  expect_match(html, "/image-export", fixed = TRUE)
  expect_match(html, "dynamic tile server", fixed = TRUE)
  expect_true(dir.exists(cache_dir))

  wsi_viewer_stop(session)
  expect_false(dir.exists(cache_dir))
})

test_that("live H&E viewer wires deconvolution as tiled channel layers", {
  wsi_skip_if_no_httpuv_server()
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  session <- wsi_viewer_live(
    slide,
    mode = "tiles",
    dynamic_tiles = TRUE,
    stain = "he",
    transport = "polling",
    open = FALSE,
    wait = FALSE,
    name = "he_tiled_stains"
  )
  on.exit(if (inherits(session, "wsi_viewer_session")) wsi_viewer_stop(session), add = TRUE)
  html <- paste(readLines(session$html, warn = FALSE), collapse = "\n")
  settings <- session$get_channel_settings(service = FALSE)

  expect_true(any(grepl("stain_hematoxylin", settings$id, fixed = TRUE)))
  expect_true(any(grepl("stain_eosin", settings$id, fixed = TRUE)))
  expect_true(any(grepl("stain_residual", settings$id, fixed = TRUE)))
  expect_match(html, "syncTiledStainChannels", fixed = TRUE)
  expect_false(grepl("setExclusiveTiledHEChannel", html, fixed = TRUE))
  expect_false(grepl("H&E tiled deconvolution displays one channel at a time", html, fixed = TRUE))
  expect_match(html, "showDefaultStains", fixed = TRUE)
  expect_match(html, "showAllStains", fixed = TRUE)
  expect_match(html, "Showing all tiled stain channels", fixed = TRUE)
  expect_match(html, "setBaseImageVisibleForStain", fixed = TRUE)
  expect_match(html, "tiledStainSettings", fixed = TRUE)
  expect_match(html, "applyTiledStainSourceVisibility", fixed = TRUE)
  expect_match(html, "removeChannelItem(src.id)", fixed = TRUE)
  expect_match(html, "if(stainOn&&s&&s.visible)active.push(i)", fixed = TRUE)
  expect_match(html, "Showing original RGB image.", fixed = TRUE)
  expect_match(html, "const pending=channelPendingItems.has(src.id)", fixed = TRUE)
  expect_match(html, "latest.visible===false", fixed = TRUE)
  expect_match(html, "osdViewer.world.removeItem(event.item)", fixed = TRUE)
  expect_match(html, "pane.viewer.world.removeItem(event.item)", fixed = TRUE)
  expect_match(html, "stainChannelIndex", fixed = TRUE)
  expect_match(html, "button.dataset.stainId", fixed = TRUE)
  expect_match(html, "setStainVisible([button.dataset.stainId||button.dataset.stainIndex],'only')", fixed = TRUE)
  expect_match(html, "channelSourceById", fixed = TRUE)
  expect_match(html, "channelNeedsReload(src,settings)", fixed = TRUE)
  expect_match(html, "stain_channel", fixed = TRUE)
})

test_that("viewer event validation allowlists live WebSocket events", {
  expected <- c(
    "roi_created", "roi_updated", "roi_label_updated", "roi_curve_edited",
    "roi_deleted", "roi_selected",
    "brush_committed", "annotation_mask_updated", "viewport_changed", "layer_updated",
    "trajectory_deleted", "trajectory_area_created", "trajectory_area_updated",
    "trajectory_profile_started", "trajectory_profile_finished",
    "trajectory_profile_failed", "trajectory_profile_cleared",
    "segmentation_started", "segmentation_progress",
    "segmentation_finished", "job_status", "project_image_reordered",
    "project_image_closed", "grandqc_loaded", "grandqc_cleared",
    "kodama_cells_selected", "seurat_cluster_coloured",
    "seurat_plot_scope_changed", "spatial_registration_saved",
    "annotation_undo", "annotation_redo",
    "spatial_object_save_requested",
    "viewer_log_updated", "viewer_log_cleared", "viewer_log_exported",
    "multi_view_layout_updated", "multi_view_pane_replaced", "multi_view_sync_updated",
    "geojson_mask_overlay_created"
  )

  expect_true(all(expected %in% wsiTools:::wsi_viewer_allowed_events()))
  expect_equal(wsiTools:::wsi_viewer_validate_event("brush_committed"), "brush_committed")
  expect_equal(wsiTools:::wsi_viewer_validate_event("roi_label_updated"), "roi_label_updated")
  expect_error(
    wsiTools:::wsi_viewer_validate_state_payload(list(event = "eval_r_code")),
    "Unsupported viewer event"
  )
  expect_error(
    wsiTools:::wsi_viewer_validate_state_payload(list(event = "roi_created", code = "system('rm -rf /')")),
    "unsupported field"
  )
  expect_silent(wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "kodama_cells_selected",
    kodama_selection = list(labels = c("1", "2"), count = 2L, matched_count = 2L)
  )))
  expect_silent(wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "roi_selected",
    selected_object = list(type = "annotation", index = 0, id = "roi-1", name = "Tumour 1")
  )))
  expect_silent(wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "multi_view_layout_updated",
    detail = list(layout = 2L, sync = TRUE)
  )))
  expect_silent(wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "multi_view_pane_replaced",
    detail = list(pane = 1L, label = "151507", key = "project:0:0", layout = 2L)
  )))
  expect_silent(wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "seurat_plot_scope_changed",
    detail = list(scope = "all", requested_scope = "all")
  )))
  expect_silent(wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "spatial_registration_saved",
    detail = list(spatial_registration = list(
      source = "Seurat",
      count = 2L,
      changed_count = 1L,
      coordinates = list(list(
        source = "Seurat",
        layer_id = "seurat_spots",
        layer_name = "Seurat spots",
        item_index = 1L,
        id = "spot_1",
        label = "spot_1",
        x = 12,
        y = 34,
        original_x = 10,
        original_y = 30,
        changed = TRUE
      ))
    ))
  )))
  expect_silent(wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "spatial_object_save_requested",
    detail = list(spatial_registration = list(
      source = "Seurat",
      count = 2L,
      changed_count = 1L,
      coordinates = list(list(
        source = "Seurat",
        layer_id = "seurat_spots",
        layer_name = "Seurat spots",
        item_index = 1L,
        id = "spot_1",
        label = "spot_1",
        x = 12,
        y = 34,
        original_x = 10,
        original_y = 30,
        changed = TRUE
      ))
    ))
  )))

  state <- wsiTools:::wsi_new_viewer_state(name = "registration_test_state", envir = new.env(parent = emptyenv()))
  wsiTools:::wsi_viewer_state_apply(state, list(
    event = "spatial_object_save_requested",
    rois = list(type = "FeatureCollection", features = list()),
    selected_rois = list(type = "FeatureCollection", features = list()),
    detail = list(spatial_registration = list(coordinates = list(list(
      source = "Seurat",
      layer_id = "seurat_spots",
      layer_name = "Seurat spots",
      item_index = 1L,
      id = "spot_1",
      label = "spot_1",
      x = 12,
      y = 34,
      original_x = 10,
      original_y = 30,
      changed = TRUE
    ))))
  ))
  expect_equal(nrow(state$spatial_registration), 1L)
  expect_equal(state$spatial_registration$id, "spot_1")
  expect_true(state$spatial_registration$changed)
})

test_that("channel source API updates live viewer settings", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  state <- wsiTools:::wsi_new_viewer_state(name = "channel_test_state", envir = new.env(parent = emptyenv()))
  session <- structure(
    list(state = state, slide = slide, url = NULL, ws_url = NULL, jobs = list()),
    class = "wsi_viewer_session"
  )
  session <- wsiTools:::wsi_attach_viewer_session_methods(session)
  source <- wsi_channel_source(
    "DAB",
    type = "stain",
    vector = c(0.268, 0.570, 0.776),
    colour = "#8b5a2b",
    opacity = 0.75,
    contrast_min = 0.1,
    contrast_max = 1.5
  )

  wsi_add_channel_source(session, source, service = FALSE)
  wsi_set_channel_visible(session, "DAB", FALSE, service = FALSE)
  wsi_set_channel_opacity(session, "DAB", 0.4, service = FALSE)
  wsi_set_channel_contrast(session, "DAB", contrast_min = 0.2, contrast_max = 1.2, gain = 1.8, service = FALSE)
  settings <- session$get_channel_settings(service = FALSE)
  commands <- state$commands

  expect_s3_class(source, "wsi_channel_source")
  expect_equal(settings$id, "DAB")
  expect_false(settings$visible)
  expect_equal(settings$opacity, 0.4)
  expect_equal(settings$gain, 1.8)
  expect_equal(settings$contrast_min, 0.2)
  expect_equal(settings$contrast_max, 1.2)
  expect_true(any(vapply(commands, function(x) identical(x$type, "set_channel_settings"), logical(1))))
})

test_that("live viewer can start the optional cell segmentation endpoint", {
  wsi_skip_if_no_httpuv_server()
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  session <- NULL
  on.exit(if (inherits(session, "wsi_viewer_session")) wsi_viewer_stop(session), add = TRUE)

  session <- wsi_viewer_live(
    slide,
    width = 128,
    open = FALSE,
    wait = FALSE,
    stardist = TRUE,
    stardist_command = "missing-wsitools-stardist-command"
  )

  expect_s3_class(session, "wsi_viewer_session")
  expect_match(session$ws_url, "^ws://")
  expect_s3_class(session$stardist_server, "wsi_stardist_server")
  expect_match(session$stardist_server$url, "/segment", fixed = TRUE)
  html <- paste(readLines(session$html, warn = FALSE), collapse = "\n")
  expect_match(html, session$ws_url, fixed = TRUE)
  expect_match(html, "WebSocket connected", fixed = TRUE)
  expect_match(html, "id=\"startSegmentation\"", fixed = TRUE)
  expect_match(html, session$stardist_server$url, fixed = TRUE)
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
          "properties": {"name": "Tumor", "classification": {"name": "tumor", "color": 16711680}},
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
  expect_match(html, "#D73027", fixed = TRUE)
  expect_match(html, "line-1", fixed = TRUE)
  expect_match(html, "Margin label", fixed = TRUE)
  expect_match(html, "LineString", fixed = TRUE)
  expect_match(html, "overlayFocusMode='annotation'", fixed = TRUE)
  expect_match(html, "roiToggle", fixed = TRUE)
  expect_match(html, "layersToggle", fixed = TRUE)
  expect_match(html, "roiOpacity", fixed = TRUE)
  expect_match(html, "saveRoiOpacityPreference", fixed = TRUE)
  expect_match(html, "savePanelPreferences", fixed = TRUE)
  expect_match(html, "setRoiPanelPosition", fixed = TRUE)
  expect_match(html, "startRoiPanelDrag", fixed = TRUE)
  expect_match(html, "GeoJSON Geometries", fixed = TRUE)
  expect_match(html, "id=\"roiPanel\" class=\"panel open\"", fixed = TRUE)
  expect_match(html, "aria-label=\"Annotation manager\"", fixed = TRUE)
  expect_match(html, "annotationExportSelected", fixed = TRUE)
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
  expect_true(file.exists(file.path(tile_dir, "slide.wsiTools.json")))
  expect_match(paste(readLines(file.path(tile_dir, "slide.dzi"), warn = FALSE), collapse = " "), "Overlap=\"1\"", fixed = TRUE)

  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "Deep Zoom tiles", fixed = TRUE)
  expect_match(html, "slide_files", fixed = TRUE)
  expect_match(html, "openseadragon.min.js", fixed = TRUE)
  expect_match(html, "OpenSeadragon", fixed = TRUE)
  expect_match(html, "maxImageCacheCount", fixed = TRUE)
  expect_match(html, "prefetchNeighborTiles", fixed = TRUE)
  expect_match(html, "placeholderFillStyle", fixed = TRUE)
  expect_match(html, "baseImageVisible", fixed = TRUE)
  expect_match(html, "baseImageOpacity", fixed = TRUE)
  expect_match(html, "baseImagePayload", fixed = TRUE)
  expect_match(html, "showNavigator:false", fixed = TRUE)
  expect_match(html, "miniNavigator", fixed = TRUE)
  expect_match(html, "navigator_image_data_uri", fixed = TRUE)
  expect_match(html, "projectNavigatorSourceKey", fixed = TRUE)
  expect_match(html, "projectNavigatorSource", fixed = TRUE)
  expect_match(html, "drawMiniNavigatorOverview", fixed = TRUE)
  expect_match(html, "drawMiniNavigatorViewport", fixed = TRUE)
  expect_match(html, "imageToViewportCoordinates", fixed = TRUE)
  expect_match(html, "viewportToImageCoordinates", fixed = TRUE)
  expect_match(html, "id=\"overlay\"", fixed = TRUE)
  expect_match(html, ".bar{position:fixed;left:12px;right:12px;top:12px", fixed = TRUE)
  expect_match(html, "z-index:30", fixed = TRUE)
  expect_match(html, "#workspacePanel{position:fixed;left:12px;top:72px", fixed = TRUE)
  expect_match(html, "#roiPanel{position:relative;width:auto", fixed = TRUE)
  expect_match(html, "z-index:29", fixed = TRUE)
  expect_match(html, "requestDraw", fixed = TRUE)
  expect_match(html, "loadingTiles", fixed = TRUE)
  expect_false(grepl("<summary title=\"Visualize GrandQC artifact GeoJSON annotations\">Artifacts</summary>", html, fixed = TRUE))
  expect_match(html, "osdBaseCanvas", fixed = TRUE)
  expect_match(html, "osdBaseCanvasCandidates", fixed = TRUE)
  expect_match(html, "stainOverlayCanvas", fixed = TRUE)
  expect_match(html, "stainOverlayCacheKey", fixed = TRUE)
  expect_match(html, "hasTiledStainChannels()){stainOverlayCanvas=null", fixed = TRUE)
  expect_match(html, "Stain channel selection needs readable tiles", fixed = TRUE)
  expect_match(html, "loadAllGrandqcGeojsons", fixed = TRUE)
  expect_match(html, "drawArtifactOverlays", fixed = TRUE)
  expect_match(html, "saveGeojson", fixed = TRUE)
  expect_match(html, "annotationExportDialog", fixed = TRUE)
  expect_match(html, "bindAnnotationExportDialogControls", fixed = TRUE)
})

test_that("Deep Zoom tile cache is rebuilt when the source image changes", {
  skip_if_not(wsi_has_vips())

  input1 <- tempfile(fileext = ".ppm")
  input2 <- tempfile(fileext = ".ppm")
  pixels1 <- paste(rep("255 0 0", 48 * 32), collapse = " ")
  pixels2 <- paste(rep("0 0 255", 48 * 32), collapse = " ")
  writeLines(c("P3", "48 32", "255", pixels1), input1)
  writeLines(c("P3", "48 32", "255", pixels2), input2)

  slide1 <- wsi_open(input1, backend = "vips")
  slide2 <- wsi_open(input2, backend = "vips")
  on.exit(wsi_close(slide1), add = TRUE)
  on.exit(wsi_close(slide2), add = TRUE)

  tile_dir <- tempfile("wsi-viewer-stale-tiles-")
  wsi_create_deepzoom_tiles(
    slide1,
    tile_dir = tile_dir,
    tile_size = 16,
    tile_format = "png",
    rebuild = FALSE
  )
  metadata1 <- jsonlite::read_json(file.path(tile_dir, "slide.wsiTools.json"), simplifyVector = TRUE)
  expect_identical(metadata1$path, normalizePath(input1, winslash = "/", mustWork = FALSE))

  expect_warning(
    wsi_create_deepzoom_tiles(
      slide2,
      tile_dir = tile_dir,
      tile_size = 16,
      tile_format = "png",
      rebuild = FALSE
    ),
    "do not match the requested image"
  )
  metadata2 <- jsonlite::read_json(file.path(tile_dir, "slide.wsiTools.json"), simplifyVector = TRUE)
  expect_identical(metadata2$path, normalizePath(input2, winslash = "/", mustWork = FALSE))
})

test_that("project items carry slide scale metadata for the scale bar", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  item <- wsiTools:::wsi_viewer_project_item_from_slide(slide, include_preview = FALSE)

  expect_equal(item$mpp, list(x = 0.25, y = 0.25))
  expect_equal(item$objective_power, 40)
  expect_equal(wsiTools:::wsi_viewer_project_mpp(item), list(x = 0.25, y = 0.25))
  expect_equal(wsiTools:::wsi_viewer_project_objective_power(item), 40)
})

test_that("CZI metadata parser extracts micron scale from physical distances", {
  xml <- paste0(
    "<Metadata><Scaling><Items>",
    "<Distance Id=\"X\"><Value>2.5e-007</Value></Distance>",
    "<Distance Id=\"Y\"><Value>2.6e-007</Value></Distance>",
    "</Items></Scaling></Metadata>"
  )
  expect_equal(wsiTools:::wsi_native_czi_mpp(xml), c(x = 0.25, y = 0.26))
})

test_that("native CZI scene previews read the requested scene", {
  scene_preview_code <- paste(deparse(wsiTools:::wsi_native_czi_scene_previews), collapse = "\n")

  expect_match(scene_preview_code, "scene = scene_index", fixed = TRUE)
})

test_that("live CZI project opening keeps section previews lazy by default", {
  live_formals <- formals(wsiTools:::wsi_viewer_czi_project_live)
  expect_true("czi_preview" %in% names(live_formals))
  expect_equal(eval(live_formals$czi_preview), c("lazy", "all"))

  live_item_code <- paste(deparse(wsiTools:::wsi_czi_live_project_item), collapse = "\n")
  expect_match(live_item_code, "preview <- match.arg(preview)", fixed = TRUE)
  expect_match(live_item_code, "if (identical(preview, \"all\"))", fixed = TRUE)
  expect_match(live_item_code, "Section previews are lazy so the viewer opens quickly", fixed = TRUE)

  live_code <- paste(deparse(wsiTools:::wsi_viewer_czi_project_live), collapse = "\n")
  expect_match(live_code, "tile_prefetch_margin = -1L", fixed = TRUE)
  expect_match(live_code, "tile_timeout_ms = 120000L", fixed = TRUE)
  expect_match(live_code, "tile_image_loader_limit = 2L", fixed = TRUE)
})

test_that("live CZI tiles use persistent native readers when available", {
  source_formals <- formals(wsiTools:::wsi_dynamic_czi_section_tile_source)
  expect_true("persistent_reader" %in% names(source_formals))
  expect_true(isTRUE(eval(source_formals$persistent_reader)))

  source_code <- paste(deparse(wsiTools:::wsi_dynamic_czi_section_tile_source), collapse = "\n")
  expect_match(source_code, "wsi_native_czi_open_handle(path)", fixed = TRUE)
  expect_match(source_code, "WSITOOLS_CZI_PERSISTENT_TILE_READER", fixed = TRUE)

  region_code <- paste(deparse(wsiTools:::wsi_dynamic_czi_section_region_to_file), collapse = "\n")
  expect_match(region_code, "wsi_native_czi_handle_read_region", fixed = TRUE)
  expect_match(region_code, "scene = if (is.finite(scene) &&", fixed = TRUE)
  expect_match(region_code, "scene >= 0L", fixed = TRUE)
  expect_match(region_code, "x = source$x + region$x", fixed = TRUE)

  cleanup_code <- paste(deparse(wsiTools:::wsi_dynamic_tile_cleanup), collapse = "\n")
  expect_match(cleanup_code, "wsi_native_czi_close_handle", fixed = TRUE)
})

test_that("tiled viewer JavaScript exposes live tile timeout and loader limit controls", {
  html_code <- paste(deparse(wsiTools:::wsi_tiled_viewer_html), collapse = "\n")
  expect_match(html_code, "function tileTimeoutMs()", fixed = TRUE)
  expect_match(html_code, "function tileImageLoaderLimit()", fixed = TRUE)
  expect_match(html_code, "timeout:tileTimeoutMs()", fixed = TRUE)
  expect_match(html_code, "options.imageLoaderLimit", fixed = TRUE)
})

test_that("desktop launcher routes CZI files to the CZI live project viewer", {
  launcher_path <- test_path("../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R")
  skip_if_not(
    file.exists(launcher_path),
    "Desktop launcher resources are not included in source-package checks."
  )
  launcher <- readLines(launcher_path, warn = FALSE)
  launcher <- paste(launcher, collapse = "\n")

  expect_match(launcher, "desktop_is_czi_path", fixed = TRUE)
  expect_match(launcher, "desktop_open_czi_project", fixed = TRUE)
  expect_match(launcher, "wsi_viewer_czi_project_live", fixed = TRUE)
  expect_match(launcher, "czi_preview = \"lazy\"", fixed = TRUE)
  expect_match(launcher, "all(czi_paths)", fixed = TRUE)
})

test_that("desktop launcher isolates static viewer assets from live R work", {
  launcher_path <- test_path("../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R")
  skip_if_not(
    file.exists(launcher_path),
    "Desktop launcher resources are not included in source-package checks."
  )
  launcher <- paste(readLines(launcher_path, warn = FALSE), collapse = "\n")

  expect_match(launcher, "WSITOOLS_DESKTOP_SEPARATE_STATIC_SERVER", fixed = TRUE)
  expect_match(launcher, "callr::r_bg", fixed = TRUE)
  expect_match(launcher, "Cache-Control", fixed = TRUE)
  expect_match(launcher, "ETag", fixed = TRUE)
  expect_match(launcher, "dense_geojson_sources", fixed = TRUE)
  expect_match(launcher, "immediate browser-side level-of-detail rendering", fixed = TRUE)
  expect_match(launcher, "desktop_force_dynamic_tiles", fixed = TRUE)
  expect_match(launcher, "WSITOOLS_FORCE_DYNAMIC_TILES", fixed = TRUE)
  expect_match(
    launcher,
    "desktop_force_dynamic_tiles(do.call(wsiTools::wsi_viewer_spatial, single_args))",
    fixed = TRUE
  )
})

test_that("dense tissue annotations load once and coalesce viewport work", {
  html_code <- paste(deparse(wsiTools:::wsi_viewer_geometry_js), collapse = "\n")
  session_code <- paste(deparse(wsiTools:::wsi_start_viewer_state_server), collapse = "\n")

  expect_match(html_code, "denseGeojsonStaticLayer", fixed = TRUE)
  expect_match(html_code, "denseGeojsonStaticLoaded", fixed = TRUE)
  expect_match(html_code, "denseGeojsonInflight", fixed = TRUE)
  expect_match(html_code, "denseGeojsonQueued", fixed = TRUE)
  expect_match(html_code, "scheduleDenseGeojsonViewportLoad", fixed = TRUE)
  expect_match(html_code, "denseStaticUsesFullResolution", fixed = TRUE)
  expect_match(html_code, "_dense_full_groups", fixed = TRUE)
  expect_match(html_code, "Tissue annotation loaded with browser-side level-of-detail rendering", fixed = TRUE)
  expect_match(session_code, "static_url", fixed = TRUE)
  expect_match(session_code, "static_source", fixed = TRUE)
  expect_match(session_code, "full_resolution_zoom", fixed = TRUE)
  expect_match(html_code, "Math.max(256", fixed = TRUE)
  expect_match(html_code, "ceiling=!Number.isFinite(z)||z<1.5?5000:12000", fixed = TRUE)
})

test_that("desktop viewer keeps full tissue polygons for R analysis", {
  launcher_path <- test_path("../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R")
  skip_if_not(file.exists(launcher_path), "Desktop launcher resources are not included in source-package checks.")
  launcher <- paste(readLines(launcher_path, warn = FALSE), collapse = "\n")

  expect_match(launcher, "desktop_register_tissue_analysis", fixed = TRUE)
  expect_match(launcher, "viewer$state$analysis_rois <- analysis_rois", fixed = TRUE)
  expect_match(launcher, "Use Annotations > Associate spots/cells", fixed = TRUE)
  geometry_code <- paste(deparse(wsiTools:::wsi_viewer_trajectory_js), collapse = "\n")
  expect_match(geometry_code, "roiHasFullAnalysisGeometry", fixed = TRUE)
  expect_match(geometry_code, "meta.full_geometry_source===true", fixed = TRUE)
  viewer_code <- paste(deparse(wsiTools:::wsi_viewer_chrome), collapse = "\n")
  expect_match(viewer_code, "Associate spots/cells", fixed = TRUE)
  viewer_js <- paste(deparse(wsiTools:::wsi_viewer_geometry_js), collapse = "\n")
  expect_match(viewer_js, "associateAnnotationSpots", fixed = TRUE)
  expect_match(viewer_code, "closeMenuAfterToolAction(this)", fixed = TRUE)
})

test_that("navigator thumbnail is independent from progressive background preview", {
  viewer_code <- paste(deparse(wsiTools::wsi_viewer), collapse = "\n")
  expect_match(
    viewer_code,
    "navigator_image_data_uri = wsi_viewer_navigator_data_uri\\(slide,\\s+width = 512\\)"
  )
})

test_that("tiled viewer HTML uses OpenSeadragon with an overlay canvas", {
  html <- wsiTools:::wsi_tiled_viewer_html(list(
    title = "synthetic tiled viewer",
    subtitle = "test Deep Zoom tiles",
    slide_width = 48,
    slide_height = 32,
    mpp = NULL,
    tile_size = 16,
    tile_format = "png",
    tile_url_base = "synthetic_tiles/slide_files",
    dzi = "slide.dzi",
    max_level = 6,
    annotation_filename = "synthetic_annotations.geojson",
    segmentation_run_url = NULL,
    viewer_state_url = NULL,
    stain = list(enabled = FALSE),
    rois = list()
  ))

  expect_match(html, "openseadragon.min.js", fixed = TRUE)
  expect_match(html, "OpenSeadragon", fixed = TRUE)
  expect_match(html, "tileSources", fixed = TRUE)
  expect_match(html, "tileSourceFromConfig", fixed = TRUE)
  expect_match(html, "activeTileMode", fixed = TRUE)
  expect_match(html, "tileCacheCount", fixed = TRUE)
  expect_match(html, "tilePrefetchCacheCount", fixed = TRUE)
  expect_match(html, "maxImageCacheCount:tileCacheCount()", fixed = TRUE)
  expect_match(html, "blendTime:0", fixed = TRUE)
  expect_match(html, "minPixelRatio:1", fixed = TRUE)
  expect_match(html, "maxZoomPixelRatio:16", fixed = TRUE)
  expect_match(html, "placeholderFillStyle:progressivePreviewEnabled()?'rgba(255,255,255,0)':'#fff'", fixed = TRUE)
  expect_match(html, "installProgressivePreviewBackground", fixed = TRUE)
  expect_match(html, "subPixelRoundingForTransparency", fixed = TRUE)
  expect_match(html, "tileOverlap:Number", fixed = TRUE)
  expect_match(html, "baseImageVisible", fixed = TRUE)
  expect_match(html, "baseImagePayload", fixed = TRUE)
  expect_match(html, "showNavigator:false", fixed = TRUE)
  expect_match(html, "miniNavigatorCanvas", fixed = TRUE)
  expect_match(html, "navigatorImage", fixed = TRUE)
  expect_match(html, "drawMiniNavigatorOverview", fixed = TRUE)
  expect_match(html, "drawMiniNavigatorMarkers", fixed = TRUE)
  expect_match(html, "prefetchNeighborTiles", fixed = TRUE)
  expect_match(html, "placeRoiLabel", fixed = TRUE)
  expect_match(html, "labelRectOverlaps", fixed = TRUE)
  expect_match(html, "labelAnchorDistanceScore", fixed = TRUE)
  expect_match(html, "labelLeaderTarget", fixed = TRUE)
  expect_match(html, "drawLabelLeader", fixed = TRUE)
  expect_match(html, "labelRectOverlaps(c,r,12)", fixed = TRUE)
  expect_match(html, "crispLabelRect", fixed = TRUE)
  expect_match(html, "700 12px -apple-system", fixed = TRUE)
  expect_match(html, "strokeRoiBorderGroup", fixed = TRUE)
  expect_match(html, "highlightAllRoiClasses", fixed = TRUE)
  expect_match(html, "renderAnnotationLabelHighlights", fixed = TRUE)
  expect_match(html, "setAnnotationLabelHighlighted", fixed = TRUE)
  expect_match(html, "roiClassHighlighted(roi)", fixed = TRUE)
  expect_match(html, "lineDashOffset", fixed = TRUE)
  expect_match(html, "id=\"viewer\" class=\"osdViewer\"", fixed = TRUE)
  expect_match(html, "id=\"overlay\"", fixed = TRUE)
  expect_match(html, "id=\"multiViewGrid\"", fixed = TRUE)
  expect_match(html, ".multiViewPaneViewer{position:absolute!important;inset:0!important;width:100%!important;height:100%!important", fixed = TRUE)
  expect_match(html, ".multiViewPaneViewer>.openseadragon-container,.multiViewPaneViewer .openseadragon-canvas{width:100%!important;height:100%!important;}", fixed = TRUE)
  expect_match(html, "body.multiViewActive .multiViewPaneViewer{pointer-events:none;}", fixed = TRUE)
  expect_match(html, ".multiViewPaneOverlay{position:absolute;inset:0;z-index:20", fixed = TRUE)
  expect_match(html, "multiViewResizeLayer", fixed = TRUE)
  expect_match(html, "multiViewResizeHandle", fixed = TRUE)
  expect_match(html, "body.multiViewResizing", fixed = TRUE)
  expect_match(html, "pointer-events:auto!important", fixed = TRUE)
  expect_match(html, "bindMultiViewPaneInteractions", fixed = TRUE)
  expect_match(html, "multiViewFocusPane", fixed = TRUE)
  expect_match(html, "multiViewActivatePane", fixed = TRUE)
  expect_match(html, "multiViewEnsureEditingContext", fixed = TRUE)
  expect_match(html, "multiViewFocusPane(index,false);if(!pane||!pane.entry", fixed = TRUE)
  expect_match(html, "if(editMode&&!multiViewEnsureEditingContext(pane,index))", fixed = TRUE)
  expect_match(html, "multiViewFocusPane(index,true);multiViewZoomAtEvent", fixed = TRUE)
  expect_match(html, "multiViewPointerToSlide", fixed = TRUE)
  expect_match(html, "drawMultiViewOverlays", fixed = TRUE)
  expect_match(html, "multiViewDrawLayers", fixed = TRUE)
  expect_match(html, "multiViewDrawVectorLayer", fixed = TRUE)
  expect_match(html, "multiViewDrawHeatmapLayer", fixed = TRUE)
  expect_match(html, "multiViewDrawImageLayer", fixed = TRUE)
  expect_match(html, "multiViewBaseCanvasCandidates", fixed = TRUE)
  expect_match(html, "multiViewApplyStainToPane", fixed = TRUE)
  expect_match(html, "applyMultiViewImageTransform", fixed = TRUE)
  expect_match(html, "multiViewDrawTileGrid", fixed = TRUE)
  expect_match(html, "multiViewDrawCrosshair", fixed = TRUE)
  expect_match(html, "multiViewStartScreenshotSelection", fixed = TRUE)
  expect_match(html, "multiViewFinishScreenshotSelection", fixed = TRUE)
  expect_match(html, "multiViewScreenshotCanvasFromRect", fixed = TRUE)
  expect_match(html, "multiViewLayerItemMatchesPane", fixed = TRUE)
  expect_match(html, "multiViewChannelSourceMatchesPane", fixed = TRUE)
  expect_match(html, "syncMultiViewChannelSourcesForPane", fixed = TRUE)
  expect_match(html, "multiViewDrawRoiSet", fixed = TRUE)
  expect_match(html, "multiViewPlaceLabel", fixed = TRUE)
  expect_match(html, "multiViewDrawLabelLeader", fixed = TRUE)
  expect_match(html, "multiViewDrawLabels", fixed = TRUE)
  expect_match(html, "multiViewFindVertexAt", fixed = TRUE)
  expect_match(html, "startCurveEditStroke(multiViewLastCanvasPointer,lastPointer,p=>multiViewSlideToCanvas(p,pane))", fixed = TRUE)
  expect_match(html, "drawCurveEditPreview(cx,p=>multiViewSlideToCanvas(p,pane))", fixed = TRUE)
  expect_match(html, "multiViewPaneMouseDown", fixed = TRUE)
  expect_match(html, "multiViewPaneMouseMove", fixed = TRUE)
  expect_match(html, "multiViewPaneMouseUp", fixed = TRUE)
  expect_match(html, "multiViewCanvasPoint", fixed = TRUE)
  expect_match(html, "multiViewLastCanvasPointer", fixed = TRUE)
  expect_match(html, "multiViewBrushPreviewPoint", fixed = TRUE)
  expect_match(html, "multiViewCommitAnnotationAction", fixed = TRUE)
  expect_match(html, "multiViewFinishBrush", fixed = TRUE)
  expect_match(html, "multiViewFinishDraft", fixed = TRUE)
  expect_match(html, "if(brushing)multiViewFinishBrush()", fixed = TRUE)
  expect_match(html, "if(mode==='draw'){multiViewFinishDraft()", fixed = TRUE)
  expect_match(html, "multiViewPanViewerByPixels", fixed = TRUE)
  expect_match(html, "multiViewSyncedPanByPixels", fixed = TRUE)
  expect_match(html, "if(multiViewSyncedPanByPixels(pane,dx,dy))return", fixed = TRUE)
  expect_match(html, "multiViewPanByPixels", fixed = TRUE)
  expect_match(html, "activeMultiViewPane();if(pane){multiViewPanByPixels(pane,dx,dy);requestDraw();return;}", fixed = TRUE)
  expect_match(html, "id=\"spatialOverlay\"", fixed = TRUE)
  expect_match(html, "spatialWebglProgram", fixed = TRUE)
  expect_match(html, "invalidateSpatialWebglCache", fixed = TRUE)
  expect_match(html, "requestMultiViewOverlayDraw", fixed = TRUE)
  expect_match(html, "viewerPerformancePayload", fixed = TRUE)
  expect_match(html, "id=\"performanceSummary\"", fixed = TRUE)
  expect_match(html, "id=\"performanceAdvice\"", fixed = TRUE)
  expect_match(html, "viewerPerformanceAdvice", fixed = TRUE)
  expect_match(html, "denseGeojsonInflight", fixed = TRUE)
  expect_match(html, "panByKeyboard(e.key,e.shiftKey)", fixed = TRUE)
  expect_match(html, "highlighted=typeof roiClassHighlighted==='function'&&roiClassHighlighted(roi)", fixed = TRUE)
  expect_match(html, "multiViewOsdOptions", fixed = TRUE)
  expect_match(html, "gestureSettingsMouse:{clickToZoom:false,dblClickToZoom:false,scrollToZoom:true,dragToPan:false}", fixed = TRUE)
  expect_match(html, "multiViewProjectEntries", fixed = TRUE)
  expect_match(html, "multiViewUsesProjectSources", fixed = TRUE)
  expect_match(html, "multiViewCustomMode", fixed = TRUE)
  expect_match(html, "multiViewPaneIsBlank", fixed = TRUE)
  expect_match(html, "entries.length===1&&!multiViewCustomMode", fixed = TRUE)
  expect_match(html, "Number(index)>=entries.length", fixed = TRUE)
  expect_match(html, "multiViewBlankTileSource", fixed = TRUE)
  expect_match(html, "multiViewTileSource", fixed = TRUE)
  expect_match(html, "multiViewPaneLabel", fixed = TRUE)
  expect_match(html, "Empty view", fixed = TRUE)
  expect_match(html, "Drop a project image or section here", fixed = TRUE)
  expect_match(html, "multiViewAssignments", fixed = TRUE)
  expect_match(html, "multiViewControlPaneIndex", fixed = TRUE)
  expect_match(html, "Use + and - inside each pane, or click a pane before using the main zoom controls", fixed = TRUE)
  expect_match(html, "multiViewColFractions", fixed = TRUE)
  expect_match(html, "multiViewRowFractions", fixed = TRUE)
  expect_match(html, "multiViewResizeDrag", fixed = TRUE)
  expect_match(html, "multiViewEnsureFractions", fixed = TRUE)
  expect_match(html, "multiViewApplyGridTemplate", fixed = TRUE)
  expect_match(html, "updateMultiViewResizeHandles", fixed = TRUE)
  expect_match(html, "startMultiViewResize", fixed = TRUE)
  expect_match(html, "multiViewResizeMove", fixed = TRUE)
  expect_match(html, "finishMultiViewResize", fixed = TRUE)
  expect_match(html, "Drag the turquoise borders to resize panes", fixed = TRUE)
  expect_match(html, "activeMultiViewPane", fixed = TRUE)
  expect_match(html, "multiViewTargetPanes", fixed = TRUE)
  expect_match(html, "multiViewZoomAt(factor)", fixed = TRUE)
  expect_match(html, "multiViewZoomPaneAt", fixed = TRUE)
  expect_match(html, "bindButton('zoomIn',()=>{if(typeof multiViewZoomAt==='function'&&multiViewZoomAt(1.5))return;", fixed = TRUE)
  expect_match(html, "bindButton('zoomOut',()=>{if(typeof multiViewZoomAt==='function'&&multiViewZoomAt(1/1.5))return;", fixed = TRUE)
  expect_match(html, "pane&&typeof multiViewCanvasUnitScale==='function'?multiViewCanvasUnitScale(pane):scaleBarSlideUnitScale()", fixed = TRUE)
  expect_match(html, "resizeMultiViewPaneViewer(pane,false)", fixed = TRUE)
  expect_match(html, "multiViewControlPaneIndex=multiViewActiveIndex", fixed = TRUE)
  expect_match(html, "setMultiViewActive(index,true)", fixed = TRUE)
  expect_false(grepl("setMultiViewActive(index,false)", html, fixed = TRUE))
  expect_match(html, "clamp(px,0,w)", fixed = TRUE)
  expect_match(html, "column_fractions", fixed = TRUE)
  expect_match(html, "row_fractions", fixed = TRUE)
  expect_match(html, "multiViewProjectEntryKey", fixed = TRUE)
  expect_match(html, "multiViewProjectEntryLabel", fixed = TRUE)
  expect_match(html, "multiViewEntryFromIndices", fixed = TRUE)
  expect_match(html, "multiViewEntryFromPayload", fixed = TRUE)
  expect_match(html, "multiViewAssignmentUsed", fixed = TRUE)
  expect_match(html, "multiViewNormalizeAssignments", fixed = TRUE)
  expect_match(html, "Number(index)>=entries.length", fixed = TRUE)
  expect_false(grepl("entries[index%entries.length]", html, fixed = TRUE))
  expect_false(grepl("This image is already displayed in another multi-view pane", html, fixed = TRUE))
  expect_false(grepl("Duplicate multi-view image was not opened", html, fixed = TRUE))
  expect_match(html, "The same image may be dropped into several panes", fixed = TRUE)
  expect_match(html, "multiViewPaneControls", fixed = TRUE)
  expect_match(html, "Zoom in this pane", fixed = TRUE)
  expect_match(html, "multiViewZoomPaneControl", fixed = TRUE)
  expect_match(html, "const direct=multiViewEntryFromIndices", fixed = TRUE)
  expect_match(html, "entries.find(entry=>entry.itemIndex===itemIndex)", fixed = TRUE)
  expect_match(html, "bindMultiViewPaneDrop", fixed = TRUE)
  expect_match(html, "pane.addEventListener('dragenter',over,true)", fixed = TRUE)
  expect_match(html, "pane.addEventListener('drop',drop,true)", fixed = TRUE)
  expect_match(html, "recordViewerLog('Multi-view pane drop could not open", fixed = TRUE)
  expect_match(html, "multiViewDroppedFiles", fixed = TRUE)
  expect_match(html, "multiViewReadableFile", fixed = TRUE)
  expect_match(html, "loadMultiViewDroppedFile", fixed = TRUE)
  expect_match(html, "addProjectImageDataUri(dataUri,file.name,img.naturalWidth,img.naturalHeight,{activate:false,apply:false,refresh_multi_view:false})", fixed = TRUE)
  expect_match(html, "replaceMultiViewPane", fixed = TRUE)
  expect_match(html, "multiViewClearChannelItems(pane)", fixed = TRUE)
  expect_match(html, "pane.blank=false", fixed = TRUE)
  expect_match(html, "refreshOpenedMultiViewPane", fixed = TRUE)
  expect_match(html, "resizeMultiViewPaneViewer", fixed = TRUE)
  expect_match(html, "scheduleMultiViewPaneRefresh", fixed = TRUE)
  expect_match(html, "openMultiViewPanePreviewFallback", fixed = TRUE)
  expect_match(html, "Multi-view tile failed to load", fixed = TRUE)
  expect_match(html, "Multi-view pane switched to preview fallback", fixed = TRUE)
  expect_match(html, "multi_view_pane_replaced", fixed = TRUE)
  expect_match(html, "layoutCustom", fixed = TRUE)
  expect_match(html, "projectEntryDragPayload", fixed = TRUE)
  expect_match(html, "projectDragSectionIndex", fixed = TRUE)
  expect_match(html, "projectDragPayloadCache", fixed = TRUE)
  expect_match(html, "multiViewDragCacheEntry", fixed = TRUE)
  expect_match(html, "clearProjectDragPayloadSoon", fixed = TRUE)
  expect_match(html, "Multi-view pane drop did not contain a project image/section", fixed = TRUE)
  expect_match(html, "bindProjectSectionDrag", fixed = TRUE)
  expect_match(html, "syncMultiViewFrom", fixed = TRUE)
  expect_match(html, "if(typeof multiViewSync!=='undefined')multiViewSync=false", fixed = TRUE)
  expect_false(grepl("prefs.multi_view_sync", html, fixed = TRUE))
  expect_false(grepl("out.multi_view_sync", html, fixed = TRUE))
  expect_match(html, "multi_view_layout_updated", fixed = TRUE)
  expect_match(html, "refreshMultiViewSources", fixed = TRUE)
  expect_match(html, ".bar{position:fixed;left:12px;right:12px;top:12px", fixed = TRUE)
  expect_match(html, "z-index:30", fixed = TRUE)
  expect_match(html, "#workspacePanel{position:fixed;left:12px;top:72px", fixed = TRUE)
  expect_match(html, "#roiPanel{position:relative;width:auto", fixed = TRUE)
  expect_match(html, "z-index:29", fixed = TRUE)
  expect_match(html, "imageToViewportCoordinates", fixed = TRUE)
  expect_match(html, "viewportToImageCoordinates", fixed = TRUE)
  expect_match(html, "zoomToSlideBounds", fixed = TRUE)
})

test_that("tiled viewer HTML writes mIHC channel overlay controls", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")
  source <- wsi_channel_source(
    "Marker A",
    type = "dynamic",
    tile_url_template = "http://127.0.0.1:8788/tiles/marker_a/{level}/{x}/{y}.{format}",
    width = 320,
    height = 240,
    max_level = 9,
    colour = "#ff00ff",
    opacity = 0.5,
    metadata = list(
      extent = list(x = 10, y = 20, width = 320, height = 240),
      server_colourized = TRUE,
      legend = list(
        list(value = 1, label = "Tumour cells", colour = "#ff0000", count = 10),
        list(value = 2, label = "Stroma cells", colour = "#00ff00", count = 5)
      )
    )
  )
  source$selected_values <- c("1")
  settings <- wsiTools:::wsi_channel_settings_from_sources(list(source))
  expect_equal(settings$selected_values[[1]], "1")

  result <- wsi_viewer(
    slide,
    output = output,
    open = FALSE,
    mode = "tiles",
    tile_url_template = "http://127.0.0.1:8788/tiles/base/{level}/{x}/{y}.{format}",
    tile_url_style = "slash",
    tile_format = "png",
    channel_sources = list(source)
  )
  html <- paste(readLines(result, warn = FALSE), collapse = "\n")

  expect_match(html, "channelMenuList", fixed = TRUE)
  expect_false(grepl("id=\"channelList\"", html, fixed = TRUE))
  expect_false(grepl("id=\"channelSummary\"", html, fixed = TRUE))
  expect_false(grepl("<div class=\"sideTitle\">Channels</div>", html, fixed = TRUE))
  expect_match(html, "buildChannelList", fixed = TRUE)
  expect_match(html, "visibleChannelSources", fixed = TRUE)
  expect_match(html, "channelSourceMatchesActive", fixed = TRUE)
  expect_match(html, "channelSourceMinZoom", fixed = TRUE)
  expect_match(html, "channelSourceZoomAllowed", fixed = TRUE)
  expect_match(html, "syncChannelSourcesForActiveImage", fixed = TRUE)
  expect_match(html, "removeChannelItem", fixed = TRUE)
  expect_match(html, "clearChannelItems", fixed = TRUE)
  expect_match(html, "filter(channelSourceMatchesActive)", fixed = TRUE)
  expect_match(html, "No image channels for the current image.", fixed = TRUE)
  expect_match(html, "channelPlacementOptions", fixed = TRUE)
  expect_match(html, "server_colourized", fixed = TRUE)
  expect_match(html, "channelLegendPanel", fixed = TRUE)
  expect_match(html, "channelLegendEntries", fixed = TRUE)
  expect_match(html, "channelMaskFilterState", fixed = TRUE)
  expect_match(html, "channelMaskCanvasFilterActive", fixed = TRUE)
  expect_match(html, "display_min_zoom", fixed = TRUE)
  expect_match(html, "drawFilteredMaskChannels", fixed = TRUE)
  expect_match(html, "multiViewDrawFilteredMaskChannels", fixed = TRUE)
  expect_match(html, "channelLegendTools", fixed = TRUE)
  expect_match(html, "selected_values", fixed = TRUE)
  expect_match(html, "Mask legend", fixed = TRUE)
  expect_match(html, "Tumour cells", fixed = TRUE)
  expect_match(html, "contrast_min", fixed = TRUE)
  expect_match(html, "Marker A", fixed = TRUE)
  expect_match(html, "baseImageVisible", fixed = TRUE)
  expect_match(html, "withTileCors", fixed = TRUE)
  expect_match(html, "crossOriginPolicy='Anonymous'", fixed = TRUE)
  expect_match(html, "crossOrigin='anonymous'", fixed = TRUE)
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
  expect_false(grepl("stainChannelControls", html, fixed = TRUE))
  expect_false(grepl("<div class=\"menuTitle\">Channels</div>", html, fixed = TRUE))
  expect_match(html, "Image channels", fixed = TRUE)
  expect_false(grepl("<input id=\"stainColor_hematoxylin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainColor_hrp_dab\"", html, fixed = TRUE))
  expect_match(html, "Stains", fixed = TRUE)
  expect_match(html, "stainShowOriginal", fixed = TRUE)
  expect_match(html, "stainShowAll", fixed = TRUE)
  expect_false(grepl("<input id=\"stainVisible_hematoxylin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainStrength_hematoxylin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainOpacity_hematoxylin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainContrastMin_hematoxylin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainContrastMax_hrp_dab\"", html, fixed = TRUE))
  expect_match(html, "class=\"stainOnly\"", fixed = TRUE)
  expect_match(html, "activeStainNames", fixed = TRUE)
  expect_match(html, "activeStainIndices", fixed = TRUE)
  expect_match(html, "stainAutoScales", fixed = TRUE)
  expect_match(html, "stainIntensity", fixed = TRUE)
  expect_match(html, "Showing deconvolved stain composite", fixed = TRUE)
  expect_match(html, "deconvolution channel", fixed = TRUE)
  expect_match(html, "syncStainStateFromControls", fixed = TRUE)
  expect_match(html, "setStainVisible", fixed = TRUE)
  expect_match(html, "showDefaultStains", fixed = TRUE)
  expect_match(html, "showAllStains", fixed = TRUE)
  expect_match(html, "stainDefaultVisible", fixed = TRUE)
  expect_match(html, "stainDisplayMode", fixed = TRUE)
  expect_match(html, "data-stain-id", fixed = TRUE)
  expect_match(html, "stainChannelIndex", fixed = TRUE)
  expect_match(html, "stainDisplayChanged", fixed = TRUE)
  expect_match(html, "saveStainPreferences", fixed = TRUE)
  expect_match(html, "applyStainPreferences", fixed = TRUE)
  expect_match(html, "addEventListener('input',redraw)", fixed = TRUE)
  expect_match(html, "addEventListener('change',redraw)", fixed = TRUE)
  expect_match(html, "applyStainToCanvas", fixed = TRUE)
  expect_match(html, "open the viewer through localhost/http", fixed = TRUE)
  expect_match(html, "Stain selection needs readable canvas pixels", fixed = TRUE)
  expect_match(html, "wsi_viewer_live(..., dynamic_tiles = TRUE)", fixed = TRUE)
  expect_false(grepl("http://127.0.0.1/localhost", html, fixed = TRUE))
  expect_match(html, "IHC H-DAB", fixed = TRUE)
})

test_that("interactive H&E viewer writes hematoxylin, eosin, and residual controls", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- wsi_viewer_he(
    slide,
    mode = "thumbnail",
    width = 256,
    output = output,
    open = FALSE
  )

  expect_identical(result, output)
  expect_true(file.exists(output))
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "H&amp;E", fixed = TRUE)
  expect_match(html, "H&E H/E/residual", fixed = TRUE)
  expect_false(grepl("<input id=\"stainColor_hematoxylin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainColor_eosin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainColor_residual\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainVisible_residual\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainOpacity_residual\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainContrastMax_residual\"", html, fixed = TRUE))
  expect_match(html, "Residual", fixed = TRUE)
  expect_match(html, '"visible":false', fixed = TRUE)
  expect_match(html, "stainConcentrations", fixed = TRUE)
  expect_match(html, "stainAutoScales", fixed = TRUE)
  expect_match(html, "Showing '+(stainChannels", fixed = TRUE)
  expect_match(html, "heGramInv", fixed = TRUE)
  expect_match(html, "sourceCanvas", fixed = TRUE)
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
  expect_false(grepl("<input id=\"stainVisible_fast_red\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainStrength_fast_red\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainOpacity_fast_red\"", html, fixed = TRUE))
  expect_match(html, "contrast_min", fixed = TRUE)
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
