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
  expect_match(html, "navPanButton", fixed = TRUE)
  expect_match(html, "iconMove", fixed = TRUE)
  expect_match(html, "navDock", fixed = TRUE)
  expect_match(html, "Navigation controls", fixed = TRUE)
  expect_match(html, "zoomIn", fixed = TRUE)
  expect_match(html, "zoomOut", fixed = TRUE)
  expect_match(html, "oneToOne", fixed = TRUE)
  expect_match(html, "toolSelect", fixed = TRUE)
  expect_match(html, "toolDraw", fixed = TRUE)
  expect_match(html, "toolBrush", fixed = TRUE)
  expect_match(html, "newRoi", fixed = TRUE)
  expect_match(html, "New ROI", fixed = TRUE)
  expect_match(html, "toolEdit", fixed = TRUE)
  expect_match(html, "Stains", fixed = TRUE)
  pos_annotations_menu <- regexpr("<summary title=\"Draw, select, import, export, segment, and manage annotations\">Annotations</summary>", html, fixed = TRUE)[[1]]
  pos_segmentation_section <- regexpr("StarDist segmentation", html, fixed = TRUE)[[1]]
  pos_separate_segmentation_menu <- regexpr("<summary title=\"StarDist segmentation import for selected ROIs\">Segmentation</summary>", html, fixed = TRUE)[[1]]
  expect_gt(pos_annotations_menu, 0)
  expect_gt(pos_segmentation_section, pos_annotations_menu)
  expect_identical(pos_separate_segmentation_menu, -1L)
  pos_stains_menu <- regexpr("<summary title=\"Stain deconvolution display options\">Stains</summary>", html, fixed = TRUE)[[1]]
  pos_help_menu <- regexpr("<summary title=\"Keyboard shortcuts\">Help</summary>", html, fixed = TRUE)[[1]]
  expect_true(all(c(pos_stains_menu, pos_help_menu) > 0))
  expect_lt(pos_stains_menu, pos_help_menu)
  expect_match(html, "No stain channels are configured for this viewer.", fixed = TRUE)
  expect_match(html, "stainChannelControls", fixed = TRUE)
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
  pos_presets <- regexpr("<div id=\"classPresetEditor\"", html, fixed = TRUE)[[1]]
  expect_true(all(c(pos_brush, pos_layers, pos_rois, pos_presets) > 0))
  expect_lt(pos_brush, pos_layers)
  expect_lt(pos_layers, pos_rois)
  expect_lt(pos_rois, pos_presets)
  expect_match(html, "viewerPreferenceKey", fixed = TRUE)
  expect_match(html, "wsiTools.viewer.preferences.v1", fixed = TRUE)
  expect_match(html, "applyViewerPreferences", fixed = TRUE)
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
  expect_match(html, "simplifyTolerance", fixed = TRUE)
  expect_match(html, "simplifyToleranceValue", fixed = TRUE)
  expect_match(html, "Brush refinement", fixed = TRUE)
  expect_match(html, "smoothRoi", fixed = TRUE)
  expect_match(html, "simplifyRoi", fixed = TRUE)
  expect_match(html, "fillRoiHoles", fixed = TRUE)
  expect_match(html, "mergeRois", fixed = TRUE)
  expect_match(html, "splitRoi", fixed = TRUE)
  expect_match(html, "startBrush", fixed = TRUE)
  expect_match(html, "finishBrush", fixed = TRUE)
  expect_match(html, "startNewAnnotation", fixed = TRUE)
  expect_match(html, "deselectAnnotation", fixed = TRUE)
  expect_match(html, "pointNearRoi", fixed = TRUE)
  expect_match(html, "roi_deselected", fixed = TRUE)
  expect_match(html, "Ready for new ROI", fixed = TRUE)
  expect_match(html, "Brush creates a new automatically named annotation", fixed = TRUE)
  expect_match(html, "If it touches an existing annotation with the same label", fixed = TRUE)
  expect_match(html, "automaticAnnotationName", fixed = TRUE)
  expect_match(html, "Pathology class for the next drawn or painted annotation", fixed = TRUE)
  expect_match(html, "Next class <select id=\"roiClassSelect\"", fixed = TRUE)
  expect_match(html, "Selected class <select id=\"annotationClassSelect\"", fixed = TRUE)
  expect_match(html, "Set next class", fixed = TRUE)
  expect_match(html, "Next annotation class:", fixed = TRUE)
  expect_match(html, "setRoiNameForClassAssignment", fixed = TRUE)
  expect_match(html, "annotationNameLooksAutomatic", fixed = TRUE)
  expect_match(html, "automatic_name:!!roi.automatic_name", fixed = TRUE)
  expect_match(html, "nextRoiNameDirty=false", fixed = TRUE)
  expect_match(html, "clearNextAnnotationName", fixed = TRUE)
  expect_match(html, "Next ROI will use automatic class name", fixed = TRUE)
  expect_match(html, "nextRoiClass='tumour'", fixed = TRUE)
  expect_match(html, "setNextRoiClass", fixed = TRUE)
  expect_match(html, "panel&&(selectedRoi<0||mode==='brush'||brushing)", fixed = TRUE)
  expect_match(html, "displayCls=(brushing&&brushOperation!=='subtract')?(brushClass||nextRoiClass||activeRoiClass||cls):cls", fixed = TRUE)
  expect_match(html, "ROI metadata updated; shape remains locked", fixed = TRUE)
  expect_match(html, "input.disabled=!selected;", fixed = TRUE)
  expect_false(grepl("input.disabled=!selected||!editable", html, fixed = TRUE))
  expect_match(html, "Click Apply to update selected annotation", fixed = TRUE)
  expect_match(html, "const nextClassButton=el('applyRoiClass');if(nextClassButton)nextClassButton.disabled=false;", fixed = TRUE)
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
  expect_match(html, "drawBrushStrokeOnMask", fixed = TRUE)
  expect_match(html, "brush_mask_contour", fixed = TRUE)
  expect_match(html, "addRoiFromBrushRings", fixed = TRUE)
  expect_match(html, "addRoiFromBrushGroups", fixed = TRUE)
  expect_match(html, "brush_ring_count", fixed = TRUE)
  expect_match(html, "brushPointSpacing", fixed = TRUE)
  expect_match(html, "densifyBrushPoints", fixed = TRUE)
  expect_match(html, "steps=96", fixed = TRUE)
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
  expect_match(html, "toastStack", fixed = TRUE)
  expect_match(html, "showToast", fixed = TRUE)
  expect_match(html, "dismissToast", fixed = TRUE)
  expect_match(html, "toast-success", fixed = TRUE)
  expect_match(html, "toast-actionable", fixed = TRUE)
  expect_match(html, "toastAction", fixed = TRUE)
  expect_match(html, "notifyAction", fixed = TRUE)
  expect_match(html, "Deleted '+deleteLabel+'.", fixed = TRUE)
  expect_match(html, "'Undo',()=>restoreAnnotationUndo()", fixed = TRUE)
  expect_match(html, "Jobs", fixed = TRUE)
  expect_match(html, "Long Jobs", fixed = TRUE)
  expect_match(html, "jobSummary", fixed = TRUE)
  expect_match(html, "jobList", fixed = TRUE)
  expect_match(html, "jobProgress", fixed = TRUE)
  expect_match(html, "viewerJobs", fixed = TRUE)
  expect_match(html, "upsertViewerJob", fixed = TRUE)
  expect_match(html, "updateViewerJob", fixed = TRUE)
  expect_match(html, "handleViewerJobs", fixed = TRUE)
  expect_match(html, "job_update", fixed = TRUE)
  expect_match(html, "queued", fixed = TRUE)
  expect_match(html, "running", fixed = TRUE)
  expect_match(html, "completed", fixed = TRUE)
  expect_match(html, "failed", fixed = TRUE)
  expect_match(html, "miniNavigator", fixed = TRUE)
  expect_match(html, "miniNavigatorCanvas", fixed = TRUE)
  expect_match(html, "miniNavigatorViewport", fixed = TRUE)
  expect_match(html, "miniNavigatorDensity", fixed = TRUE)
  expect_match(html, "navigatorImage", fixed = TRUE)
  expect_match(html, "miniNavigatorOverviewImage", fixed = TRUE)
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
  expect_match(html, "annotationsDirty", fixed = TRUE)
  expect_match(html, "markAnnotationsDirty", fixed = TRUE)
  expect_match(html, "markAnnotationsSaved", fixed = TRUE)
  expect_match(html, "updateAnnotationDirtyIndicator", fixed = TRUE)
  expect_match(html, "annotations:{dirty:annotationsDirty", fixed = TRUE)
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
  expect_match(html, "Run StarDist", fixed = TRUE)
  expect_match(html, "Show tile grid", fixed = TRUE)
  expect_match(html, "Save project", fixed = TRUE)
  expect_match(html, "tileGridVisible", fixed = TRUE)
  expect_match(html, "drawTileGrid", fixed = TRUE)
  expect_match(html, "tile_grid_toggled", fixed = TRUE)
  expect_match(html, "project_save_requested", fixed = TRUE)
  expect_match(html, "shortcutHelp", fixed = TRUE)
  expect_match(html, "Keyboard Shortcuts", fixed = TRUE)
  expect_match(html, "shortcutHelpButton", fixed = TRUE)
  expect_match(html, "bindShortcutHelp", fixed = TRUE)
  expect_match(html, "openShortcutHelp", fixed = TRUE)
  expect_match(html, "Press ? anytime", fixed = TRUE)
  expect_match(html, "Pan mode", fixed = TRUE)
  expect_match(html, "Draw polygon ROI", fixed = TRUE)
  expect_match(html, "Brush annotation editing", fixed = TRUE)
  expect_match(html, "Edit selected ROI vertices", fixed = TRUE)
  expect_match(html, "Measure distance between two points", fixed = TRUE)
  expect_match(html, "Undo annotation edit", fixed = TRUE)
  expect_match(html, "Redo annotation edit", fixed = TRUE)
  expect_match(html, "Ctrl+S", fixed = TRUE)
  expect_match(html, "Ctrl+I", fixed = TRUE)
  expect_match(html, "Ctrl+E", fixed = TRUE)
  expect_match(html, "shortcutImportGeojson", fixed = TRUE)
  expect_match(html, "ROI locked", fixed = TRUE)
  expect_match(html, "ROI saved", fixed = TRUE)
  expect_match(html, "GeoJSON exported", fixed = TRUE)
  expect_match(html, "StarDist finished", fixed = TRUE)
  expect_match(html, "roiCompositeGeometry", fixed = TRUE)
  expect_match(html, "clippedHoleForGroup", fixed = TRUE)
  expect_match(html, "roiContainsPoint", fixed = TRUE)
  expect_match(html, "annotationUndo", fixed = TRUE)
  expect_match(html, "annotationRedo", fixed = TRUE)
  expect_match(html, "undoAnnotation", fixed = TRUE)
  expect_match(html, "redoAnnotation", fixed = TRUE)
  expect_match(html, "pushAnnotationUndo", fixed = TRUE)
  expect_match(html, "restoreAnnotationUndo", fixed = TRUE)
  expect_match(html, "restoreAnnotationRedo", fixed = TRUE)
  expect_match(html, "pushHistory", fixed = TRUE)
  expect_match(html, "stack.length>10", fixed = TRUE)
  expect_match(html, "annotation_undo", fixed = TRUE)
  expect_match(html, "annotation_redo", fixed = TRUE)
  expect_match(html, "Nothing to undo", fixed = TRUE)
  expect_match(html, "Nothing to redo", fixed = TRUE)
  expect_match(html, "smoothSelectedRoi", fixed = TRUE)
  expect_match(html, "simplifySelectedRoi", fixed = TRUE)
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
  expect_match(html, "Delete selected", fixed = TRUE)
  expect_match(html, "Segmentation", fixed = TRUE)
  expect_match(html, "exportSelectedRoi", fixed = TRUE)
  expect_match(html, "startSegmentation", fixed = TRUE)
  expect_match(html, "Run segmentation", fixed = TRUE)
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
  expect_match(html, "segmentationResultDetail", fixed = TRUE)
  expect_match(html, "segmentation_run_url", fixed = TRUE)
  expect_match(html, "No StarDist command was found. Install/configure it, or load a segmentation GeoJSON/CSV instead.", fixed = TRUE)
  expect_match(html, "Copy R command", fixed = TRUE)
  expect_match(html, "wsi_install_stardist(method = \"conda\")", fixed = TRUE)
  expect_match(html, "addSegmentationGeojson", fixed = TRUE)
  expect_match(html, "stardist", fixed = TRUE)
  expect_match(html, "Artifacts", fixed = TRUE)
  expect_match(html, "Viewport QC", fixed = TRUE)
  expect_match(html, "detectVisibleArtifacts", fixed = TRUE)
  expect_match(html, "flagArtifactViewport", fixed = TRUE)
  expect_match(html, "toggleArtifactOverlay", fixed = TRUE)
  expect_match(html, "clearArtifactFlags", fixed = TRUE)
  expect_match(html, "artifactSensitivity", fixed = TRUE)
  expect_match(html, "artifactPayload", fixed = TRUE)
  expect_match(html, "artifactPixelMetrics", fixed = TRUE)
  expect_match(html, "drawArtifactOverlays", fixed = TRUE)
  expect_match(html, "bindArtifactControls", fixed = TRUE)
  expect_match(html, "artifact_detection", fixed = TRUE)
  expect_match(html, "artifact_filter = TRUE", fixed = TRUE)
  expect_match(html, "Detect visible artifacts", fixed = TRUE)
  expect_match(html, "Measure", fixed = TRUE)
  expect_match(html, "toolMeasure", fixed = TRUE)
  expect_match(html, "clearMeasures", fixed = TRUE)
  expect_match(html, "measureSummary", fixed = TRUE)
  expect_match(html, "measureList", fixed = TRUE)
  expect_match(html, "measurePixelSize", fixed = TRUE)
  expect_match(html, "measurementRecord", fixed = TRUE)
  expect_match(html, "drawMeasurements", fixed = TRUE)
  expect_match(html, "bindMeasureControls", fixed = TRUE)
  expect_match(html, "Trajectories", fixed = TRUE)
  expect_match(html, "toolTrajectory", fixed = TRUE)
  expect_match(html, "finishTrajectory", fixed = TRUE)
  expect_match(html, "trajectoryResolution", fixed = TRUE)
  expect_match(html, "trajectoryAreaWidth", fixed = TRUE)
  expect_match(html, "trajectoryAreaRoi", fixed = TRUE)
  expect_match(html, "createTrajectoryAreaRoi", fixed = TRUE)
  expect_match(html, "trajectoryAreaGeometry", fixed = TRUE)
  expect_match(html, "trajectoryPayload", fixed = TRUE)
  expect_match(html, "drawTrajectories", fixed = TRUE)
  expect_match(html, "bindTrajectoryControls", fixed = TRUE)
  expect_match(html, "Project", fixed = TRUE)
  expect_match(html, "projectOpenPanel", fixed = TRUE)
  expect_match(html, "projectOpenImage", fixed = TRUE)
  expect_match(html, "projectImageFile", fixed = TRUE)
  expect_match(html, "openProjectPanel", fixed = TRUE)
  expect_match(html, "ensureProjectWorkspaceVisible", fixed = TRUE)
  expect_match(html, "setProjectPanelClosed(false)", fixed = TRUE)
  expect_match(html, "setProjectPanelMinimized(false)", fixed = TRUE)
  expect_match(html, "loadProjectImageFile", fixed = TRUE)
  expect_match(html, "addProjectImageDataUri", fixed = TRUE)
  expect_match(html, "project_image_added", fixed = TRUE)
  expect_match(html, "Image", fixed = TRUE)
  expect_match(html, "rotateImageLeft", fixed = TRUE)
  expect_match(html, "rotateImageRight", fixed = TRUE)
  expect_match(html, "flipImageHorizontal", fixed = TRUE)
  expect_match(html, "flipImageVertical", fixed = TRUE)
  expect_match(html, "resetImageTransform", fixed = TRUE)
  expect_match(html, "imageTransformPayload", fixed = TRUE)
  expect_match(html, "drawTransformedImage", fixed = TRUE)
  expect_match(html, "bindImageTransformControls", fixed = TRUE)
  expect_match(html, "mpp", fixed = TRUE)
  expect_match(html, "id=\"scaleBar\"", fixed = TRUE)
  expect_match(html, "Micron scale bar", fixed = TRUE)
  expect_match(html, "updateScaleBar", fixed = TRUE)
  expect_match(html, "niceScaleLength", fixed = TRUE)
  expect_match(html, "\\u00b5m", fixed = TRUE)
  expect_match(html, "saveGeojson", fixed = TRUE)
  expect_match(html, "FeatureCollection", fixed = TRUE)
  expect_match(html, "crosshairToggle", fixed = TRUE)
  expect_match(html, "copyCoord", fixed = TRUE)
  expect_match(html, "GeoJSON Geometries", fixed = TRUE)
  expect_match(html, "#workspacePanel{position:fixed;left:12px;top:72px", fixed = TRUE)
  expect_match(html, "#roiPanel{position:static;width:auto", fixed = TRUE)
  expect_match(html, "roiPanelHeader", fixed = TRUE)
  expect_match(html, "roiPanelBody", fixed = TRUE)
  expect_match(html, "roiPanelMinimizeState", fixed = TRUE)
  expect_match(html, "Double-click to minimize or restore the annotation manager", fixed = TRUE)
  expect_match(html, "#roiPanel.minimized #roiPanelBody{display:none;}", fixed = TRUE)
  expect_match(html, "Project and annotation panels", fixed = TRUE)
  expect_true(regexpr("id=\"projectPanel\"", html, fixed = TRUE) < regexpr("id=\"roiPanel\"", html, fixed = TRUE))
  expect_match(html, "toggleRoiPanelMinimized", fixed = TRUE)
  expect_match(html, "bindRoiPanelControls", fixed = TRUE)
  expect_match(html, "selectionCard", fixed = TRUE)
  expect_match(html, "Selected ROI summary", fixed = TRUE)
  expect_match(html, "selectionCardToggle", fixed = TRUE)
  expect_match(html, "ROI summary", fixed = TRUE)
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
  expect_match(html, "#roiPanel{position:static;width:auto", fixed = TRUE)
  expect_match(html, "z-index:29", fixed = TRUE)
  expect_match(html, "Annotation manager", fixed = TRUE)
  expect_match(html, "annotationSearchInput", fixed = TRUE)
  expect_match(html, "search name/class", fixed = TRUE)
  expect_match(html, "annotationFilter", fixed = TRUE)
  expect_match(html, "annotationSort", fixed = TRUE)
  expect_match(html, "annotationFilterClear", fixed = TRUE)
  expect_match(html, "bindAnnotationListControls", fixed = TRUE)
  expect_match(html, "currentRoiListEntries", fixed = TRUE)
  expect_match(html, "roiMatchesAnnotationSearch", fixed = TRUE)
  expect_match(html, "roiMatchesAnnotationFilter", fixed = TRUE)
  expect_match(html, "annotationListSummary", fixed = TRUE)
  expect_match(html, "roiListEmpty", fixed = TRUE)
  expect_match(html, "area_desc", fixed = TRUE)
  expect_match(html, "selectedRoiAreaValue", fixed = TRUE)
  expect_match(html, "annotationNameInput", fixed = TRUE)
  expect_match(html, "annotationClassSelect", fixed = TRUE)
  expect_match(html, "annotationColorInput", fixed = TRUE)
  expect_match(html, "annotationVisible", fixed = TRUE)
  expect_match(html, "annotationLock", fixed = TRUE)
  expect_match(html, "annotationZoom", fixed = TRUE)
  expect_match(html, "annotationDuplicate", fixed = TRUE)
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
  expect_match(html, "pointerdown", fixed = TRUE)
  expect_match(html, "closest('.toolMenu')", fixed = TRUE)
  expect_match(html, "menu!==active", fixed = TRUE)
  expect_false(grepl("<summary title=\"Pan and zoom controls\">Navigate</summary>", html, fixed = TRUE))
  expect_match(html, "navDock", fixed = TRUE)
  expect_match(html, "navPanButton", fixed = TRUE)
  expect_match(html, "Annotations", fixed = TRUE)
  expect_match(html, "Draw, select, import, export, segment, and manage annotations", fixed = TRUE)
  expect_match(html, "GeoJSON and display", fixed = TRUE)
  expect_match(html, "StarDist segmentation", fixed = TRUE)
  expect_false(grepl("<summary title=\"StarDist segmentation import for selected ROIs\">Segmentation</summary>", html, fixed = TRUE))
  expect_false(grepl("<summary title=\"ROI overlay and GeoJSON geometry list\">GeoJSON</summary>", html, fixed = TRUE))
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
  expect_match(html, "roiClassCustom", fixed = TRUE)
  expect_match(html, "custom category", fixed = TRUE)
  expect_match(html, "customCategoryValue", fixed = TRUE)
  expect_match(html, "tumour", fixed = TRUE)
  expect_match(html, "stroma", fixed = TRUE)
  expect_match(html, "necrosis", fixed = TRUE)
  expect_match(html, "invasive front", fixed = TRUE)
  expect_match(html, "#D73027", fixed = TRUE)
  expect_match(html, "Class Presets", fixed = TRUE)
  expect_match(html, "classPresetEditor", fixed = TRUE)
  expect_match(html, "classPresetList", fixed = TRUE)
  expect_match(html, "maximizeClassPresets", fixed = TRUE)
  expect_match(html, "Maximize class presets", fixed = TRUE)
  expect_match(html, "addClassPreset", fixed = TRUE)
  expect_match(html, "resetClassPresets", fixed = TRUE)
  expect_match(html, "respectClassExportRules", fixed = TRUE)
  expect_match(html, "roiClassPresetDefaults", fixed = TRUE)
  expect_match(html, "populateRoiClassSelects", fixed = TRUE)
  expect_match(html, "buildClassPresetEditor", fixed = TRUE)
  expect_match(html, "classPresetColour", fixed = TRUE)
  expect_match(html, "applyClassPresetColoursToRois", fixed = TRUE)
  expect_match(html, "roiAllowedByExportRules", fixed = TRUE)
  expect_match(html, "exportableRoiFeatures", fixed = TRUE)
  expect_match(html, "applyRoiClass", fixed = TRUE)
  expect_match(html, "Set next class", fixed = TRUE)
  expect_match(html, "ROI updated", fixed = TRUE)
  expect_match(html, "annotationLabelValue", fixed = TRUE)
  expect_match(html, "activeRoiName", fixed = TRUE)
  expect_match(html, "label:name", fixed = TRUE)
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
    viewer_state_url = "http://127.0.0.1:8788/viewer-state"
  )

  expect_identical(result, output)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "http://127.0.0.1:8788/viewer-state", fixed = TRUE)
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
  expect_match(html, "geojson_imported", fixed = TRUE)
  expect_match(html, "measurement_added", fixed = TRUE)
  expect_match(html, "segmentation_added", fixed = TRUE)
  expect_match(html, "annotationHistory", fixed = TRUE)
  expect_match(html, "maximizeAnnotationHistory", fixed = TRUE)
  expect_match(html, "annotationSectionBackdrop", fixed = TRUE)
  expect_match(html, "setAnnotationSectionMaximized", fixed = TRUE)
  expect_match(html, "toggleAnnotationSectionMaximized", fixed = TRUE)
  expect_match(html, "bindAnnotationSectionMaximizeControls", fixed = TRUE)
  expect_match(html, "#classPresetEditor.maximized", fixed = TRUE)
  expect_match(html, "#annotationHistory.maximized", fixed = TRUE)
  expect_match(html, "recordAnnotationHistory", fixed = TRUE)
  expect_match(html, "annotationHistoryPayload", fixed = TRUE)
  expect_match(html, "Brush subtract", fixed = TRUE)
  expect_match(html, "Imported GeoJSON", fixed = TRUE)
  expect_match(html, "Ran StarDist", fixed = TRUE)
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
  expect_true(is.function(session$get_measurements))
  expect_true(is.function(session$get_trajectories))
  expect_true(is.function(session$get_roi_summary))
  expect_true(is.function(session$get_cell_summary))
  expect_true(is.function(session$get_class_summary))
  expect_true(is.function(session$get_ihc_summary))
  expect_true(is.function(session$get_ihc_class_summary))
  expect_true(is.function(session$get_segmentation))
  expect_true(is.function(session$get_layers))
  expect_true(is.function(session$get_history))
  expect_true(is.function(session$get_tile_preview))
  expect_true(is.function(session$list_layers))
  expect_true(is.function(session$on))
  expect_true(is.function(session$off))
  expect_true(is.function(session$list_callbacks))
  expect_true(is.function(session$capabilities))
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
    segmentation = list(type = "FeatureCollection", features = list()),
    measurements = list(),
    trajectories = list(),
    view = list(mode = "pan"),
    annotations = list(dirty = FALSE),
    history = list(),
    detail = list()
  )

  expect_silent(wsiTools:::wsi_viewer_state_apply(state, payload))

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
      source = "stardist",
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
    stain = list(enabled = FALSE),
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
  expect_identical(env$live, state)
  expect_s3_class(env$live_rois, "wsi_roi")
  expect_s3_class(env$live_selected_rois, "wsi_roi")
  expect_equal(env$live_measurements$id, "measure_1")
  expect_equal(env$live_trajectories$id, "trajectory_1")
  expect_s3_class(env$live_roi_summary, "data.frame")
  expect_s3_class(env$live_cell_summary, "data.frame")
  expect_s3_class(env$live_class_summary, "data.frame")
  expect_equal(env$live_history$action, "roi_added")
  expect_equal(env$live_last_event$event, "roi_added")

  payload$event <- "segmentation_added"
  payload$detail <- list(
    added = 1,
    crop = "roi_crop.png",
    output = "stardist_cells.geojson",
    status = "complete"
  )
  wsiTools:::wsi_viewer_state_apply(state, payload)
  snapshot <- wsi_viewer_state(state)

  expect_equal(snapshot$last_segmentation$crop, "roi_crop.png")
  expect_equal(env$live_last_segmentation$output, "stardist_cells.geojson")
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
  expect_match(html, "segmentation_requested", fixed = TRUE)
  expect_match(html, "Sending selected ROI to R", fixed = TRUE)
  expect_match(html, "keepSelection:true", fixed = TRUE)
  expect_match(html, "Running segmentation on selected ROI", fixed = TRUE)
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
  result <- wsiTools:::wsi_stardist_result(
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
    detail = list(engine = "stardist")
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

test_that("live viewer exposes a direct StarDist switch", {
  args <- names(formals(wsi_viewer_session))

  expect_true("stardist" %in% args)
  expect_true("transport" %in% args)
  expect_true("dynamic_tiles" %in% args)
  expect_true("stardist_command" %in% args)
  expect_true("stardist_args" %in% args)
  expect_true("stardist_output_dir" %in% args)
  expect_true("autosave" %in% args)
  expect_true("autosave_path" %in% args)
  expect_true("autosave_interval" %in% args)
  expect_true("state" %in% names(formals(wsi_stardist_server)))
  expect_identical(formals(wsi_viewer_stardist)$stardist, TRUE)
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
  html <- paste(readLines(session$html, warn = FALSE), collapse = "\n")
  expect_match(html, "H&E", fixed = TRUE)
  expect_match(html, "H&E H/E/residual", fixed = TRUE)
  expect_match(html, "stainVisible_hematoxylin", fixed = TRUE)
  expect_match(html, "stainVisible_eosin", fixed = TRUE)
  expect_match(html, "stainVisible_residual", fixed = TRUE)
  expect_match(html, "stainShowOriginal", fixed = TRUE)
  expect_match(html, "baseImageVisible", fixed = TRUE)
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
  expect_match(html, "dynamic tile server", fixed = TRUE)
  expect_true(dir.exists(cache_dir))

  wsi_viewer_stop(session)
  expect_false(dir.exists(cache_dir))
})

test_that("viewer event validation allowlists live WebSocket events", {
  expected <- c(
    "roi_created", "roi_updated", "roi_deleted", "roi_selected",
    "brush_committed", "viewport_changed", "layer_updated",
    "trajectory_area_created",
    "segmentation_started", "segmentation_progress",
    "segmentation_finished", "job_status"
  )

  expect_true(all(expected %in% wsiTools:::wsi_viewer_allowed_events()))
  expect_equal(wsiTools:::wsi_viewer_validate_event("brush_committed"), "brush_committed")
  expect_error(
    wsiTools:::wsi_viewer_validate_state_payload(list(event = "eval_r_code")),
    "Unsupported viewer event"
  )
  expect_error(
    wsiTools:::wsi_viewer_validate_state_payload(list(event = "roi_created", code = "system('rm -rf /')")),
    "unsupported field"
  )
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

test_that("live viewer keeps StarDist optional when no command is available", {
  wsi_skip_if_no_httpuv_server()
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  session <- NULL
  warning_message <- NULL
  on.exit(if (inherits(session, "wsi_viewer_session")) wsi_viewer_stop(session), add = TRUE)

  withCallingHandlers(
    session <- wsi_viewer_live(
      slide,
      width = 128,
      open = FALSE,
      wait = FALSE,
      stardist = TRUE,
      stardist_command = "missing-wsitools-stardist-command"
    ),
    warning = function(w) {
      warning_message <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  expect_match(warning_message, "No StarDist command was found", fixed = TRUE)
  expect_match(warning_message, "load a segmentation GeoJSON/CSV", fixed = TRUE)
  expect_match(warning_message, "wsi_install_stardist(method = \"conda\")", fixed = TRUE)
  expect_s3_class(session, "wsi_viewer_session")
  expect_match(session$ws_url, "^ws://")
  expect_null(session$stardist_server)
  html <- paste(readLines(session$html, warn = FALSE), collapse = "\n")
  expect_match(html, session$ws_url, fixed = TRUE)
  expect_match(html, "WebSocket connected", fixed = TRUE)
  expect_match(html, "Run segmentation", fixed = TRUE)
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
  expect_match(html, "roiToggle", fixed = TRUE)
  expect_match(html, "layersToggle", fixed = TRUE)
  expect_match(html, "roiOpacity", fixed = TRUE)
  expect_match(html, "saveRoiOpacityPreference", fixed = TRUE)
  expect_match(html, "savePanelPreferences", fixed = TRUE)
  expect_match(html, "setRoiPanelPosition", fixed = TRUE)
  expect_match(html, "startRoiPanelDrag", fixed = TRUE)
  expect_match(html, "GeoJSON Geometries", fixed = TRUE)
  expect_match(html, "id=\"roiPanel\" class=\"panel\"", fixed = TRUE)
  expect_false(grepl("id=\"roiPanel\" class=\"panel open\"", html, fixed = TRUE))
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
  expect_match(paste(readLines(file.path(tile_dir, "slide.dzi"), warn = FALSE), collapse = " "), "Overlap=\"1\"", fixed = TRUE)

  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "full-resolution tiled viewer", fixed = TRUE)
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
  expect_match(html, "drawMiniNavigatorOverview", fixed = TRUE)
  expect_match(html, "drawMiniNavigatorViewport", fixed = TRUE)
  expect_match(html, "imageToViewportCoordinates", fixed = TRUE)
  expect_match(html, "viewportToImageCoordinates", fixed = TRUE)
  expect_match(html, "id=\"overlay\"", fixed = TRUE)
  expect_match(html, ".bar{position:fixed;left:12px;right:12px;top:12px", fixed = TRUE)
  expect_match(html, "z-index:30", fixed = TRUE)
  expect_match(html, "#workspacePanel{position:fixed;left:12px;top:72px", fixed = TRUE)
  expect_match(html, "#roiPanel{position:static;width:auto", fixed = TRUE)
  expect_match(html, "z-index:29", fixed = TRUE)
  expect_match(html, "requestDraw", fixed = TRUE)
  expect_match(html, "loadingTiles", fixed = TRUE)
  expect_match(html, "Artifacts", fixed = TRUE)
  expect_match(html, "osdBaseCanvas", fixed = TRUE)
  expect_match(html, "osdBaseCanvasCandidates", fixed = TRUE)
  expect_match(html, "Stain channel selection needs readable tiles", fixed = TRUE)
  expect_match(html, "detectVisibleArtifacts", fixed = TRUE)
  expect_match(html, "drawArtifactOverlays", fixed = TRUE)
  expect_match(html, "saveGeojson", fixed = TRUE)
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
  expect_match(html, "maxImageCacheCount:512", fixed = TRUE)
  expect_match(html, "blendTime:0", fixed = TRUE)
  expect_match(html, "minPixelRatio:1", fixed = TRUE)
  expect_match(html, "maxZoomPixelRatio:16", fixed = TRUE)
  expect_match(html, "placeholderFillStyle:'#fff'", fixed = TRUE)
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
  expect_match(html, "id=\"viewer\" class=\"osdViewer\"", fixed = TRUE)
  expect_match(html, "id=\"overlay\"", fixed = TRUE)
  expect_match(html, ".bar{position:fixed;left:12px;right:12px;top:12px", fixed = TRUE)
  expect_match(html, "z-index:30", fixed = TRUE)
  expect_match(html, "#workspacePanel{position:fixed;left:12px;top:72px", fixed = TRUE)
  expect_match(html, "#roiPanel{position:static;width:auto", fixed = TRUE)
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
      server_colourized = TRUE
    )
  )

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
  expect_match(html, "channelList", fixed = TRUE)
  expect_match(html, "buildChannelList", fixed = TRUE)
  expect_match(html, "channelPlacementOptions", fixed = TRUE)
  expect_match(html, "server_colourized", fixed = TRUE)
  expect_match(html, "contrast_min", fixed = TRUE)
  expect_match(html, "Marker A", fixed = TRUE)
  expect_match(html, "baseImageVisible", fixed = TRUE)
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
  expect_match(html, "stainVisible_hematoxylin", fixed = TRUE)
  expect_false(grepl("<input id=\"stainStrength_hematoxylin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainOpacity_hematoxylin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainContrastMin_hematoxylin\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainContrastMax_hrp_dab\"", html, fixed = TRUE))
  expect_match(html, "class=\"stainOnly\"", fixed = TRUE)
  expect_match(html, "activeStainNames", fixed = TRUE)
  expect_match(html, "syncStainStateFromControls", fixed = TRUE)
  expect_match(html, "setStainVisible", fixed = TRUE)
  expect_match(html, "stainDisplayChanged", fixed = TRUE)
  expect_match(html, "saveStainPreferences", fixed = TRUE)
  expect_match(html, "applyStainPreferences", fixed = TRUE)
  expect_match(html, "addEventListener('input',redraw)", fixed = TRUE)
  expect_match(html, "addEventListener('change',redraw)", fixed = TRUE)
  expect_match(html, "applyStainToCanvas", fixed = TRUE)
  expect_match(html, "open the viewer through localhost/http", fixed = TRUE)
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
  expect_match(html, "stainColor_hematoxylin", fixed = TRUE)
  expect_match(html, "stainColor_eosin", fixed = TRUE)
  expect_match(html, "stainColor_residual", fixed = TRUE)
  expect_match(html, "stainVisible_residual", fixed = TRUE)
  expect_false(grepl("<input id=\"stainOpacity_residual\"", html, fixed = TRUE))
  expect_false(grepl("<input id=\"stainContrastMax_residual\"", html, fixed = TRUE))
  expect_match(html, "Residual", fixed = TRUE)
  expect_match(html, '"visible":false', fixed = TRUE)
  expect_match(html, "stainConcentrations", fixed = TRUE)
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
  expect_match(html, "stainVisible_fast_red", fixed = TRUE)
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
