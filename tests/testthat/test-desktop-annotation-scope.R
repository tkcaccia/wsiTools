test_that("R queues annotations for their image without changing the active image", {
  env <- new.env(parent = emptyenv())
  state <- wsiTools:::wsi_new_viewer_state(name = "annotation_scope", envir = env)
  session <- wsiTools:::wsi_attach_viewer_session_methods(structure(
    list(server = NULL, url = "http://127.0.0.1/viewer-state", state = state,
         slide = wsiTools:::wsi_mock_slide(width = 100, height = 80),
         html = tempfile(fileext = ".html"), name = "annotation_scope", envir = env,
         stardist_server = NULL),
    class = "wsi_viewer_session"
  ))
  roi <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection", features = list(list(
      type = "Feature", id = "tissue", properties = list(name = "Tissue"),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(1, 1), c(9, 1), c(9, 9), c(1, 9), c(1, 1)
      )))
    ))
  ))
  for (index in 0:3) {
    session$add_rois(roi, name = paste("Image", index + 1L), service = FALSE,
                     project_image_index = index)
  }
  expect_equal(nrow(state$rois), 0L)
  commands <- wsiTools:::wsi_viewer_state_response(state)$commands
  expect_equal(vapply(commands, function(command) command$payload$project_image_index,
                      integer(1)), 0:3)
  expect_error(session$add_rois(roi, project_image_index = -1L, service = FALSE),
               "project_image_index")
})

test_that("the desktop association loop targets each of four images", {
  launcher_path <- test_path("../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R")
  skip_if_not(file.exists(launcher_path), "Desktop resources are not included in source-package checks.")
  definitions <- parse(launcher_path)
  scope <- new.env(parent = baseenv())
  scope$`%||%` <- function(value, fallback) if (is.null(value)) fallback else value
  scope$desktop_log <- function(...) invisible(NULL)
  scope$desktop_should_defer_geojson <- function(path) FALSE
  scope$desktop_tag_tissue_rois <- identity
  scope$desktop_register_tissue_analysis <- function(...) invisible(TRUE)
  scope$desktop_geojson_geometry_summary <- function(rois) paste(nrow(rois), "regions")
  for (name in c("desktop_add_annotation_files", "desktop_apply_associated_data")) {
    definition <- Filter(function(expr) is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
                           identical(as.character(expr[[2L]]), name), definitions)
    expect_length(definition, 1L)
    eval(definition[[1L]], envir = scope)
  }
  files <- vapply(seq_len(4L), function(index) {
    path <- tempfile(fileext = ".geojson")
    jsonlite::write_json(list(type = "FeatureCollection", features = list(list(
      type = "Feature", id = paste0("roi-", index), properties = list(name = "Tumour"),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(1, 1), c(9, 1), c(9, 9), c(1, 9), c(1, 1)
      )))
    ))), path, auto_unbox = TRUE)
    path
  }, character(1))
  calls <- list()
  viewer <- structure(list(add_rois = function(rois, name, service, project_image_index) {
    calls[[length(calls) + 1L]] <<- list(id = rois$roi_id[[1L]],
                                        project_image_index = project_image_index)
  }), class = "wsi_viewer_session")
  items <- lapply(seq_len(4L), function(index) list(
    image = paste0("slide-", index, ".tif"), tissue_annotation = files[[index]],
    cell_annotation = ""
  ))
  scope$desktop_apply_associated_data(viewer, items, output = tempfile(), log_file = NULL)
  expect_equal(vapply(calls, `[[`, integer(1), "project_image_index"), 0:3)
  expect_equal(vapply(calls, `[[`, character(1), "id"), paste0("roi-", 1:4))
})

test_that("deferred GeoJSON chunks retain their target image", {
  launcher_path <- test_path("../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R")
  skip_if_not(file.exists(launcher_path), "Desktop resources are not included in source-package checks.")
  definition <- Filter(function(expr) is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
                         identical(as.character(expr[[2L]]), "desktop_poll_pending_imports"),
                       parse(launcher_path))
  expect_length(definition, 1L)
  scope <- new.env(parent = baseenv())
  scope$`%||%` <- function(value, fallback) if (is.null(value)) fallback else value
  scope$desktop_log <- function(...) invisible(NULL)
  scope$desktop_geojson_chunk_size <- function() 20L
  scope$desktop_viewer_ready_for_deferred_import <- function(viewer) TRUE
  eval(definition[[1L]], envir = scope)
  roi <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection", features = list(list(
      type = "Feature", id = "pancreas", properties = list(name = "Pancreas"),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(1, 1), c(9, 1), c(9, 9), c(1, 9), c(1, 1)
      )))
    ))
  ))
  calls <- list()
  viewer <- structure(list(add_rois = function(rois, name, service, project_image_index) {
    calls[[length(calls) + 1L]] <<- list(id = rois$roi_id[[1L]],
                                        index = project_image_index)
  }), class = "wsi_viewer_session")
  pending <- list(list(kind = "tissue", path = "pancreas.geojson", name = "Tissue annotation",
                       loaded = TRUE, done = FALSE, offset = 0L,
                       project_image_index = 3L, rois = roi))
  result <- scope$desktop_poll_pending_imports(viewer, pending)
  expect_true(result[[1L]]$done)
  expect_equal(calls[[1L]], list(id = "pancreas", index = 3L))
})

test_that("browser imports four slide annotations into distinct project sets", {
  skip_if_not_installed("V8")
  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 80, levels = c(1, 4))
  path <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = path, open = FALSE, overwrite = TRUE)
  lines <- readLines(path, warn = FALSE)
  start <- which(startsWith(lines, "function handleProjectScopedRoisCommand("))
  expect_length(start, 1L)
  end <- which(seq_along(lines) > start & lines == "}")
  expect_gt(length(end), 0L)
  definition <- paste(lines[start:end[[1L]]], collapse = "\n")
  js <- V8::v8()
  js$eval(paste(
    "var projectItems=['lymph','tonsil','brain','pancreas'].map(label=>({label}));",
    "var activeProjectIndex=0,activeProjectSectionIndex=-1,rois=[{id:'lymph'}];",
    "var projectAnnotationStore=new Map(),stateCommandSeen=new Set(),logs=[];",
    "function projectAnnotationKey(i){return projectItems[i].label+'::image';}",
    "function defaultProjectSectionIndex(){return -1;}",
    "function preloadProjectAnnotations(){}",
    "function saveActiveProjectAnnotations(){projectAnnotationStore.set('lymph::image',{rois:rois.slice()});}",
    "function geojsonFeatures(obj){return obj.features||[];}",
    "function geojsonGeometryParts(){return [{}];}",
    "function importedRoiFromFeature(feature){return {id:feature.id,drawable:true};}",
    "function wsiSnapshotRoi(roi){return roi;}",
    "function addImportedGeojson(obj){rois.push(...obj.features);}",
    "function recordViewerSyncHistory(){}",
    "function recordViewerLog(message){logs.push(message);}",
    "function renderProjectPanel(){}"
  ))
  js$eval(definition)
  js$eval(paste(
    "for(const [index,count] of [[1,7],[2,5],[3,3]]){",
    "const features=Array.from({length:count},(_,i)=>({id:index+'-'+i,geometry:{}}));",
    "handleProjectScopedRoisCommand({id:'cmd-'+index,type:'add_rois',",
    "payload:{project_image_index:index,name:'Tissue annotation',geojson:{features}}});}"
  ))
  expect_identical(js$eval("rois.length"), "1")
  expect_identical(js$eval("projectAnnotationStore.get('lymph::image').rois.length"), "1")
  expect_identical(js$eval("projectAnnotationStore.get('tonsil::image').rois.length"), "7")
  expect_identical(js$eval("projectAnnotationStore.get('brain::image').rois.length"), "5")
  expect_identical(js$eval("projectAnnotationStore.get('pancreas::image').rois.length"), "3")
  js$eval("handleProjectScopedRoisCommand({id:'cmd-2',type:'add_rois',payload:{project_image_index:2,geojson:{features:[{id:'duplicate',geometry:{}}]}}})")
  expect_identical(js$eval("projectAnnotationStore.get('brain::image').rois.length"), "5")
})

test_that("desktop and dense layers carry the associated image index", {
  launcher_path <- test_path("../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R")
  skip_if_not(file.exists(launcher_path), "Desktop resources are not included in source-package checks.")
  launcher <- paste(readLines(launcher_path, warn = FALSE), collapse = "\n")
  expect_match(launcher, "image_index = i", fixed = TRUE)
  expect_match(launcher, "project_image_index = image_index - 1L", fixed = TRUE)
  expect_match(launcher, "project_image_index = item$project_image_index", fixed = TRUE)
  expect_match(launcher, "project_image_index = 0L", fixed = TRUE)
  expect_match(paste(deparse(wsiTools:::wsi_start_viewer_state_server), collapse = "\n"),
               "project_image_index = source$project_image_index", fixed = TRUE)
})

test_that("a scoped annotation layer appears only on its microscopy image", {
  skip_if_not_installed("V8")
  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 80, levels = c(1, 4))
  path <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = path, open = FALSE, overwrite = TRUE)
  lines <- readLines(path, warn = FALSE)
  js <- V8::v8()
  js$eval("var projectItems=[{label:'lymph'},{label:'tonsil'},{label:'brain'},{label:'pancreas'}]; var activeProjectIndex=0,activeProjectSectionIndex=-1;")
  for (name in c("layerScopeText", "layerScopeNumber", "layerItemHasProjectScope",
                 "layerItemMatchesActiveProject")) {
    definition <- grep(paste0("^function ", name, "\\("), lines, value = TRUE)
    expect_length(definition, 1L)
    js$eval(definition)
  }
  js$eval("var tissue={project_image_index:2};")
  expect_identical(js$eval("layerItemMatchesActiveProject({},tissue)"), "false")
  js$eval("activeProjectIndex=2")
  expect_identical(js$eval("layerItemMatchesActiveProject({},tissue)"), "true")
  js$eval("activeProjectIndex=3")
  expect_identical(js$eval("layerItemMatchesActiveProject({},tissue)"), "false")
  expect_match(paste(wsiTools:::wsi_viewer_geometry_js(), collapse = "\n"),
               "project_image_index:source.project_image_index", fixed = TRUE)
})

test_that("R analysis selects the annotation set for the requested image", {
  state <- wsiTools:::wsi_new_viewer_state(name = "analysis_scope",
                                           envir = new.env(parent = emptyenv()))
  make_roi <- function(id) wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection", features = list(list(
      type = "Feature", id = id, properties = list(name = id),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(1, 1), c(9, 1), c(9, 9), c(1, 9), c(1, 1)
      )))
    ))
  ))
  state$analysis_rois <- make_roi("lymph")
  state$analysis_rois_by_project[["0"]] <- make_roi("lymph")
  state$analysis_rois_by_project[["1"]] <- make_roi("tonsil")
  expect_equal(wsiTools:::wsi_spatial_object_annotation_rois(
    list(project_image_index = 0L), state)$roi_id, "lymph")
  expect_equal(wsiTools:::wsi_spatial_object_annotation_rois(
    list(project_image_index = 1L), state)$roi_id, "tonsil")
  expect_equal(nrow(wsiTools:::wsi_spatial_object_annotation_rois(
    list(project_image_index = 2L), state)), 0L)
})
