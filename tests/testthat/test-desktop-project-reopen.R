test_that("Desktop accepts saved viewer project files and preserves image associations", {
  launcher <- test_path("..", "..", "tools", "wsiToolsDesktop", "src-tauri", "resources", "launch-viewer.R")
  expressions <- parse(launcher)
  definition <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("desktop_viewer_project_items"))
  }, as.list(expressions))
  expect_length(definition, 1L)
  scope <- new.env(parent = baseenv())
  scope$`%||%` <- getFromNamespace("%||%", "wsiTools")
  eval(definition[[1L]], envir = scope)

  folder <- tempfile("saved-viewer-project-")
  dir.create(folder)
  image <- file.path(folder, "slide one.tif")
  writeBin(as.raw(0), image)
  project <- file.path(folder, "study.wsiproject.json")
  jsonlite::write_json(list(
    schema = "wsiTools-viewer-project",
    project = list(items = list(list(path = basename(image)))),
    session_inputs = list(list(tissue_annotation = "regions.geojson"))
  ), project, auto_unbox = TRUE)

  saved <- scope$desktop_viewer_project_items(project)
  expect_identical(saved$items[[1L]]$image, normalizePath(image, winslash = "/"))
  expect_identical(saved$items[[1L]]$tissue_annotation, "regions.geojson")
  invalid <- file.path(folder, "invalid.wsiproject.json")
  writeLines("{}", invalid)
  expect_error(scope$desktop_viewer_project_items(invalid), "not a wsiTools viewer project")
})

test_that("Desktop distinguishes viewer projects from R project manifests", {
  launcher <- test_path("..", "..", "tools", "wsiToolsDesktop", "src-tauri", "resources", "launch-viewer.R")
  expressions <- parse(launcher)
  definition <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("desktop_project_input"))
  }, as.list(expressions))
  expect_length(definition, 1L)
  scope <- new.env(parent = baseenv())
  eval(definition[[1L]], envir = scope)

  folder <- tempfile(fileext = ".wsiproject")
  dir.create(folder)
  manifest <- file.path(folder, "project.json")
  jsonlite::write_json(list(schema = "wsiTools-project"), manifest, auto_unbox = TRUE)
  expect_identical(scope$desktop_project_input(folder)$kind, "r")
  expect_identical(scope$desktop_project_input(manifest)$path,
                   normalizePath(folder, winslash = "/"))

  viewer_file <- tempfile(fileext = ".wsiproject.json")
  jsonlite::write_json(list(schema = "wsiTools-viewer-project"), viewer_file,
                       auto_unbox = TRUE)
  expect_identical(scope$desktop_project_input(viewer_file)$kind, "viewer")
  invalid <- tempfile(fileext = ".json")
  writeLines("{}", invalid)
  expect_error(scope$desktop_project_input(invalid), "not a wsiTools viewer project")
})

test_that("viewer project save omits undo snapshots and R restore uses fresh sources", {
  slide <- wsiTools:::wsi_mock_slide(width = 640, height = 320, levels = c(1, 4))
  path <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = path, open = FALSE, overwrite = TRUE)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "projectAnnotationSetsFull(false)", fixed = TRUE)
  expect_match(html, "restoreBrowserProjectFromR", fixed = TRUE)
  expect_match(html, "snapshot.project=Object.assign({},project,{items:live", fixed = TRUE)
  expect_match(html, "source.startsWith('imported:')", fixed = TRUE)
  expect_match(html, "if(!liveSyncAvailable()||!cfg.autosave_enabled)return saveProjectFile()", fixed = TRUE)
})

test_that("large imported tissue remains visible at overview zoom", {
  skip_if_not_installed("V8")
  slide <- wsiTools:::wsi_mock_slide(width = 640, height = 320, levels = c(1, 4))
  path <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = path, open = FALSE, overwrite = TRUE)
  lines <- readLines(path, warn = FALSE)
  function_line <- function(name) {
    matches <- grep(paste0("^function ", name, "\\("), lines, value = TRUE)
    expect_true(length(matches) > 0L)
    tail(matches, 1L)
  }
  js <- V8::v8()
  js$eval("var rois=[]; function pointCount(roi){return roi.point_count||0;} function denseGeometryZoom(){return 1;} function denseGeometryMinZoom(){return 5;}")
  js$eval(function_line("denseGeometryRoi"))
  js$eval(function_line("tissueAnnotationRoi"))
  js$eval(function_line("denseGeometryVisible"))
  expect_identical(js$eval("denseGeometryVisible({source:'imported: regions.geojson',point_count:10000,properties:{}})"), "true")
  expect_identical(js$eval("denseGeometryVisible({source:'imported: cell_segmentations.geojson',point_count:10000,properties:{}})"), "false")
})
