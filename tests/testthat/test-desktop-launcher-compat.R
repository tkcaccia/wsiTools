test_that("desktop live viewer omits options absent from an older R package", {
  launcher_path <- test_path("../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R")
  skip_if_not(file.exists(launcher_path), "Desktop launcher sources are not available.")
  expressions <- as.list(parse(launcher_path))
  definition <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("desktop_compatible_live_args"))
  }, expressions)[[1L]]
  scope <- new.env(parent = baseenv())
  scope$desktop_log <- function(...) invisible(NULL)
  eval(definition, envir = scope)

  args <- list(
    mode = "tiles", dynamic_tiles = TRUE,
    dynamic_tile_persistent_cache = TRUE,
    session_inputs = list(list(image = "sample.tif")),
    dense_geojson_sources = list(), annotation_masks = list()
  )
  old_live <- function(slide, ..., dynamic_tiles = FALSE) NULL
  old_viewer <- function(slide, mode = "tiles") NULL
  compatible <- scope$desktop_compatible_live_args(
    args, live_fun = old_live, viewer_fun = old_viewer
  )
  expect_equal(names(compatible), c("mode", "dynamic_tiles"))

  current_live <- function(slide, ..., dynamic_tiles = FALSE,
                           dynamic_tile_persistent_cache = FALSE,
                           dense_geojson_sources = NULL, annotation_masks = NULL) NULL
  current_viewer <- function(slide, session_inputs = NULL,
                             dense_geojson_sources = NULL) NULL
  expect_identical(
    scope$desktop_compatible_live_args(
      args, live_fun = current_live, viewer_fun = current_viewer
    ),
    args
  )

  args$annotation_masks <- list("cells.ome.tif")
  expect_error(
    scope$desktop_compatible_live_args(args, live_fun = old_live, viewer_fun = old_viewer),
    "Update the R package"
  )
})
