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

test_that("new desktop projects do not reuse previous project caches", {
  launcher_path <- test_path("../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R")
  skip_if_not(file.exists(launcher_path), "Desktop launcher sources are not available.")
  expressions <- as.list(parse(launcher_path))
  names_to_load <- c(
    "desktop_fresh_project", "desktop_static_cache_control",
    "desktop_geojson_cache_root", "desktop_use_prebuilt_tiles"
  )
  scope <- new.env(parent = baseenv())
  for (definition in expressions) {
    if (is.call(definition) && identical(definition[[1L]], as.name("<-")) &&
        as.character(definition[[2L]]) %in% names_to_load) {
      eval(definition, envir = scope)
    }
  }

  previous <- Sys.getenv(c("WSITOOLS_DESKTOP_FRESH_PROJECT",
                           "WSITOOLS_DESKTOP_FRESH_SESSION_DIR"), unset = NA_character_)
  on.exit({
    for (name in names(previous)) {
      if (is.na(previous[[name]])) Sys.unsetenv(name)
      else do.call(Sys.setenv, stats::setNames(list(previous[[name]]), name))
    }
  }, add = TRUE)
  fresh_dir <- tempfile("fresh-project-")
  Sys.setenv(
    WSITOOLS_DESKTOP_FRESH_PROJECT = "true",
    WSITOOLS_DESKTOP_FRESH_SESSION_DIR = fresh_dir
  )

  expect_true(scope$desktop_fresh_project())
  expect_identical(scope$desktop_static_cache_control("slide_files/10/0_0.jpg"), "no-store")
  expect_identical(scope$desktop_static_cache_control("annotations.geojson"), "no-store")
  expect_identical(scope$desktop_geojson_cache_root(), file.path(fresh_dir, "geojson_cache"))
  expect_false(scope$desktop_use_prebuilt_tiles())

  launcher <- paste(deparse(parse(launcher_path)[[length(expressions) - 1L]]), collapse = "\n")
  expect_match(launcher, "paste0(\"viewer/\"", fixed = TRUE)
  expect_match(launcher, "file.path(tempdir(), paste0(\"wsiTools_project_\"", fixed = TRUE)
})

test_that("fresh desktop viewers serve project-specific files without HTTP caching", {
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")
  skip_if_not_installed("curl")
  launcher_path <- test_path("../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R")
  skip_if_not(file.exists(launcher_path), "Desktop launcher sources are not available.")
  definitions <- as.list(parse(launcher_path))
  names_to_load <- c(
    "%||%", "desktop_fresh_project", "desktop_content_type",
    "desktop_http_response", "desktop_static_cache_control",
    "desktop_static_etag", "desktop_static_url_prefix",
    "desktop_start_html_server_in_process",
    "desktop_start_html_server", "desktop_stop_html_server"
  )
  scope <- new.env(parent = baseenv())
  for (definition in definitions) {
    if (is.call(definition) && identical(definition[[1L]], as.name("<-")) &&
        as.character(definition[[2L]]) %in% names_to_load) {
      eval(definition, envir = scope)
    }
  }

  roots <- file.path(tempdir(), c("viewer-project-one", "viewer-project-two"))
  for (i in seq_along(roots)) {
    dir.create(roots[[i]], recursive = TRUE, showWarnings = FALSE)
    writeLines(paste0("project ", i), file.path(roots[[i]], "index.html"))
    writeLines(paste0("tile ", i), file.path(roots[[i]], "0_0.jpg"))
    writeLines(paste0('{"project":', i, '}'), file.path(roots[[i]], "annotation.geojson"))
  }
  servers <- list()
  on.exit(lapply(servers, scope$desktop_stop_html_server), add = TRUE)
  for (i in seq_along(roots)) {
    servers[[i]] <- scope$desktop_start_html_server(
      file.path(roots[[i]], "index.html"),
      port = httpuv::randomPort(min = 18000L, max = 49000L),
      fresh_project = TRUE,
      url_prefix = paste0("viewer/project-", i)
    )
    html <- curl::curl_fetch_memory(servers[[i]]$url)
    expect_identical(rawToChar(html$content), paste0("project ", i, "\n"))
    response <- curl::curl_fetch_memory(paste0(servers[[i]]$url, "annotation.geojson"))
    expect_identical(response$status_code, 200L)
    expect_identical(rawToChar(response$content), paste0('{"project":', i, '}\n'))
    expect_match(rawToChar(response$headers), "Cache-Control: no-store", ignore.case = TRUE)
    tile <- curl::curl_fetch_memory(paste0(servers[[i]]$url, "0_0.jpg"))
    expect_identical(rawToChar(tile$content), paste0("tile ", i, "\n"))
    expect_match(rawToChar(tile$headers), "Cache-Control: no-store", ignore.case = TRUE)
  }
  expect_false(identical(servers[[1L]]$url, servers[[2L]]$url))
  expect_match(servers[[1L]]$url, "/viewer/project-1/", fixed = TRUE)
  expect_match(servers[[2L]]$url, "/viewer/project-2/", fixed = TRUE)
  second_origin <- sub("/viewer/project-2/$", "", servers[[2L]]$url)
  stale <- curl::curl_fetch_memory(paste0(second_origin, "/viewer/project-1/annotation.geojson"))
  expect_identical(stale$status_code, 404L)
})
