sync_feature <- function(id, x = 0) {
  list(type = "Feature", id = id, properties = list(name = id, class = "tumour"),
    geometry = list(type = "Polygon", coordinates = list(list(c(x, 0), c(x + 10, 0),
      c(x + 10, 10), c(x, 10), c(x, 0)))))
}

sync_header <- function(revision, base = revision - 1, key = "slide-a", full = FALSE) {
  list(version = 1, client = "test-browser", revision = revision, base_revision = base,
    project_key = key, full = full, selected_ids = list(), selected_id = NULL)
}

test_that("no-content responses have no body for httpuv to compress", {
  for (status in c(204L, 304L)) {
    response <- wsiTools:::wsi_http_json_response(status, body = "")
    expect_null(response$body)
    expect_identical(response$status, status)
  }
  expect_equal(as.character(wsiTools:::wsi_http_json_response(body = list(ok = TRUE))$body), '{"ok":true}')
  tile <- tempfile(fileext = ".png")
  on.exit(unlink(tile))
  writeBin(as.raw(1:8), tile)
  response <- wsiTools:::wsi_http_file_response(tile, "image/png",
    request_etag = wsiTools:::wsi_http_file_etag(tile))
  expect_identical(response$status, 304L)
  expect_null(response$body)
})

test_that("live request bodies are capped before parsing", {
  old_options <- options(wsiTools.max_request_bytes = 4)
  on.exit(options(old_options), add = TRUE)
  called <- FALSE
  input <- list(read = function(n) { called <<- TRUE; charToRaw("abcde") })
  expect_error(wsiTools:::wsi_http_request_body(list(rook.input = input, CONTENT_LENGTH = "5")),
               "maximum body size")
  expect_false(called)
  expect_error(wsiTools:::wsi_http_request_body(list(rook.input = input)),
               "maximum body size")
  expect_true(called)
  input$read <- function(n) charToRaw("abcd")
  expect_identical(wsiTools:::wsi_http_request_body(list(rook.input = input)), "abcd")
  expect_identical(wsiTools:::wsi_check_request_body_size("abcd"), "abcd")
  expect_error(wsiTools:::wsi_check_request_body_size("abcde"), "maximum body size")
})

test_that("zero-time viewer polling does not enter the httpuv service loop", {
  skip_if_not_installed("httpuv")
  calls <- list()
  testthat::local_mocked_bindings(service = function(timeoutMs) {
    calls[[length(calls) + 1L]] <<- timeoutMs
  }, .package = "httpuv")
  session <- structure(list(), class = "wsi_viewer_session")
  wsiTools:::wsi_viewer_session_pump(session, 0L)
  wsi_viewer_service(session, timeout = 0L)
  wsi_viewer_service(session, timeout = 20L)
  expect_identical(calls, list(NA_integer_, NA_integer_, 20L))
})

test_that("live endpoints require their session token", {
  token <- wsiTools:::wsi_viewer_session_token()
  expect_match(token, "^[0-9a-f]{64}$")
  url <- wsiTools:::wsi_viewer_auth_url("http://127.0.0.1:8788/viewer-state", token)
  expect_true(grepl(paste0("?wsitools_token=", token), url, fixed = TRUE))
  expect_false(wsiTools:::wsi_viewer_request_authorized(list(QUERY_STRING = ""), token))
  expect_false(wsiTools:::wsi_viewer_request_authorized(
    list(QUERY_STRING = "wsitools_token=wrong"), token))
  expect_true(wsiTools:::wsi_viewer_request_authorized(
    list(QUERY_STRING = paste0("wsitools_token=", token)), token))
  expect_true(wsiTools:::wsi_viewer_request_authorized(
    list(QUERY_STRING = paste0("?wsitools_token=", token, "&v=1")), token))
  source <- structure(list(
    id = "slide", route = "/tiles", width = 100, height = 100,
    tile_size = 512L, tile_format = "jpg", tile_overlap = 1L,
    min_level = 0L, max_level = 7L, cache_dir = tempdir(),
    access_token = token
  ), class = "wsi_dynamic_tile_source")
  metadata <- wsiTools:::wsi_dynamic_tile_metadata(source, base_url = "http://127.0.0.1:8788")
  expect_true(grepl(paste0("?wsitools_token=", token), metadata$tile_url_template, fixed = TRUE))
})

test_that("a busy tile lock never starts a duplicate tile read", {
  tile <- tempfile()
  dir.create(paste0(tile, ".lock"))
  on.exit(unlink(paste0(tile, ".lock"), recursive = TRUE))
  lock <- wsiTools:::wsi_dynamic_tile_lock(tile, wait_seconds = 0)
  expect_false(lock$acquired)
  expect_true(lock$busy)

  slide <- wsiTools:::wsi_mock_slide(width = 128, height = 128, levels = c(1))
  source <- wsi_dynamic_tile_source(slide, cache_dir = tempfile("busy_tiles_"))
  on.exit(unlink(source$cache_dir, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    wsi_dynamic_tile_lock = function(...) list(acquired = FALSE, busy = TRUE),
    .package = "wsiTools"
  )
  expect_error(wsiTools:::wsi_dynamic_tile_file(source, 7L, 0L, 0L),
               "still in progress", class = "wsi_tile_busy")
})

test_that("annotation deltas preserve unchanged ROIs and level-zero coordinates", {
  state <- wsiTools:::wsi_new_viewer_state()
  wsiTools:::wsi_viewer_state_apply(state, list(event = "viewer_loaded",
    sync = sync_header(1, full = TRUE), rois = list(type = "FeatureCollection",
      features = list(sync_feature("a"), sync_feature("b", 20)))))
  original <- state$rois$coordinates[[2]]
  patch <- sync_header(2)
  patch$rois_patch <- list(upsert = list(sync_feature("a", 1.123456)),
    remove = list(), order = c("a", "b"))
  patch$selected_ids <- "a"
  patch$selected_id <- "a"
  wsiTools:::wsi_viewer_state_apply(state, list(event = "roi_brush_edited", sync = patch))
  expect_equal(state$rois$xmin, c(1.123456, 20))
  expect_identical(state$rois$coordinates[[2]], original)
  expect_equal(state$selected_roi$roi_id, "a")
  expect_equal(state$selected_rois$roi_id, "a")
  expect_equal(state$browser_sync_ack$revision, 2)
  wsiTools:::wsi_viewer_state_apply(state, list(event = "viewport_changed",
    sync = sync_header(3), view = list(scale = 0.5, offset_x = 10, offset_y = 20)))
  expect_equal(state$rois$xmin, c(1.123456, 20))
  expect_equal(state$view$scale, 0.5)
})

test_that("sync patches reject stale base revisions and cross-slide changes", {
  state <- wsiTools:::wsi_new_viewer_state()
  first <- list(event = "viewer_loaded", sync = sync_header(1, full = TRUE),
    rois = list(type = "FeatureCollection", features = list(sync_feature("a"))))
  wsiTools:::wsi_viewer_state_apply(state, first)
  expect_error(wsiTools:::wsi_viewer_state_apply(state, list(event = "roi_updated",
    sync = sync_header(3, base = 0))), "snapshot")
  expect_error(wsiTools:::wsi_viewer_state_apply(state, list(event = "roi_updated",
    sync = sync_header(2, key = "slide-b"))), "snapshot")
  expect_equal(state$rois$roi_id, "a")
  # Replayed HTTP fallback after an acknowledged WebSocket request is idempotent.
  first$rois$features <- list(sync_feature("wrong"))
  wsiTools:::wsi_viewer_state_apply(state, first)
  expect_equal(state$rois$roi_id, "a")
  wsiTools:::wsi_viewer_state_apply(state, list(event = "project_image_selected",
    sync = sync_header(2, key = "slide-b", full = TRUE),
    rois = list(type = "FeatureCollection", features = list(sync_feature("b")))))
  expect_equal(state$rois$roi_id, "b")
})

test_that("sync deletions and undo patches address stable IDs, not row indices", {
  state <- wsiTools:::wsi_new_viewer_state()
  wsiTools:::wsi_viewer_state_apply(state, list(event = "viewer_loaded",
    sync = sync_header(1, full = TRUE), rois = list(type = "FeatureCollection",
      features = list(sync_feature("a"), sync_feature("b", 20)))))
  patch <- sync_header(2)
  patch$rois_patch <- list(upsert = list(), remove = "a", order = "b")
  wsiTools:::wsi_viewer_state_apply(state, list(event = "roi_deleted", sync = patch))
  expect_equal(state$rois$roi_id, "b")
  patch <- sync_header(3)
  patch$rois_patch <- list(upsert = list(sync_feature("a")), remove = list(), order = c("a", "b"))
  wsiTools:::wsi_viewer_state_apply(state, list(event = "annotation_undo", sync = patch))
  expect_equal(state$rois$roi_id, c("a", "b"))
})

test_that("local edits can omit unchanged annotation ordering", {
  state <- wsiTools:::wsi_new_viewer_state()
  wsiTools:::wsi_viewer_state_apply(state, list(event = "viewer_loaded",
    sync = sync_header(1, full = TRUE), rois = list(type = "FeatureCollection",
      features = list(sync_feature("a"), sync_feature("b", 20), sync_feature("c", 40)))))
  patch <- sync_header(2)
  patch$rois_patch <- list(upsert = list(sync_feature("b", 25)), remove = list())
  wsiTools:::wsi_viewer_state_apply(state, list(event = "roi_brush_edited", sync = patch))
  expect_identical(state$rois$roi_id, c("a", "b", "c"))
  expect_equal(state$rois$xmin, c(0, 25, 40))
  patch <- sync_header(3)
  patch$rois_patch <- list(upsert = list(sync_feature("d", 60)), remove = "a")
  wsiTools:::wsi_viewer_state_apply(state, list(event = "roi_brush_edited", sync = patch))
  expect_identical(state$rois$roi_id, c("b", "c", "d"))
})
