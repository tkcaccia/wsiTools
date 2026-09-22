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
