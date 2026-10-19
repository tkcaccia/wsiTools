test_that("generated browser sync events are accepted by the live R bridge", {
  ns <- asNamespace("wsiTools")
  helpers <- ls(ns, pattern = "^wsi_viewer_.*_js$")
  helpers <- helpers[vapply(helpers, function(name) length(formals(get(name, ns))) == 0L, logical(1))]
  js <- paste(vapply(helpers, function(name) get(name, ns)(), character(1)), collapse = "\n")
  hits <- regmatches(js, gregexpr("scheduleViewerStateSync\\('[a-z_]+'", js, perl = TRUE))[[1L]]
  events <- unique(sub("'$", "", sub("^scheduleViewerStateSync\\('", "", hits)))
  # Vertex drag chooses its event dynamically, outside the literal-call scan.
  events <- unique(c(events, "roi_vertex_moved", "trajectory_border_vertex_moved"))
  expect_gt(length(events), 0L)
  expect_equal(setdiff(events, wsiTools:::wsi_viewer_allowed_events()), character())
  for (event in events) {
    expect_silent(wsiTools:::wsi_viewer_validate_state_payload(list(event = event, detail = list())))
  }
  expect_error(wsiTools:::wsi_viewer_validate_state_payload(list(event = "eval_r_code")), "Unsupported viewer event")
})

test_that("a moved polygon vertex reaches R without losing its hole", {
  state <- wsiTools:::wsi_new_viewer_state(name = "vertex", envir = new.env(parent = emptyenv()))
  make_feature <- function(x) list(type = "Feature", id = "roi-vertex", properties = list(name = "QA polygon", classification = list(name = "stroma")), geometry = list(type = "Polygon", coordinates = list(list(c(1,1),c(x,1),c(x,8),c(1,8),c(1,1)), list(c(2,2),c(2,3),c(3,3),c(3,2),c(2,2)))))
  payload <- list(event = "roi_created", rois = list(type = "FeatureCollection", features = list(make_feature(8))), annotations = list(dirty = TRUE))
  expect_silent(wsiTools:::wsi_viewer_state_apply(state, payload))
  payload$event <- "roi_vertex_moved"
  payload$rois$features <- list(make_feature(12))
  expect_silent(wsiTools:::wsi_viewer_state_apply(state, payload))
  expect_equal(state$last_event, "roi_vertex_moved")
  expect_equal(nrow(state$rois), 1L)
  geometry <- state$rois$geometry[[1L]]
  expect_equal(max(vapply(geometry$coordinates[[1L]], function(point) point[[1L]], numeric(1))), 12)
  expect_equal(length(geometry$coordinates), 2L)
})
