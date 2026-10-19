review_live_fixture <- function() {
  path <- tempfile(fileext = ".png")
  writeBin(charToRaw("bounded-test-image-identity"), path)
  image <- list(tissue_id = "section", frame_id = "native", path = path,
    sha256 = wsi_experiment_sha256(path), width = 224L, height = 224L,
    mpp_x = .5, mpp_y = .5, coordinate_convention = "pixel-edge-origin_centres-in-native-pixels")
  observations <- lapply(1:3, function(i) list(id = paste0("s::", i), original_id = paste0("BC", i),
    tissue_id = "section", frame_id = "native", observation_unit = "spot",
    x = 20 + i, y = 25, cluster_values = list(cluster = "A")))
  wsi_review_context(list(image), observations, context_id = "study", experiment_id = "study")
}

test_that("live review planning uses explicit selected IDs and source identity", {
  context <- review_live_fixture()
  on.exit(unlink(context$images[[1L]]$path), add = TRUE)
  state <- wsi_new_viewer_state(name = paste0("review_test_", Sys.getpid()), envir = new.env())
  state$review_context <- context
  state$review_checkpoint <- wsi_review_checkpoint(context)
  request <- list(action = "tile_plan", context_id = context$context_id,
    source_signature = context$source_signature, selected_ids = c("s::1", "s::2"), tile_size = 112)
  result <- wsi_review_live_request(state, request)
  expect_equal(result$plan$work_count, 1L)
  expect_setequal(unlist(result$plan$tiles[[1L]]$observation_ids), c("s::1", "s::2"))
  request$selected_ids <- character()
  expect_equal(wsi_review_live_request(state, request)$plan$work_count, 0L)
  request$source_signature <- strrep("f", 64L)
  expect_error(wsi_review_live_request(state, request), "source changed")
  before <- state$review_checkpoint
  broken <- before; broken$selected_ids <- "unknown"
  expect_error(wsi_review_live_request(state, list(action = "checkpoint", checkpoint = broken)), "unknown")
  expect_identical(state$review_checkpoint, before)
})

test_that("saved review restore verifies bytes and retains authoritative state on failure", {
  context <- review_live_fixture()
  on.exit(unlink(context$images[[1L]]$path), add = TRUE)
  state <- wsi_new_viewer_state(name = paste0("review_restore_", Sys.getpid()), envir = new.env())
  state$review_context <- context
  checkpoint <- wsi_review_checkpoint(context, "s::1")
  result <- wsi_review_live_request(state, list(action = "restore", context = context, checkpoint = checkpoint))
  expect_equal(unlist(result$checkpoint$selected_ids), "s::1")
  changed <- context; changed$images[[1L]]$mpp_x <- 1
  changed <- wsi_review_context(changed$images, changed$observations, context_id = "study", experiment_id = "study")
  expect_error(wsi_review_live_request(state, list(action = "restore", context = changed)), "calibration differ")
  expect_identical(state$review_context, context)
  writeBin(charToRaw("changed-image"), context$images[[1L]]$path)
  expect_error(wsi_review_live_request(state, list(action = "restore", context = context, checkpoint = checkpoint)), "SHA-256 mismatch")
  expect_identical(state$review_checkpoint, checkpoint)
})
