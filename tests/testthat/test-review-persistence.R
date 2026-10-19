# Persistence and concurrency regressions use only owned synthetic image bytes.
review_persistence_fixture <- function() {
  root <- tempfile("review-persistence-")
  dir.create(root)
  images <- observations <- list()
  tissues <- list()
  for (i in 1:2) {
    path <- file.path(root, paste0("image", i, ".ppm"))
    writeBin(c(charToRaw("P6\n64 64\n255\n"), as.raw(rep(100L + i, 64L * 64L * 3L))), path)
    image <- list(tissue_id = paste0("t", i), name = paste("Synthetic tissue", i),
      frame_id = paste0("frame", i), path = path, sha256 = wsi_experiment_sha256(path),
      width = 64L, height = 64L, mpp_x = .5, mpp_y = .5,
      coordinate_convention = "pixel-edge-origin_centres-in-native-pixels")
    images[[i]] <- image
    observations[[i]] <- list(id = paste0("study::t", i, "::rna::AA"), original_id = "AA",
      tissue_id = image$tissue_id, frame_id = image$frame_id, dataset_id = "rna",
      observation_unit = "spot", x = 16.1234567890123, y = 24.9876543210987,
      cluster_values = list(rna_cluster = as.character(i)))
    tissues[[i]] <- list(tissue_id = image$tissue_id, name = image$name,
      image = image[setdiff(names(image), c("tissue_id", "name"))], feature_sets = list(), artifacts = list())
  }
  path <- file.path(root, "experiment.cellphenotyper.json")
  jsonlite::write_json(list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "study", name = "Synthetic persistence fixture", tissues = tissues,
    settings = list(), provenance = list(synthetic = TRUE)), path, auto_unbox = TRUE, digits = 17)
  provenance <- list(source_reference = list(type = "cellphenotyper-experiment", manifest_path = path,
    manifest_sha256 = wsi_experiment_sha256(path)))
  context <- wsi_review_context(images, observations, context_id = "experiment:study",
    experiment_id = "study", provenance = provenance)
  list(root = root, path = path, context = context)
}

review_persistence_record <- function(id, label = "reviewed new") {
  list(id = id, status = "reviewed", label = label, reviewer = "software tester",
    updated_at = "2026-10-19T00:00:00Z", action = "confirm")
}

review_persistence_checkpoint <- function(context, revision, label = "reviewed new") {
  id <- context$observations[[1L]]$id
  wsi_review_checkpoint(context, selected_ids = id,
    review_records = list(review_persistence_record(id, label)), revision = revision)
}

review_persistence_state <- function(context, checkpoint = NULL) {
  state <- wsi_new_viewer_state(name = "review_persistence_state", envir = new.env(parent = emptyenv()))
  state$review_context <- context
  state$review_checkpoint <- checkpoint %||% wsi_review_checkpoint(context)
  state
}

test_that("restore checks experiment, context and source manifest identities before changing state", {
  skip_if_not_installed("openssl")
  fixture <- review_persistence_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  original <- fixture$context
  checkpoint <- review_persistence_checkpoint(original, 4L)
  state <- review_persistence_state(original, checkpoint)
  for (change in c("experiment", "missing_experiment", "context", "manifest", "missing_manifest", "frame", "calibration")) {
    images <- original$images
    provenance <- original$provenance
    experiment_id <- original$experiment_id
    context_id <- original$context_id
    rows <- original$observations
    if (change == "experiment") experiment_id <- "another-study"
    if (change == "missing_experiment") experiment_id <- NULL
    if (change == "context") context_id <- "another-context"
    if (change == "manifest") provenance$source_reference$manifest_sha256 <- strrep("f", 64L)
    if (change == "missing_manifest") provenance$source_reference$manifest_sha256 <- NULL
    if (change == "frame") {
      images[[1L]]$frame_id <- "another-frame"
      rows[[1L]]$frame_id <- "another-frame"
    }
    if (change == "calibration") images[[1L]]$mpp_x <- 1
    candidate <- wsi_review_context(images, rows, context_id = context_id,
      experiment_id = experiment_id, provenance = provenance)
    candidate_checkpoint <- review_persistence_checkpoint(candidate, 1L)
    expect_error(wsi_review_live_request(state, list(action = "restore", context = candidate,
      checkpoint = candidate_checkpoint)), info = change)
    expect_identical(state$review_context, original, info = change)
    expect_identical(state$review_checkpoint, checkpoint, info = change)
  }
  writeBin(charToRaw("changed owned image bytes"), original$images[[1L]]$path)
  expect_error(wsi_review_live_request(state, list(action = "restore", context = original,
    checkpoint = checkpoint)), "SHA-256 mismatch")
  expect_identical(state$review_checkpoint, checkpoint)
})

test_that("same-source restore permits added datasets and rebases explicit checkpoint revision", {
  skip_if_not_installed("openssl")
  fixture <- review_persistence_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- fixture$context
  added <- context$observations[[1L]]
  added$id <- "study::t1::rna-second::AA"
  added$dataset_id <- "rna-second"
  restored <- wsi_review_context(context$images, c(context$observations, list(added)),
    context_id = context$context_id, experiment_id = context$experiment_id, provenance = context$provenance)
  expect_error(wsi_review_restore_context(context, restored), NA)
  expect_false(identical(restored$source_signature, context$source_signature))
  state <- review_persistence_state(context, review_persistence_checkpoint(context, 20L))
  incoming <- review_persistence_checkpoint(restored, 2L, label = "intentional restored label")
  result <- wsi_review_live_request(state, list(action = "restore", context = restored, checkpoint = incoming))
  expect_identical(result$context$source_signature, restored$source_signature)
  expect_gt(result$checkpoint$revision, 20L)
  expect_identical(result$checkpoint$review_records[[1L]]$label, "intentional restored label")
  expect_identical(state$review_checkpoint, result$checkpoint)
  future <- review_persistence_checkpoint(restored, 100L)
  result <- wsi_review_live_request(state, list(action = "restore", context = restored, checkpoint = future))
  expect_gt(result$checkpoint$revision, 100L)
})

test_that("acknowledged viewer packets replay before validating an obsolete review context", {
  skip_if_not_installed("openssl")
  fixture <- review_persistence_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  before <- fixture$context
  changed_rows <- before$observations
  changed_rows[[1L]]$cluster_values$rna_cluster <- "changed source metadata"
  after <- wsi_review_context(before$images, changed_rows, context_id = before$context_id,
    experiment_id = before$experiment_id, provenance = before$provenance)
  checkpoint <- review_persistence_checkpoint(after, 8L)
  state <- review_persistence_state(after, checkpoint)
  state$browser_sync_clients <- list(browser = list(client = "browser", revision = 7L, project_key = "project"))
  state$browser_sync_project <- "project"
  payload <- list(event = "viewer_state", review_checkpoint = review_persistence_checkpoint(before, 3L),
    sync = list(version = 1, client = "browser", revision = 7, base_revision = 6,
      project_key = "project", full = FALSE))
  expect_true(wsi_viewer_sync_prepare(state, payload)$replay)
  expect_error(wsi_viewer_state_apply(state, payload), NA)
  expect_identical(state$review_context, after)
  expect_identical(state$review_checkpoint, checkpoint)
  expect_equal(state$browser_sync_ack$revision, 7L)
})

test_that("checkpoint commits cannot silently replace newer human decisions", {
  skip_if_not_installed("openssl")
  fixture <- review_persistence_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- fixture$context
  current <- review_persistence_checkpoint(context, 3L, "new decision")
  older <- review_persistence_checkpoint(context, 2L, "obsolete decision")
  conflict <- review_persistence_checkpoint(context, 3L, "conflicting same revision")
  state <- review_persistence_state(context, current)
  expect_error(wsi_review_commit_checkpoint(state, older))
  expect_identical(state$review_checkpoint, current)
  expect_error(wsi_review_commit_checkpoint(state, older, ignore_stale = TRUE), NA)
  expect_identical(state$review_checkpoint, current)
  expect_error(wsi_review_commit_checkpoint(state, current), NA)
  expect_identical(state$review_checkpoint, current)
  for (ignore in c(FALSE, TRUE)) {
    expect_error(wsi_review_commit_checkpoint(state, conflict, ignore_stale = ignore))
    expect_identical(state$review_checkpoint, current)
  }
  expect_error(wsi_review_live_request(state, list(action = "checkpoint", checkpoint = older)))
  expect_identical(state$review_checkpoint, current)
  expect_error(wsi_review_live_request(state, list(action = "checkpoint", checkpoint = conflict)))
  expect_identical(state$review_checkpoint, current)
  newer <- review_persistence_checkpoint(context, 4L, "accepted newer decision")
  expect_error(wsi_review_live_request(state, list(action = "checkpoint", checkpoint = newer)), NA)
  expect_identical(state$review_checkpoint, newer)
})

test_that("a delayed viewer checkpoint is ignored while unrelated viewer state still applies", {
  skip_if_not_installed("openssl")
  fixture <- review_persistence_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- fixture$context
  current <- review_persistence_checkpoint(context, 3L, "new decision")
  state <- review_persistence_state(context, current)
  state$view <- list(scale = 1)
  payload <- list(event = "viewer_state", view = list(scale = 2),
    review_checkpoint = review_persistence_checkpoint(context, 2L, "obsolete decision"))
  expect_error(wsi_viewer_state_apply(state, payload), NA)
  expect_identical(state$review_checkpoint, current)
  expect_equal(state$view$scale, 2)
  payload$review_checkpoint <- review_persistence_checkpoint(context, 3L, "same revision conflict")
  payload$view$scale <- 999
  expect_error(wsi_viewer_state_apply(state, payload))
  expect_identical(state$review_checkpoint, current)
  expect_equal(state$view$scale, 2)
})

test_that("R projects retain experiment references and full-precision review state on disk", {
  skip_if_not_installed("openssl")
  fixture <- review_persistence_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- fixture$context
  checkpoint <- review_persistence_checkpoint(context, 4L)
  state <- review_persistence_state(context, checkpoint)
  slide <- wsi_mock_slide(width = 64L, height = 64L)
  slide$path <- context$images[[1L]]$path
  project <- wsi_project(slide = slide, viewer_state = state)
  reference <- project$metadata$cellphenotyper_experiment
  expect_identical(reference$experiment_id, context$experiment_id)
  expect_identical(reference$manifest_path, fixture$path)
  expect_identical(reference$manifest_sha256, context$provenance$source_reference$manifest_sha256)
  saved <- wsi_save_project(project, file.path(fixture$root, "saved.wsiproject"))
  reopened <- wsi_read_project(saved$path)
  expect_identical(reopened$metadata$cellphenotyper_experiment$experiment_id, context$experiment_id)
  restored <- wsi_validate_review_context(reopened$viewer_state$review_context)
  expect_identical(restored$source_signature, context$source_signature)
  expect_equal(restored$observations[[1L]]$x, context$observations[[1L]]$x, tolerance = 1e-14)
  expect_identical(wsi_validate_review_checkpoint(reopened$viewer_state$review_checkpoint, restored)$source_signature,
    checkpoint$source_signature)
  expect_identical(wsi_read_experiment(reopened)$experiment_id, context$experiment_id)
})

test_that("R project restoration rebases old review decisions above the active revision", {
  skip_if_not_installed("openssl")
  fixture <- review_persistence_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- fixture$context
  state <- review_persistence_state(context, review_persistence_checkpoint(context, 20L, "active decision"))
  slide <- wsi_mock_slide(width = 64L, height = 64L)
  slide$path <- context$images[[1L]]$path
  viewer <- structure(list(state = state, slide = slide), class = "wsi_viewer_session")
  saved <- wsi_project(slide = slide, viewer_state = list(review_context = context,
    review_checkpoint = review_persistence_checkpoint(context, 2L, "intentional restored decision")))
  expect_error(restore_project_state(viewer, saved, service = FALSE), NA)
  expect_gt(state$review_checkpoint$revision, 20L)
  expect_identical(state$review_checkpoint$review_records[[1L]]$label, "intentional restored decision")
})

test_that("appending an explicitly mapped spatial dataset preserves existing human decisions", {
  skip_if_not_installed("openssl")
  fixture <- review_persistence_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- fixture$context
  previous <- review_persistence_checkpoint(context, 9L, "keep this reviewed class")
  state <- review_persistence_state(context, previous)
  object <- list(coordinates = data.frame(barcode = c("BB", "CC"), x = c(30, 40), y = c(20, 30)),
    meta.data = data.frame(seurat_clusters = c("0", "1"), row.names = c("BB", "CC")),
    reductions = list(pca = list(cell.embeddings = matrix(c(1, 2, -1, -2), ncol = 2,
      dimnames = list(c("BB", "CC"), c("PC1", "PC2"))))))
  path <- file.path(fixture$root, "new-spatial.rds")
  saveRDS(object, path)
  image <- context$images[[1L]]
  result <- wsi_review_live_request(state, list(action = "attach_spatial", path = path, trusted = TRUE,
    tissue_id = image$tissue_id, dataset_id = "new-rna", cluster_columns = list("seurat_clusters"),
    reduction_names = list("pca"), coordinate_mapping = list(confirmed = TRUE, same_section = TRUE,
      coordinate_space = "native_pixels", image_sha256 = image$sha256, frame_id = image$frame_id)))
  expect_length(result$context$observations, 4L)
  expect_length(result$context$reductions, 1L)
  expect_identical(result$checkpoint$review_records, previous$review_records)
  expect_identical(result$checkpoint$selected_ids, previous$selected_ids)
  expect_gt(result$checkpoint$revision, previous$revision)
  expect_false(identical(result$context$source_signature, context$source_signature))
  expect_identical(result$checkpoint$source_signature, result$context$source_signature)
  expect_identical(result$context$observations[1:2], context$observations)
  expect_identical(state$review_checkpoint, result$checkpoint)
})
