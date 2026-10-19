spatial_project_live_fixture <- function() {
  root <- tempfile("spatial-live-"); dir.create(root)
  image <- file.path(root, "image.ppm")
  writeBin(c(charToRaw("P6\n64 48\n255\n"), as.raw(rep(128L, 64L * 48L * 3L))), image)
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "spatial-live", name = "Spatial live", settings = structure(list(), names = character()),
    provenance = structure(list(), names = character()), tissues = list(list(tissue_id = "t1", name = "Tissue",
      image = list(path = basename(image), sha256 = wsi_experiment_sha256(image), width = 64L, height = 48L,
        mpp_x = .5, mpp_y = .5, frame_id = "frame", coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"),
      artifacts = list(), feature_sets = list())))
  path <- file.path(root, "experiment.cellphenotyper.json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, null = "null", digits = 17)
  object <- file.path(root, "source.rds")
  saveRDS(list(coordinates = data.frame(id = c("BC1", "BC2"), x = c(10, 25), y = c(12, 26)),
    meta.data = data.frame(seurat_clusters = c("0", "1"), row.names = c("BC1", "BC2")),
    reductions = list(umap = list(cell.embeddings = matrix(1:4, ncol = 2,
      dimnames = list(c("BC1", "BC2"), c("U1", "U2")))))), object)
  mapping <- list(confirmed = TRUE, same_section = TRUE, image_sha256 = manifest$tissues[[1]]$image$sha256,
    frame_id = "frame", coordinate_space = "native_pixels")
  first <- wsi_experiment_add_spatial(path, object, "t1", "first", mapping,
    cluster_columns = "seurat_clusters", reduction_names = "umap", trusted = TRUE)
  state <- wsi_new_viewer_state(name = "spatial_test", envir = new.env())
  state$review_context <- first$context
  ids <- vapply(first$context$observations, `[[`, character(1), "id")
  record <- list(id = ids[[1]], label = "Reviewed morphology", status = "reviewed", action = "confirm",
    reviewer = "QA", updated_at = "2026-10-19T10:00:00Z")
  state$review_checkpoint <- wsi_review_checkpoint(first$context, ids[[1]], list(record), revision = 4,
    history = list(past = list(list(selected_ids = character(), review_records = list())), future = list()))
  payload <- list(action = "attach_spatial", path = object, tissue_id = "t1", dataset_id = "second",
    trusted = TRUE, persist_to_experiment = TRUE, coordinate_mapping = mapping,
    cluster_columns = list(), reduction_names = list("umap"), checkpoint = state$review_checkpoint)
  list(root = root, path = path, object = object, state = state, payload = payload)
}

test_that("live project import preserves reviews and explicit empty metadata choices", {
  skip_if_not_installed("openssl")
  fixture <- spatial_project_live_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  previous <- fixture$state$review_checkpoint
  before <- fixture$state$review_context
  result <- wsi_review_live_request(fixture$state, fixture$payload)
  expect_true(result$persisted)
  expect_equal(length(result$context$observations), 4L)
  expect_identical(result$checkpoint$selected_ids, previous$selected_ids)
  expect_identical(result$checkpoint$review_records, previous$review_records)
  expect_identical(result$checkpoint$history, previous$history)
  expect_equal(result$checkpoint$revision, 5)
  added <- Filter(function(row) identical(row$dataset_id, "second"), result$context$observations)
  expect_true(all(vapply(added, function(row) length(row$cluster_values) == 0L, logical(1))))
  expect_equal(length(result$context$reductions), 2L)
  expect_identical(result$experiment_reference$manifest_sha256, wsi_experiment_sha256(fixture$path))
  expect_identical(fixture$state$review_experiment$manifest_sha256, result$experiment_reference$manifest_sha256)
  expect_identical(before$observations, fixture$state$review_context$observations[1:2])
  expect_true(file.exists(result$backup_path))
  scoped <- wsi_review_select(result$context, ids = vapply(result$context$observations, `[[`, character(1), "id"),
    dataset_ids = "second")
  expect_setequal(scoped, vapply(added, `[[`, character(1), "id"))
  expect_error(wsi_review_select(result$context, dataset_ids = "missing"), "unknown")
  invalid <- fixture$payload; invalid$dataset_id <- "*"; invalid$persist_to_experiment <- FALSE
  invalid$checkpoint <- result$checkpoint
  expect_error(wsi_review_live_request(fixture$state, invalid), "dataset_id must")
  expect_identical(fixture$state$review_checkpoint, result$checkpoint)
})

test_that("project import refuses to drop session-only data before touching the index", {
  skip_if_not_installed("openssl")
  fixture <- spatial_project_live_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- wsi_review_attach_spatial(fixture$state$review_context, fixture$object,
    "t1", "session-only", coordinate_mapping = fixture$payload$coordinate_mapping, trusted = TRUE)
  fixture$state$review_context <- context
  fixture$state$review_checkpoint <- wsi_review_checkpoint(context)
  fixture$payload$checkpoint <- fixture$state$review_checkpoint
  before <- wsi_experiment_sha256(fixture$path)
  expect_error(wsi_review_live_request(fixture$state, fixture$payload), "viewer-only")
  expect_identical(wsi_experiment_sha256(fixture$path), before)
  expect_identical(fixture$state$review_context, context)
})

test_that("a viewer-only spatial object can be stored without losing its human review", {
  skip_if_not_installed("openssl")
  fixture <- spatial_project_live_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- wsi_review_attach_spatial(fixture$state$review_context, fixture$object,
    "t1", "viewer-only", coordinate_mapping = fixture$payload$coordinate_mapping, trusted = TRUE,
    reduction_names = "umap")
  promoted <- Filter(function(row) identical(row$dataset_id, "viewer-only"), context$observations)
  record <- list(id = promoted[[1L]]$id, label = "Existing review", status = "reviewed", reviewer = "QA",
    updated_at = "2026-10-19T10:00:00Z")
  fixture$state$review_context <- context
  fixture$state$review_checkpoint <- wsi_review_checkpoint(context, promoted[[1L]]$id, list(record), revision = 7)
  fixture$payload$dataset_id <- "viewer-only"
  fixture$payload$cluster_columns <- list("seurat_clusters")
  fixture$payload$checkpoint <- fixture$state$review_checkpoint
  result <- wsi_review_live_request(fixture$state, fixture$payload)
  expect_true(result$persisted)
  expect_identical(result$checkpoint$selected_ids, list(promoted[[1L]]$id))
  expect_identical(result$checkpoint$review_records, list(record))
  expect_equal(result$checkpoint$revision, 8)
  expect_equal(length(result$context$observations), 4)
  expect_equal(length(wsi_experiment_review_context(fixture$path)$observations), 4)
})

test_that("viewer-only promotion rejects a changed source before updating the project", {
  skip_if_not_installed("openssl")
  fixture <- spatial_project_live_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- wsi_review_attach_spatial(fixture$state$review_context, fixture$object,
    "t1", "viewer-only", coordinate_mapping = fixture$payload$coordinate_mapping, trusted = TRUE,
    reduction_names = "umap")
  fixture$state$review_context <- context
  fixture$state$review_checkpoint <- wsi_review_checkpoint(context)
  fixture$payload$dataset_id <- "viewer-only"
  fixture$payload$cluster_columns <- list("seurat_clusters")
  fixture$payload$checkpoint <- fixture$state$review_checkpoint
  original <- readRDS(fixture$object)
  original$unrelated_assay_setting <- "changed bytes, same displayed observations"
  saveRDS(original, fixture$object)
  before <- wsi_experiment_sha256(fixture$path)
  expect_error(wsi_review_live_request(fixture$state, fixture$payload), "RDS bytes changed")
  expect_identical(wsi_experiment_sha256(fixture$path), before)
  expect_identical(fixture$state$review_context, context)
})

test_that("project import cannot silently replace a session reduction over persisted IDs", {
  skip_if_not_installed("openssl")
  fixture <- spatial_project_live_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  old <- fixture$state$review_context
  reductions <- old$reductions
  reductions[[1L]]$points[[1L]]$x <- reductions[[1L]]$points[[1L]]$x + 10
  context <- wsi_review_context(old$images, old$observations, reductions,
    old$context_id, old$experiment_id, old$provenance)
  fixture$state$review_context <- context
  fixture$state$review_checkpoint <- wsi_review_checkpoint(context)
  fixture$payload$checkpoint <- fixture$state$review_checkpoint
  before <- wsi_experiment_sha256(fixture$path)
  expect_error(wsi_review_live_request(fixture$state, fixture$payload), "existing viewer reductions")
  expect_identical(wsi_experiment_sha256(fixture$path), before)
  expect_identical(fixture$state$review_context, context)
})

test_that("empty cluster metadata serializes as an object without source signature drift", {
  skip_if_not_installed("openssl")
  fixture <- spatial_project_live_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  context <- fixture$state$review_context
  observations <- lapply(context$observations, function(row) { row$cluster_values <- list(); row })
  context <- wsi_review_context(context$images, observations, context$reductions,
    context$context_id, context$experiment_id, context$provenance)
  encoded <- jsonlite::toJSON(context, auto_unbox = TRUE, null = "null", digits = 17)
  expect_match(encoded, '"cluster_values":\\{\\}')
  expect_false(grepl('"cluster_values":\\[\\]', encoded))
  roundtrip <- jsonlite::fromJSON(encoded, simplifyVector = FALSE)
  expect_identical(wsi_validate_review_context(roundtrip)$source_signature, context$source_signature)
  # A saved version-1 context with [] for empty metadata retains its identity.
  legacy <- context; legacy$observations <- observations
  expect_identical(wsi_validate_review_context(legacy)$source_signature, context$source_signature)
})
