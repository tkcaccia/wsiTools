# Synthetic CSV/image contracts only; no encoder, KODAMA fit or GPU execution.
review_experiment_fixture <- function(tissues = 2L, artifacts = TRUE) {
  root <- tempfile("review-experiment-")
  dir.create(root)
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "review-fixture", name = "Synthetic review", settings = list(), provenance = list(), tissues = list())
  for (i in seq_len(tissues)) {
    image <- file.path(root, paste0("image", i, ".ppm"))
    writeBin(c(charToRaw("P6\n64 32\n255\n"), as.raw(rep(128L, 64L * 32L * 3L))), image)
    columns <- paste0("feat_", seq_len(2560L))
    x <- outer(seq_len(8L), seq_len(2560L), function(row, column) sin(row / 2 + column / 13) + row * (column %% 3) / 10)
    colnames(x) <- columns
    features <- file.path(root, paste0("features", i, ".csv"))
    utils::write.csv(cbind(data.frame(id = sprintf("%03d", 1:8), x = seq(4, 60, 8), y = 4), x), features, row.names = FALSE)
    feature <- list(feature_set_id = "virchow2-pooled", kind = "virchow2-pooled", observation_unit = "tile",
      path = basename(features), sha256 = wsi_experiment_sha256(features), format = "csv", row_count = 8L,
      feature_columns = as.list(columns), id_column = "id", x_column = "x", y_column = "y", coordinate_frame = "image",
      encoder = list(name = "Virchow2", revision = "synthetic", checkpoint_sha256 = paste(rep("a", 64), collapse = "")),
      preprocessing = list(feature_definition = "CLS+mean_patch"),
      grid = list(tile_width_px = 8, tile_height_px = 8, spacing_x_px = 8, spacing_y_px = 8, origin_x_px = 0, origin_y_px = 0))
    tissue <- list(tissue_id = paste0("t", i), name = paste("Tissue", i),
      image = list(path = basename(image), sha256 = wsi_experiment_sha256(image), width = 64L, height = 32L,
        mpp_x = .5, mpp_y = .5, frame_id = paste0("frame", i), coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"),
      feature_sets = list(feature), artifacts = list())
    if (artifacts) {
      observations <- file.path(root, paste0("spots", i, ".csv"))
      utils::write.csv(data.frame(barcode = c("001", "002", "003"), native_x = c(8, 24, 40), native_y = 16,
        rna_cluster = c("0", "0", "1")), observations, row.names = FALSE)
      reduction <- file.path(root, paste0("kodama", i, ".csv"))
      utils::write.csv(data.frame(barcode = c("003", "001", "002"), K1 = c(30, 10, 20), K2 = c(-3, -1, -2)), reduction, row.names = FALSE)
      tissue$artifacts <- list(
        list(artifact_id = "spots", role = "review_observations", dataset_id = "rna", observation_unit = "spot",
          frame_id = tissue$image$frame_id, path = basename(observations), sha256 = wsi_experiment_sha256(observations),
          format = "csv", row_count = 3L, id_column = "barcode", x_column = "native_x", y_column = "native_y",
          cluster_columns = list("rna_cluster"), footprint_width_px = 4, footprint_height_px = 4),
        list(artifact_id = "kodama", role = "observation_reduction", dataset_id = "rna", reduction_id = "KODAMA",
          method = "KODAMA", label = "Identical synthetic reduction label", observation_unit = "spot", frame_id = tissue$image$frame_id,
          path = basename(reduction), sha256 = wsi_experiment_sha256(reduction), format = "csv", row_count = 3L,
          id_column = "barcode", dimension_columns = list("K1", "K2")))
    }
    manifest$tissues[[i]] <- tissue
  }
  path <- file.path(root, "experiment.cellphenotyper.json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE)
  list(root = root, path = path, manifest = manifest)
}

test_that("review contexts join shuffled reductions to tissue-scoped stable IDs", {
  skip_if_not_installed("openssl")
  fixture <- review_experiment_fixture()
  context <- wsi_experiment_review_context(fixture$path)
  expect_length(context$observations, 22L)
  expect_length(context$reductions, 2L)
  expect_length(unique(vapply(context$observations, `[[`, character(1), "id")), 22L)
  expect_length(unique(vapply(context$reductions, `[[`, character(1), "id")), 2L)
  expect_length(unique(vapply(context$reductions, `[[`, character(1), "label")), 2L)
  expect_match(context$reductions[[1]]$label, "Tissue 1.*rna")
  for (reduction in context$reductions) {
    rows <- context$observations[match(vapply(reduction$points, `[[`, character(1), "id"),
      vapply(context$observations, `[[`, character(1), "id"))]
    expect_identical(vapply(rows, `[[`, character(1), "original_id"), c("001", "002", "003"))
    expect_equal(vapply(reduction$points, `[[`, numeric(1), "x"), c(10, 20, 30))
    expect_true(all(vapply(rows, function(row) identical(row$observation_unit, "spot"), logical(1))))
    expect_equal(rows[[1]]$cluster_values$rna_cluster, "0")
  }
  expect_null(context$review_records)
  roundtrip <- jsonlite::fromJSON(jsonlite::toJSON(context, auto_unbox = TRUE, null = "null", digits = 17), simplifyVector = FALSE)
  expect_identical(wsi_validate_review_context(roundtrip)$source_signature, context$source_signature)
})

test_that("version one experiments need no new artifacts and review is bounded", {
  skip_if_not_installed("openssl")
  fixture <- review_experiment_fixture(artifacts = FALSE)
  context <- wsi_experiment_review_context(fixture$path, tissue_ids = "t1")
  expect_length(context$observations, 8L)
  expect_length(context$reductions, 0L)
  expect_identical(context$observations[[1]]$feature_set_id, "virchow2-pooled")
  expect_error(wsi_experiment_review_context(fixture$path, max_observations = 5L), "exceeding")
  unavailable <- wsi_experiment_review_context(fixture$path, max_observations = 5L, strict = FALSE)
  expect_length(unavailable$images, 2L)
  expect_length(unavailable$observations, 0L)
  expect_identical(unavailable$provenance$availability$status, "unavailable")
  expect_match(unavailable$provenance$availability$reason, "no rows were sampled")
  expect_length(unavailable$provenance$dataset_catalog, 2L)
  expect_error(wsi_experiment_review_context(fixture$path, feature_set_ids = "unknown"), "Unknown feature")
})

test_that("explicit review artifacts fail closed on hashes, frames and reduction identity", {
  skip_if_not_installed("openssl")
  fixture <- review_experiment_fixture(tissues = 1L)
  for (failure in c("hash", "frame", "unit", "unknown_ids", "duplicate_ids", "dataset")) {
    manifest <- fixture$manifest
    artifact <- manifest$tissues[[1]]$artifacts[[2]]
    if (failure == "hash") artifact$sha256 <- paste(rep("f", 64L), collapse = "")
    if (failure == "frame") artifact$frame_id <- "another-frame"
    if (failure == "unit") artifact$observation_unit <- "tile"
    if (failure == "dataset") artifact$dataset_id <- "missing"
    if (failure %in% c("unknown_ids", "duplicate_ids")) {
      path <- file.path(fixture$root, paste0(failure, ".csv"))
      ids <- if (failure == "unknown_ids") c("003", "001", "foreign") else c("003", "001", "001")
      utils::write.csv(data.frame(barcode = ids, K1 = 1:3, K2 = 4:6), path, row.names = FALSE)
      artifact$path <- basename(path)
      artifact$sha256 <- wsi_experiment_sha256(path)
    }
    manifest$tissues[[1]]$artifacts[[2]] <- artifact
    path <- file.path(fixture$root, paste0("manifest-", failure, ".json"))
    jsonlite::write_json(manifest, path, auto_unbox = TRUE)
    expect_error(wsi_experiment_review_context(path), info = failure)
  }
})

test_that("only confirmed reviewed image observations train a versioned CPU model", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("fastPLS")
  fixture <- review_experiment_fixture(tissues = 1L)
  context <- wsi_experiment_review_context(fixture$path)
  tiles <- Filter(function(row) identical(row$observation_unit, "tile"), context$observations)
  ids <- vapply(tiles, `[[`, character(1), "id")
  records <- lapply(seq_along(ids), function(i) list(id = ids[[i]], status = "reviewed",
    label = if (i <= 4) "Pilot A" else "Pilot B", reviewer = "software tester", updated_at = "2026-10-19T00:00:00Z"))
  checkpoint <- wsi_review_checkpoint(context, selected_ids = ids, review_records = records, revision = 3L)
  model <- wsi_experiment_train_reviewed_classifier(fixture$path, context, checkpoint,
    "virchow2-pooled", train_ids = ids, ncomp = 1L, max_features = 8L)
  expect_identical(model$backend, "cpu")
  expect_equal(model$human_review$checkpoint_revision, 3L)
  expect_identical(model$training_ids, ids)
  expect_identical(model$human_review$label_origin, "explicit_human_review")
  expect_error(wsi_experiment_train_reviewed_classifier(fixture$path, context, checkpoint,
    "virchow2-pooled", train_ids = character()), "empty selection")
  uncertain <- checkpoint
  uncertain$review_records[[1]]$status <- "uncertain"
  expect_error(wsi_experiment_train_reviewed_classifier(fixture$path, context, uncertain,
    "virchow2-pooled", train_ids = ids), "confirmed reviewed")
  spot <- Filter(function(row) identical(row$observation_unit, "spot"), context$observations)[[1]]
  spot_checkpoint <- wsi_review_checkpoint(context, review_records = list(list(id = spot$id,
    status = "reviewed", label = "RNA cluster", reviewer = "software tester", updated_at = "2026-10-19T00:00:00Z")))
  expect_error(wsi_experiment_train_reviewed_classifier(fixture$path, context, spot_checkpoint,
    "virchow2-pooled", train_ids = spot$id), "spot/cell")
  altered <- context
  altered$observations[[1]]$x <- altered$observations[[1]]$x + 1
  altered <- wsi_review_context(altered$images, altered$observations, altered$reductions,
    altered$context_id, altered$experiment_id, altered$provenance)
  altered_checkpoint <- wsi_review_checkpoint(altered, review_records = records)
  expect_error(wsi_experiment_train_reviewed_classifier(fixture$path, altered, altered_checkpoint,
    "virchow2-pooled", train_ids = ids), "native image feature metadata")
})
