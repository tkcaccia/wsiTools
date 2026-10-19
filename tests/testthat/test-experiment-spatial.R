# Small synthetic RDS/PPM contracts; no expression analysis or model execution.
spatial_experiment_fixture <- function(tissues = 2L) {
  root <- tempfile("persistent-spatial-"); dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "persistent-fixture", name = "Synthetic spatial import", settings = structure(list(), names = character()),
    provenance = list(note = "existing metadata must survive"), tissues = list())
  for (i in seq_len(tissues)) {
    image <- file.path(root, paste0("image", i, ".ppm"))
    writeBin(c(charToRaw("P6\n64 48\n255\n"), as.raw(rep(128L, 64L * 48L * 3L))), image)
    manifest$tissues[[i]] <- list(tissue_id = paste0("t", i), name = paste("Tissue", i),
      image = list(path = basename(image), sha256 = wsi_experiment_sha256(image), width = 64L, height = 48L,
        mpp_x = .5, mpp_y = .7, frame_id = paste0("frame", i),
        coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"),
      feature_sets = list(), artifacts = list(), user_note = paste("Keep", i))
  }
  path <- file.path(root, "experiment.cellphenotyper.json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  source <- tempfile(fileext = ".rds")
  object <- list(coordinates = data.frame(barcode = c("001", "NA", "a space"),
      x = c(10.1234567890123, 20, 30), y = c(11, 21, 31)),
    meta.data = data.frame(seurat_clusters = c("NA", NA_character_, "2"), row.names = c("001", "NA", "a space")),
    reductions = list(kodama = list(cell.embeddings = matrix(c(30, 10, 20, -3, -1, -2), ncol = 2,
      dimnames = list(c("a space", "001", "NA"), c("K1", "K2"))))),
    assays = list(expression = "ORIGINAL_EXPRESSION_STAYS_ONLY_IN_RDS"))
  saveRDS(object, source)
  source <- normalizePath(source, winslash = "/", mustWork = TRUE)
  mapping <- function(i = 1L) list(confirmed = TRUE, same_section = TRUE,
    image_sha256 = manifest$tissues[[i]]$image$sha256, frame_id = manifest$tissues[[i]]$image$frame_id,
    coordinate_space = "native_pixels")
  list(root = root, path = path, manifest = manifest, source = source, object = object, mapping = mapping)
}

spatial_import_fixture <- function(fixture, tissue_id = "t1", dataset_id = "rna", ...) {
  i <- match(tissue_id, vapply(fixture$manifest$tissues, `[[`, character(1), "tissue_id"))
  wsi_experiment_add_spatial(fixture$path, fixture$source, tissue_id = tissue_id, dataset_id = dataset_id,
    coordinate_mapping = fixture$mapping(i), cluster_columns = "seurat_clusters", reduction_names = "kodama",
    trusted = TRUE, ...)
}

test_that("spatial import preserves original bytes and publishes a bounded path-free bundle", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  old <- readBin(fixture$path, "raw", n = file.info(fixture$path)$size)
  source_hash <- wsi_experiment_sha256(fixture$source)
  before <- wsi_read_experiment(fixture$path)
  old_context <- wsi_experiment_review_context(before)
  result <- spatial_import_fixture(fixture)
  expect_identical(readBin(result$backup_path, "raw", n = length(old)), old)
  expect_identical(wsi_experiment_sha256(fixture$source), source_hash)
  expect_identical(result$object_artifact$path, paste0("spatial/objects/", source_hash, ".rds"))
  expect_identical(result$object_artifact$review_bundle_artifact_id, result$bundle_artifact$artifact_id)
  expect_identical(result$bundle_artifact$source_artifact_id, result$object_artifact$artifact_id)
  expect_identical(result$experiment$manifest_path, fixture$path)
  expect_identical(result$experiment$manifest_sha256, wsi_experiment_sha256(fixture$path))
  expect_identical(result$context$provenance$source_reference$manifest_path, fixture$path)
  expect_identical(result$context$provenance$source_reference$manifest_sha256, result$experiment$manifest_sha256)
  expect_false(identical(before$manifest_sha256, result$experiment$manifest_sha256))
  expect_length(old_context$observations, 0L)
  expect_length(before$tissues[[1]]$artifacts, 0L)
  expect_identical(result$experiment$manifest$provenance, fixture$manifest$provenance)
  expect_identical(result$experiment$manifest$tissues[[2]], fixture$manifest$tissues[[2]])
  bundle_path <- file.path(fixture$root, result$bundle_artifact$path)
  bundle <- jsonlite::fromJSON(bundle_path, simplifyVector = FALSE)
  expect_identical(bundle$schema, "cellphenotyper-spatial-review-bundle")
  expect_false(grepl("ORIGINAL_EXPRESSION|source_reference|resolved_path|\"path\"", paste(readLines(bundle_path), collapse = "")))
  expect_length(bundle$observations, 3L)
  expect_length(bundle$reductions, 1L)
  expect_identical(bundle$observations[[1]]$x, fixture$object$coordinates$x[[1]])
  expect_identical(bundle$observations[[1]]$cluster_values$seurat_clusters, "NA")
  expect_true("seurat_clusters" %in% names(bundle$observations[[2]]$cluster_values))
  expect_null(bundle$observations[[2]]$cluster_values$seurat_clusters)
  expect_identical(vapply(result$context$observations, `[[`, character(1), "original_id"), c("001", "NA", "a space"))
  expect_equal(vapply(result$context$reductions[[1]]$points, `[[`, numeric(1), "x"), c(10, 20, 30))
  expect_null(result$context$review_records)
  expect_false(dir.exists(file.path(fixture$root, ".experiment.cellphenotyper.json.spatial-lock")))
})

test_that("fresh experiment opening hashes copied RDS without deserializing it", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  result <- spatial_import_fixture(fixture)
  unlink(fixture$source)
  local_mocked_bindings(wsi_review_spatial_source = function(...) stop("RDS deserialization forbidden"), .package = "wsiTools")
  opened <- wsi_read_experiment(fixture$path)
  context <- wsi_experiment_review_context(opened)
  expect_identical(context$source_signature, result$context$source_signature)
  expect_identical(context$observations, result$context$observations)
  expect_identical(context$reductions, result$context$reductions)
  expect_identical(wsi_validate_review_context(context)$source_signature, context$source_signature)
})

test_that("copied experiment trees reopen with relative spatial artifact paths", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  result <- spatial_import_fixture(fixture)
  relocated <- tempfile("relocated-spatial-"); dir.create(relocated)
  copied <- file.copy(list.files(fixture$root, full.names = TRUE, all.files = TRUE, no.. = TRUE), relocated, recursive = TRUE)
  expect_true(all(copied))
  new_path <- file.path(relocated, basename(fixture$path))
  opened <- wsi_read_experiment(new_path)
  context <- wsi_experiment_review_context(opened)
  expect_identical(opened$manifest_sha256, result$experiment$manifest_sha256)
  expect_identical(context$observations, result$context$observations)
  expect_identical(context$reductions, result$context$reductions)
  expect_false(identical(context$source_signature, result$context$source_signature))
  expect_match(context$images[[1]]$path, normalizePath(relocated, winslash = "/", mustWork = TRUE), fixed = TRUE)
  expect_false(any(grepl("^/", vapply(opened$manifest$tissues[[1]]$artifacts, `[[`, character(1), "path"))))
})

test_that("tissues may reuse local IDs and dataset IDs without collisions", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  first <- spatial_import_fixture(fixture)
  second <- spatial_import_fixture(fixture, tissue_id = "t2")
  expect_length(second$context$observations, 6L)
  expect_length(unique(vapply(second$context$observations, `[[`, character(1), "id")), 6L)
  expect_length(unique(vapply(second$context$reductions, `[[`, character(1), "id")), 2L)
  expect_identical(second$object_artifact$path, first$object_artifact$path)
  expect_false(identical(second$bundle_artifact$path, first$bundle_artifact$path))
  expect_length(first$context$observations, 3L)
  old_bytes <- readBin(fixture$path, "raw", n = file.info(fixture$path)$size)
  expect_error(spatial_import_fixture(fixture), "already has")
  expect_identical(readBin(fixture$path, "raw", n = length(old_bytes)), old_bytes)
  expect_error(wsi_experiment_add_spatial(first$experiment, fixture$source, "t1", "another",
    fixture$mapping(), trusted = TRUE), "changed since")
})

test_that("a whole multi-FOV RDS is copied once while each tissue gets its selected view", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  object <- fixture$object
  object$coordinates <- NULL
  coords <- fixture$object$coordinates
  object$images <- list(fov_A = list(coordinates = coords[1:2, ]), fov_B = list(coordinates = coords[c(3, 1), ]))
  saveRDS(object, fixture$source)
  first <- wsi_experiment_add_spatial(fixture$path, fixture$source, "t1", dataset_id = "rna",
    coordinate_mapping = fixture$mapping(1), trusted = TRUE, image_name = "fov_A",
    cluster_columns = "seurat_clusters", reduction_names = "kodama")
  second <- wsi_experiment_add_spatial(fixture$path, fixture$source, "t2", dataset_id = "rna",
    coordinate_mapping = fixture$mapping(2), trusted = TRUE, image_name = "fov_B",
    cluster_columns = "seurat_clusters", reduction_names = "kodama")
  expect_identical(first$object_artifact$path, second$object_artifact$path)
  expect_equal(first$object_artifact$row_count, 2)
  expect_equal(second$object_artifact$row_count, 2)
  expect_length(second$context$observations, 4L)
  expect_length(unique(vapply(second$context$observations, `[[`, character(1), "id")), 4L)
  expect_identical(vapply(first$context$observations, `[[`, character(1), "original_id"), c("001", "NA"))
  expect_identical(vapply(tail(second$context$observations, 2), `[[`, character(1), "original_id"), c("a space", "001"))
  copied_object <- readRDS(file.path(fixture$root, first$object_artifact$path))
  expect_identical(copied_object, object)
})

test_that("invalid selected-FOV rows cannot silently disappear during persistent import", {
  skip_if_not_installed("openssl")
  for (failure in c("invalid_coordinate", "missing_id", "unidentified_fov", "boundary_coordinate")) {
    fixture <- spatial_experiment_fixture()
    object <- fixture$object; object$coordinates <- NULL
    coords <- fixture$object$coordinates
    object$images <- list(fov_A = list(coordinates = coords), fov_B = list(coordinates = coords))
    if (failure == "invalid_coordinate") object$images$fov_A$coordinates$x[[2]] <- NA_real_
    if (failure == "missing_id") object$images$fov_A$coordinates$barcode[[2]] <- ""
    if (failure == "unidentified_fov") {
      object$images$fov_A <- list()
      object$meta.data$fullres_x <- coords$x
      object$meta.data$fullres_y <- coords$y
    }
    if (failure == "boundary_coordinate") {
      matrix <- as.matrix(coords[, c("x", "y")]); matrix[2, 1] <- NA_real_
      object$images$fov_A <- list(boundaries = list(centroids = list(coords = matrix, cells = coords$barcode)))
    }
    saveRDS(object, fixture$source)
    old_hash <- wsi_experiment_sha256(fixture$path)
    expect_error(wsi_experiment_add_spatial(fixture$path, fixture$source, "t1", coordinate_mapping = fixture$mapping(),
      trusted = TRUE, image_name = "fov_A"), "no rows were imported|no identifiable coordinate table", info = failure)
    expect_identical(wsi_experiment_sha256(fixture$path), old_hash)
  }
})

test_that("invalid trust, IDs, mapping and bounds leave the index untouched", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  old_hash <- wsi_experiment_sha256(fixture$path)
  expect_error(wsi_experiment_add_spatial(fixture$path, fixture$object, "t1", coordinate_mapping = fixture$mapping(), trusted = TRUE), "local RDS path")
  expect_error(wsi_experiment_add_spatial(fixture$path, fixture$source, "t1", coordinate_mapping = fixture$mapping()), "trusted=TRUE")
  expect_error(spatial_import_fixture(fixture, dataset_id = "bad/id"), "dataset_id")
  mapping <- fixture$mapping(); mapping$frame_id <- "wrong"
  expect_error(wsi_experiment_add_spatial(fixture$path, fixture$source, "t1", coordinate_mapping = mapping, trusted = TRUE), "SHA-256/frame")
  expect_error(spatial_import_fixture(fixture, max_observations = 2L), "exceeds")
  expect_identical(wsi_experiment_sha256(fixture$path), old_hash)
  expect_false(dir.exists(file.path(fixture$root, ".experiment.cellphenotyper.json.spatial-lock")))
})

test_that("lock contention and corrupt immutable assets never replace the index", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  old_hash <- wsi_experiment_sha256(fixture$path)
  lock <- file.path(fixture$root, ".experiment.cellphenotyper.json.spatial-lock")
  dir.create(lock)
  expect_error(spatial_import_fixture(fixture), "holds.*lock")
  expect_true(dir.exists(lock))
  unlink(lock, recursive = TRUE)
  dir.create(file.path(fixture$root, "spatial/objects"), recursive = TRUE)
  asset <- file.path(fixture$root, "spatial/objects", paste0(wsi_experiment_sha256(fixture$source), ".rds"))
  writeBin(charToRaw("different original work"), asset)
  expect_error(spatial_import_fixture(fixture), "not overwritten")
  expect_identical(readBin(asset, "raw", n = 23L), charToRaw("different original work"))
  expect_identical(wsi_experiment_sha256(fixture$path), old_hash)
})

test_that("source mutation fails before replacement", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  old_hash <- wsi_experiment_sha256(fixture$path)
  original_copy <- wsi_experiment_spatial_copy
  local_mocked_bindings(wsi_experiment_spatial_copy = function(source, destination, expected) {
    if (identical(source, fixture$source)) writeBin(charToRaw("changed while copying"), source)
    original_copy(source, destination, expected)
  }, .package = "wsiTools")
  expect_error(spatial_import_fixture(fixture), "changed while copying")
  expect_identical(wsi_experiment_sha256(fixture$path), old_hash)
})

test_that("failed final atomic replacement preserves the exact original index", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  old <- readBin(fixture$path, "raw", n = file.info(fixture$path)$size)
  local_mocked_bindings(wsi_experiment_spatial_replace = function(...) FALSE, .package = "wsiTools")
  expect_error(spatial_import_fixture(fixture), "atomically update")
  expect_identical(readBin(fixture$path, "raw", n = length(old)), old)
  backup <- list.files(fixture$root, pattern = "before-spatial", full.names = TRUE, all.files = TRUE)
  expect_length(backup, 1L)
  expect_identical(readBin(backup, "raw", n = length(old)), old)
  expect_false(dir.exists(file.path(fixture$root, ".experiment.cellphenotyper.json.spatial-lock")))
})

test_that("concurrent index edits survive an import conflict", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  old_bytes <- readBin(fixture$path, "raw", n = file.info(fixture$path)$size)
  concurrent_bytes <- c(old_bytes, charToRaw("\n "))
  original_copy <- wsi_experiment_spatial_copy
  local_mocked_bindings(wsi_experiment_spatial_copy = function(source, destination, expected) {
    original_copy(source, destination, expected)
    if (identical(source, fixture$source)) writeBin(concurrent_bytes, fixture$path)
  }, .package = "wsiTools")
  expect_error(spatial_import_fixture(fixture), "changed during import")
  expect_identical(readBin(fixture$path, "raw", n = length(concurrent_bytes)), concurrent_bytes)
})

test_that("future context is validated before index replacement", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture()
  old_hash <- wsi_experiment_sha256(fixture$path)
  original_context <- wsi_experiment_review_context
  local_mocked_bindings(wsi_experiment_review_context = function(experiment, ...) {
    if (inherits(experiment, "wsi_experiment") && !identical(experiment$manifest_sha256, old_hash))
      stop("future context rejected")
    original_context(experiment, ...)
  }, .package = "wsiTools")
  expect_error(spatial_import_fixture(fixture), "future context rejected")
  expect_identical(wsi_experiment_sha256(fixture$path), old_hash)
})

test_that("spatial bundle tampering and invalid lifecycle links fail on passive open", {
  skip_if_not_installed("openssl")
  for (failure in c("object_hash", "bundle_hash", "missing_pair", "link", "frame", "mapping", "duplicate_ids", "reduction_ids", "paths", "nested_path")) {
    fixture <- spatial_experiment_fixture(tissues = 1L)
    result <- spatial_import_fixture(fixture)
    manifest <- result$experiment$manifest
    object <- manifest$tissues[[1]]$artifacts[[1]]
    artifact <- manifest$tissues[[1]]$artifacts[[2]]
    if (failure == "object_hash") writeBin(charToRaw("modified"), file.path(fixture$root, object$path))
    if (failure == "bundle_hash") writeLines("{}", file.path(fixture$root, artifact$path))
    if (failure == "missing_pair") manifest$tissues[[1]]$artifacts <- list(object)
    if (failure == "link") manifest$tissues[[1]]$artifacts[[2]]$source_artifact_id <- "foreign"
    if (failure %in% c("frame", "mapping", "duplicate_ids", "reduction_ids", "paths", "nested_path")) {
      bundle <- jsonlite::fromJSON(file.path(fixture$root, artifact$path), simplifyVector = FALSE)
      if (failure == "frame") bundle$frame_id <- "foreign"
      if (failure == "mapping") bundle$coordinate_mapping$confirmed <- FALSE
      if (failure == "duplicate_ids") bundle$observations[[2]] <- bundle$observations[[1]]
      if (failure == "reduction_ids") bundle$reductions[[1]]$points[[1]]$id <- "foreign"
      if (failure == "paths") bundle$path <- "/must/not/be/serialized"
      if (failure == "nested_path") bundle$observations[[1]]$cluster_values$Source_Path <- "/must/not/be/serialized"
      changed <- file.path(fixture$root, "altered-bundle.json")
      jsonlite::write_json(bundle, changed, auto_unbox = TRUE, null = "null", digits = 17)
      manifest$tissues[[1]]$artifacts[[2]]$path <- basename(changed)
      manifest$tissues[[1]]$artifacts[[2]]$sha256 <- wsi_experiment_sha256(changed)
    }
    jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
    expect_error(wsi_read_experiment(fixture$path), info = failure)
  }
})

test_that("selected path metadata is rejected before any project mutation", {
  skip_if_not_installed("openssl")
  fixture <- spatial_experiment_fixture(tissues = 1L)
  object <- fixture$object
  object$meta.data$source_path <- c("/a", "/b", "/c")
  saveRDS(object, fixture$source)
  old_hash <- wsi_experiment_sha256(fixture$path)
  expect_error(wsi_experiment_add_spatial(fixture$path, fixture$source, "t1", coordinate_mapping = fixture$mapping(),
    trusted = TRUE, cluster_columns = "source_path"), "path metadata columns")
  expect_identical(wsi_experiment_sha256(fixture$path), old_hash)
  expect_false(dir.exists(file.path(fixture$root, "spatial")))
})
