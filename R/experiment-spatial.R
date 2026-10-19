# Persistent spatial attachments use immutable, path-free review bundles.
# Opening an experiment hashes RDS bytes but never deserializes an R object.
wsi_experiment_spatial_roles <- function() c("spatial_transcriptomics_object", "spatial_review_bundle")

wsi_experiment_spatial_no_paths <- function(value) {
  if (!is.list(value)) return(invisible(TRUE))
  keys <- tolower(names(value) %||% character())
  if (any(keys %in% c("path", "paths") | grepl("_paths?$", keys)))
    wsi_abort("Spatial review bundles cannot contain internal path fields; omit path metadata columns.")
  for (child in value) wsi_experiment_spatial_no_paths(child)
  invisible(TRUE)
}

wsi_experiment_spatial_pairs <- function(tissue) {
  artifacts <- tissue$artifacts %||% list()
  spatial <- Filter(function(a) isTRUE(a$role %in% wsi_experiment_spatial_roles()), artifacts)
  if (!length(spatial)) return(list())
  ids <- vapply(artifacts, function(a) a$artifact_id %||% "", character(1))
  spatial_ids <- vapply(spatial, function(a) wsi_experiment_text(a$artifact_id, "spatial artifact_id"), character(1))
  if (anyDuplicated(spatial_ids) || any(vapply(spatial_ids, function(id) sum(ids == id) != 1L, logical(1))))
    wsi_abort("Spatial artifact IDs must be unique within their tissue.")
  datasets <- unique(vapply(spatial, function(a) wsi_experiment_text(a$dataset_id, "spatial dataset_id"), character(1)))
  lapply(datasets, function(dataset) {
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.:-]*$", dataset))
      wsi_abort("Spatial dataset_id must begin with a letter/digit and contain only letters, digits, underscore, dot, colon or hyphen.")
    entries <- Filter(function(a) identical(a$dataset_id, dataset), spatial)
    objects <- Filter(function(a) identical(a$role, "spatial_transcriptomics_object"), entries)
    bundles <- Filter(function(a) identical(a$role, "spatial_review_bundle"), entries)
    if (length(objects) != 1L || length(bundles) != 1L)
      wsi_abort("Each spatial dataset requires exactly one object artifact and one review bundle artifact.")
    object <- objects[[1L]]; bundle <- bundles[[1L]]
    expected_object <- paste0("spatial:", dataset, ":object")
    expected_bundle <- paste0("spatial:", dataset, ":review")
    if (!identical(object$artifact_id, expected_object) || !identical(bundle$artifact_id, expected_bundle) ||
        !identical(object$review_bundle_artifact_id, bundle$artifact_id) ||
        !identical(bundle$source_artifact_id, object$artifact_id))
      wsi_abort("Spatial object/review bundle artifact linkage is inconsistent.")
    if (!identical(object$format, "rds") || !identical(bundle$format, "json"))
      wsi_abort("Spatial object/review bundle formats must be rds/json.")
    for (entry in list(object, bundle)) {
      if (!identical(entry$observation_unit, "spot") || !identical(entry$frame_id, tissue$image$frame_id) ||
          !identical(entry$image_sha256, tolower(tissue$image$sha256)))
        wsi_abort("Spatial artifact observation unit or H&E image SHA-256/frame differs.")
      wsi_experiment_number(entry$row_count, "spatial row_count", integer = TRUE)
    }
    if (!identical(as.numeric(object$row_count), as.numeric(bundle$row_count)) ||
        !identical(bundle$source_object_sha256, tolower(object$sha256 %||% "")))
      wsi_abort("Spatial object/review bundle row count or source hash differs.")
    list(object = object, artifact = bundle)
  })
}

wsi_experiment_spatial_bundle <- function(experiment, tissue, pair, max_observations = 10000L) {
  object <- wsi_experiment_file(pair$object, experiment$root, "spatial object", TRUE)
  artifact <- wsi_experiment_file(pair$artifact, experiment$root, "spatial review bundle", TRUE)
  limit <- getOption("wsiTools.review_max_checkpoint_bytes", 16 * 1024^2)
  if (file.info(artifact$resolved_path)$size > limit)
    wsi_abort("Spatial review bundle exceeds the bounded JSON size limit.")
  bundle <- tryCatch(jsonlite::fromJSON(artifact$resolved_path, simplifyVector = FALSE),
    error = function(e) wsi_abort(paste("Cannot read spatial review bundle:", conditionMessage(e))))
  wsi_experiment_spatial_no_paths(bundle)
  allowed <- c("schema", "schema_version", "experiment_id", "tissue_id", "dataset_id", "observation_unit",
    "frame_id", "image_sha256", "source_artifact_id", "source_object_sha256", "coordinate_mapping", "observations", "reductions")
  if (!is.list(bundle) || is.null(names(bundle)) || anyDuplicated(names(bundle)) ||
      any(!names(bundle) %in% allowed) || any(!allowed %in% names(bundle)) ||
      !identical(bundle$schema, "cellphenotyper-spatial-review-bundle") ||
      !is.numeric(bundle$schema_version) || length(bundle$schema_version) != 1L || bundle$schema_version != 1)
    wsi_abort("Unsupported or malformed spatial review bundle schema/version.")
  expected <- list(experiment_id = experiment$experiment_id, tissue_id = tissue$tissue_id,
    dataset_id = artifact$dataset_id, observation_unit = "spot", frame_id = tissue$image$frame_id,
    image_sha256 = tolower(tissue$image$sha256), source_artifact_id = object$artifact_id,
    source_object_sha256 = tolower(object$sha256))
  if (any(!vapply(names(expected), function(name) identical(bundle[[name]], expected[[name]]), logical(1))))
    wsi_abort("Spatial review bundle identity, source hash, or H&E image frame differs from its artifact pair.")
  image <- tissue$image
  image$tissue_id <- tissue$tissue_id
  image$path <- image$resolved_path
  mapping_keys <- c("confirmed", "same_section", "image_sha256", "frame_id", "coordinate_space", "affine")
  if (!is.list(bundle$coordinate_mapping) || is.null(names(bundle$coordinate_mapping)) ||
      anyDuplicated(names(bundle$coordinate_mapping)) ||
      !setequal(names(bundle$coordinate_mapping), mapping_keys))
    wsi_abort("Spatial coordinate_mapping must contain only the explicit confirmed image/frame/affine contract.")
  mapping <- wsi_review_spatial_mapping(image, bundle$coordinate_mapping)
  if (!identical(bundle$coordinate_mapping$same_section, TRUE))
    wsi_abort("Spatial review bundle must declare same_section=TRUE.")
  observations <- wsi_review_rows(bundle$observations, "spatial observations")
  if (!is.null(names(bundle$observations)) || length(observations) != artifact$row_count ||
      length(observations) > max_observations)
    wsi_abort("Spatial review bundle observation count differs or exceeds max_observations.")
  row_keys <- c("id", "original_id", "tissue_id", "frame_id", "dataset_id", "observation_unit", "x", "y",
    "cluster_values", "footprint", "footprint_width_px", "footprint_height_px")
  for (row in observations) {
    if (any(!names(row) %in% row_keys) || anyDuplicated(names(row)) ||
        !identical(row$tissue_id, tissue$tissue_id) || !identical(row$frame_id, artifact$frame_id) ||
        !identical(row$dataset_id, artifact$dataset_id) || !identical(row$observation_unit, "spot") ||
        !identical(row$id, wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, artifact$dataset_id, row$original_id)))
      wsi_abort("Spatial review observation identity/unit/frame or canonical ID differs.")
  }
  reductions <- wsi_review_rows(bundle$reductions, "spatial reductions")
  if (!is.null(names(bundle$reductions))) wsi_abort("Spatial reductions must be a JSON array.")
  known <- vapply(observations, `[[`, character(1), "id")
  reduction_keys <- c("id", "label", "observation_unit", "dataset_id", "frame_id", "points")
  for (reduction in reductions) {
    prefix <- paste0(wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, artifact$dataset_id, ""))
    if (any(!names(reduction) %in% reduction_keys) || anyDuplicated(names(reduction)) ||
        !is.character(reduction$id) || length(reduction$id) != 1L || !startsWith(reduction$id, prefix) ||
        nchar(reduction$id) <= nchar(prefix) || !identical(reduction$dataset_id, artifact$dataset_id) ||
        !identical(reduction$frame_id, artifact$frame_id) || !identical(reduction$observation_unit, "spot"))
      wsi_abort("Spatial reduction identity/unit/frame differs from its declared dataset.")
    points <- wsi_review_rows(reduction$points, "spatial reduction points")
    if (!is.null(names(reduction$points)) || any(vapply(points, function(point)
        any(!names(point) %in% c("id", "x", "y")) || anyDuplicated(names(point)), logical(1))))
      wsi_abort("Spatial reduction points must contain only canonical id/x/y records.")
    ids <- wsi_review_ids(vapply(points, function(point) wsi_review_text(point$id, "spatial reduction point.id"), character(1)),
      "spatial reduction IDs", known)
    if (length(ids) != length(known) || !setequal(ids, known))
      wsi_abort("Spatial reduction IDs must match all observations exactly once.")
  }
  checked <- wsi_review_context(list(image), observations, reductions,
    context_id = paste0("experiment:", experiment$experiment_id), experiment_id = experiment$experiment_id)
  list(observations = checked$observations, reductions = checked$reductions,
    attachment = list(dataset_id = artifact$dataset_id, tissue_id = tissue$tissue_id,
      source_reference = list(type = "indexed-spatial-rds", sha256 = tolower(object$sha256), artifact_id = object$artifact_id),
      review_bundle_sha256 = tolower(artifact$sha256), mapping = mapping$metadata,
      observation_unit = "spot", observation_count = length(observations)),
    object = object, artifact = artifact, bundle = bundle)
}

# Stream a source into a staging file, then verify both source and copy. Never
# replace existing content-addressed files, including a corrupt existing file.
wsi_experiment_spatial_copy <- function(source, destination, expected) {
  if (file.exists(destination)) {
    if (dir.exists(destination) || nzchar(Sys.readlink(destination)) || !identical(wsi_experiment_sha256(destination), expected))
      wsi_abort("Existing content-addressed spatial asset has different bytes; it was not overwritten.")
    if (!identical(wsi_experiment_sha256(source), expected))
      wsi_abort("Spatial source changed while copying; the experiment index was not updated.")
    return(invisible(destination))
  }
  stage <- tempfile(".spatial-asset-", tmpdir = dirname(destination))
  on.exit(unlink(stage), add = TRUE)
  input <- file(source, "rb"); output <- file(stage, "wb")
  tryCatch({
    repeat {
      bytes <- readBin(input, "raw", n = 1024L * 1024L)
      if (!length(bytes)) break
      writeBin(bytes, output)
    }
  }, finally = { close(input); close(output) })
  if (!identical(wsi_experiment_sha256(stage), expected) || !identical(wsi_experiment_sha256(source), expected))
    wsi_abort("Spatial source changed while copying; the experiment index was not updated.")
  # Hard-link publication is atomic and refuses an existing destination, unlike
  # rename on systems where rename replaces an existing file.
  if (file.exists(destination) || !file.link(stage, destination))
    wsi_abort("Could not publish immutable spatial asset; the experiment index was not updated.")
  invisible(destination)
}

wsi_experiment_spatial_replace <- function(source, destination) {
  isTRUE(.Call("wsi_atomic_replace_file", source, destination, PACKAGE = "wsiTools"))
}

#' Add a spatial transcriptomics object to a CellPhenotyper experiment
#'
#' Explicitly imports a trusted local RDS, copies its original bytes and a bounded
#' metadata/reduction bundle into the experiment directory, and atomically updates
#' the existing index. A byte-identical old index is retained beside it. Subsequent
#' opening verifies RDS hashes without deserializing the object. Human checkpoints
#' must be explicitly migrated to the returned fresh context by their caller.
#'
#' @param experiment A CellPhenotyper experiment or local manifest path.
#' @param object Accessible trusted local RDS path (loaded objects are not accepted).
#' @param tissue_id Exact existing tissue identity.
#' @param dataset_id New dataset identity, unique within the tissue.
#' @param coordinate_mapping Explicit confirmed same-section image hash/frame and
#'   native-pixel mapping or source-pixel affine; see wsi_review_attach_spatial().
#' @param cluster_columns Selected categorical metadata columns.
#' @param reduction_names Selected named spatial reductions; NULL includes none.
#' @param image_name Explicit source image name for multi-image Seurat objects.
#' @param trusted Must be TRUE to deserialize the selected local RDS once.
#' @param max_observations Total bounded review-context observation limit.
#' @return List containing experiment, context, backup_path, object_artifact and
#'   bundle_artifact. The caller's old experiment/context objects are untouched.
#' @export
wsi_experiment_add_spatial <- function(experiment, object, tissue_id, dataset_id = "spatial",
  coordinate_mapping, cluster_columns = NULL, reduction_names = NULL, image_name = NULL,
  trusted = FALSE, max_observations = 10000L) {
  if (!is.character(object) || length(object) != 1L || is.na(object) ||
      grepl("^[[:alpha:]][[:alnum:]+.-]*://", object) || !file.exists(object) || dir.exists(object))
    wsi_abort("object must be an accessible local RDS path; loaded objects are not accepted.")
  if (!isTRUE(trusted)) wsi_abort("Importing an RDS requires trusted=TRUE; select only a trusted local file.")
  object <- normalizePath(object, winslash = "/", mustWork = TRUE)
  input <- experiment
  experiment <- wsi_read_experiment(if (inherits(input, "wsi_experiment")) input$manifest_path else input)
  if (inherits(input, "wsi_experiment") && !identical(input$manifest_sha256, experiment$manifest_sha256))
    wsi_abort("Experiment manifest changed since it was opened; reopen it before adding spatial data.")
  max_observations <- wsi_experiment_number(max_observations, "max_observations", integer = TRUE)
  tissue_id <- wsi_review_text(tissue_id, "tissue_id"); dataset_id <- wsi_review_text(dataset_id, "dataset_id")
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.:-]*$", dataset_id))
    wsi_abort("dataset_id must begin with a letter/digit and contain only letters, digits, underscore, dot, colon or hyphen.")
  index <- match(tissue_id, vapply(experiment$tissues, `[[`, character(1), "tissue_id"))
  if (is.na(index)) wsi_abort("Select an existing tissue_id for the spatial object.")
  for (artifact in experiment$tissues[[index]]$artifacts %||% list())
    if (identical(artifact$dataset_id, dataset_id)) wsi_abort("This tissue already has the declared dataset_id; choose a new stable ID.")
  path <- experiment$manifest_path
  lock <- file.path(experiment$root, paste0(".", basename(path), ".spatial-lock"))
  if (!dir.create(lock, showWarnings = FALSE))
    wsi_abort("Another spatial import holds the experiment lock; no files were changed. Do not remove an active lock.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  unchanged <- function() {
    if (!identical(wsi_experiment_sha256(path), experiment$manifest_sha256))
      wsi_abort("Experiment manifest changed during import; the index was not replaced.")
  }
  unchanged()
  previous <- wsi_experiment_review_context(experiment, max_observations = max_observations)
  attached <- wsi_review_attach_spatial(previous, object, tissue_id = tissue_id, dataset_id = dataset_id,
    coordinate_mapping = coordinate_mapping, cluster_columns = cluster_columns, reduction_names = reduction_names,
    image_name = image_name, trusted = TRUE, max_observations = max_observations)
  source <- attached$provenance$spatial_attachments[[length(attached$provenance$spatial_attachments)]]
  object_sha <- source$source_reference$sha256
  object_id <- paste0("spatial:", dataset_id, ":object"); bundle_id <- paste0("spatial:", dataset_id, ":review")
  selected <- function(row) identical(row$tissue_id, tissue_id) && identical(row$dataset_id, dataset_id)
  observations <- Filter(selected, attached$observations)
  observations <- lapply(observations, function(row) {
    if (!length(row$cluster_values)) row$cluster_values <- structure(list(), names = character())
    row
  })
  reduction_prefix <- wsi_experiment_namespace_id(experiment$experiment_id, tissue_id, dataset_id, "")
  reductions <- Filter(function(row) identical(row$dataset_id, dataset_id) &&
    startsWith(row$id, reduction_prefix), attached$reductions)
  bundle <- list(schema = "cellphenotyper-spatial-review-bundle", schema_version = 1L,
    experiment_id = experiment$experiment_id, tissue_id = tissue_id, dataset_id = dataset_id,
    observation_unit = "spot", frame_id = experiment$tissues[[index]]$image$frame_id,
    image_sha256 = tolower(experiment$tissues[[index]]$image$sha256), source_artifact_id = object_id,
    source_object_sha256 = object_sha, coordinate_mapping = source$mapping,
    observations = observations, reductions = reductions)
  wsi_experiment_spatial_no_paths(bundle)
  object_relative <- paste0("spatial/objects/", object_sha, ".rds")
  for (directory in c("spatial/objects", "spatial/review")) {
    target <- file.path(experiment$root, directory)
    if (!dir.exists(target) && !dir.create(target, recursive = TRUE, showWarnings = FALSE))
      wsi_abort("Cannot create spatial artifact directory; the index was not updated.")
    if (!startsWith(normalizePath(target, winslash = "/", mustWork = TRUE), paste0(experiment$root, "/")))
      wsi_abort("Spatial artifact directories must remain inside the experiment directory.")
  }
  temporary_bundle <- tempfile(".spatial-bundle-", tmpdir = file.path(experiment$root, "spatial/review"))
  on.exit(unlink(temporary_bundle), add = TRUE)
  jsonlite::write_json(bundle, temporary_bundle, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = 17)
  if (file.info(temporary_bundle)$size > getOption("wsiTools.review_max_checkpoint_bytes", 16 * 1024^2))
    wsi_abort("Spatial review bundle exceeds the bounded JSON size limit; the index was not updated.")
  bundle_sha <- wsi_experiment_sha256(temporary_bundle)
  bundle_relative <- paste0("spatial/review/", bundle_sha, ".json")
  common <- list(dataset_id = dataset_id, observation_unit = "spot", frame_id = bundle$frame_id,
    image_sha256 = bundle$image_sha256, row_count = length(observations))
  object_artifact <- c(list(artifact_id = object_id, role = "spatial_transcriptomics_object", format = "rds",
    path = object_relative, sha256 = object_sha, review_bundle_artifact_id = bundle_id), common)
  bundle_artifact <- c(list(artifact_id = bundle_id, role = "spatial_review_bundle", format = "json",
    path = bundle_relative, sha256 = bundle_sha, source_artifact_id = object_id, source_object_sha256 = object_sha), common)
  wsi_experiment_spatial_copy(object, file.path(experiment$root, object_relative), object_sha)
  wsi_experiment_spatial_copy(temporary_bundle, file.path(experiment$root, bundle_relative), bundle_sha)
  manifest <- experiment$manifest
  manifest$tissues[[index]]$artifacts <- c(manifest$tissues[[index]]$artifacts %||% list(), list(object_artifact, bundle_artifact))
  stage <- tempfile(".spatial-index-", tmpdir = experiment$root, fileext = ".json")
  on.exit(unlink(stage), add = TRUE)
  jsonlite::write_json(manifest, stage, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = 17)
  # Validate the complete candidate before the only mutable publication step.
  candidate <- wsi_read_experiment(stage)
  candidate$manifest_path <- path
  future_context <- wsi_experiment_review_context(candidate, max_observations = max_observations)
  unchanged()
  backup <- file.path(experiment$root, paste0(".", basename(path), ".before-spatial-", experiment$manifest_sha256, ".json"))
  wsi_experiment_spatial_copy(path, backup, experiment$manifest_sha256)
  unchanged()
  if (!wsi_experiment_spatial_replace(stage, path))
    wsi_abort("Could not atomically update experiment index; its previous bytes were preserved.")
  list(experiment = candidate, context = future_context,
    backup_path = backup, object_artifact = object_artifact, bundle_artifact = bundle_artifact)
}
