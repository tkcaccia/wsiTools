# CellPhenotyper experiment manifests are references to typed cached artifacts.
# They never trigger encoder extraction or reinterpret tile observations as cells.

wsi_experiment_text <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
    wsi_abort(sprintf("`%s` must be one non-empty string.", name))
  x
}

wsi_experiment_number <- function(x, name, positive = TRUE, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      (positive && x <= 0) || (integer && x != floor(x)))
    wsi_abort(sprintf("`%s` must be one %sfinite number.", name,
                      if (positive) "positive " else ""))
  as.numeric(x)
}

wsi_experiment_sha256 <- function(path) {
  if (!requireNamespace("openssl", quietly = TRUE))
    wsi_abort("Experiment hash verification requires the optional `openssl` package.")
  con <- file(path, "rb")
  on.exit(close(con))
  unname(unclass(as.character(openssl::sha256(con))))
}

wsi_experiment_file <- function(entry, root, name, verify_hashes) {
  path <- wsi_experiment_text(entry$path, paste0(name, ".path"))
  if (grepl("^[[:alpha:]][[:alnum:]+.-]*://", path))
    wsi_abort(sprintf("%s must reference a local file.", name))
  path <- wsi_project_abs(path, root)
  if (!file.exists(path) || dir.exists(path))
    wsi_abort(sprintf("%s file does not exist: %s", name, path))
  expected <- tolower(wsi_experiment_text(entry$sha256, paste0(name, ".sha256")))
  if (!grepl("^[0-9a-f]{64}$", expected))
    wsi_abort(sprintf("%s.sha256 must contain 64 hexadecimal characters.", name))
  if (isTRUE(verify_hashes) && !identical(wsi_experiment_sha256(path), expected))
    wsi_abort(sprintf("SHA-256 mismatch for %s: %s", name, path))
  entry$resolved_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  entry
}

#' Read a typed CellPhenotyper experiment
#' @param path Experiment JSON, its directory, or a saved experiment wsi_project.
#' @param verify_hashes Verify referenced image and feature SHA-256 digests.
#' @return A wsi_experiment; no image pixels or feature matrices are loaded.
#' @export
wsi_read_experiment <- function(path, verify_hashes = TRUE) {
  if (inherits(path, "wsi_experiment")) return(path)
  reference <- NULL
  if (inherits(path, "wsi_project")) {
    reference <- path$metadata$cellphenotyper_experiment
    if (is.null(reference)) wsi_abort("Project has no CellPhenotyper experiment reference.")
    path <- reference$manifest_path
  }
  path <- wsi_experiment_text(path, "path")
  if (dir.exists(path)) path <- file.path(path, "experiment.cellphenotyper.json")
  if (!file.exists(path)) wsi_abort(sprintf("Experiment manifest does not exist: %s", path))
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  root <- dirname(path)
  manifest <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(manifest$schema, "cellphenotyper-experiment") ||
      !is.numeric(manifest$schema_version) || length(manifest$schema_version) != 1L ||
      manifest$schema_version != 1L)
    wsi_abort("Expected cellphenotyper-experiment schema_version 1.")
  wsi_experiment_text(manifest$experiment_id, "experiment_id")
  wsi_experiment_text(manifest$name, "name")
  tissues <- manifest$tissues
  if (!is.list(tissues) || !length(tissues) || !is.null(names(tissues)))
    wsi_abort("`tissues` must be a non-empty JSON array.")
  ids <- character()
  for (i in seq_along(tissues)) {
    tissue <- tissues[[i]]
    id <- wsi_experiment_text(tissue$tissue_id, "tissue_id")
    if (id %in% ids) wsi_abort(sprintf("Duplicate tissue_id: %s", id))
    ids <- c(ids, id)
    wsi_experiment_text(tissue$name, paste0(id, ".name"))
    image <- wsi_experiment_file(tissue$image, root, paste0(id, ".image"), verify_hashes)
    for (field in c("width", "height", "mpp_x", "mpp_y"))
      wsi_experiment_number(image[[field]], paste0(id, ".image.", field),
                            integer = field %in% c("width", "height"))
    wsi_experiment_text(image$frame_id, paste0(id, ".image.frame_id"))
    if (!identical(image$coordinate_convention, "pixel-edge-origin_centres-in-native-pixels"))
      wsi_abort(sprintf("%s image must declare native-pixel centre coordinates.", id))
    if (!is.null(image$display)) {
      display <- image$display
      display$backend <- display$backend %||% "auto"
      if (!identical(display$format, "native-pixel-pyramid") ||
          !identical(display$source_sha256, image$sha256) ||
          !identical(display$source_frame_id, image$frame_id) ||
          !is.character(display$backend) || length(display$backend) != 1L ||
          !display$backend %in% c("vips", "openslide", "auto"))
        wsi_abort("Display pyramid must preserve the analyzed image identity and declare its reader.")
      image$display <- wsi_experiment_file(display, root,
        paste0(id, ".display pyramid"), verify_hashes)
    }
    tissue$image <- image
    if (!is.null(tissue$registered_tissue))
      tissue$registered_tissue <- wsi_experiment_registered_tissue_validate(
        tissue$registered_tissue, root, image, verify_hashes)
    sets <- tissue$feature_sets %||% list()
    if (!is.list(sets) || (length(sets) && !is.null(names(sets))))
      wsi_abort(sprintf("%s.feature_sets must be an array.", id))
    set_ids <- character()
    for (j in seq_along(sets)) {
      f <- sets[[j]]
      fid <- wsi_experiment_text(f$feature_set_id, "feature_set_id")
      wsi_experiment_text(f$kind, "kind")
      wsi_experiment_text(f$format, "format")
      if (fid %in% set_ids) wsi_abort(sprintf("Duplicate feature_set_id in %s: %s", id, fid))
      set_ids <- c(set_ids, fid)
      f <- wsi_experiment_validate_feature(f)
      if (f$kind %in% c("cellvit-cell", "cell-morphology") &&
          !is.null(f$frame_id) && !identical(f$frame_id, image$frame_id))
        wsi_abort("Cell feature frame_id differs from its tissue image.")
      sets[[j]] <- if (!is.null(tissue$cell_classification$index) &&
          f$kind %in% c("cellvit-cell", "cell-morphology"))
        wsi_cell_index_checked_file(f, root, paste0(id, ".", fid)) else
        wsi_experiment_file(f, root, paste0(id, ".", fid), verify_hashes)
    }
    tissue$feature_sets <- sets
    # Artifacts are optional display derivatives. Resolve only their declared paths.
    for (j in seq_along(tissue$artifacts %||% list())) {
      a <- tissue$artifacts[[j]]
      if (!is.null(a$path)) a$resolved_path <- wsi_project_abs(a$path, root)
      tissue$artifacts[[j]] <- a
    }
    tissues[[i]] <- tissue
  }
  digest <- wsi_experiment_sha256(path)
  if (!is.null(reference$manifest_sha256) && !identical(reference$manifest_sha256, digest))
    wsi_abort("Saved experiment manifest identity changed; reopen the revised experiment explicitly.")
  result <- structure(list(manifest_path = path, root = root, manifest_sha256 = digest,
                 experiment_id = manifest$experiment_id, name = manifest$name,
                 settings = manifest$settings %||% list(), provenance = manifest$provenance %||% list(),
                 tissues = tissues, manifest = manifest), class = "wsi_experiment")
  # Passive RDS artifacts are hash-checked; only bounded plain JSON is parsed.
  for (tissue in tissues) for (pair in wsi_experiment_spatial_pairs(tissue))
    wsi_experiment_spatial_bundle(result, tissue, pair,
      max_observations = getOption("wsiTools.review_max_observations", 10000L))
  wsi_experiment_gigatime_validate(result)
  wsi_experiment_cell_catalog(result)
  result
}

wsi_experiment_namespace_id <- function(experiment_id, tissue_id, feature_set_id, id) {
  encode <- function(x) vapply(as.character(x), utils::URLencode, character(1), reserved = TRUE)
  paste(encode(experiment_id), encode(tissue_id), encode(feature_set_id), encode(id), sep = "::")
}

wsi_experiment_feature_signature <- function(f) {
  list(kind = f$kind, observation_unit = f$observation_unit,
       feature_columns = unname(f$feature_columns), encoder = f$encoder,
       preprocessing = f$preprocessing, descriptor = f$descriptor)
}

#' Retrieve typed cached Virchow2 feature rows
#' @param experiment Experiment object or manifest path.
#' @param feature_set_id Explicit feature-set identifier shared by chosen tissues.
#' @param tissue_ids Optional tissue identifiers; defaults to all tissues.
#' @return List with x (ordered matrix), observations, signature and grids.
#' @export
wsi_experiment_features <- function(experiment, feature_set_id, tissue_ids = NULL) {
  experiment <- wsi_read_experiment(experiment)
  feature_set_id <- wsi_experiment_text(feature_set_id, "feature_set_id")
  tissues <- experiment$tissues
  ids <- vapply(tissues, `[[`, character(1), "tissue_id")
  if (!is.null(tissue_ids)) {
    if (anyDuplicated(tissue_ids) || any(!tissue_ids %in% ids)) wsi_abort("Unknown or duplicate tissue_ids.")
    tissues <- tissues[match(tissue_ids, ids)]
  }
  if (any(vapply(tissues, function(t) any(vapply(t$feature_sets, function(f)
      identical(f$feature_set_id, feature_set_id) && identical(f$format, "partitioned-sqlite"), logical(1))), logical(1))))
    wsi_abort("Partitioned CellViT features require bounded indexed-cell selection; select explicit reviewed IDs.")
  cap <- getOption("wsiTools.experiment_max_rows", 10000L)
  total <- sum(vapply(tissues, function(t) {
    found <- Filter(function(f) identical(f$feature_set_id, feature_set_id), t$feature_sets)
    if (length(found) == 1L) found[[1L]]$row_count else 0
  }, numeric(1)))
  if (!is.numeric(cap) || length(cap) != 1L || !is.finite(cap) || cap < 1 || total > cap)
    wsi_abort("Requested feature rows exceed wsiTools.experiment_max_rows; select fewer tissues or explicitly raise the bounded limit.")
  bytes <- sum(vapply(tissues, function(t) {
    found <- Filter(function(f) identical(f$feature_set_id, feature_set_id), t$feature_sets)
    if (length(found) == 1L) found[[1L]]$row_count * length(found[[1L]]$feature_columns) * 8 else 0
  }, numeric(1)))
  byte_cap <- getOption("wsiTools.experiment_max_matrix_bytes", 256 * 1024^2)
  if (!is.numeric(byte_cap) || length(byte_cap) != 1L || !is.finite(byte_cap) || bytes > byte_cap)
    wsi_abort("Requested matrix exceeds wsiTools.experiment_max_matrix_bytes; use a bounded tissue subset.")
  matrices <- observations <- grids <- list()
  signature <- NULL
  for (t in tissues) {
    matches <- which(vapply(t$feature_sets, function(f) identical(f$feature_set_id, feature_set_id), logical(1)))
    if (length(matches) != 1L) wsi_abort(sprintf("Tissue %s has no feature set %s.", t$tissue_id, feature_set_id))
    f <- t$feature_sets[[matches]]
    s <- wsi_experiment_feature_signature(f)
    if (!is.null(signature) && !identical(signature, s)) wsi_abort("Feature schemas/encoder/preprocessing differ across tissues.")
    signature <- s
    # Hash again at use time: a cache modified after opening must not be consumed.
    if (!identical(wsi_experiment_sha256(f$resolved_path), tolower(f$sha256)))
      wsi_abort("Feature SHA-256 changed since the experiment was opened.")
    con <- if (identical(f$format, "csv.gz")) gzfile(f$resolved_path, "rt") else file(f$resolved_path, "rt")
    table <- tryCatch(utils::read.csv(con, colClasses = "character", check.names = FALSE, nrows = f$row_count + 1L,
                                     stringsAsFactors = FALSE), finally = close(con))
    if (nrow(table) != f$row_count || anyDuplicated(names(table))) wsi_abort("Feature row_count/header mismatch.")
    required <- c(f$id_column, f$x_column, f$y_column, f$feature_columns)
    if (any(!required %in% names(table)) ||
        !identical(names(table)[names(table) %in% f$feature_columns], f$feature_columns))
      wsi_abort("Feature columns are missing or appear in a different order.")
    local_ids <- table[[f$id_column]]
    if (anyNA(local_ids) || any(!nzchar(local_ids)) || anyDuplicated(local_ids)) wsi_abort("Observation IDs must be non-missing and unique within a tissue feature set.")
    numeric_column <- function(value, name, missing = FALSE) {
      out <- suppressWarnings(as.numeric(value))
      blank <- is.na(value) | value %in% c("", "NA", "NaN")
      if (any(is.na(out) & !blank) || any(is.infinite(out)) || (!missing && anyNA(out)))
        wsi_abort(sprintf("Invalid numeric values in %s.", name))
      out
    }
    x <- numeric_column(table[[f$x_column]], f$x_column)
    y <- numeric_column(table[[f$y_column]], f$y_column)
    if (any(x < 0 | x >= t$image$width | y < 0 | y >= t$image$height))
      wsi_abort("Feature centres fall outside the declared image dimensions.")
    gx <- (x - f$grid$origin_x_px - f$grid$tile_width_px / 2) / f$grid$spacing_x_px
    gy <- (y - f$grid$origin_y_px - f$grid$tile_height_px / 2) / f$grid$spacing_y_px
    if (any(abs(gx - round(gx)) > 1e-5 | abs(gy - round(gy)) > 1e-5) || any(gx < -1e-5 | gy < -1e-5))
      wsi_abort("Feature centres disagree with the declared grid (origin is its top-left edge).")
    matrix <- vapply(f$feature_columns, function(column) numeric_column(table[[column]], column, TRUE), numeric(nrow(table)))
    matrix <- matrix(matrix, nrow = nrow(table), dimnames = list(NULL, f$feature_columns))
    global <- wsi_experiment_namespace_id(experiment$experiment_id, t$tissue_id, feature_set_id, local_ids)
    rownames(matrix) <- global
    obs <- data.frame(id = global, original_id = local_ids, label = local_ids,
                      x = x, y = y, tissue_id = t$tissue_id,
                      specimen_id = t$specimen_id %||% NA_character_, unit = f$observation_unit,
                      feature_set_id = feature_set_id, project_image = t$name, image_id = t$tissue_id,
                      sample_id = t$tissue_id, project_image_index = match(t$tissue_id, ids) - 1L,
                      project_section_index = -1L, footprint_width_px = f$grid$tile_width_px,
                      footprint_height_px = f$grid$tile_height_px, stringsAsFactors = FALSE)
    matrices[[length(matrices) + 1L]] <- matrix
    observations[[length(observations) + 1L]] <- obs
    grids[[t$tissue_id]] <- f$grid
  }
  if (!length(matrices)) wsi_abort("Select at least one tissue.")
  structure(list(x = do.call(rbind, matrices), observations = do.call(rbind, observations),
                 signature = signature, grids = grids, experiment_id = experiment$experiment_id,
                 feature_set_id = feature_set_id), class = "wsi_experiment_features")
}

wsi_experiment_catalog <- function(experiment) {
  entries <- list()
  for (t in experiment$tissues) for (f in t$feature_sets) {
    id <- paste0("experiment:", f$feature_set_id)
    if (id %in% vapply(entries, `[[`, character(1), "id")) next
    entries[[length(entries) + 1L]] <- list(id = id, feature_set_id = f$feature_set_id,
      label = paste("Virchow2", f$observation_unit, "features:", f$feature_set_id),
      type = f$kind, unit = f$observation_unit, feature_count = length(f$feature_columns),
      row_count = sum(vapply(experiment$tissues, function(tissue) {
        selected <- Filter(function(s) identical(s$feature_set_id, f$feature_set_id), tissue$feature_sets)
        if (length(selected)) selected[[1L]]$row_count else 0
      }, numeric(1))))
  }
  entries
}

#' Create an experiment prediction context without loading feature rows
#' @rdname wsi_read_experiment
#' @param experiment Experiment object or manifest path.
#' @param tissue_ids Optional tissue scope.
#' @export
wsi_experiment_prediction_context <- function(experiment, tissue_ids = NULL) {
  experiment <- wsi_read_experiment(experiment)
  list(experiment = experiment, tissue_ids = tissue_ids,
       experiment_sources = wsi_experiment_catalog(experiment),
       experiment_models = new.env(parent = emptyenv()))
}

wsi_experiment_images <- function(experiment) {
  lapply(seq_along(experiment$tissues), function(i) {
    t <- experiment$tissues[[i]]
    rois <- wsi_experiment_annotation_rois(experiment, t, i - 1L)
    bora_review <- wsi_experiment_bora_review_layer(experiment, t, i - 1L)
    bora_local <- wsi_experiment_bora_local_review_layer(experiment, t, i - 1L)
    image <- list(id = t$tissue_id, label = t$name,
      path = t$image$display$resolved_path %||% t$image$resolved_path,
      backend = t$image$display$backend %||% "auto",
      width = t$image$width, height = t$image$height,
      mpp = list(x = t$image$mpp_x, y = t$image$mpp_y), tissue_id = t$tissue_id,
      frame_id = t$image$frame_id, rois = wsi_viewer_roi_features(rois),
      artifact_references = t$artifacts)
    # Bora vectors are tissue annotations in the viewer by default. An explicit
    # legacy setting retains the historical separate review layer for old
    # projects that intentionally depend on that presentation.
    if (!identical(experiment$settings$bora_annotations_mode,
                   "legacy_review_layer")) {
      predictions <- Filter(Negate(is.null), list(bora_review, bora_local))
      for (layer in predictions) {
        for (item in layer$items %||% list()) {
          item$name <- paste("Bora tissue annotation ·", item$class %||% "predicted region")
          item$label <- item$name
          item$locked <- FALSE
          item$isLocked <- FALSE
          item$review_only <- FALSE
          item$tissue_annotation <- TRUE
          item$source_type <- "tissue_annotation"
          item$source <- "Bora tissue annotation"
          item$properties <- wsi_geojson_list(item$properties)
          item$properties$source_type <- "tissue_annotation"
          item$properties$name <- item$name
          item$properties$tissue_annotation <- TRUE
          item$properties$review_only <- FALSE
          item$properties$isLocked <- FALSE
          item$properties$locked <- FALSE
          item$properties$bora_prediction_sha256 <- layer$metadata$source_sha256
          item$properties$bora_prediction_source <- layer$source_type
          image$rois[[length(image$rois) + 1L]] <- item
        }
      }
      image$layers <- list()
    } else {
      image$layers <- Filter(Negate(is.null), list(bora_review, bora_local))
    }
    image
  })
}

# Earlier CellPhenotyper exports assigned anonymous hard-group Bora vectors the
# general tissue_annotations role. Recognize only a source-bound legacy artifact;
# an incomplete hard-group claim must never enter editable/training ROIs.
wsi_experiment_legacy_bora_review_artifact <- function(artifact, image) {
  if (!identical(artifact$role, "tissue_annotations")) return(NULL)
  provenance <- artifact$provenance %||% list()
  # A continuous anonymous candidate must declare its review role explicitly.
  # Otherwise the generic annotation path would make it editable and eligible
  # for tissue training before biological identity has been established.
  continuous_id <- is.character(artifact$artifact_id) &&
    length(artifact$artifact_id) == 1L && !is.na(artifact$artifact_id) &&
    startsWith(artifact$artifact_id, "bora-continuous-")
  semantic_id <- is.character(artifact$artifact_id) &&
    length(artifact$artifact_id) == 1L && !is.na(artifact$artifact_id) &&
    startsWith(artifact$artifact_id, "bora-semantic-")
  refined_semantic_id <- is.character(artifact$artifact_id) &&
    length(artifact$artifact_id) == 1L && !is.na(artifact$artifact_id) &&
    startsWith(artifact$artifact_id, "bora-refined-semantic-")
  if (semantic_id || refined_semantic_id ||
      identical(provenance$producer, "bora-wsi-semantic-chain-v1") ||
      identical(provenance$producer, "bora-wsi-native-he-refined-semantic-chain-v1") ||
      identical(provenance$prediction_unit,
                "semantic_virchow2_spatial_token_native_probability") ||
      identical(provenance$prediction_unit,
                "semantic_virchow2_spatial_token_native_he_refined_probability"))
    wsi_abort("Exploratory Bora semantic boundaries cannot be tissue_annotations; import them as review only.")
  if (continuous_id ||
      identical(provenance$producer, "bora-wsi-continuous-chain-v1") ||
      identical(provenance$prediction_unit, "continuous_rgb9_rgb19_native_probability"))
    wsi_abort("Continuous anonymous Bora boundaries cannot be tissue_annotations; import them as review only.")
  scalar <- function(value) if (is.character(value) && length(value) == 1L && !is.na(value))
    value else ""
  marker <- startsWith(scalar(artifact$artifact_id), "bora-global-exact-") ||
    identical(provenance$producer, "bora-wsi-global-v1") ||
    identical(provenance$algorithm_id, "bora-global-hard-group-v1") ||
    identical(provenance$prediction_unit, "global_anonymous_Bora-AF_group")
  if (!marker) return(NULL)
  hash <- function(value) is.character(value) && length(value) == 1L &&
    grepl("^[0-9a-f]{64}$", value)
  if (!identical(provenance$producer, "bora-wsi-global-v1") ||
      !identical(provenance$algorithm_id, "bora-global-hard-group-v1") ||
      !identical(provenance$prediction_unit, "global_anonymous_Bora-AF_group") ||
      !isTRUE(provenance$identity_status %in% c("exploratory_transfer", "externally_validated")) ||
      !hash(provenance$chain_complete_sha256) || !hash(provenance$manifest_sha256) ||
      !hash(artifact$sha256) || !identical(artifact$source_frame_id, image$frame_id) ||
      !identical(artifact$image_sha256, image$sha256))
    wsi_abort("Legacy anonymous Bora tissue_annotations lack complete source binding; refusing ROI import.")
  artifact$role <- "bora_exploratory_boundary_review"
  artifact$review_only <- TRUE
  artifact
}

# Anonymous global Bora boundaries are useful to inspect, but are not tissue
# annotations or PLS-LDA training regions. Only their geometry enters a locked
# project layer; semantic labels in the source GeoJSON are discarded.
wsi_experiment_recovered_bora_receipts <- function(provenance, artifact, tissue,
                                                    root) {
  corner_stage <- identical(provenance$producer,
    "bora-recovered-kodama-he-diagnostic-review-v1")
  names <- c("mask", "diagnostic", "vector", "smoothing",
             if (corner_stage) "corner")
  receipts <- lapply(names, function(name) {
    entry <- provenance[[paste0(name, "_receipt")]]
    if (!is.list(entry)) wsi_abort(paste("Recovered Bora", name, "receipt is missing."))
    entry <- wsi_experiment_file(entry, root, paste("Recovered Bora", name, "receipt"), TRUE)
    if (file.info(entry$resolved_path)$size > 8 * 1024^2)
      wsi_abort(paste("Recovered Bora", name, "receipt exceeds 8 MiB."))
    jsonlite::fromJSON(entry$resolved_path, simplifyVector = FALSE)
  })
  names(receipts) <- names
  m <- receipts$mask
  d <- receipts$diagnostic
  v <- receipts$vector
  s <- receipts$smoothing
  corner <- receipts$corner
  receipt_sha <- function(name) provenance[[paste0(name, "_receipt")]]$sha256
  source_hashes <- function(receipt) unlist(receipt$source_sha256, use.names = FALSE)
  same_pair <- function(actual, expected) {
    actual <- as.numeric(unlist(actual, use.names = FALSE))
    length(actual) == 2L && isTRUE(all.equal(actual, expected, tolerance = 1e-8,
                                              check.attributes = FALSE))
  }
  if (!identical(m$format, "pm016-recovered-bora-full-slide-diagnostic-v1") ||
      !identical(m$algorithm, paste("Frozen r2 complete token field -> unchanged",
                                    "COLON_BOUNDARY_SPEC native H&E refinement ->",
                                    "optional global vectorizer")) ||
      !identical(m$accepted_annotation, FALSE) ||
      !identical(m$technical_seam_gate_pass, FALSE) ||
      !identical(m$source_files_sha256$native_he, tissue$image$sha256) ||
      !identical(m$r2_token_field_sha256, provenance$r2_token_field_sha256) ||
      !identical(m$r2_model_receipt_sha256, provenance$r2_model_receipt_sha256) ||
      !identical(m$native_refinement$mask_sha256,
                 provenance$source_semantic_mask_sha256) ||
      !identical(d$format, "pm016-recovered-bora-full-slide-diagnostic-v1") ||
      !identical(d$status, "DIAGNOSTIC_ONLY_NOT_ACCEPTED") ||
      !identical(d$accepted_annotation, FALSE) ||
      !identical(d$technical_seam_gate_pass, FALSE) ||
      !identical(d$biological_accuracy_qualified, FALSE) ||
      !identical(d$mask_receipt_sha256, receipt_sha("mask")) ||
      !identical(d$vector$receipt_sha256, receipt_sha("vector")) ||
      !identical(v$format, "bora-wsi-global-vector-v1") ||
      !identical(v$source_image_sha256, tissue$image$sha256) ||
      !identical(v$source_frame_id, tissue$image$frame_id) ||
      !same_pair(v$native_shape_yx, c(tissue$image$height, tissue$image$width)) ||
      !same_pair(v$native_mpp_xy, c(tissue$image$mpp_x, tissue$image$mpp_y)) ||
      !identical(v$source_semantic_mask_sha256,
                 provenance$source_semantic_mask_sha256) ||
      !identical(v$global_model_binding_sha256,
                 provenance$global_model_binding_sha256) ||
      !identical(v$output_sha256[["exact/mask.geojson"]],
                 provenance$source_exact_geojson_sha256) ||
      !is.numeric(v$minimum_area_um2) ||
      length(v$minimum_area_um2) != 1L ||
      v$minimum_area_um2 != 100 ||
      !identical(v$minimum_area_strictly_less_removed, TRUE) ||
      !identical(v$exact_vector_matches_cleaned_mask, TRUE) ||
      !identical(v$global_contours, TRUE) ||
      !identical(v$no_macroregion_cuts, TRUE) ||
      !identical(v$independently_fitted_vector_windows, FALSE) ||
      !identical(s$format, "bora-wsi-review-smoothing-v1") ||
      !identical(s$status, "REVIEW_ONLY_NOT_ACCEPTED") ||
      !identical(s$technical_r4_seam_failure_remains, TRUE) ||
      !identical(s$exact_cleaned_mask_and_geojson_unchanged, TRUE) ||
      !identical(s$new_tissue_classifier_used, FALSE) ||
      !identical(s$biological_accuracy_qualified, FALSE) ||
      !identical(s$selected$status, "PASSED_DIAGNOSTIC_REVIEW") ||
      !identical(s$review_geojson_sha256,
                 provenance$r6_review_geojson_sha256) ||
      !receipt_sha("vector") %in% source_hashes(s) ||
      !provenance$source_exact_geojson_sha256 %in% source_hashes(s) ||
      !tissue$image$sha256 %in% source_hashes(s))
    wsi_abort("Recovered Bora review receipts do not form a source-bound diagnostic chain.")
  if (corner_stage &&
      (!identical(corner$format, "bora-wsi-corner-rounding-v1") ||
      !identical(corner$status, "REVIEW_ONLY_NOT_ACCEPTED") ||
      !identical(corner$technical_r4_seam_failure_remains, TRUE) ||
      !identical(corner$long_straight_model_seam_cannot_be_removed_by_local_rounding,
                 TRUE) ||
      !identical(corner$exact_r4_and_r6_unchanged, TRUE) ||
      !identical(corner$biological_accuracy_qualified, FALSE) ||
      !identical(corner$review_geojson_sha256, artifact$sha256) ||
      !identical(corner$review_geojson_sha256, provenance$review_geojson_sha256) ||
      !provenance$r6_review_geojson_sha256 %in% source_hashes(corner) ||
      !receipt_sha("vector") %in% source_hashes(corner) ||
      !provenance$source_exact_geojson_sha256 %in% source_hashes(corner) ||
      !tissue$image$sha256 %in% source_hashes(corner) ||
      !any(vapply(corner$attempts %||% list(), function(a)
        identical(a$status, "PASSED_REVIEW_ONLY"), logical(1)))))
    wsi_abort("Recovered Bora review receipts do not form a source-bound diagnostic chain.")
  if (!corner_stage &&
      (!identical(s$review_geojson_sha256, artifact$sha256) ||
       !identical(provenance$r6_review_geojson_sha256, artifact$sha256) ||
       !is.null(provenance$corner_receipt)))
    wsi_abort("Recovered Bora r6 fallback must use its own signed review geometry.")
  receipts
}

wsi_experiment_bora_review_layer <- function(experiment, tissue, image_index) {
  matches <- Filter(Negate(is.null), lapply(tissue$artifacts %||% list(), function(a) {
    if (identical(a$role, "bora_exploratory_boundary_review")) return(a)
    wsi_experiment_legacy_bora_review_artifact(a, tissue$image)
  }))
  if (!length(matches)) return(NULL)
  if (length(matches) != 1L) wsi_abort("Bora candidate review requires exactly one artifact per tissue.")
  artifact <- matches[[1L]]
  provenance <- artifact$provenance %||% list()
  if (isTRUE(provenance$producer %in% c(
      "bora-kodama-three-group-he-diagnostic-review-v1",
      "bora-kodama-three-group-topology-guarded-review-v3")))
    return(wsi_experiment_bora_three_group_review_layer(
      experiment, tissue, image_index, artifact))
  hash <- function(value) is.character(value) && length(value) == 1L &&
    grepl("^[0-9a-f]{64}$", value)
  continuous <- identical(provenance$producer, "bora-wsi-continuous-chain-v1") ||
    identical(provenance$prediction_unit, "continuous_rgb9_rgb19_native_probability")
  refined_semantic <- identical(provenance$producer,
                                "bora-wsi-native-he-refined-semantic-chain-v1") ||
    identical(provenance$prediction_unit,
              "semantic_virchow2_spatial_token_native_he_refined_probability") ||
    (is.character(artifact$artifact_id) && length(artifact$artifact_id) == 1L &&
     !is.na(artifact$artifact_id) &&
     startsWith(artifact$artifact_id, "bora-refined-semantic-"))
  semantic <- refined_semantic ||
    identical(provenance$producer, "bora-wsi-semantic-chain-v1") ||
    identical(provenance$prediction_unit,
              "semantic_virchow2_spatial_token_native_probability") ||
    (is.character(artifact$artifact_id) && length(artifact$artifact_id) == 1L &&
     !is.na(artifact$artifact_id) && startsWith(artifact$artifact_id, "bora-semantic-"))
  manuscript_transfer <- identical(provenance$producer, "bora-wsi-global-v1") &&
    identical(provenance$prediction_unit,
              "global_bora_neighbor_context_classification") &&
    identical(provenance$algorithm_id,
              "bora-global-af-neighbor-context-v4-review")
  manuscript_transfer_v5 <- identical(provenance$producer,
    "bora-exploratory-transfer-unknown-aware-global-vector-v1") &&
    identical(provenance$prediction_unit,
              "Virchow2 spatial token; native H&E boundary refinement")
  recovered_corner <- identical(provenance$producer,
    "bora-recovered-kodama-he-diagnostic-review-v1")
  recovered_kodama <- is.character(provenance$producer) &&
    length(provenance$producer) == 1L && provenance$producer %in% c(
    "bora-recovered-kodama-he-diagnostic-review-v1",
    "bora-recovered-kodama-he-smoothed-diagnostic-review-v1") &&
    identical(provenance$prediction_unit,
              "KODAMA_two_group_token_field_native_HE_refinement")
  named_prediction <- semantic || manuscript_transfer || manuscript_transfer_v5
  recovered_receipts <- NULL
  valid_provenance <- if (recovered_kodama) {
    identical(provenance$identity_status, "exploratory_transfer") &&
      identical(provenance$biological_accuracy_qualified, FALSE) &&
      identical(provenance$accepted_annotation, FALSE) &&
      identical(provenance$technical_seam_gate_pass, FALSE) &&
      identical(artifact$training_eligible, FALSE) &&
      identical(artifact$predicted_not_expert_annotation, TRUE) &&
      is.numeric(provenance$minimum_area_um2) &&
      length(provenance$minimum_area_um2) == 1L &&
      provenance$minimum_area_um2 == 100 &&
      identical(provenance$review_geojson_sha256, artifact$sha256) &&
      all(vapply(c("r2_token_field_sha256", "r2_model_receipt_sha256",
                   "source_semantic_mask_sha256", "global_model_binding_sha256",
                   "source_exact_geojson_sha256", "r6_review_geojson_sha256"),
                 function(field) hash(provenance[[field]]), logical(1))) &&
      is.list(recovered_receipts <- wsi_experiment_recovered_bora_receipts(
        provenance, artifact, tissue, experiment$root))
  } else if (manuscript_transfer) {
    identical(provenance$identity_status, "exploratory_transfer") &&
      identical(artifact$training_eligible, FALSE) &&
      identical(artifact$predicted_not_expert_annotation, TRUE) &&
      identical(provenance$biological_accuracy_qualified, FALSE) &&
      identical(provenance$review_geojson_sha256, artifact$sha256) &&
      identical(as.numeric(unlist(provenance$native_shape_yx, use.names = FALSE)),
                c(as.numeric(tissue$image$height), as.numeric(tissue$image$width))) &&
      isTRUE(all.equal(as.numeric(unlist(provenance$native_mpp_xy,
                                        use.names = FALSE)),
                       c(tissue$image$mpp_x, tissue$image$mpp_y),
                       tolerance = 1e-8, check.attributes = FALSE)) &&
      is.numeric(provenance$minimum_area_um2) &&
      length(provenance$minimum_area_um2) == 1L &&
      provenance$minimum_area_um2 == 100 &&
      identical(provenance$exact_vector_matches_cleaned_mask, TRUE) &&
      all(vapply(c("chain_complete_sha256", "source_vector_complete_sha256",
                   "review_complete_sha256", "source_exact_mask_sha256",
                   "source_exact_geojson_sha256", "semantic_mask_sha256",
                   "global_model_binding_sha256"),
                 function(field) hash(provenance[[field]]), logical(1)))
  } else if (manuscript_transfer_v5) {
    identical(provenance$identity_status,
              "exploratory_manuscript_transfer_unvalidated") &&
      identical(provenance$biological_accuracy_qualified, FALSE) &&
      identical(provenance$independent_pm016_validation, FALSE) &&
      identical(provenance$unknown_semantics,
                "abstention_not_background_or_tissue_class") &&
      identical(provenance$global_contours, TRUE) &&
      identical(provenance$no_stair_step_fallback, TRUE) &&
      is.numeric(provenance$minimum_area_um2) &&
      length(provenance$minimum_area_um2) == 1L &&
      provenance$minimum_area_um2 == 100 &&
      is.numeric(provenance$smoothing_tolerance_um) &&
      length(provenance$smoothing_tolerance_um) == 1L &&
      is.finite(provenance$smoothing_tolerance_um) &&
      provenance$smoothing_tolerance_um > 0 &&
      all(vapply(c("model_sha256", "policy_sha256",
                   "materialization_receipt_sha256", "registration_sha256",
                   "registered_support_source_sha256", "tissue_support_sha256",
                   "native_label_sha256", "native_unknown_sha256",
                   "global_vector_receipt_sha256"),
                 function(field) hash(provenance[[field]]), logical(1)))
  } else if (semantic) {
    identical(provenance$producer, if (refined_semantic)
      "bora-wsi-native-he-refined-semantic-chain-v1" else
      "bora-wsi-semantic-chain-v1") &&
      identical(provenance$prediction_unit,
                if (refined_semantic)
                  "semantic_virchow2_spatial_token_native_he_refined_probability" else
                  "semantic_virchow2_spatial_token_native_probability") &&
      identical(provenance$identity_status, "exploratory_same_slide_holdout") &&
      identical(provenance$evidence_class,
                "expert_trained_same_slide_holdout_not_independent_patient_validated") &&
      identical(artifact$training_eligible, FALSE) &&
      identical(artifact$predicted_not_expert_annotation, TRUE) &&
      identical(provenance$predicted_not_expert_annotation, TRUE) &&
      identical(provenance$native_he_boundary_refinement_applied,
                refined_semantic) &&
      identical(provenance$biological_accuracy_qualified, FALSE) &&
      (!refined_semantic ||
       (hash(provenance$native_he_source_sha256) &&
        identical(provenance$native_he_source_sha256, tissue$image$sha256) &&
        hash(provenance$token_materialization_receipt_sha256))) &&
      all(vapply(c("chain_complete_sha256", "roi_spec_sha256",
                   "expert_annotation_sha256", "same_slide_holdout_receipt_sha256",
                   "model_sha256", "token_receipt_sha256", "registration_sha256",
                   "registered_support_source_sha256", "tissue_support_sha256",
                   "semantic_mask_sha256", "exact_mask_sha256",
                   "exact_geojson_sha256", "review_geojson_sha256",
                   "global_model_binding_sha256"),
                 function(field) hash(provenance[[field]]), logical(1))) &&
      is.numeric(provenance$minimum_area_um2) &&
      length(provenance$minimum_area_um2) == 1L &&
      is.finite(provenance$minimum_area_um2) &&
      provenance$minimum_area_um2 == 100 &&
      identical(provenance$exact_vector_matches_cleaned_mask, TRUE) &&
      identical(provenance$review_geojson_sha256, artifact$sha256)
  } else if (continuous) {
    identical(provenance$producer, "bora-wsi-continuous-chain-v1") &&
      identical(provenance$prediction_unit, "continuous_rgb9_rgb19_native_probability") &&
      identical(provenance$identity_status, "anonymous_exploratory_review") &&
      hash(provenance$chain_complete_sha256) &&
      hash(provenance$semantic_mask_sha256) &&
      hash(provenance$exact_mask_sha256) &&
      hash(provenance$exact_geojson_sha256) &&
      hash(provenance$review_geojson_sha256) &&
      hash(provenance$global_model_binding_sha256) &&
      hash(provenance$registration_sha256) &&
      hash(provenance$registered_support_source_sha256) &&
      hash(provenance$tissue_support_sha256) &&
      identical(provenance$biological_accuracy_qualified, FALSE) &&
      identical(provenance$exact_vector_matches_cleaned_mask, TRUE) &&
      is.numeric(provenance$minimum_area_um2) &&
      length(provenance$minimum_area_um2) == 1L &&
      is.finite(provenance$minimum_area_um2) &&
      provenance$minimum_area_um2 == 100 &&
      identical(provenance$review_geojson_sha256, artifact$sha256)
  } else {
    identical(provenance$prediction_unit, "global_anonymous_Bora-AF_group") &&
      isTRUE(provenance$identity_status %in% c("exploratory_transfer", "externally_validated")) &&
      hash(provenance$chain_complete_sha256) && hash(provenance$manifest_sha256)
  }
  if (!identical(artifact$format, "geojson") || !identical(artifact$review_only, TRUE) ||
      !valid_provenance ||
      !identical(artifact$source_frame_id, tissue$image$frame_id) ||
      !identical(artifact$image_sha256, tissue$image$sha256))
    wsi_abort("Bora candidate review format, status, frame or image identity disagrees with its tissue.")
  artifact <- wsi_experiment_file(artifact, experiment$root, "Bora candidate review", TRUE)
  size <- file.info(artifact$resolved_path)$size
  cap <- getOption("wsiTools.experiment_max_bora_review_bytes", 32 * 1024^2)
  if (!is.numeric(cap) || length(cap) != 1L || !is.finite(cap) || cap < 1)
    wsi_abort("wsiTools.experiment_max_bora_review_bytes must be a positive finite size.")
  id <- paste0("experiment_bora_candidate_", tissue$tissue_id)
  base <- list(id = id, name = if (recovered_kodama)
      paste("Bora tissue annotations · KODAMA groups ·", if (recovered_corner) "corner-rounded" else "smoothed",
            "diagnostic prediction · seam gate failed") else if (manuscript_transfer_v5)
      "Bora tissue annotations · manuscript model transfer prediction" else if (manuscript_transfer)
      "Bora tissue annotations · manuscript model transfer prediction" else if (refined_semantic)
      "Bora tissue annotations · H&E-refined exploratory same-slide holdout prediction" else if (semantic)
      "Bora tissue annotations · exploratory same-slide holdout prediction" else if (continuous)
      "Bora tissue annotations · exploratory continuous anonymous prediction" else
      "Bora tissue annotations · anonymous prediction",
    type = "vector", source_type = "bora_candidate_review", colour = "#FBBF24",
    visible = TRUE, opacity = 1, outline = TRUE,
    project_image_index = image_index,
    image_id = tissue$tissue_id, project_managed = TRUE,
    metadata = list(review_only = TRUE, training_eligible = FALSE,
      source_sha256 = artifact$sha256, source_frame_id = artifact$source_frame_id,
      image_sha256 = artifact$image_sha256, identity_status = provenance$identity_status,
      producer = provenance$producer, evidence_class = provenance$evidence_class,
      model_sha256 = provenance$model_sha256,
      global_vector_receipt_sha256 = provenance$global_vector_receipt_sha256,
      native_label_sha256 = provenance$native_label_sha256,
      native_unknown_sha256 = provenance$native_unknown_sha256,
      native_he_boundary_refinement_applied =
        provenance$native_he_boundary_refinement_applied,
      native_he_source_sha256 = provenance$native_he_source_sha256,
      biological_accuracy_qualified = provenance$biological_accuracy_qualified,
      accepted_annotation = provenance$accepted_annotation,
      technical_seam_gate_pass = provenance$technical_seam_gate_pass,
      diagnostic_only = if (recovered_kodama) TRUE else NULL,
      mask_receipt_sha256 = provenance$mask_receipt$sha256,
      diagnostic_receipt_sha256 = provenance$diagnostic_receipt$sha256,
      vector_receipt_sha256 = provenance$vector_receipt$sha256,
      smoothing_receipt_sha256 = provenance$smoothing_receipt$sha256,
      corner_receipt_sha256 = provenance$corner_receipt$sha256,
      same_slide_holdout_receipt_sha256 = provenance$same_slide_holdout_receipt_sha256,
      chain_complete_sha256 = provenance$chain_complete_sha256,
      manifest_sha256 = provenance$manifest_sha256,
      prediction_unit = provenance$prediction_unit,
      semantic_mask_sha256 = provenance$semantic_mask_sha256,
      exact_mask_sha256 = provenance$exact_mask_sha256,
      exact_geojson_sha256 = provenance$exact_geojson_sha256))
  unavailable <- function(reason) {
    base$name <- paste0("Bora candidate boundaries unavailable · ", reason)
    base$visible <- FALSE
    base$items <- list()
    base$count <- 0L
    base$metadata$unavailable_reason <- reason
    base
  }
  if (is.na(size) || size > cap)
    return(unavailable("review GeoJSON exceeds the bounded viewer size; export an indexed derivative"))
  geo <- jsonlite::fromJSON(artifact$resolved_path, simplifyVector = FALSE)
  if (!identical(geo$type, "FeatureCollection") || !is.list(geo$features))
    wsi_abort("Bora candidate review requires a FeatureCollection.")
  # The artifact label alone cannot bind a native-pixel vector to an image:
  # older exporters could stamp the working (0.5 um/px) frame on a vector
  # produced in the native (often 0.25 um/px) frame. Check the signed file's
  # own coordinate metadata before any polygon can enter the viewer.
  numeric_pair <- function(value) {
    if (!is.list(value) || length(value) != 2L) return(NULL)
    parts <- unlist(value, use.names = FALSE)
    if (!is.numeric(parts) || length(parts) != 2L || any(!is.finite(parts))) return(NULL)
    as.numeric(parts)
  }
  shape <- numeric_pair(geo$native_shape_yx)
  mpp <- numeric_pair(geo$native_mpp_xy)
  expected_shape <- c(as.numeric(tissue$image$height), as.numeric(tissue$image$width))
  expected_mpp <- c(as.numeric(tissue$image$mpp_x), as.numeric(tissue$image$mpp_y))
  if (!identical(geo$coordinate_space, "native_image_pixel_edges") ||
      !identical(geo$source_frame_id, artifact$source_frame_id) ||
      !identical(geo$source_frame_id, tissue$image$frame_id) ||
      !identical(geo$source_image_sha256, artifact$image_sha256) ||
      !identical(geo$source_image_sha256, tissue$image$sha256) ||
      is.null(shape) || !identical(shape, expected_shape) ||
      is.null(mpp) || any(mpp <= 0) ||
      !isTRUE(all.equal(mpp, expected_mpp, tolerance = 1e-8,
                        check.attributes = FALSE)))
    wsi_abort("Bora candidate review GeoJSON native frame, image, dimensions or mpp disagree with its tissue and artifact.")
  if (manuscript_transfer_v5 &&
      (!identical(geo$format, provenance$producer) ||
       !identical(geo$identity_status, provenance$identity_status) ||
       !identical(geo$biological_accuracy_qualified, FALSE) ||
       !identical(geo$unknown_semantics, provenance$unknown_semantics) ||
       !identical(geo$source_label_sha256, provenance$native_label_sha256) ||
       !identical(geo$source_unknown_sha256, provenance$native_unknown_sha256) ||
       !identical(geo$registered_support_source_sha256,
                  provenance$registered_support_source_sha256) ||
       !identical(geo$vector_export$geometry_role,
                  "smoothed_review_unknown_preserved") ||
       !identical(geo$vector_export$global_contours, TRUE) ||
       !identical(geo$vector_export$independently_fitted_windows, FALSE) ||
       !identical(geo$vector_export$unknown_holes_exempt_from_minimum, TRUE) ||
       !identical(geo$vector_export$unsupported_holes_exempt_from_minimum, TRUE) ||
       !is.numeric(geo$vector_export$minimum_part_and_unprotected_hole_area_um2) ||
       length(geo$vector_export$minimum_part_and_unprotected_hole_area_um2) != 1L ||
       geo$vector_export$minimum_part_and_unprotected_hole_area_um2 != 100 ||
       !isTRUE(all.equal(geo$vector_export$smoothing_tolerance_um,
                         provenance$smoothing_tolerance_um))))
    wsi_abort("Unknown-aware Bora transfer review does not match its native geometry and source labels.")
  if (recovered_kodama &&
      (!identical(geo$source_semantic_mask_sha256,
                  provenance$source_semantic_mask_sha256) ||
       !identical(geo$global_model_binding_sha256,
                  provenance$global_model_binding_sha256) ||
       !identical(geo$predicted_not_expert_annotation, TRUE) ||
       !identical(geo$tissue_identity_status, "exploratory_transfer") ||
       !identical(geo$vector_export$format, "bora-wsi-global-vector-v1") ||
       !identical(geo$vector_export$geometry_role,
                  if (recovered_corner) "corner_rounded_review_only" else
                    "smoothed_review_only") ||
       !identical(geo$vector_export$global_contours, TRUE) ||
       !identical(geo$vector_export$independently_fitted_windows, FALSE) ||
       !is.numeric(geo$vector_export$minimum_part_and_hole_area_um2) ||
       length(geo$vector_export$minimum_part_and_hole_area_um2) != 1L ||
       geo$vector_export$minimum_part_and_hole_area_um2 != 100))
    wsi_abort("Recovered Bora corner review disagrees with the source-bound diagnostic receipts.")
  if (continuous && (is.null(numeric_pair(provenance$native_shape_yx)) ||
      !identical(numeric_pair(provenance$native_shape_yx), expected_shape) ||
      is.null(numeric_pair(provenance$native_mpp_xy)) ||
      !isTRUE(all.equal(numeric_pair(provenance$native_mpp_xy), expected_mpp,
                        tolerance = 1e-8, check.attributes = FALSE)) ||
      (!is.null(tissue$registered_tissue) &&
       (!identical(provenance$registration_sha256,
                   tissue$registered_tissue$registration$sha256) ||
        !identical(provenance$registered_support_source_sha256,
                   tissue$registered_tissue$sidecar$full_support_mask$sha256)))))
    wsi_abort("Continuous Bora review native registration or support identity differs from its project.")
  if (semantic && (is.null(numeric_pair(provenance$native_shape_yx)) ||
      !identical(numeric_pair(provenance$native_shape_yx), expected_shape) ||
      is.null(numeric_pair(provenance$native_mpp_xy)) ||
      !isTRUE(all.equal(numeric_pair(provenance$native_mpp_xy), expected_mpp,
                        tolerance = 1e-8, check.attributes = FALSE)) ||
      (!is.null(tissue$registered_tissue) &&
       (!identical(provenance$registration_sha256,
                   tissue$registered_tissue$registration$sha256) ||
        !identical(provenance$registered_support_source_sha256,
                   tissue$registered_tissue$sidecar$full_support_mask$sha256)))))
    wsi_abort("Semantic Bora review native registration or support identity differs from its project.")
  if (continuous && (!identical(geo$source_semantic_mask_sha256,
                              provenance$semantic_mask_sha256) ||
      !identical(geo$global_model_binding_sha256,
                 provenance$global_model_binding_sha256) ||
      !identical(geo$predicted_not_expert_annotation, TRUE) ||
      !identical(geo$tissue_identity_status, "exploratory_transfer") ||
      !is.list(geo$vector_export) ||
      !identical(geo$vector_export$format, "bora-wsi-global-vector-v1") ||
      !identical(geo$vector_export$geometry_role, "smoothed_review") ||
      !identical(geo$vector_export$global_contours, TRUE) ||
      !identical(geo$vector_export$independently_fitted_windows, FALSE) ||
      !is.numeric(geo$vector_export$minimum_part_and_hole_area_um2) ||
      length(geo$vector_export$minimum_part_and_hole_area_um2) != 1L ||
      !is.finite(geo$vector_export$minimum_part_and_hole_area_um2) ||
      geo$vector_export$minimum_part_and_hole_area_um2 < 100))
    wsi_abort("Continuous Bora review provenance or global vector geometry disagrees with its completion receipt.")
  if (manuscript_transfer &&
      (!identical(geo$source_semantic_mask_sha256,
                  provenance$semantic_mask_sha256) ||
       !identical(geo$global_model_binding_sha256,
                  provenance$global_model_binding_sha256) ||
       !identical(geo$predicted_not_expert_annotation, TRUE) ||
       !identical(geo$tissue_identity_status, "exploratory_transfer") ||
       !identical(geo$vector_export$geometry_role, "smoothed_review") ||
       !identical(geo$vector_export$global_contours, TRUE) ||
       !identical(geo$vector_export$independently_fitted_windows, FALSE) ||
       !is.numeric(geo$vector_export$minimum_part_and_hole_area_um2) ||
       length(geo$vector_export$minimum_part_and_hole_area_um2) != 1L ||
       geo$vector_export$minimum_part_and_hole_area_um2 != 100))
    wsi_abort("Manuscript Bora transfer vector identity or geometry disagrees with its completion receipt.")
  if (semantic && (!identical(geo$source_semantic_mask_sha256,
                            provenance$semantic_mask_sha256) ||
      !identical(geo$global_model_binding_sha256,
                 provenance$global_model_binding_sha256) ||
      !identical(geo$predicted_not_expert_annotation, TRUE) ||
      !identical(geo$tissue_identity_status, "exploratory_transfer") ||
      !is.list(geo$vector_export) ||
      !identical(geo$vector_export$format, "bora-wsi-global-vector-v1") ||
      !identical(geo$vector_export$geometry_role, "smoothed_review") ||
      !identical(geo$vector_export$global_contours, TRUE) ||
      !identical(geo$vector_export$independently_fitted_windows, FALSE) ||
      !is.numeric(geo$vector_export$minimum_part_and_hole_area_um2) ||
      length(geo$vector_export$minimum_part_and_hole_area_um2) != 1L ||
      !is.finite(geo$vector_export$minimum_part_and_hole_area_um2) ||
      geo$vector_export$minimum_part_and_hole_area_um2 != 100))
    wsi_abort("Semantic Bora review provenance or global vector geometry disagrees with its completion receipt.")
  if (length(geo$features) > 5000L)
    return(unavailable("review GeoJSON exceeds 5,000 features; export an indexed derivative"))
  vertices <- 0L
  vertex_cap <- if (manuscript_transfer_v5) 150000L else 100000L
  for (i in seq_along(geo$features)) {
    feature <- geo$features[[i]]
    predicted_tissue <- NULL
    if (recovered_kodama) {
      properties <- feature$properties %||% list()
      group <- as.numeric(properties$value %||% NA_real_)
      expected_name <- if (length(group) == 1L && group %in% c(1, 2))
        paste("KODAMA Group", group) else ""
      if (!nzchar(expected_name) ||
          !identical(properties$name, expected_name) ||
          !identical(properties$predicted_tissue, expected_name) ||
          !identical(properties$predicted_not_expert_annotation, TRUE) ||
          !identical(properties$tissue_identity_status, "exploratory_transfer") ||
          !identical(properties$boundary_role,
                     if (recovered_corner) "corner_rounded_review_only" else
                       "smoothed_review_only") ||
          !is.numeric(properties$area_um2) ||
          length(properties$area_um2) != 1L ||
          properties$area_um2 < 100)
        wsi_abort("Recovered Bora review feature is not a 100 µm² KODAMA diagnostic component.")
    }
    if (named_prediction) {
      properties <- feature$properties %||% list()
      predicted_tissue <- properties$predicted_tissue
      if (!is.character(predicted_tissue) || length(predicted_tissue) != 1L ||
          is.na(predicted_tissue) || !nzchar(trimws(predicted_tissue)) ||
          !identical(properties$predicted_not_expert_annotation, TRUE) ||
          !identical(properties$tissue_identity_status,
            if (manuscript_transfer_v5) "exploratory_manuscript_transfer_unvalidated"
            else "exploratory_transfer"))
        wsi_abort("Semantic Bora review feature lacks an exploratory predicted tissue identity.")
    }
    kind <- feature$geometry$type %||% ""
    if (!kind %in% c("Polygon", "MultiPolygon"))
      wsi_abort("Bora candidate review requires Polygon or MultiPolygon geometry.")
    polygons <- if (identical(kind, "Polygon")) list(feature$geometry$coordinates) else
      feature$geometry$coordinates
    if (!is.list(polygons) || !length(polygons))
      wsi_abort("Bora candidate review has empty polygon geometry.")
    for (polygon in polygons) {
      if (!is.list(polygon) || !length(polygon))
        wsi_abort("Bora candidate review has an empty polygon.")
      for (ring in polygon) {
      if (!is.list(ring) || length(ring) < 4L)
        wsi_abort("Bora candidate review has an invalid polygon ring.")
      points <- lapply(ring, function(p) unlist(p, use.names = FALSE))
      if (any(vapply(points, length, integer(1)) != 2L) ||
          any(!vapply(points, is.numeric, logical(1))))
        wsi_abort("Bora candidate review requires two-dimensional coordinates.")
      xy <- do.call(rbind, points)
      if (any(!is.finite(xy)) || any(xy[, 1L] < 0 | xy[, 1L] > tissue$image$width) ||
          any(xy[, 2L] < 0 | xy[, 2L] > tissue$image$height) ||
          !identical(unname(xy[1L, ]), unname(xy[nrow(xy), ])))
        wsi_abort("Bora candidate review coordinates disagree with the native image frame.")
      vertices <- vertices + nrow(xy)
      if (vertices > vertex_cap)
        return(unavailable(paste0("review geometry exceeds ",
                                  format(vertex_cap, big.mark = ","),
                                  " vertices; export an indexed derivative")))
      }
    }
    label <- if (recovered_kodama)
      paste(properties$name, "· Bora diagnostic · seam gate failed") else if (named_prediction)
      paste("Bora predicted", gsub("_", " ", predicted_tissue, fixed = TRUE),
            "· exploratory") else
      paste("Bora candidate boundary", i)
    geo$features[[i]] <- list(type = "Feature", id = paste0("candidate_", i),
      geometry = list(type = kind, coordinates = feature$geometry$coordinates),
      properties = list(name = label,
        classification = list(name = if (recovered_kodama) properties$name else if (named_prediction)
          paste("Predicted", gsub("_", " ", predicted_tissue, fixed = TRUE)) else
          "Candidate boundary"), isLocked = TRUE,
        source_type = "bora_candidate_review", tissue_annotation = TRUE,
        review_only = TRUE, predicted_tissue = predicted_tissue,
        diagnostic_only = recovered_kodama,
        technical_seam_gate_pass = if (recovered_kodama) FALSE else NULL))
  }
  rois <- wsi_roi_from_geojson(geo)
  items <- wsi_viewer_roi_features(rois, fill_alpha = 0.02)
  for (i in seq_along(items)) {
    colour <- if (recovered_kodama) switch(
      as.character(geo$features[[i]]$properties$classification$name),
      "KODAMA Group 1" = "#3B82F6", "KODAMA Group 2" = "#F43F5E",
      "#FBBF24") else if (manuscript_transfer || manuscript_transfer_v5) switch(
      geo$features[[i]]$properties$predicted_tissue,
      stroma = "#3B82F6", tumor = "#F43F5E",
      inflamed_stroma = "#F43F5E", "#FBBF24") else "#FBBF24"
    items[[i]]$colour <- colour
    items[[i]]$fill <- wsi_viewer_hex_to_rgba(colour, alpha = 0.02)
    items[[i]]$locked <- TRUE
    items[[i]]$tissue_annotation <- TRUE
    items[[i]]$source <- "Bora tissue annotation · prediction · click to edit"
    if (named_prediction) items[[i]]$predicted_tissue <-
      geo$features[[i]]$properties$predicted_tissue
    items[[i]]$project_image_index <- image_index
    items[[i]]$image_id <- tissue$tissue_id
    # The viewer paints the checked rings; retaining three more GeoJSON copies
    # would needlessly enlarge full-slide HTML and browser memory.
    items[[i]]$coordinates <- NULL
    items[[i]]$geometry <- NULL
    items[[i]]$feature <- NULL
    items[[i]]$measurements <- NULL
  }
  base$items <- items
  base$count <- length(items)
  base
}

wsi_experiment_annotation_rois <- function(experiment, tissue, image_index) {
  all <- list()
  for (a in tissue$artifacts %||% list()) {
    if (!identical(a$role, "tissue_annotations")) next
    if (!is.null(wsi_experiment_legacy_bora_review_artifact(a, tissue$image))) next
    a <- wsi_experiment_file(a, experiment$root, "tissue_annotations", TRUE)
    if (!is.null(a$frame_id) && !identical(a$frame_id, tissue$image$frame_id))
      wsi_abort("Annotation frame differs from image; a validated native-frame derivative is required.")
    if (file.info(a$resolved_path)$size > getOption("wsiTools.experiment_max_annotation_bytes", 64 * 1024^2))
      wsi_abort("Annotation file exceeds wsiTools.experiment_max_annotation_bytes; use a bounded annotation derivative.")
    rois <- read_geojson(a$resolved_path)
    for (j in seq_len(nrow(rois))) {
      rois$roi_id[[j]] <- wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id,
        "annotation", rois$roi_id[[j]])
      properties <- wsi_geojson_list(rois$properties[[j]])
      properties$tissue_id <- tissue$tissue_id
      properties$image_id <- tissue$tissue_id
      properties$project_image <- tissue$name
      properties$project_image_index <- image_index
      properties$project_section_index <- -1L
      properties$source_role <- a$role
      properties$source_sha256 <- a$sha256
      if (!is.null(a$description)) properties$source_description <- a$description
      rois$properties[[j]] <- properties
    }
    all[[length(all) + 1L]] <- rois
  }
  if (!length(all)) return(wsi_empty_roi())
  result <- do.call(rbind, all)
  class(result) <- class(all[[1L]])
  if (anyDuplicated(result$roi_id)) wsi_abort("Imported annotation IDs collide within a tissue.")
  result
}

#' Construct a project retaining the experiment source identity
#' @rdname wsi_read_experiment
#' @param output Optional project directory to save.
#' @param overwrite Whether an existing output may be overwritten.
#' @export
wsi_experiment_project <- function(experiment, output = NULL, overwrite = FALSE) {
  experiment <- wsi_read_experiment(experiment)
  ref <- list(manifest_path = experiment$manifest_path, experiment_id = experiment$experiment_id,
              manifest_sha256 = experiment$manifest_sha256)
  project <- wsi_project(path = output, slide = experiment$tissues[[1L]]$image$resolved_path,
    metadata = list(cellphenotyper_experiment = ref, experiment_images = wsi_experiment_images(experiment)),
    processing_provenance = list(experiment = ref), overwrite = overwrite)
  project$experiment <- experiment
  project
}

#' Open a live multi-image experiment viewer
#' @rdname wsi_read_experiment
#' @param open Open the browser when TRUE.
#' @param wait Keep the live event loop running when TRUE.
#' @param ... Arguments for wsi_viewer_live().
#' @export
wsi_viewer_experiment <- function(experiment, open = interactive(), wait = FALSE, ...) {
  previous <- if (inherits(experiment, "wsi_experiment")) experiment else NULL
  experiment <- wsi_read_experiment(if (is.null(previous)) experiment else previous$manifest_path)
  if (!is.null(previous) && !identical(previous$manifest_sha256, experiment$manifest_sha256))
    wsi_abort("The experiment index changed since it was opened. Reopen the updated index before viewing it.")
  image <- experiment$tissues[[1L]]$image
  slide <- wsi_open(image$display$resolved_path %||% image$resolved_path,
                    backend = image$display$backend %||% "auto")
  handed_off <- FALSE
  on.exit(if (!handed_off) wsi_close(slide), add = TRUE)
  if (!identical(as.numeric(slide$dimensions[c("width", "height")]),
                 as.numeric(c(image$width, image$height))))
    wsi_abort("Viewer image dimensions differ from the registered experiment frame.")
  display_artifacts <- wsi_experiment_viewer_artifacts(experiment)
  context <- wsi_experiment_prediction_context(experiment)
  args <- list(...)
  args$slide <- slide
  args$open <- open
  args$wait <- wait
  args$project_images <- wsi_experiment_images(experiment)
  gigatime <- wsi_experiment_gigatime_viewer(experiment)
  args$project_images <- lapply(args$project_images, function(image) {
    scoped <- Filter(function(layer) identical(layer$metadata$gigatime$tissue_id, image$tissue_id), gigatime$layers)
    if (length(scoped)) image$layers <- c(wsi_viewer_layers_config(image$layers), scoped)
    image
  })
  args$channel_sources <- wsi_channel_sources_combine(args$channel_sources, gigatime$channel_sources)
  args$prediction_context <- context
  args$review_context <- args$review_context %||%
    wsi_experiment_review_context(experiment, strict = FALSE)
  args$experiment <- list(manifest_path = experiment$manifest_path,
    experiment_id = experiment$experiment_id, manifest_sha256 = experiment$manifest_sha256)
  args$experiment$bora_annotations_mode <-
    experiment$settings$bora_annotations_mode %||% "ordinary_editable"
  args$experiment$cell_classification <- wsi_experiment_cell_catalog(experiment)
  args$experiment$registered_tissue <- wsi_experiment_registered_tissue_catalog(experiment)
  args$experiment$viewer_evidence <- display_artifacts[c("cell_coverage", "bora_interpretation", "bora_status")]
  if (isTRUE(display_artifacts$kodama$enabled) && is.null(args$cellphenotyper))
    args$cellphenotyper <- list(enabled = FALSE, kodama = display_artifacts$kodama)
  args$experiment_prediction <- wsi_prediction_config(experiment = experiment)
  args$title <- args$title %||% experiment$name
  args$session_inputs <- utils::modifyList(args$session_inputs %||% list(),
    list(cellphenotyper_experiment = list(manifest_path = experiment$manifest_path,
      experiment_id = experiment$experiment_id, manifest_sha256 = experiment$manifest_sha256)))
  project <- wsi_experiment_project(experiment)
  session <- do.call(wsi_viewer_live, args)
  session$experiment <- experiment
  session$experiment_project <- project
  handed_off <- TRUE
  session
}

wsi_experiment_training_labels <- function(labels) {
  if (is.data.frame(labels)) {
    if (!all(c("id", "label") %in% names(labels))) wsi_abort("Labels require id and label columns.")
    labels <- stats::setNames(as.character(labels$label), as.character(labels$id))
  }
  if (is.factor(labels)) labels <- stats::setNames(as.character(labels), names(labels))
  if (!is.character(labels) || is.null(names(labels)) || anyNA(names(labels)) ||
      any(!nzchar(names(labels))) || anyDuplicated(names(labels)))
    wsi_abort("Labels must be a uniquely named character vector or id/label table.")
  labels
}

wsi_experiment_preprocess_fit <- function(x, max_features = NULL, scaling = "autoscaling") {
  scaling <- match.arg(scaling, c("autoscaling", "centering", "none"))
  finite <- is.finite(x)
  eligible <- colSums(finite) >= 2L
  if (!any(eligible)) wsi_abort("Training rows have no features with two finite observations.")
  x <- x[, eligible, drop = FALSE]
  x[!is.finite(x)] <- NA_real_
  impute <- colMeans(x, na.rm = TRUE)
  for (i in seq_len(ncol(x))) x[is.na(x[, i]), i] <- impute[[i]]
  variance <- apply(x, 2L, stats::var)
  keep <- which(is.finite(variance) & variance > 0)
  if (!length(keep)) wsi_abort("Training features have zero variance.")
  if (!is.null(max_features)) {
    max_features <- wsi_experiment_number(max_features, "max_features", integer = TRUE)
    if (length(keep) > max_features) keep <- keep[order(-variance[keep], keep)[seq_len(max_features)]]
  }
  columns <- colnames(x)[keep]
  center <- if (scaling == "none") stats::setNames(rep(0, length(keep)), columns) else colMeans(x[, keep, drop = FALSE])
  scale <- if (scaling == "autoscaling") sqrt(variance[keep]) else stats::setNames(rep(1, length(keep)), columns)
  list(feature_columns = columns, impute = impute[keep], center = center, scale = scale,
       scaling = scaling, fitted_on = "training_rows_only")
}

wsi_experiment_preprocess_apply <- function(x, recipe) {
  if (any(!recipe$feature_columns %in% colnames(x))) wsi_abort("Model feature columns are missing.")
  x <- x[, recipe$feature_columns, drop = FALSE]
  for (i in seq_len(ncol(x))) {
    missing <- !is.finite(x[, i])
    x[missing, i] <- recipe$impute[[i]]
    x[, i] <- (x[, i] - recipe$center[[i]]) / recipe$scale[[i]]
  }
  x
}

#' Fit and reuse a CPU classifier on cached experiment observations
#' @param experiment Experiment object or path.
#' @param feature_set_id Typed feature set to use.
#' @param labels Named character labels or an id/label data frame.
#' @param train_ids Explicit training observation IDs.
#' @param train_tissue_ids Alternatively select training tissues explicitly.
#' @param ncomp PLS component count.
#' @param max_features Optional maximum features selected by training variance.
#' @param scaling Training-only scaling: autoscaling, centering or none.
#' @param seed Fixed solver seed.
#' @param groups Optional named training group identifiers; recorded, not a CV claim.
#' @return A portable CPU model, or a typed prediction data frame.
#' @export
wsi_experiment_train_classifier <- function(experiment, feature_set_id, labels, train_ids = NULL,
  train_tissue_ids = NULL, ncomp = 2L, max_features = NULL, scaling = "autoscaling", seed = 4L,
  groups = NULL) {
  features <- if (inherits(experiment, "wsi_experiment_features")) experiment else
    wsi_experiment_features(experiment, feature_set_id)
  labels <- wsi_experiment_training_labels(labels)
  if (is.null(train_ids)) {
    if (is.null(train_tissue_ids)) wsi_abort("Declare train_ids or train_tissue_ids explicitly.")
    if (any(!train_tissue_ids %in% features$observations$tissue_id)) wsi_abort("Unknown training tissue.")
    train_ids <- features$observations$id[features$observations$tissue_id %in% train_tissue_ids]
    train_ids <- train_ids[train_ids %in% names(labels)]
  }
  if (!length(train_ids) || anyNA(train_ids) || anyDuplicated(train_ids)) wsi_abort("Training IDs must be non-empty and unique.")
  rows <- match(train_ids, features$observations$id)
  if (anyNA(rows)) wsi_abort("Training IDs do not match feature observations.")
  y <- labels[match(train_ids, names(labels))]
  if (anyNA(y) || any(!nzchar(y))) wsi_abort("Every training ID requires a non-missing label.")
  y <- factor(y)
  if (nlevels(y) < 2L || any(table(y) < 2L)) wsi_abort("Each of at least two training classes requires two observations.")
  if (!is.null(groups)) {
    if (is.null(names(groups)) || anyNA(groups[train_ids])) wsi_abort("Groups must name every training ID.")
    groups <- groups[train_ids]
  }
  ncomp <- wsi_experiment_number(ncomp, "ncomp", integer = TRUE)
  seed <- wsi_experiment_number(seed, "seed", positive = FALSE, integer = TRUE)
  recipe <- wsi_experiment_preprocess_fit(features$x[rows, , drop = FALSE], max_features, scaling)
  x <- wsi_experiment_preprocess_apply(features$x[rows, , drop = FALSE], recipe)
  ncomp <- min(ncomp, nrow(x) - 1L, ncol(x))
  if (!wsi_has_fastpls()) wsi_abort("CPU classifier fitting requires the optional fastPLS package.")
  fit <- wsi_fastpls_cpu_call(get("pls", asNamespace(wsi_fastpls_package())), list(Xtrain = x, Ytrain = y,
    ncomp = ncomp, method = "simpls", classifier = "lda", backend = "cpu",
    scaling = "none", fit = FALSE, proj = FALSE, return_variance = FALSE, seed = seed))
  structure(list(schema = "wsitools-experiment-classifier", schema_version = 1L,
    experiment_id = features$experiment_id, feature_set_id = features$feature_set_id,
    signature = features$signature, preprocessing = recipe, model = fit,
    training_ids = as.character(train_ids), training_labels = as.character(y),
    training_tissues = unique(features$observations$tissue_id[rows]), groups = groups,
    classes = levels(y), ncomp = ncomp, seed = seed, backend = "cpu",
    fastpls_version = as.character(utils::packageVersion(wsi_fastpls_package())),
    validation = list(status = "not_run", note = "Training is not independent validation.")),
    class = "wsi_experiment_classifier")
}

#' @rdname wsi_experiment_train_classifier
#' @param model A fitted experiment classifier.
#' @param tissue_ids Optional prediction tissue scope.
#' @export
wsi_experiment_predict_classifier <- function(model, experiment, tissue_ids = NULL) {
  if (!inherits(model, "wsi_experiment_classifier") || !identical(model$backend, "cpu"))
    wsi_abort("Expected a portable CPU experiment classifier.")
  features <- if (inherits(experiment, "wsi_experiment_features")) experiment else
    wsi_experiment_features(experiment, model$feature_set_id, tissue_ids)
  if (!identical(features$signature, model$signature)) wsi_abort("Model and feature encoder/schema/preprocessing are incompatible.")
  x <- wsi_experiment_preprocess_apply(features$x, model$preprocessing)
  prediction <- wsi_fastpls_cpu_predict(model$model, x, raw_scores = TRUE)
  labels <- prediction$Ypred
  if (is.data.frame(labels) && ncol(labels) == 1L) labels <- labels[[1L]]
  if (is.matrix(labels) && ncol(labels) == 1L) labels <- labels[, 1L]
  if (!is.null(dim(labels)) || length(labels) != nrow(x)) wsi_abort("Classifier prediction length/shape mismatch; labels were not recycled.")
  labels <- as.character(labels)
  if (anyNA(labels) || any(!labels %in% model$classes)) wsi_abort("Classifier returned an unknown/missing class.")
  out <- features$observations
  out$predicted <- labels
  out$uncertainty_status <- "not_calibrated"
  attr(out, "raw_scores") <- prediction$lda_scores %||% prediction$LDA_scores %||% NULL
  attr(out, "training_ids") <- model$training_ids
  class(out) <- c("wsi_experiment_prediction", class(out))
  out
}

#' @rdname wsi_experiment_train_classifier
#' @param path Local RDS path. Read only trusted model files.
#' @param overwrite Replace an existing saved model when TRUE.
#' @export
wsi_write_experiment_classifier <- function(model, path, overwrite = FALSE) {
  if (!inherits(model, "wsi_experiment_classifier")) wsi_abort("Expected an experiment classifier.")
  path <- wsi_validate_output_path(path, overwrite = overwrite)
  temporary <- tempfile(".classifier-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(model, temporary, version = 3L)
  if (!file.rename(temporary, path)) wsi_abort("Could not atomically save classifier.")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' @rdname wsi_experiment_train_classifier
#' @export
wsi_read_experiment_classifier <- function(path) {
  model <- readRDS(path)
  if (!inherits(model, "wsi_experiment_classifier") ||
      !identical(model$schema, "wsitools-experiment-classifier") || model$schema_version != 1L ||
      !identical(model$backend, "cpu")) wsi_abort("Unsupported experiment classifier artifact.")
  model
}

wsi_experiment_derivative_dir <- function(experiment, kind) {
  root <- file.path(experiment$root, "derivatives", "wsitools", kind)
  if (!dir.exists(root) && !dir.create(root, recursive = TRUE, showWarnings = FALSE))
    wsi_abort("Experiment derivative directory is not writable.")
  path <- tempfile(paste0(format(Sys.time(), "%Y%m%dT%H%M%S"), "-"), tmpdir = root)
  if (!dir.create(path, showWarnings = FALSE)) wsi_abort("Cannot create experiment derivative run directory.")
  path
}

#' Create or replay a frozen Bora discriminative appearance recipe
#' @param experiment Experiment object or path.
#' @param feature_set_id Typed feature-set ID.
#' @param labels Named training labels; query rows remain unlabelled.
#' @param train_ids Explicit IDs allowed to supply training labels.
#' @param tissue_ids Tissue scope for application.
#' @param mode local uses only active_tissue_id; shared uses an explicit common recipe.
#' @param active_tissue_id Required tissue for local mode.
#' @param recipe_path Existing frozen JSON recipe to replay without labels/refitting.
#' @param output_dir Optional new derivative directory.
#' @param python Optional Python interpreter with Bora installed; no installation is attempted.
#' @return Bounded appearance points, class order, recipe and derivative paths.
#' @export
wsi_experiment_appearance <- function(experiment, feature_set_id, labels = NULL, train_ids = NULL,
  tissue_ids = NULL, mode = c("local", "shared"), active_tissue_id = NULL,
  recipe_path = NULL, output_dir = NULL, python = NULL) {
  experiment <- wsi_read_experiment(experiment)
  mode <- match.arg(mode)
  if (mode == "local") {
    active_tissue_id <- wsi_experiment_text(active_tissue_id, "active_tissue_id")
    if (!is.null(tissue_ids) && any(tissue_ids != active_tissue_id))
      wsi_abort("Local appearance cannot include another tissue.")
    tissue_ids <- active_tissue_id
  }
  f <- wsi_experiment_features(experiment, feature_set_id, tissue_ids)
  if (any(!is.finite(f$x))) wsi_abort("Appearance currently requires finite cached features; missing-value imputation is available only in the separate classifier recipe.")
  cap <- getOption("wsiTools.experiment_max_viewer_rows", 10000L)
  if (nrow(f$x) > cap) wsi_abort("Appearance exceeds wsiTools.experiment_max_viewer_rows; select fewer tissues.")
  rows <- f$observations[, c("id", "x", "y"), drop = FALSE]
  rows$label <- ""
  rows$split <- "query"
  if (is.null(recipe_path)) {
    labels <- wsi_experiment_training_labels(labels)
    if (is.null(train_ids) || !length(train_ids) || anyDuplicated(train_ids))
      wsi_abort("Fitting appearance requires explicit unique train_ids.")
    if (length(train_ids) < 16L)
      wsi_abort(sprintf("Appearance needs at least 16 fully labelled reference observations; only %d were selected. Choose a compatible spatial-token cache or add reference annotations; no encoder extraction was started.", length(train_ids)))
    index <- match(train_ids, rows$id)
    if (anyNA(index)) wsi_abort("Appearance training IDs are outside the selected tissue scope.")
    y <- labels[match(train_ids, names(labels))]
    if (anyNA(y) || any(!nzchar(y)) || length(unique(y)) < 2L)
      wsi_abort("Appearance requires non-missing training labels from at least two classes.")
    rows$label[index] <- y
    rows$split[index] <- "train"
  } else {
    recipe_path <- normalizePath(wsi_experiment_text(recipe_path, "recipe_path"), winslash = "/", mustWork = TRUE)
    if (!is.null(labels) || !is.null(train_ids))
      wsi_abort("Recipe replay does not accept labels or training IDs.")
    if (mode == "local") {
      saved <- jsonlite::fromJSON(recipe_path, simplifyVector = FALSE)
      provenance <- saved$payload$training$provenance$user_provenance
      if (!identical(provenance$experiment_id, experiment$experiment_id) ||
          !identical(unlist(provenance$tissue_ids, use.names = FALSE), active_tissue_id))
        wsi_abort("Local recipe belongs to another tissue/experiment; choose its original tissue or fit a shared recipe.")
    }
  }
  output_dir <- output_dir %||% wsi_experiment_derivative_dir(experiment, "appearance")
  if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE))
    wsi_abort("Cannot create appearance output directory.")
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  table_path <- file.path(output_dir, "features.csv")
  if (file.exists(table_path)) wsi_abort("Appearance output directory already contains a feature table.")
  utils::write.csv(cbind(rows, as.data.frame(f$x, check.names = FALSE)), table_path,
                   row.names = FALSE, na = "")
  prefix <- file.path(output_dir, "contrast")
  request <- list(format = "bora-appearance-request-v1", mode = mode,
    feature_kind = f$signature$kind, feature_table = table_path,
    feature_columns = as.list(colnames(f$x)), label_column = "label", split_column = "split",
    id_column = "id", x_column = "x", y_column = "y", coordinate_space = "native_pixel_centres",
    label_kind = if (mode == "local") "anonymous" else "semantic",
    encoder = list(revision = f$signature$encoder$revision, sha256 = f$signature$encoder$checkpoint_sha256),
    provenance = list(experiment_id = experiment$experiment_id, manifest_sha256 = experiment$manifest_sha256,
      feature_set_id = feature_set_id, tissue_ids = as.list(unique(f$observations$tissue_id))),
    output_prefix = prefix)
  if (is.null(recipe_path)) {
    class_labels <- sort(unique(as.character(rows$label[rows$split == "train"])))
    request$classes <- as.list(stats::setNames(class_labels, class_labels))
  }
  if (!is.null(recipe_path)) request$recipe_path <- recipe_path
  request_path <- file.path(output_dir, "request.json")
  jsonlite::write_json(request, request_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  python <- python %||% getOption("wsiTools.bora_python", Sys.getenv("WSITOOLS_BORA_PYTHON", "python3"))
  if (!file.exists(python) && !nzchar(Sys.which(python)))
    wsi_abort("Bora Python interpreter is unavailable; set option wsiTools.bora_python or WSITOOLS_BORA_PYTHON.")
  log <- file.path(output_dir, "bridge.log")
  pythonpath <- getOption("wsiTools.bora_pythonpath", NULL)
  env <- if (is.null(pythonpath)) character() else paste0("PYTHONPATH=", shQuote(pythonpath))
  status <- suppressWarnings(system2(python, c("-m", "bora.appearance_bridge", shQuote(request_path)),
    stdout = log, stderr = log, env = env, timeout = 120))
  if (!identical(as.integer(status), 0L)) {
    detail <- if (file.exists(log)) paste(utils::tail(readLines(log, warn = FALSE), 8L), collapse = " ") else "No bridge log was produced."
    detail <- gsub("[{}]", "", substr(detail, 1L, 1600L))
    if (grepl("No module named|ModuleNotFoundError|not found|cannot import", detail))
      wsi_abort(sprintf("Bora appearance backend unavailable. Configure the existing Bora Python interpreter in preferences (wsiTools.bora_python). %s", detail))
    wsi_abort(sprintf("Bora appearance failed: %s (status %s; log: %s)", detail, status, log))
  }
  scores_path <- paste0(prefix, ".scores.csv")
  rgb_path <- paste0(prefix, ".rgb.csv")
  frozen_path <- paste0(prefix, ".recipe.json")
  if (!all(file.exists(c(scores_path, rgb_path, frozen_path)))) wsi_abort("Bora bridge did not produce its required artifacts.")
  scores <- utils::read.csv(scores_path, check.names = FALSE, stringsAsFactors = FALSE, colClasses = c(id = "character"))
  rgb <- utils::read.csv(rgb_path, check.names = FALSE, stringsAsFactors = FALSE, colClasses = c(id = "character"))
  if (!all(c("id", "x", "y", "predicted_label") %in% names(scores)) ||
      !all(c("id", "x", "y", "r", "g", "b") %in% names(rgb)) ||
      nrow(scores) != nrow(rows) || nrow(rgb) != nrow(rows) ||
      anyDuplicated(scores$id) || anyDuplicated(rgb$id) ||
      !setequal(scores$id, rows$id) || !setequal(rgb$id, rows$id))
    wsi_abort("Bora bridge observation IDs/row count/schema disagree with the request.")
  scores <- scores[match(rows$id, scores$id), , drop = FALSE]
  rgb <- rgb[match(rows$id, rgb$id), , drop = FALSE]
  if (!isTRUE(all.equal(as.matrix(scores[, c("x", "y")]), as.matrix(rows[, c("x", "y")]), check.attributes = FALSE)) ||
      !isTRUE(all.equal(as.matrix(rgb[, c("x", "y")]), as.matrix(rows[, c("x", "y")]), check.attributes = FALSE)))
    wsi_abort("Bora bridge changed native image coordinates.")
  probability_columns <- grep("^probability:", names(scores), value = TRUE)
  probabilities <- as.matrix(scores[, probability_columns, drop = FALSE])
  colour <- as.matrix(rgb[, c("r", "g", "b")])
  if (!length(probability_columns) || !is.numeric(probabilities) || any(!is.finite(probabilities)) ||
      any(probabilities < 0 | probabilities > 1) ||
      !is.numeric(colour) || any(!is.finite(colour)) || any(colour < 0 | colour > 255))
    wsi_abort("Bora bridge returned invalid probability/colour values.")
  recipe <- jsonlite::fromJSON(frozen_path, simplifyVector = FALSE)
  points <- lapply(seq_len(nrow(rows)), function(i) list(id = rows$id[[i]],
    tissue_id = f$observations$tissue_id[[i]], x = rows$x[[i]], y = rows$y[[i]],
    r = colour[i, 1L], g = colour[i, 2L], b = colour[i, 3L],
    predicted_label = as.character(scores$predicted_label[[i]]),
    probabilities = as.list(unname(probabilities[i, ]))))
  list(recipe_path = frozen_path, recipe_id = recipe$recipe_id %||% wsi_experiment_sha256(frozen_path),
    scope = mode, feature_source = paste0("experiment:", feature_set_id),
    classes = as.list(sub("^probability:", "", probability_columns)), points = points,
    grids = f$grids, model_path = frozen_path, result_path = scores_path,
    uncertainty_status = "uncalibrated_discriminative_appearance", recipe = recipe)
}

wsi_experiment_footprint_in_roi <- function(points, rois, index) {
  width <- points$footprint_width_px
  height <- points$footprint_height_px
  if (is.null(width) || is.null(height)) wsi_abort("Experiment training requires declared observation footprints.")
  epsilon <- pmin(width, height) * 1e-8
  left <- points$x - width / 2 + epsilon
  right <- points$x + width / 2 - epsilon
  top <- points$y - height / 2 + epsilon
  bottom <- points$y + height / 2 - epsilon
  inside <- wsi_points_in_roi(rois, index, points$x, points$y)
  for (corner in list(c("left", "top"), c("left", "bottom"), c("right", "top"), c("right", "bottom"))) {
    xx <- if (corner[[1L]] == "left") left else right
    yy <- if (corner[[2L]] == "top") top else bottom
    inside <- inside & wsi_points_in_roi(rois, index, xx, yy)
  }
  # Corners alone miss enclosed holes or concave notches. Reject any polygon
  # boundary segment entering a footprint's open interior (slab intersection).
  type <- tolower(rois$geometry_type[[index]])
  polygons <- if (type == "polygon") list(rois$coordinates[[index]]) else if (type == "multipolygon") rois$coordinates[[index]] else list()
  if (!length(polygons)) wsi_abort("Experiment training annotations must be polygons or multipolygons.")
  for (polygon in polygons) for (ring in polygon) {
    vertices <- wsi_ring_matrix(ring)
    if (nrow(vertices) < 3L) next
    if (!identical(unname(vertices[1L, ]), unname(vertices[nrow(vertices), ]))) vertices <- rbind(vertices, vertices[1L, ])
    for (j in seq_len(nrow(vertices) - 1L)) {
      p <- vertices[j, ]; delta <- vertices[j + 1L, ] - p
      axis_interval <- function(value, change, lower, upper) {
        if (abs(change) < .Machine$double.eps) {
          good <- value >= lower & value <= upper
          return(list(lo = ifelse(good, -Inf, Inf), hi = ifelse(good, Inf, -Inf)))
        }
        a <- (lower - value) / change; b <- (upper - value) / change
        list(lo = pmin(a, b), hi = pmax(a, b))
      }
      tx <- axis_interval(p[[1L]], delta[[1L]], left, right)
      ty <- axis_interval(p[[2L]], delta[[2L]], top, bottom)
      crosses <- pmax(0, tx$lo, ty$lo) <= pmin(1, tx$hi, ty$hi)
      inside[crosses] <- FALSE
    }
  }
  inside
}

wsi_experiment_roi_training <- function(points, rois, train_ids) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) wsi_abort("Draw or import training annotations first.")
  selected <- wsi_prediction_selected_roi_indices(rois, train_ids, include_all = FALSE)
  if (!length(selected)) wsi_abort("Select training annotations explicitly.")
  wsi_prediction_reject_review_training(rois, selected)
  assigned <- rep(NA_character_, nrow(points))
  centre_labels <- assigned
  for (i in selected) {
    properties <- wsi_geojson_list(wsi_roi_column_value(rois, "properties", i, default = list()))
    index <- suppressWarnings(as.integer(unlist(properties[["project_image_index", exact = TRUE]] %||% NA_integer_)))
    declared <- properties[["tissue_id", exact = TRUE]] %||% properties[["image_id", exact = TRUE]] %||%
      properties[["sample_id", exact = TRUE]] %||% properties[["project_image", exact = TRUE]]
    if (length(index) == 1L && is.finite(index)) {
      scoped <- points$project_image_index == index
    } else if (!is.null(declared) && length(declared) == 1L && !is.na(declared) && nzchar(declared)) {
      scoped <- points$tissue_id == declared | points$project_image == declared
    } else if (length(unique(points$tissue_id)) > 1L) {
      wsi_abort("Multi-tissue training annotations must declare tissue/image identity or project_image_index.")
    } else scoped <- rep(TRUE, nrow(points))
    one <- list(label = rep(NA_character_, nrow(points)))
    inside <- scoped & wsi_points_in_roi(rois, i, points$x, points$y)
    one$label[inside] <- wsi_prediction_roi_label(rois, i)
    conflict <- !is.na(centre_labels) & !is.na(one$label) & centre_labels != one$label
    if (any(conflict)) wsi_abort("Conflicting training annotations overlap experiment observations.")
    centre_keep <- !is.na(one$label)
    centre_labels[centre_keep] <- one$label[centre_keep]
    keep <- centre_keep & wsi_experiment_footprint_in_roi(points, rois, i)
    assigned[keep] <- one$label[keep]
  }
  rows <- which(!is.na(assigned) & nzchar(assigned))
  excluded <- which(!is.na(centre_labels) & is.na(assigned))
  if (!length(rows)) wsi_abort(sprintf(
    "No complete tile/token footprints lie inside the selected training annotations. Enlarge the annotations or choose a spatial-token source. Accepted: 0; centre-labelled: %d; boundary/mixed excluded: %d.",
    sum(!is.na(centre_labels)), length(excluded)))
  list(rows = rows, labels = stats::setNames(assigned[rows], points$id[rows]),
       coverage = list(policy = "complete_rectangular_footprint_inside_one_annotation",
         accepted = length(rows), excluded_boundary_or_mixed = length(excluded),
         excluded_ids = as.list(points$id[excluded]), reason = "annotation_boundary_or_hole_intersects_footprint"),
       assignment = list(label = assigned))
}

wsi_experiment_prediction_run <- function(context, rois, source_id, train_ids, test_ids,
  ncomp, scaling, max_features) {
  experiment <- context$experiment
  if (!inherits(experiment, "wsi_experiment")) wsi_abort("No experiment is attached.")
  fid <- substring(source_id, 12L)
  f <- wsi_experiment_features(experiment, fid, context$tissue_ids)
  training <- wsi_experiment_roi_training(f$observations, rois, train_ids)
  test <- wsi_prediction_test_rows(f$observations, rois, test_ids, train_rows = training$rows)
  if (!length(test$rows)) wsi_abort("No non-training experiment observations were selected.")
  model <- wsi_experiment_train_classifier(f, fid, training$labels,
    train_ids = f$observations$id[training$rows], ncomp = ncomp, scaling = scaling,
    max_features = max_features)
  model$training_annotation_ids <- as.character(train_ids)
  model$training_coverage <- training$coverage
  predicted <- wsi_experiment_predict_classifier(model, f)
  run_dir <- wsi_experiment_derivative_dir(experiment, "classifiers")
  model_path <- wsi_write_experiment_classifier(model, file.path(run_dir, "classifier.rds"))
  result <- predicted[test$rows, , drop = FALSE]
  result$set <- "test"
  result$train_annotation_id <- NA_character_
  result$test_annotation_id <- test$roi_id[test$rows]
  result$observed <- test$label[test$rows]
  result$predicted_pls_lda <- result$predicted
  result$svm_refined <- FALSE
  result$feature_source <- source_id
  utils::write.csv(as.data.frame(result), file.path(run_dir, "predictions.csv"), row.names = FALSE, na = "")
  class(result) <- c("wsi_prediction_result", class(result))
  attr(result, "point_unit") <- f$signature$observation_unit
  attr(result, "source_name") <- source_id
  attr(result, "ncomp") <- model$ncomp
  attr(result, "method") <- "simpls"
  attr(result, "scaling") <- scaling
  attr(result, "feature_count") <- length(model$preprocessing$feature_columns)
  attr(result, "classifier_model") <- model
  attr(result, "classifier_path") <- model_path
  attr(result, "training_coverage") <- training$coverage
  result
}

wsi_experiment_appearance_response <- function(context, rois, payload) {
  experiment <- context$experiment
  if (!inherits(experiment, "wsi_experiment")) wsi_abort("No experiment is attached.")
  source <- as.character(payload$feature_source %||% "")
  if (!startsWith(source, "experiment:")) wsi_abort("Appearance requires an explicit experiment feature source.")
  fid <- substring(source, 12L)
  mode <- as.character(payload$mode %||% "local")
  tissues <- unlist(payload$tissue_ids, use.names = FALSE) %||% context$tissue_ids
  active <- payload$active_tissue_id
  if (identical(mode, "local")) tissues <- wsi_experiment_text(active, "active_tissue_id")
  labels <- train <- NULL
  recipe <- payload$recipe_path %||% NULL
  if (is.null(recipe)) {
    f <- wsi_experiment_features(experiment, fid, tissues)
    training <- wsi_experiment_roi_training(f$observations, rois,
      as.character(unlist(payload$train_annotations, use.names = FALSE)))
    labels <- training$labels
    train <- f$observations$id[training$rows]
  }
  result <- wsi_experiment_appearance(experiment, fid, labels = labels, train_ids = train,
    tissue_ids = tissues, mode = mode, active_tissue_id = active, recipe_path = recipe)
  if (is.null(recipe)) result$training_coverage <- training$coverage
  result
}

wsi_experiment_apply_saved_classifier <- function(context, state, payload) {
  if (!is.null(payload$model_path))
    wsi_experiment_verify_classifier_reference(state, context$experiment, payload$model_path)
  model <- if (!is.null(payload$model_path)) wsi_read_experiment_classifier(payload$model_path) else state$experiment_classifier
  if (!inherits(model, "wsi_experiment_classifier")) wsi_abort("Choose a saved experiment classifier first.")
  source <- paste0("experiment:", model$feature_set_id)
  if (!is.null(payload$feature_source) && !identical(payload$feature_source, source))
    wsi_abort("Selected feature source differs from the saved classifier.")
  tissues <- unlist(payload$tissue_ids, use.names = FALSE) %||% context$tissue_ids
  indexed <- any(vapply(context$experiment$tissues, function(tissue)
    (is.null(tissues) || tissue$tissue_id %in% tissues) && !is.null(tissue$cell_classification$index), logical(1)))
  if (indexed) {
    result <- wsi_experiment_index_apply_classifier(model, context$experiment, tissues,
      model_path = payload$model_path %||% NULL)
    if (!is.null(payload$model_path)) wsi_experiment_register_classifier(state, context$experiment, payload$model_path, model)
    state$experiment_classifier <- model
    if (!is.null(payload$model_path)) state$experiment_classifier_path <- payload$model_path
    return(result)
  }
  result <- wsi_experiment_predict_classifier(model, context$experiment, tissues)
  result$set <- "application"
  result$train_annotation_id <- NA_character_
  result$test_annotation_id <- NA_character_
  result$observed <- NA_character_
  result$predicted_pls_lda <- result$predicted
  result$svm_refined <- FALSE
  result$feature_source <- source
  run_dir <- wsi_experiment_derivative_dir(context$experiment, "classifier-applications")
  result_path <- file.path(run_dir, "predictions.csv")
  utils::write.csv(as.data.frame(result), result_path, row.names = FALSE, na = "")
  class(result) <- c("wsi_prediction_result", class(result))
  attr(result, "classifier_model") <- model
  attr(result, "classifier_path") <- payload$model_path %||% NULL
  attr(result, "result_path") <- result_path
  attr(result, "point_unit") <- model$signature$observation_unit
  attr(result, "feature_count") <- length(model$preprocessing$feature_columns)
  attr(result, "ncomp") <- model$ncomp
  result
}
