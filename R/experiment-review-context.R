# Read observation columns without allocating a Virchow2 predictor matrix.
wsi_experiment_review_table <- function(entry, experiment, columns, label, max_rows) {
  entry <- wsi_experiment_file(entry, experiment$root, label, TRUE)
  count <- wsi_experiment_number(entry$row_count, paste0(label, ".row_count"), positive = !identical(entry$observation_unit, "cell"), integer = TRUE)
  if (count < 0) wsi_abort("row_count cannot be negative.")
  if (count > max_rows) wsi_abort("Review observations exceed max_observations; select a smaller tissue/dataset scope.")
  if (!entry$format %in% c("csv", "csv.gz")) wsi_abort("Review artifacts require csv or csv.gz format.")
  if (identical(entry$format, "csv.gz") != grepl("\\.csv\\.gz$", entry$resolved_path, ignore.case = TRUE))
    wsi_abort("Review artifact format must match its CSV/CSV.GZ filename.")
  connection <- function() if (entry$format == "csv.gz") gzfile(entry$resolved_path, "rt") else file(entry$resolved_path, "rt")
  stream <- connection()
  header <- tryCatch(names(utils::read.csv(stream, nrows = 0L, check.names = FALSE)), finally = close(stream))
  if (anyDuplicated(header) || any(!columns %in% header)) wsi_abort(paste(label, "has missing or duplicate declared columns."))
  classes <- ifelse(header %in% columns, "character", "NULL")
  stream <- connection()
  table <- tryCatch(utils::read.csv(stream, colClasses = classes, nrows = count + 1L,
    check.names = FALSE, stringsAsFactors = FALSE, fill = FALSE), finally = close(stream))
  if (nrow(table) != count) wsi_abort(paste(label, "row_count disagrees with the CSV."))
  list(table = table, header = header, entry = entry)
}

wsi_experiment_review_numeric <- function(values, label) {
  if (is.factor(values)) values <- as.character(values)
  result <- suppressWarnings(as.numeric(values))
  if (anyNA(result) || any(!is.finite(result))) wsi_abort(paste(label, "must contain finite numeric values."))
  result
}

wsi_experiment_review_rows <- function(experiment, tissue, entry, dataset_id, max_rows, feature = FALSE) {
  columns <- c(entry$id_column, entry$x_column, entry$y_column)
  if (length(columns) != 3L || anyDuplicated(columns)) wsi_abort("Review ID/X/Y columns must be declared and distinct.")
  clusters <- as.character(unlist(entry$cluster_columns %||% list(), use.names = FALSE))
  if (anyNA(clusters) || any(!nzchar(clusters)) || anyDuplicated(clusters)) wsi_abort("cluster_columns must be unique names.")
  read <- wsi_experiment_review_table(entry, experiment, unique(c(columns, clusters)), dataset_id, max_rows)
  table <- read$table
  ids <- table[[entry$id_column]]
  if (anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids)) wsi_abort("Review observation IDs must be nonempty and unique within a dataset/tissue.")
  x <- wsi_experiment_review_numeric(table[[entry$x_column]], "Review X")
  y <- wsi_experiment_review_numeric(table[[entry$y_column]], "Review Y")
  if (feature) {
    if (any(!entry$feature_columns %in% read$header) ||
        !identical(read$header[read$header %in% entry$feature_columns], entry$feature_columns))
      wsi_abort("Review source feature columns are missing or appear in a different order.")
    geometry <- wsi_experiment_feature_geometry(entry, tissue$image, x, y)
  }
  unit <- entry$observation_unit
  if (!is.character(unit) || length(unit) != 1L || !unit %in% c("spot", "cell", "tile", "token"))
    wsi_abort("Review observation_unit must be spot, cell, tile or token.")
  if (!feature && !identical(entry$frame_id, tissue$image$frame_id))
    wsi_abort("Review observation frame_id differs from the declared tissue image.")
  fw <- if (feature) entry$grid$tile_width_px else entry$footprint_width_px
  fh <- if (feature) entry$grid$tile_height_px else entry$footprint_height_px
  if (!is.null(fw)) wsi_experiment_number(fw, "footprint_width_px")
  if (!is.null(fh)) wsi_experiment_number(fh, "footprint_height_px")
  if (xor(is.null(fw), is.null(fh))) wsi_abort("Declare both footprint dimensions or neither.")
  if (unit %in% c("tile", "token") && is.null(fw)) wsi_abort("Tile/token review observations require explicit footprints.")
  namespace <- if (feature) entry$feature_set_id else dataset_id
  global <- wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, namespace, ids)
  lapply(seq_along(ids), function(i) {
    values <- stats::setNames(lapply(clusters, function(column) {
      value <- table[[column]][[i]]
      if (is.na(value)) NULL else as.character(value)
    }), clusters)
    row <- list(id = global[[i]], original_id = ids[[i]], tissue_id = tissue$tissue_id,
      frame_id = tissue$image$frame_id, observation_unit = unit, x = x[[i]], y = y[[i]],
      dataset_id = dataset_id, cluster_values = values)
    if (!is.null(fw)) { row$footprint_width_px <- fw; row$footprint_height_px <- fh }
    if (feature && ncol(geometry)) {
      row$footprint <- list(x0 = geometry$logical_x[[i]] - fw / 2, y0 = geometry$logical_y[[i]] - fh / 2,
        x1 = geometry$logical_x[[i]] + fw / 2, y1 = geometry$logical_y[[i]] + fh / 2)
      row$valid_bounds <- as.list(geometry[i, c("valid_x0", "valid_y0", "valid_x1", "valid_y1")])
      row$logical_x <- geometry$logical_x[[i]]; row$logical_y <- geometry$logical_y[[i]]; row$padded <- geometry$padded[[i]]
    }
    if (feature) row$feature_set_id <- entry$feature_set_id
    row
  })
}

#' Construct a bounded review context from an experiment
#'
#' Optional tissue artifacts with role `review_observations` declare a CSV's
#' dataset_id, observation_unit, frame_id, row_count, ID/X/Y columns and optional
#' cluster_columns/footprints. Artifacts with role `observation_reduction` declare
#' reduction_id, dataset_id (or source_feature_set_id), observation_unit, frame_id,
#' row_count, id_column and exactly two dimension_columns. Both require path,
#' sha256 and csv/csv.gz format. Reductions join the complete declared dataset by
#' explicit local IDs; files are never discovered or matched by row position.
#'
#' @param experiment A typed experiment or its manifest path.
#' @param tissue_ids Optional explicit tissue subset.
#' @param feature_set_ids Optional typed feature subset; NULL includes all.
#' @param max_observations Total bounded review row limit; no sampling is done.
#' @param strict If FALSE, an exceeded row limit returns image metadata and an
#'   unavailable status without reading observation/reduction artifacts. No
#'   unvalidated observations are exposed in that metadata-only context.
#' @return A plain validated wsitools-review-context; predictor matrices are not loaded.
#' @export
wsi_experiment_review_context <- function(experiment, tissue_ids = NULL, feature_set_ids = NULL,
  max_observations = 10000L, strict = TRUE) {
  experiment <- wsi_read_experiment(experiment)
  max_observations <- wsi_experiment_number(max_observations, "max_observations", integer = TRUE)
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) wsi_abort("strict must be TRUE or FALSE.")
  tissues <- experiment$tissues
  ids <- vapply(tissues, `[[`, character(1), "tissue_id")
  if (!is.null(tissue_ids)) {
    if (!length(tissue_ids) || anyNA(tissue_ids) || anyDuplicated(tissue_ids) || any(!tissue_ids %in% ids))
      wsi_abort("Unknown, duplicate or empty tissue_ids.")
    tissues <- tissues[match(tissue_ids, ids)]
  }
  if (!is.null(feature_set_ids) && (anyNA(feature_set_ids) || anyDuplicated(feature_set_ids)))
    wsi_abort("feature_set_ids must be unique and nonmissing.")
  images <- lapply(tissues, function(t) {
    image <- t$image
    list(tissue_id = t$tissue_id, name = t$name, frame_id = image$frame_id, sha256 = image$sha256,
      width = image$width, height = image$height, mpp_x = image$mpp_x, mpp_y = image$mpp_y,
      path = image$resolved_path, coordinate_convention = image$coordinate_convention)
  })
  descriptors <- catalog <- list()
  seen <- requested <- character()
  for (tissue in tissues) {
    for (feature in tissue$feature_sets) {
      if (!is.null(feature_set_ids) && !feature$feature_set_id %in% feature_set_ids) next
      requested <- union(requested, feature$feature_set_id)
      dataset <- paste0("experiment:", feature$feature_set_id)
      key <- paste(tissue$tissue_id, dataset, sep = "\034")
      seen <- c(seen, key)
      indexed <- feature$kind %in% c("cellvit-cell", "cell-morphology") && !is.null(tissue$cell_classification$index)
      if (!indexed) descriptors[[length(descriptors) + 1L]] <- list(tissue = tissue, entry = feature, dataset_id = dataset, feature = TRUE)
      catalog[[length(catalog) + 1L]] <- list(tissue_id = tissue$tissue_id, dataset_id = dataset,
        feature_set_id = feature$feature_set_id, observation_unit = feature$observation_unit,
        row_count = feature$row_count, indexed = indexed)
    }
    for (artifact in tissue$artifacts %||% list()) {
      if (!identical(artifact$role, "review_observations")) next
      dataset <- wsi_experiment_text(artifact$dataset_id, "review dataset_id")
      key <- paste(tissue$tissue_id, dataset, sep = "\034")
      if (key %in% seen) wsi_abort("Duplicate review dataset_id within a tissue.")
      seen <- c(seen, key)
      count <- wsi_experiment_number(artifact$row_count, "review row_count", integer = TRUE)
      descriptors[[length(descriptors) + 1L]] <- list(tissue = tissue, entry = artifact, dataset_id = dataset, feature = FALSE)
      catalog[[length(catalog) + 1L]] <- list(tissue_id = tissue$tissue_id, dataset_id = dataset,
        observation_unit = artifact$observation_unit, row_count = count)
    }
    for (pair in wsi_experiment_spatial_pairs(tissue)) {
      dataset <- pair$artifact$dataset_id
      key <- paste(tissue$tissue_id, dataset, sep = "\034")
      if (key %in% seen) wsi_abort("Duplicate review dataset_id within a tissue.")
      seen <- c(seen, key)
      descriptors[[length(descriptors) + 1L]] <- list(tissue = tissue, pair = pair,
        dataset_id = dataset, spatial = TRUE)
      catalog[[length(catalog) + 1L]] <- list(tissue_id = tissue$tissue_id, dataset_id = dataset,
        observation_unit = pair$artifact$observation_unit, row_count = as.numeric(pair$artifact$row_count),
        role = "spatial_transcriptomics_object", object_path = pair$object$path,
        object_sha256 = pair$object$sha256)
    }
  }
  if (!is.null(feature_set_ids) && any(!feature_set_ids %in% requested)) wsi_abort("Unknown feature_set_ids in the selected tissues.")
  requested_total <- sum(vapply(catalog, `[[`, numeric(1), "row_count"))
  total <- sum(vapply(catalog, function(entry) if (isTRUE(entry$indexed)) 0 else entry$row_count, numeric(1)))
  provenance <- list(source_reference = list(type = "cellphenotyper-experiment", manifest_path = experiment$manifest_path,
    manifest_sha256 = experiment$manifest_sha256), dataset_catalog = catalog,
    availability = list(status = "available", max_observations = max_observations, requested_observations = requested_total,
      indexed_observations = requested_total - total, loading_policy = "indexed cells load only viewport or explicit review IDs"))
  review_defaults <- wsi_experiment_review_defaults(experiment)
  if (length(review_defaults)) provenance$review_defaults <- review_defaults
  construct <- function(observations, reductions) wsi_review_context(images, observations, reductions,
    context_id = paste0("experiment:", experiment$experiment_id), experiment_id = experiment$experiment_id,
    provenance = provenance)
  if (total > max_observations) {
    reason <- sprintf("Review needs %s observations, exceeding max_observations=%s. Select a bounded tissue/feature subset; no rows were sampled.", total, max_observations)
    if (strict) wsi_abort(reason)
    provenance$availability$status <- "unavailable"
    provenance$availability$reason <- reason
    return(construct(list(), list()))
  }
  observations <- list()
  spatial_reductions <- list()
  datasets <- list()
  for (descriptor in descriptors) {
    if (isTRUE(descriptor$spatial)) {
      bundle <- wsi_experiment_spatial_bundle(experiment, descriptor$tissue, descriptor$pair, max_observations)
      rows <- bundle$observations
      spatial_reductions <- c(spatial_reductions, bundle$reductions)
      provenance$spatial_attachments <- c(provenance$spatial_attachments %||% list(), list(bundle$attachment))
    } else rows <- wsi_experiment_review_rows(experiment, descriptor$tissue, descriptor$entry,
      descriptor$dataset_id, max_observations, descriptor$feature)
    key <- paste(descriptor$tissue$tissue_id, descriptor$dataset_id, sep = "\034")
    if (isTRUE(descriptor$feature) && descriptor$entry$kind %in% c("cellvit-cell", "cell-morphology"))
      rows <- wsi_experiment_cell_review_rows(rows, wsi_experiment_cell_classification(experiment, descriptor$tissue))
    datasets[[key]] <- rows
    observations <- c(observations, rows)
  }
  reductions <- spatial_reductions
  for (tissue in tissues) for (artifact in tissue$artifacts %||% list()) {
    if (!identical(artifact$role, "observation_reduction")) next
    dataset <- artifact$dataset_id
    if (!is.null(artifact$source_feature_set_id)) {
      expected <- paste0("experiment:", artifact$source_feature_set_id)
      if (!is.null(dataset) && !identical(dataset, expected)) wsi_abort("Reduction dataset_id and source_feature_set_id disagree.")
      dataset <- expected
    }
    dataset <- wsi_experiment_text(dataset, "reduction dataset_id")
    rows <- datasets[[paste(tissue$tissue_id, dataset, sep = "\034")]]
    if (is.null(rows)) {
      if (!is.null(tissue$cell_classification$index) && !is.null(artifact$source_feature_set_id)) next
      if (!is.null(feature_set_ids) && !is.null(artifact$source_feature_set_id) && !artifact$source_feature_set_id %in% feature_set_ids) next
      wsi_abort("Reduction refers to an unknown review dataset in its tissue.")
    }
    if (!identical(artifact$frame_id, tissue$image$frame_id) ||
        !identical(artifact$observation_unit, rows[[1L]]$observation_unit))
      wsi_abort("Reduction frame/unit differs from its declared observation dataset.")
    dims <- as.character(unlist(artifact$dimension_columns, use.names = FALSE))
    id_column <- wsi_experiment_text(artifact$id_column, "reduction id_column")
    if (length(dims) != 2L || anyNA(dims) || any(!nzchar(dims)) || anyDuplicated(c(id_column, dims)))
      wsi_abort("Reduction requires two distinct dimension_columns separate from its ID column.")
    read <- wsi_experiment_review_table(artifact, experiment, c(id_column, dims), "reduction", max_observations)
    table <- read$table
    local <- vapply(rows, `[[`, character(1), "original_id")
    incoming <- table[[id_column]]
    if (anyNA(incoming) || anyDuplicated(incoming) || length(incoming) != length(local) || !setequal(incoming, local))
      wsi_abort("Reduction IDs must match every declared observation exactly once; row order is never used for joins.")
    order <- match(local, incoming)
    x <- wsi_experiment_review_numeric(table[[dims[[1L]]]][order], "Reduction dimension 1")
    y <- wsi_experiment_review_numeric(table[[dims[[2L]]]][order], "Reduction dimension 2")
    rid <- wsi_experiment_text(artifact$reduction_id, "reduction_id")
    reductions[[length(reductions) + 1L]] <- list(
      id = wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, dataset, rid),
      label = paste(tissue$name, artifact$label %||% artifact$method %||% rid, dataset, sep = " \u00b7 "),
      observation_unit = artifact$observation_unit, frame_id = artifact$frame_id,
      source_feature_set_id = artifact$source_feature_set_id, dataset_id = dataset,
      source_reference = list(path = read$entry$resolved_path, sha256 = read$entry$sha256,
        method = artifact$method, dimension_columns = as.list(dims)),
      points = lapply(seq_along(rows), function(i) list(id = rows[[i]]$id, x = x[[i]], y = y[[i]])))
  }
  construct(observations, reductions)
}

#' Fit an image-feature classifier from explicitly confirmed human labels
#' @param experiment A typed experiment or manifest.
#' @param context The exact validated review context used for the checkpoint.
#' @param checkpoint A validated human-review checkpoint.
#' @param feature_set_id Compatible typed image feature set to train.
#' @param train_ids Explicit canonical IDs to use as training observations.
#' @param ... CPU fitting settings passed to wsi_experiment_train_classifier().
#' @return A portable CPU experiment classifier with checkpoint revision and
#'   human-label provenance. Typed cell features are supported; spatial spot/cell
#'   cluster labels are never promoted without explicit human review.
#' @export
wsi_experiment_train_reviewed_classifier <- function(experiment, context, checkpoint,
  feature_set_id, train_ids, ...) {
  context <- wsi_validate_review_context(context)
  checkpoint <- wsi_validate_review_checkpoint(checkpoint, context)
  train_ids <- wsi_review_ids(train_ids, "train_ids", vapply(context$observations, `[[`, character(1), "id"))
  if (!length(train_ids)) wsi_abort("Select explicit reviewed image observations for training; empty selection never means all.")
  records <- checkpoint$review_records
  record_ids <- vapply(records, `[[`, character(1), "id")
  selected <- match(train_ids, record_ids)
  if (anyNA(selected)) wsi_abort("Every training observation requires an explicit human review record.")
  records <- records[selected]
  if (any(!vapply(records, function(record) identical(record$status, "reviewed"), logical(1))))
    wsi_abort("Only confirmed reviewed labels can train a classifier; excluded/uncertain records are not training targets.")
  rows <- context$observations[match(train_ids, vapply(context$observations, `[[`, character(1), "id"))]
  if (any(!vapply(rows, function(row) row$observation_unit %in% c("tile", "token", "cell") &&
      identical(row$feature_set_id, feature_set_id), logical(1))))
    wsi_abort("Reviewed training IDs must belong to the compatible typed image feature set; spatial spot/cell cluster labels without the exact typed feature set are not training labels.")
  experiment <- wsi_read_experiment(experiment)
  if (!identical(context$experiment_id, experiment$experiment_id)) wsi_abort("Review context belongs to another experiment.")
  reference <- context$provenance$source_reference
  if (!identical(reference$manifest_sha256, experiment$manifest_sha256))
    wsi_abort("Review context experiment manifest identity changed; reopen and review the revised source.")
  tissues <- unique(vapply(rows, `[[`, character(1), "tissue_id"))
  indexed <- any(vapply(experiment$tissues, function(tissue) tissue$tissue_id %in% tissues &&
    !is.null(tissue$cell_classification$index), logical(1)))
  features <- if (indexed) wsi_experiment_index_features(experiment, feature_set_id, train_ids) else
    wsi_experiment_features(experiment, feature_set_id, tissues)
  if (any(!train_ids %in% features$observations$id)) wsi_abort("Reviewed observations do not match current image feature IDs.")
  expected <- features$observations[match(train_ids, features$observations$id), , drop = FALSE]
  for (i in seq_along(rows)) {
    tissue <- experiment$tissues[[match(rows[[i]]$tissue_id,
      vapply(experiment$tissues, `[[`, character(1), "tissue_id"))]]
    image <- context$images[[match(rows[[i]]$tissue_id, vapply(context$images, `[[`, character(1), "tissue_id"))]]
    if (!identical(image$sha256, tolower(tissue$image$sha256)) ||
        !identical(rows[[i]]$frame_id, tissue$image$frame_id) ||
        !identical(rows[[i]]$observation_unit, expected$unit[[i]]) ||
        abs(rows[[i]]$x - expected$x[[i]]) > 1e-8 || abs(rows[[i]]$y - expected$y[[i]]) > 1e-8 ||
        (expected$unit[[i]] != "cell" && (is.null(rows[[i]]$footprint_width_px) || is.null(rows[[i]]$footprint_height_px) ||
        abs(rows[[i]]$footprint_width_px - expected$footprint_width_px[[i]]) > 1e-8 ||
        abs(rows[[i]]$footprint_height_px - expected$footprint_height_px[[i]]) > 1e-8)))
      wsi_abort("Review observations no longer match the current native image feature metadata.")
  }
  labels <- stats::setNames(vapply(records, `[[`, character(1), "label"), train_ids)
  model <- wsi_experiment_train_classifier(features, feature_set_id, labels, train_ids = train_ids, ...)
  model$human_review <- list(context_id = context$context_id, source_signature = context$source_signature,
    checkpoint_revision = checkpoint$revision, records = records,
    review_sha256 = wsi_review_digest(records), label_origin = "explicit_human_review",
    fitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  model
}
