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
    tissue$image <- image
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
      expected <- switch(f$kind, "virchow2-pooled" = 2560L, "virchow2-spatial" = 1280L, NULL)
      if (is.null(expected)) wsi_abort("Unsupported feature kind; declare virchow2-pooled or virchow2-spatial.")
      unit <- if (identical(f$kind, "virchow2-pooled")) "tile" else "token"
      if (!identical(f$observation_unit, unit)) wsi_abort("Feature kind and observation_unit disagree.")
      if (!identical(f$coordinate_frame, "image")) wsi_abort("Feature coordinates must already be in the declared image frame.")
      if (!f$format %in% c("csv", "csv.gz")) wsi_abort("Only explicit csv/csv.gz feature sets are supported.")
      columns <- unlist(f$feature_columns, use.names = FALSE)
      if (!is.character(columns) || length(columns) != expected ||
          anyNA(columns) || any(!nzchar(columns)) || anyDuplicated(columns))
        wsi_abort(sprintf("%s requires %d unique ordered feature_columns.", f$kind, expected))
      for (field in c("id_column", "x_column", "y_column")) wsi_experiment_text(f[[field]], field)
      if (any(columns %in% c(f$id_column, f$x_column, f$y_column)) ||
          length(unique(c(f$id_column, f$x_column, f$y_column))) != 3L)
        wsi_abort("Predictor allowlist must exclude identity and coordinate columns.")
      wsi_experiment_number(f$row_count, "row_count", integer = TRUE)
      for (field in c("name", "revision", "checkpoint_sha256"))
        wsi_experiment_text(f$encoder[[field]], paste0("encoder.", field))
      if (!identical(tolower(f$encoder$name), "virchow2") ||
          !grepl("^[0-9a-fA-F]{64}$", f$encoder$checkpoint_sha256))
        wsi_abort("Feature encoder must identify Virchow2 and a checkpoint SHA-256.")
      definition <- if (unit == "tile") "CLS+mean_patch" else "spatial_patch_tokens"
      if (!identical(f$preprocessing$feature_definition, definition))
        wsi_abort("Feature definition disagrees with the declared feature kind.")
      for (field in c("tile_width_px", "tile_height_px", "spacing_x_px", "spacing_y_px"))
        wsi_experiment_number(f$grid[[field]], paste0("grid.", field))
      for (field in c("origin_x_px", "origin_y_px"))
        wsi_experiment_number(f$grid[[field]], paste0("grid.", field), positive = FALSE)
      f$feature_columns <- columns
      sets[[j]] <- wsi_experiment_file(f, root, paste0(id, ".", fid), verify_hashes)
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
  result
}

wsi_experiment_namespace_id <- function(experiment_id, tissue_id, feature_set_id, id) {
  encode <- function(x) vapply(as.character(x), utils::URLencode, character(1), reserved = TRUE)
  paste(encode(experiment_id), encode(tissue_id), encode(feature_set_id), encode(id), sep = "::")
}

wsi_experiment_feature_signature <- function(f) {
  list(kind = f$kind, observation_unit = f$observation_unit,
       feature_columns = unname(f$feature_columns), encoder = f$encoder,
       preprocessing = f$preprocessing)
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
    list(id = t$tissue_id, label = t$name,
      path = t$image$resolved_path, width = t$image$width, height = t$image$height,
      mpp = list(x = t$image$mpp_x, y = t$image$mpp_y), tissue_id = t$tissue_id,
      frame_id = t$image$frame_id, rois = wsi_viewer_roi_features(rois),
      artifact_references = t$artifacts)
  })
}

wsi_experiment_annotation_rois <- function(experiment, tissue, image_index) {
  all <- list()
  for (a in tissue$artifacts %||% list()) {
    if (!identical(a$role, "tissue_annotations")) next
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
  slide <- wsi_open(experiment$tissues[[1L]]$image$resolved_path)
  handed_off <- FALSE
  on.exit(if (!handed_off) wsi_close(slide), add = TRUE)
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
  model <- if (!is.null(payload$model_path)) wsi_read_experiment_classifier(payload$model_path) else state$experiment_classifier
  if (!inherits(model, "wsi_experiment_classifier")) wsi_abort("Choose a saved experiment classifier first.")
  source <- paste0("experiment:", model$feature_set_id)
  if (!is.null(payload$feature_source) && !identical(payload$feature_source, source))
    wsi_abort("Selected feature source differs from the saved classifier.")
  tissues <- unlist(payload$tissue_ids, use.names = FALSE) %||% context$tissue_ids
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

# Read observation columns without allocating a Virchow2 predictor matrix.
wsi_experiment_review_table <- function(entry, experiment, columns, label, max_rows) {
  entry <- wsi_experiment_file(entry, experiment$root, label, TRUE)
  count <- wsi_experiment_number(entry$row_count, paste0(label, ".row_count"), integer = TRUE)
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
    gx <- (x - entry$grid$origin_x_px - entry$grid$tile_width_px / 2) / entry$grid$spacing_x_px
    gy <- (y - entry$grid$origin_y_px - entry$grid$tile_height_px / 2) / entry$grid$spacing_y_px
    if (any(abs(gx - round(gx)) > 1e-5 | abs(gy - round(gy)) > 1e-5 | gx < -1e-5 | gy < -1e-5))
      wsi_abort("Review feature centres disagree with the declared native grid.")
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
      descriptors[[length(descriptors) + 1L]] <- list(tissue = tissue, entry = feature, dataset_id = dataset, feature = TRUE)
      catalog[[length(catalog) + 1L]] <- list(tissue_id = tissue$tissue_id, dataset_id = dataset,
        feature_set_id = feature$feature_set_id, observation_unit = feature$observation_unit, row_count = feature$row_count)
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
  total <- sum(vapply(catalog, `[[`, numeric(1), "row_count"))
  provenance <- list(source_reference = list(type = "cellphenotyper-experiment", manifest_path = experiment$manifest_path,
    manifest_sha256 = experiment$manifest_sha256), dataset_catalog = catalog,
    availability = list(status = "available", max_observations = max_observations, requested_observations = total))
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
#'   human-label provenance. Spot/cell cluster labels are never promoted.
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
  if (any(!vapply(rows, function(row) row$observation_unit %in% c("tile", "token") &&
      identical(row$feature_set_id, feature_set_id), logical(1))))
    wsi_abort("Reviewed training IDs must belong to the compatible typed image feature set; spatial spot/cell labels are not image training labels.")
  experiment <- wsi_read_experiment(experiment)
  if (!identical(context$experiment_id, experiment$experiment_id)) wsi_abort("Review context belongs to another experiment.")
  reference <- context$provenance$source_reference
  if (!identical(reference$manifest_sha256, experiment$manifest_sha256))
    wsi_abort("Review context experiment manifest identity changed; reopen and review the revised source.")
  tissues <- unique(vapply(rows, `[[`, character(1), "tissue_id"))
  features <- wsi_experiment_features(experiment, feature_set_id, tissues)
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
        is.null(rows[[i]]$footprint_width_px) || is.null(rows[[i]]$footprint_height_px) ||
        abs(rows[[i]]$footprint_width_px - expected$footprint_width_px[[i]]) > 1e-8 ||
        abs(rows[[i]]$footprint_height_px - expected$footprint_height_px[[i]]) > 1e-8)
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
