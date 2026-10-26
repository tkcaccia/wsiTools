wsi_review_text <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
    wsi_abort(sprintf("`%s` must be one non-empty string.", name))
  x
}

wsi_review_number <- function(x, name, positive = FALSE, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      (positive && x <= 0) || (integer && x != floor(x)))
    wsi_abort(sprintf("`%s` must be a finite%s%s number.", name,
      if (positive) " positive" else "", if (integer) " integer" else ""))
  as.numeric(x)
}

wsi_review_rows <- function(x, name) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) return(lapply(seq_len(nrow(x)), function(i)
    lapply(x, function(column) if (is.list(column)) column[[i]] else column[[i]])))
  if (!is.list(x) || any(!vapply(x, is.list, logical(1))))
    wsi_abort(sprintf("`%s` must be an array of records or a data frame.", name))
  unname(x)
}

wsi_review_ids <- function(x, name, known = NULL) {
  if (is.null(x) || !length(x)) return(character())
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x))) || anyDuplicated(x))
    wsi_abort(sprintf("`%s` must contain unique, non-empty observation IDs.", name))
  if (!is.null(known) && any(!x %in% known))
    wsi_abort(sprintf("`%s` contains unknown observation IDs: %s.", name,
      paste(utils::head(setdiff(x, known), 5L), collapse = ", ")))
  unname(x)
}

wsi_review_canonical_json <- function(x) {
  canonical <- function(value) {
    if (is.factor(value)) return(as.character(value))
    if (!is.list(value)) return(value)
    if (!is.null(names(value))) value <- value[order(names(value))]
    lapply(value, canonical)
  }
  jsonlite::toJSON(canonical(x), auto_unbox = TRUE, null = "null", na = "null", digits = 17)
}

wsi_review_digest <- function(x) {
  if (!requireNamespace("openssl", quietly = TRUE))
    wsi_abort("Review context integrity requires the optional openssl package.")
  paste0(openssl::sha256(charToRaw(enc2utf8(wsi_review_canonical_json(x)))))
}

wsi_review_image <- function(image, verify_hashes = FALSE) {
  for (name in c("tissue_id", "frame_id", "sha256", "coordinate_convention"))
    image[[name]] <- wsi_review_text(image[[name]], paste0("image.", name))
  if (!grepl("^[a-fA-F0-9]{64}$", image$sha256)) wsi_abort("Image sha256 must be a SHA-256 hex digest.")
  image$sha256 <- tolower(image$sha256)
  if (!identical(image$coordinate_convention, "pixel-edge-origin_centres-in-native-pixels"))
    wsi_abort("Review images require explicit native pixel-edge origin and pixel-centre coordinates.")
  image$width <- wsi_review_number(image$width, "image.width", TRUE, TRUE)
  image$height <- wsi_review_number(image$height, "image.height", TRUE, TRUE)
  absent <- is.null(image$mpp_x) && is.null(image$mpp_y)
  if (!is.null(image$calibration_status)) {
    image$calibration_status <- wsi_review_text(image$calibration_status, "image.calibration_status")
    if (!image$calibration_status %in% c("available", "unavailable"))
      wsi_abort("Review image calibration_status must be available or unavailable.")
  }
  if (absent && identical(image$calibration_status, "unavailable")) {
    image$mpp_x <- NULL
    image$mpp_y <- NULL
  } else {
    image$mpp_x <- wsi_review_number(image$mpp_x, "image.mpp_x", TRUE)
    image$mpp_y <- wsi_review_number(image$mpp_y, "image.mpp_y", TRUE)
    if (identical(image$calibration_status, "unavailable"))
      wsi_abort("An unavailable calibration cannot also declare microns-per-pixel values.")
    image$calibration_status <- image$calibration_status %||% "available"
  }
  if (!is.null(image$same_section) && !identical(image$same_section, TRUE))
    wsi_abort("Cross-section spatial mappings are outside the same-section review contract.")
  if (isTRUE(verify_hashes)) {
    path <- image$resolved_path %||% image$path
    path <- wsi_review_text(path, "image.path for hash verification")
    if (!file.exists(path) || dir.exists(path)) wsi_abort(sprintf("Review image is inaccessible: %s", path))
    con <- file(path, "rb")
    actual <- tryCatch(paste0(openssl::sha256(con)), finally = close(con))
    if (!identical(actual, image$sha256)) wsi_abort(sprintf("Review image SHA-256 mismatch: %s", path))
  }
  image
}

wsi_review_observation <- function(observation, images) {
  if (is.null(observation$observation_unit)) observation$observation_unit <- observation$unit
  observation$unit <- NULL
  for (name in c("id", "original_id", "tissue_id", "frame_id", "observation_unit"))
    observation[[name]] <- wsi_review_text(observation[[name]], paste0("observation.", name))
  if (!observation$observation_unit %in% c("spot", "cell", "tile", "token"))
    wsi_abort("Review observation_unit must be spot, cell, tile or token.")
  index <- match(observation$tissue_id, vapply(images, `[[`, character(1), "tissue_id"))
  if (is.na(index)) wsi_abort("Review observation references an unknown tissue_id.")
  image <- images[[index]]
  if (!identical(observation$frame_id, image$frame_id))
    wsi_abort("Observation and image frame_id differ; transform coordinates explicitly before review.")
  observation$x <- wsi_review_number(observation$x, "observation.x")
  observation$y <- wsi_review_number(observation$y, "observation.y")
  if (observation$x < 0 || observation$x >= image$width || observation$y < 0 || observation$y >= image$height)
    wsi_abort("Review observation centre lies outside its declared image frame.")
  footprint <- observation$footprint
  width <- observation$footprint_width_px
  height <- observation$footprint_height_px
  if (!is.null(footprint)) {
    if (!is.list(footprint)) wsi_abort("Observation footprint must be a native-pixel edge rectangle.")
    for (name in c("x0", "y0", "x1", "y1")) footprint[[name]] <- wsi_review_number(footprint[[name]], paste0("footprint.", name))
    if (footprint$x1 <= footprint$x0 || footprint$y1 <= footprint$y0 ||
        observation$x < footprint$x0 || observation$x > footprint$x1 ||
        observation$y < footprint$y0 || observation$y > footprint$y1)
      wsi_abort("Observation footprint must have positive area and contain its observation centre.")
    actual_width <- footprint$x1 - footprint$x0
    actual_height <- footprint$y1 - footprint$y0
    if ((!is.null(width) && abs(wsi_review_number(width, "footprint_width_px", TRUE) - actual_width) > 1e-7) ||
        (!is.null(height) && abs(wsi_review_number(height, "footprint_height_px", TRUE) - actual_height) > 1e-7))
      wsi_abort("Footprint edge rectangle and declared footprint dimensions disagree.")
    observation$footprint <- footprint
    observation$footprint_width_px <- actual_width
    observation$footprint_height_px <- actual_height
  } else if (!is.null(width) || !is.null(height)) {
    width <- wsi_review_number(width, "footprint_width_px", TRUE)
    height <- wsi_review_number(height, "footprint_height_px", TRUE)
    observation$footprint_width_px <- width
    observation$footprint_height_px <- height
    observation$footprint <- list(x0 = observation$x - width / 2, y0 = observation$y - height / 2,
      x1 = observation$x + width / 2, y1 = observation$y + height / 2)
  }
  clusters <- observation$cluster_values %||% list()
  if (!is.list(clusters) || (length(clusters) &&
      (is.null(names(clusters)) || any(!nzchar(names(clusters))) || anyDuplicated(names(clusters)))))
    wsi_abort("Observation cluster_values must be a named metadata object.")
  for (name in names(clusters)) {
    value <- clusters[[name]]
    if (is.null(value) || (length(value) == 1L && is.na(value))) next
    if (length(value) != 1L || !is.atomic(value)) wsi_abort("Cluster values must be scalar categorical metadata.")
    clusters[[name]] <- as.character(value)
  }
  observation$cluster_values <- clusters
  observation
}

#' Construct a typed, same-section human review context
#' @param images Image records with explicit tissue/frame identity, SHA-256,
#'   native dimensions and calibration. Unknown calibration is declared as
#'   `calibration_status = "unavailable"` with both MPP values absent.
#' @param observations Observation records with canonical IDs, original IDs,
#'   tissue/frame identity, observation units and native image coordinates.
#' @param reductions ID-keyed embedding records, separate from image coordinates.
#' @param context_id Optional stable context identity.
#' @param experiment_id Optional experiment identity.
#' @param provenance JSON-compatible source and mapping provenance.
#' @return A JSON-compatible review context with a deterministic source signature.
#' @export
wsi_review_context <- function(images, observations, reductions = list(), context_id = NULL,
                               experiment_id = NULL, provenance = list()) {
  if (!is.list(provenance)) wsi_abort("Review provenance must be a JSON-compatible metadata object.")
  images <- lapply(wsi_review_rows(images, "images"), wsi_review_image)
  if (!length(images)) wsi_abort("A review context needs at least one explicitly identified image.")
  tissue_ids <- vapply(images, `[[`, character(1), "tissue_id")
  if (anyDuplicated(tissue_ids)) wsi_abort("Review image tissue IDs must be unique.")
  observations <- wsi_review_rows(observations, "observations")
  maximum <- getOption("wsiTools.review_max_observations", 10000L)
  if (length(observations) > maximum) wsi_abort(sprintf("Review context has %s observations; limit is %s. Select a bounded subset; no sampling was performed.", length(observations), maximum))
  observations <- lapply(observations, wsi_review_observation, images = images)
  ids <- vapply(observations, `[[`, character(1), "id")
  if (anyDuplicated(ids)) wsi_abort("Canonical review observation IDs must be unique across tissues and datasets.")
  reductions <- wsi_review_rows(reductions, "reductions")
  reductions <- lapply(reductions, function(reduction) {
    reduction$id <- wsi_review_text(reduction$id, "reduction.id")
    reduction$label <- reduction$label %||% reduction$id
    reduction$observation_unit <- wsi_review_text(reduction$observation_unit, "reduction.observation_unit")
    points <- wsi_review_rows(reduction$points, "reduction.points")
    point_ids <- wsi_review_ids(vapply(points, function(point) wsi_review_text(point$id, "reduction point.id"), character(1)), "reduction point IDs", ids)
    if (length(points) > maximum) wsi_abort("Reduction exceeds the review observation limit.")
    for (i in seq_along(points)) {
      observation <- observations[[match(point_ids[[i]], ids)]]
      if (!identical(observation$observation_unit, reduction$observation_unit)) wsi_abort("Reduction joins incompatible observation units.")
      for (key in c("tissue_id", "frame_id")) if (!is.null(points[[i]][[key]]) &&
          !identical(points[[i]][[key]], observation[[key]])) wsi_abort("Reduction point tissue/frame provenance differs from its observation.")
      points[[i]]$x <- wsi_review_number(points[[i]]$x, "reduction point.x")
      points[[i]]$y <- wsi_review_number(points[[i]]$y, "reduction point.y")
    }
    reduction$points <- points
    reduction
  })
  if (anyDuplicated(vapply(reductions, `[[`, character(1), "id"))) wsi_abort("Review reduction IDs must be unique.")
  semantic_images <- lapply(images, function(image) image[setdiff(names(image), c("path", "resolved_path", "name", "label"))])
  semantic_reductions <- lapply(reductions, function(reduction) {
    reduction$label <- NULL
    reduction$points <- reduction$points[order(vapply(reduction$points, `[[`, character(1), "id"))]
    reduction
  })
  signature <- wsi_review_digest(list(images = semantic_images[order(tissue_ids)],
    observations = observations[order(ids)],
    reductions = semantic_reductions[order(vapply(semantic_reductions, `[[`, character(1), "id"))],
    experiment_id = experiment_id,
    source_reference = provenance$source_reference,
    spatial_attachments = provenance$spatial_attachments))
  if (is.null(context_id)) context_id <- paste0("review:", substr(signature, 1L, 24L))
  context_id <- wsi_review_text(context_id, "context_id")
  if (!is.null(experiment_id)) experiment_id <- wsi_review_text(experiment_id, "experiment_id")
  # Empty categorical metadata is a JSON object. Shape it after the canonical
  # digest, preserving signatures of existing empty-metadata contexts. Validation
  # normalizes this presentation back through wsi_review_observation before hashing.
  observations <- lapply(observations, function(row) {
    if (!length(row$cluster_values)) row$cluster_values <- structure(list(), names = character())
    row
  })
  list(schema = "wsitools-review-context", schema_version = 1L,
    context_id = context_id, source_signature = signature, experiment_id = experiment_id,
    images = images, observations = observations, reductions = reductions, provenance = provenance)
}

#' @rdname wsi_review_context
#' @param context A canonical review context.
#' @param verify_hashes Verify image files against their declared SHA-256 hashes.
#' @param require_calibration Require positive physical calibration on all images.
#' @export
wsi_validate_review_context <- function(context, verify_hashes = FALSE, require_calibration = FALSE) {
  if (!is.list(context) || !identical(context$schema, "wsitools-review-context") ||
      length(context$schema_version) != 1L || !is.numeric(context$schema_version) ||
      !is.finite(context$schema_version) || context$schema_version != 1)
    wsi_abort("Unsupported review context schema/version.")
  normalized <- wsi_review_context(context$images, context$observations, context$reductions,
    context$context_id, context$experiment_id, context$provenance %||% list())
  if (!identical(context$source_signature, normalized$source_signature))
    wsi_abort("Review context source signature changed; rebuild it explicitly before reuse.")
  if (isTRUE(require_calibration) && any(vapply(normalized$images, function(image)
      is.null(image$mpp_x) || is.null(image$mpp_y), logical(1))))
    wsi_abort("Physical tile processing requires explicit positive image calibration; no microns-per-pixel values were inferred.")
  if (isTRUE(verify_hashes)) normalized$images <- lapply(normalized$images, wsi_review_image, verify_hashes = TRUE)
  normalized
}

#' Select canonical observations by IDs or categorical clusters
#' @param ids Explicit canonical IDs; an empty selection means no observations.
#' @param cluster_field Categorical metadata field to select.
#' @param cluster_values Explicit categorical values in that field.
#' @param tissue_ids Optional tissue scope.
#' @param observation_unit Optional spot, cell, tile or token scope.
#' @param dataset_ids Optional explicit dataset identities. Use this to keep
#'   equally named clusters from different spatial objects separate.
#' @inheritParams wsi_review_context
#' @return Selected canonical IDs in context observation order.
#' @export
wsi_review_select <- function(context, ids = NULL, cluster_field = NULL,
                              cluster_values = NULL, tissue_ids = NULL, observation_unit = NULL,
                              dataset_ids = NULL) {
  context <- wsi_validate_review_context(context)
  observations <- context$observations
  known <- vapply(observations, `[[`, character(1), "id")
  if (!is.null(ids) && !is.null(cluster_field)) wsi_abort("Select by explicit IDs or by clusters, not both.")
  keep <- rep(FALSE, length(known))
  if (!is.null(ids)) keep <- known %in% wsi_review_ids(ids, "selected IDs", known)
  if (!is.null(cluster_field)) {
    cluster_field <- wsi_review_text(cluster_field, "cluster_field")
    fields <- unique(unlist(lapply(observations, function(observation) names(observation$cluster_values))))
    if (!cluster_field %in% fields) wsi_abort("Unknown review cluster field.")
    values <- as.character(unlist(cluster_values, use.names = FALSE))
    if (anyNA(values) || any(!nzchar(values))) wsi_abort("Cluster selection values must be non-missing categories.")
    keep <- vapply(observations, function(observation) {
      value <- observation$cluster_values[[cluster_field]]
      !is.null(value) && length(value) == 1L && !is.na(value) && as.character(value) %in% values
    }, logical(1))
  }
  if (!is.null(tissue_ids)) {
    tissue_ids <- wsi_review_ids(tissue_ids, "tissue_ids", vapply(context$images, `[[`, character(1), "tissue_id"))
    keep <- keep & vapply(observations, function(observation) observation$tissue_id %in% tissue_ids, logical(1))
  }
  if (!is.null(observation_unit)) {
    observation_unit <- wsi_review_text(observation_unit, "observation_unit")
    if (!observation_unit %in% c("spot", "cell", "tile", "token")) wsi_abort("Unknown review observation unit.")
    keep <- keep & vapply(observations, function(observation) identical(observation$observation_unit, observation_unit), logical(1))
  }
  if (!is.null(dataset_ids)) {
    available <- vapply(observations, function(row) row$dataset_id %||% "", character(1))
    wanted <- wsi_review_ids(dataset_ids, "dataset_ids", unique(available))
    keep <- keep & available %in% wanted
  }
  known[keep]
}

wsi_review_records <- function(records, ids) {
  records <- wsi_review_rows(records, "review_records")
  records <- lapply(records, function(record) {
    record$id <- wsi_review_ids(record$id, "review record ID", ids)
    if (length(record$id) != 1L) wsi_abort("A review record needs exactly one observation ID.")
    record$status <- wsi_review_text(record$status, "review status")
    if (!record$status %in% c("reviewed", "excluded", "uncertain")) wsi_abort("Unknown human review status.")
    if (identical(record$status, "reviewed")) record$label <- wsi_review_text(record$label, "reviewed label")
    if (!is.null(record$label)) record$label <- wsi_review_text(record$label, "review label")
    record$reviewer <- wsi_review_text(record$reviewer, "reviewer")
    record$updated_at <- wsi_review_text(record$updated_at, "review updated_at")
    if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z$", record$updated_at) ||
        is.na(as.POSIXct(record$updated_at, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")))
      wsi_abort("Human review timestamps must be explicit UTC ISO-8601 values.")
    if (!is.null(record$action)) {
      record$action <- wsi_review_text(record$action, "review action")
      statuses <- c(confirm = "reviewed", correct = "reviewed", exclude = "excluded", uncertain = "uncertain")
      if (!record$action %in% names(statuses)) wsi_abort("Unknown human review action.")
      if (!identical(record$status, unname(statuses[[record$action]])))
        wsi_abort("Human review action and status disagree.")
    }
    if (any(c("cluster_values", "predictions", "model_sources") %in% names(record)))
      wsi_abort("Human review records must not overwrite raw clusters, predictions or model sources.")
    record
  })
  if (anyDuplicated(vapply(records, `[[`, character(1), "id"))) wsi_abort("Human review records contain duplicate observation IDs.")
  records
}

#' Create, validate and persist an explicit review checkpoint
#' @inheritParams wsi_review_context
#' @param selected_ids Explicit canonical selected IDs; empty means no selection.
#' @param review_records Human labels/status records, separate from source clusters.
#' @param revision Non-negative integer checkpoint revision.
#' @param history Bounded undo/redo lists named past and future.
#' @return A JSON-compatible checkpoint, or a saved local path.
#' @export
wsi_review_checkpoint <- function(context, selected_ids = character(), review_records = list(),
                                  revision = 0L, history = list(past = list(), future = list())) {
  context <- wsi_validate_review_context(context)
  wsi_review_checkpoint_validated(context, selected_ids, review_records, revision, history)
}

# Private field validation for the normalized context from the same public call.
# Do not cache/trust a marker on caller-supplied contexts: every public entry
# point above/below still reconstructs and verifies their complete signature.
wsi_review_checkpoint_validated <- function(context, selected_ids, review_records,
                                            revision, history) {
  ids <- vapply(context$observations, `[[`, character(1), "id")
  selected_ids <- wsi_review_ids(selected_ids, "selected_ids", ids)
  review_records <- wsi_review_records(review_records, ids)
  revision <- wsi_review_number(revision, "revision", integer = TRUE)
  if (revision < 0) wsi_abort("Review checkpoint revision must be non-negative.")
  if (!is.list(history) || !all(c("past", "future") %in% names(history)) ||
      anyDuplicated(names(history)) || any(!names(history) %in% c("past", "future")))
    wsi_abort("Review history must contain past and future snapshot arrays.")
  history <- lapply(history, function(snapshots) {
    snapshots <- wsi_review_rows(snapshots, "review history")
    if (length(snapshots) > getOption("wsiTools.review_max_history", 100L)) wsi_abort("Review undo/redo history exceeds its bounded limit.")
    lapply(snapshots, function(snapshot) list(
      selected_ids = as.list(wsi_review_ids(snapshot$selected_ids, "history selected_ids", ids)),
      review_records = wsi_review_records(snapshot$review_records, ids)))
  })
  list(schema = "wsitools-review-checkpoint", schema_version = 1L,
    context_id = context$context_id, source_signature = context$source_signature,
    selected_ids = as.list(selected_ids), review_records = review_records,
    revision = revision, history = history)
}

#' @rdname wsi_review_checkpoint
#' @param checkpoint A review checkpoint.
#' @export
wsi_validate_review_checkpoint <- function(checkpoint, context) {
  context <- wsi_validate_review_context(context)
  if (!is.list(checkpoint) || !identical(checkpoint$schema, "wsitools-review-checkpoint") ||
      length(checkpoint$schema_version) != 1L || !is.numeric(checkpoint$schema_version) ||
      !is.finite(checkpoint$schema_version) || checkpoint$schema_version != 1)
    wsi_abort("Unsupported review checkpoint schema/version.")
  if (!identical(checkpoint$context_id, context$context_id) ||
      !identical(checkpoint$source_signature, context$source_signature))
    wsi_abort("Review checkpoint belongs to a different or changed context/image source signature.")
  wsi_review_checkpoint_validated(context, checkpoint$selected_ids, checkpoint$review_records,
    checkpoint$revision, checkpoint$history %||% list(past = list(), future = list()))
}

#' @rdname wsi_review_checkpoint
#' @param path Local JSON checkpoint path.
#' @param overwrite Replace an existing checkpoint when true.
#' @export
wsi_write_review_checkpoint <- function(checkpoint, context, path, overwrite = FALSE) {
  checkpoint <- wsi_validate_review_checkpoint(checkpoint, context)
  path <- wsi_validate_output_path(path, overwrite = overwrite)
  temporary <- tempfile(".review-checkpoint-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(checkpoint, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 17)
  if (file.info(temporary)$size > getOption("wsiTools.review_max_checkpoint_bytes", 16 * 1024^2))
    wsi_abort("Review checkpoint exceeds its bounded size limit.")
  if (!isTRUE(.Call("wsi_atomic_replace_file", temporary, path, PACKAGE = "wsiTools")))
    wsi_abort("Could not atomically save the review checkpoint; any previous file was left unchanged.")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' @rdname wsi_review_checkpoint
#' @export
wsi_read_review_checkpoint <- function(path, context) {
  path <- wsi_review_text(path, "checkpoint path")
  if (!file.exists(path) || dir.exists(path)) wsi_abort("Review checkpoint file is inaccessible.")
  if (file.info(path)$size > getOption("wsiTools.review_max_checkpoint_bytes", 16 * 1024^2))
    wsi_abort("Review checkpoint exceeds its bounded size limit.")
  checkpoint <- jsonlite::read_json(path, simplifyVector = FALSE)
  wsi_validate_review_checkpoint(checkpoint, context)
}
