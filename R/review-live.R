# Authenticated live-viewer actions. These only manipulate review metadata.
wsi_review_restore_context <- function(current, candidate, checkpoint = NULL) {
  candidate <- wsi_validate_review_context(candidate)
  if (!is.null(current)) {
    current <- wsi_validate_review_context(current)
    if (!identical(current$experiment_id, candidate$experiment_id) ||
        !identical(current$context_id, candidate$context_id))
      wsi_abort("Saved review belongs to a different experiment/context.")
    old_source <- current$provenance$source_reference
    new_source <- candidate$provenance$source_reference
    if (!identical(old_source$manifest_sha256, new_source$manifest_sha256))
      wsi_abort("Saved review experiment manifest source differs from this viewer.")
    image_key <- function(x) lapply(x$images[order(vapply(x$images, `[[`, character(1), "tissue_id"))], function(i) i[c("tissue_id", "frame_id", "sha256", "width", "height", "mpp_x", "mpp_y", "coordinate_convention")])
    if (!identical(wsi_review_digest(image_key(current)), wsi_review_digest(image_key(candidate))))
      wsi_abort("Saved review images/calibration differ from this viewer. Reopen the matching experiment.")
  }
  wsi_validate_review_context(candidate, verify_hashes = TRUE)
  if (!is.null(checkpoint)) wsi_validate_review_checkpoint(checkpoint, candidate)
  candidate
}

wsi_review_commit_checkpoint <- function(state, checkpoint, ignore_stale = FALSE) {
  checked <- wsi_validate_review_checkpoint(checkpoint, state$review_context)
  current <- state$review_checkpoint
  if (!is.null(current) && identical(current$source_signature, checked$source_signature)) {
    if (checked$revision < current$revision) {
      if (isTRUE(ignore_stale)) return(current)
      wsi_abort("Stale review checkpoint; newer human decisions are already stored.")
    }
    if (checked$revision == current$revision &&
        !identical(wsi_review_digest(current), wsi_review_digest(checked)))
      wsi_abort("Conflicting review checkpoint at the same revision; reopen the authoritative saved state.")
  }
  checked
}

wsi_review_live_request <- function(state, payload) {
  if (!is.list(payload)) wsi_abort("Review request must be an object.")
  action <- payload$action %||% "context"
  context <- state$review_context
  if (is.null(context)) wsi_abort("Open a CellPhenotyper experiment with image identities before reviewing spatial data.")
  if (identical(action, "restore")) {
    candidate <- wsi_review_restore_context(context, payload$context, payload$checkpoint)
    checkpoint <- if (is.null(payload$checkpoint)) wsi_review_checkpoint(candidate) else wsi_validate_review_checkpoint(payload$checkpoint, candidate)
    if (!is.null(state$review_checkpoint)) checkpoint$revision <- max(checkpoint$revision, state$review_checkpoint$revision) + 1L
    state$review_context <- candidate
    state$review_checkpoint <- checkpoint
  } else if (identical(action, "attach_spatial")) {
    if (!isTRUE(payload$trusted)) wsi_abort("Confirm that this local RDS file is trusted before opening it.")
    candidate <- wsi_review_attach_spatial(context, payload$path,
      tissue_id = payload$tissue_id, dataset_id = payload$dataset_id %||% "spatial",
      image_name = payload$image_name, coordinate_mapping = payload$coordinate_mapping,
      trusted = TRUE, cluster_columns = unlist(payload$cluster_columns, use.names = FALSE),
      reduction_names = unlist(payload$reduction_names, use.names = FALSE))
    previous <- state$review_checkpoint %||% wsi_review_checkpoint(context)
    checkpoint <- wsi_review_checkpoint(candidate, selected_ids = previous$selected_ids,
      review_records = previous$review_records, revision = previous$revision + 1L)
    state$review_context <- candidate
    state$review_checkpoint <- checkpoint
  } else if (identical(action, "inspect_spatial")) {
    if (!isTRUE(payload$trusted)) wsi_abort("Confirm the local RDS file is trusted.")
    return(list(ok = TRUE, inspection = wsi_review_inspect_spatial(payload$path, trusted = TRUE,
      image_name = payload$image_name)))
  } else if (identical(action, "checkpoint")) {
    checkpoint <- wsi_review_commit_checkpoint(state, payload$checkpoint)
    state$review_checkpoint <- checkpoint
  } else if (identical(action, "tile_plan")) {
    if (!identical(payload$context_id, context$context_id) ||
        !identical(payload$source_signature, context$source_signature))
      wsi_abort("Review source changed; refresh the selection before planning tiles.")
    plan <- wsi_review_tile_plan(context, selected_ids = unlist(payload$selected_ids, use.names = FALSE),
      tile_size = payload$tile_size %||% 112, units = payload$units %||% "px",
      bounds = payload$bounds %||% "drop", context_px = payload$context_px %||% 0)
    return(list(ok = TRUE, plan = plan))
  } else if (!identical(action, "context")) wsi_abort("Unknown review action.")
  wsi_assign_viewer_state(state)
  list(ok = TRUE, context = state$review_context, checkpoint = state$review_checkpoint)
}
