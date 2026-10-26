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
    dataset_id <- wsi_review_text(payload$dataset_id %||% "spatial", "dataset_id")
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.:-]*$", dataset_id))
      wsi_abort("dataset_id must begin with a letter/digit and contain only letters, digits, underscore, dot, colon or hyphen.")
    previous <- if (!is.null(payload$checkpoint)) wsi_review_commit_checkpoint(state, payload$checkpoint) else wsi_validate_review_checkpoint(state$review_checkpoint %||% wsi_review_checkpoint(context), context)
    persisted <- NULL
    if (isTRUE(payload$persist_to_experiment)) {
      source <- context$provenance$source_reference
      if (!identical(source$type, "cellphenotyper-experiment"))
        wsi_abort("Project storage requires a CellPhenotyper experiment. Open its experiment index first.")
      experiment <- wsi_read_experiment(source$manifest_path)
      if (!identical(experiment$manifest_sha256, source$manifest_sha256))
        wsi_abort("The experiment index changed outside this viewer. Reopen it before adding an object.")
      # A project save must not silently drop another session-only dataset.
      base <- wsi_experiment_review_context(experiment)
      ids <- vapply(context$observations, `[[`, character(1), "id")
      known <- vapply(base$observations, `[[`, character(1), "id")
      import_path <- payload$path
      extra <- context$observations[!ids %in% known]
      if (length(extra)) {
        if (any(!vapply(extra, function(row) identical(row$tissue_id, payload$tissue_id) &&
            identical(row$dataset_id, dataset_id), logical(1))))
          wsi_abort("Use the original dataset ID to save a viewer-only object. Other viewer-only datasets must be saved separately before reopening the experiment.")
        preview <- wsi_review_attach_spatial(base, payload$path, tissue_id = payload$tissue_id,
          dataset_id = dataset_id, image_name = payload$image_name,
          coordinate_mapping = payload$coordinate_mapping, trusted = TRUE,
          cluster_columns = if ("cluster_columns" %in% names(payload)) as.character(unlist(payload$cluster_columns, use.names = FALSE)) else NULL,
          reduction_names = if ("reduction_names" %in% names(payload)) as.character(unlist(payload$reduction_names, use.names = FALSE)) else NULL)
        old <- Filter(function(attachment) identical(attachment$tissue_id, payload$tissue_id) &&
          identical(attachment$dataset_id, dataset_id), context$provenance$spatial_attachments %||% list())
        incoming <- utils::tail(preview$provenance$spatial_attachments, 1L)[[1L]]
        if (length(old) != 1L || !identical(old[[1L]]$source_reference$sha256, incoming$source_reference$sha256))
          wsi_abort("The viewer-only object's RDS bytes changed. Keep the original source object to preserve its reviewed observations.")
        base <- preview
        known <- vapply(base$observations, `[[`, character(1), "id")
        # Pin the exact verified bytes for promotion; later source-file changes
        # cannot swap another object into the project while retaining old labels.
        temporary <- tempfile("wsi-spatial-promotion-"); dir.create(temporary)
        on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
        import_path <- file.path(temporary, "object.rds")
        wsi_experiment_spatial_copy(payload$path, import_path, incoming$source_reference$sha256)
      }
      positions <- match(ids, known)
      if (anyNA(positions) || !identical(wsi_review_digest(context$observations),
          wsi_review_digest(base$observations[positions])))
        wsi_abort("Preserve the existing viewer-only object's metadata and mapping when adding it to the project.")
      reduction_positions <- match(vapply(context$reductions, `[[`, character(1), "id"),
        vapply(base$reductions, `[[`, character(1), "id"))
      if (anyNA(reduction_positions) || !identical(wsi_review_digest(context$reductions),
          wsi_review_digest(base$reductions[reduction_positions])))
        wsi_abort("Preserve the existing viewer reductions when saving an object to the project. Include the viewer-only object's reductions or reopen the unchanged project before importing.")
      persisted <- wsi_experiment_add_spatial(experiment, import_path,
        tissue_id = payload$tissue_id, dataset_id = payload$dataset_id %||% "spatial",
        image_name = payload$image_name, coordinate_mapping = payload$coordinate_mapping,
        trusted = TRUE, cluster_columns = if ("cluster_columns" %in% names(payload)) as.character(unlist(payload$cluster_columns, use.names = FALSE)) else NULL,
        reduction_names = if ("reduction_names" %in% names(payload)) as.character(unlist(payload$reduction_names, use.names = FALSE)) else NULL)
      candidate <- persisted$context
    } else {
      candidate <- wsi_review_attach_spatial(context, payload$path,
        tissue_id = payload$tissue_id, dataset_id = payload$dataset_id %||% "spatial",
        image_name = payload$image_name, coordinate_mapping = payload$coordinate_mapping,
        trusted = TRUE, cluster_columns = if ("cluster_columns" %in% names(payload)) as.character(unlist(payload$cluster_columns, use.names = FALSE)) else NULL,
        reduction_names = if ("reduction_names" %in% names(payload)) as.character(unlist(payload$reduction_names, use.names = FALSE)) else NULL)
    }
    checkpoint <- wsi_review_checkpoint(candidate, selected_ids = previous$selected_ids,
      review_records = previous$review_records, revision = previous$revision + 1L,
      history = previous$history)
    state$review_context <- candidate
    state$review_checkpoint <- checkpoint
    if (!is.null(persisted)) {
      state$review_experiment <- persisted$experiment
      reference <- list(manifest_path = persisted$experiment$manifest_path,
        experiment_id = persisted$experiment$experiment_id,
        manifest_sha256 = persisted$experiment$manifest_sha256)
      wsi_assign_viewer_state(state)
      return(list(ok = TRUE, context = candidate, checkpoint = checkpoint,
        persisted = TRUE, experiment_reference = reference, backup_path = persisted$backup_path,
        object_artifact = persisted$object_artifact, bundle_artifact = persisted$bundle_artifact))
    }
  } else if (identical(action, "inspect_spatial")) {
    if (!isTRUE(payload$trusted)) wsi_abort("Confirm the local RDS file is trusted.")
    return(list(ok = TRUE, inspection = wsi_review_inspect_spatial(payload$path, trusted = TRUE,
      image_name = payload$image_name)))
  } else if (identical(action, "checkpoint")) {
    checkpoint <- wsi_review_commit_checkpoint(state, payload$checkpoint)
    state$review_checkpoint <- checkpoint
    # The browser already owns this immutable context. A selection/review
    # acknowledgement must not serialize every source observation again.
    wsi_assign_viewer_state(state)
    return(list(ok = TRUE, context_id = context$context_id,
      source_signature = context$source_signature, revision = checkpoint$revision,
      checkpoint = checkpoint))
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
