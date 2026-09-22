wsi_viewer_sync_prepare <- function(state, payload) {
  sync <- payload[["sync", exact = TRUE]]
  if (is.null(sync)) return(NULL)
  if (!is.list(sync) || !identical(sync$version, 1L) && !identical(sync$version, 1)) {
    wsi_abort("Unsupported viewer synchronization protocol.")
  }
  for (name in c("client", "project_key")) {
    value <- sync[[name]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      wsi_abort(sprintf("Viewer synchronization requires a non-empty %s.", name))
    }
  }
  revision <- sync$revision
  if (!is.numeric(revision) || length(revision) != 1L || !is.finite(revision) ||
      revision < 1 || revision != floor(revision)) wsi_abort("Invalid viewer synchronization revision.")
  previous <- (state$browser_sync_clients %||% list())[[sync$client]]
  if (!is.null(previous) && revision <= previous$revision) {
    return(list(replay = TRUE, ack = list(client = sync$client, revision = revision,
      project_key = sync$project_key)))
  }
  if (!isTRUE(sync$full) && (is.null(previous) ||
      !identical(as.numeric(sync$base_revision), as.numeric(previous$revision)) ||
      !identical(sync$project_key, previous$project_key) ||
      !identical(sync$project_key, state$browser_sync_project))) {
    wsi_abort("Viewer synchronization revision mismatch. A full snapshot is required.", class = "wsi_sync_resnapshot")
  }
  if (isTRUE(sync$full) && is.null(payload[["rois", exact = TRUE]])) {
    wsi_abort("A full viewer snapshot must include annotations.")
  }
  patch <- sync$rois_patch
  rois <- NULL
  if (!is.null(patch)) {
    if (!is.list(patch) || !is.list(patch$upsert %||% list())) wsi_abort("Invalid annotation changes.")
    updates <- wsi_rois_from_payload(list(type = "FeatureCollection", features = patch$upsert %||% list()))
    if (anyDuplicated(updates$roi_id)) wsi_abort("Annotation changes contain duplicate IDs.")
    rois <- state$rois %||% wsi_empty_roi()
    previous_order <- rois$roi_id
    removed <- as.character(unlist(patch$remove %||% list(), use.names = FALSE))
    keep <- !rois$roi_id %in% c(removed, updates$roi_id)
    rois <- wsi_viewer_bind_rois(rois[keep, , drop = FALSE], updates)
    order <- if (is.null(patch$order)) {
      c(previous_order[!previous_order %in% removed],
        updates$roi_id[!updates$roi_id %in% previous_order])
    } else as.character(unlist(patch$order, use.names = FALSE))
    if (length(order) != nrow(rois) || anyDuplicated(order) || !setequal(order, rois$roi_id)) {
      wsi_abort("Annotation changes have inconsistent IDs. A full snapshot is required.", class = "wsi_sync_resnapshot")
    }
    rois <- rois[match(order, rois$roi_id), , drop = FALSE]
  }
  list(replay = FALSE, rois = rois, selection = sync$selected_ids,
    selected_id = sync$selected_id, selection_supplied = "selected_ids" %in% names(sync),
    ack = list(client = sync$client, revision = revision, project_key = sync$project_key))
}

wsi_viewer_sync_commit <- function(state, update) {
  if (is.null(update)) return(invisible(NULL))
  if (isTRUE(update$selection_supplied)) {
    ids <- as.character(unlist(update$selection %||% list(), use.names = FALSE))
    selected <- state$rois$roi_id %in% ids
    state$selected_rois <- state$rois[selected, , drop = FALSE]
    state$selected_roi <- state$rois[state$rois$roi_id %in% (update$selected_id %||% character()), , drop = FALSE]
    if (!nrow(state$selected_roi)) state$selected_roi <- NULL
  }
  state$browser_sync_clients <- state$browser_sync_clients %||% list()
  state$browser_sync_clients[[update$ack$client]] <- update$ack
  state$browser_sync_project <- update$ack$project_key
  state$browser_sync_ack <- update$ack
  invisible(NULL)
}
