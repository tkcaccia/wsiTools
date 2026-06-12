wsi_empty_tile_preview <- function() {
  out <- data.frame(
    tile_id = character(),
    x = numeric(),
    y = numeric(),
    width = numeric(),
    height = numeric(),
    level = integer(),
    row = integer(),
    col = integer(),
    downsample = numeric(),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_tile_preview", "wsi_tile_manifest", class(out))
  out
}

wsi_tile_preview <- function(grid) {
  if (is.null(grid)) {
    return(wsi_empty_tile_preview())
  }
  if (!is.data.frame(grid)) {
    wsi_abort("Tile preview must be a data frame.")
  }
  needed <- c("x", "y", "width", "height")
  if (!all(needed %in% names(grid))) {
    wsi_abort("Tile preview data must contain `x`, `y`, `width`, and `height` columns.")
  }
  class(grid) <- c(
    "wsi_tile_preview",
    "wsi_tile_manifest",
    setdiff(class(grid), c("wsi_tile_preview", "wsi_tile_manifest"))
  )
  grid
}

wsi_new_viewer_state <- function(name = "wsi_viewer_live_state", envir = parent.frame(),
                                 max_events = 1000L) {
  state <- new.env(parent = emptyenv())
  state$rois <- wsi_empty_roi()
  state$measurements <- wsi_empty_measurements()
  state$trajectories <- wsi_empty_trajectories()
  state$segmentation <- wsi_empty_roi()
  state$roi_summary <- wsi_empty_roi_summary()
  state$cell_summary <- wsi_empty_cell_summary()
  state$class_summary <- wsi_empty_class_summary()
  state$ihc_summary <- wsi_empty_ihc_intensity_summary("roi")
  state$ihc_class_summary <- wsi_empty_ihc_intensity_summary("class")
  state$layers <- list()
  state$project <- list()
  state$project_snapshot <- NULL
  state$tile_preview <- wsi_empty_tile_preview()
  state$prediction <- wsi_empty_prediction_result()
  state$proximity <- wsi_empty_proximity_result()
  state$proximity_stats <- wsi_empty_proximity_stats_result()
  state$trajectory_profile <- wsi_empty_trajectory_profile()
  state$selected_roi <- NULL
  state$selected_rois <- wsi_empty_roi()
  state$selected_object <- NULL
  state$last_segmentation <- NULL
  state$pixel_size <- NULL
  state$view <- list()
  state$stain <- NULL
  state$channel_sources <- list()
  state$channel_settings <- wsi_empty_channel_settings()
  state$tile_sources <- list()
  state$kodama_selection <- list(labels = character(), count = 0L, matched_count = 0L)
  state$seurat_selection <- list(labels = character(), count = 0L, matched_count = 0L)
  state$annotation_spots <- wsi_empty_annotation_spots()
  state$annotations <- list(dirty = FALSE, dirty_reason = "")
  state$history <- wsi_empty_annotation_history()
  state$logs <- wsi_empty_viewer_logs()
  state$jobs <- list()
  state$autosave <- list(enabled = FALSE)
  state$events <- list()
  state$commands <- list()
  state$command_sequence <- 0L
  state$ws_clients <- list()
  state$ws_sequence <- 0L
  state$callbacks <- list()
  state$callback_sequence <- 0L
  state$callback_errors <- list()
  state$dispatching_callback <- FALSE
  state$last_event <- NULL
  state$last_payload <- NULL
  state$last_sync <- NULL
  state$export_name <- name
  state$export_envir <- envir
  state$max_events <- as.integer(max_events)
  class(state) <- c("wsi_viewer_state", "environment")
  wsi_assign_viewer_state(state)
  state
}

wsi_viewer_autosave_config <- function(autosave = FALSE, path = NULL,
                                       interval = 5, overwrite = TRUE,
                                       name = "wsi_viewer_live_state") {
  if (is.character(autosave) && length(autosave) == 1L && !is.na(autosave) && nzchar(autosave)) {
    path <- autosave
    autosave <- TRUE
  }
  if (!is.logical(autosave) || length(autosave) != 1L || is.na(autosave)) {
    wsi_abort("`autosave` must be `TRUE`, `FALSE`, or a single project directory path.")
  }
  interval <- as.numeric(wsi_check_scalar_number(interval, "autosave_interval", allow_zero = FALSE))
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    wsi_abort("`autosave_overwrite` must be `TRUE` or `FALSE`.")
  }
  if (!isTRUE(autosave)) {
    return(list(enabled = FALSE, interval = interval, overwrite = overwrite))
  }
  if (is.null(path)) {
    path <- paste0(wsi_safe_id(name, "wsi_viewer_live_state"), "_autosave.wsiproject")
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    wsi_abort("`autosave_path` must be `NULL` or a single non-empty project directory path.")
  }
  list(
    enabled = TRUE,
    path = path,
    interval = interval,
    interval_ms = as.integer(max(1000, round(interval * 1000))),
    overwrite = overwrite,
    last_save = NULL,
    last_error = NULL,
    last_error_time = NULL,
    last_event = NULL,
    last_project = NULL,
    count = 0L
  )
}

wsi_viewer_autosave_status <- function(state) {
  autosave <- state$autosave %||% list(enabled = FALSE)
  autosave$last_save <- as.character(autosave$last_save %||% NA_character_)
  autosave
}

wsi_viewer_allowed_events <- function() {
  c(
    "viewer_state", "viewer_loaded", "autosave_tick", "autosave_unload",
    "project_save_requested", "project_saved", "project_opened",
    "project_image_added", "project_image_reordered", "project_image_closed",
    "project_image_selected", "project_section_selected",
    "roi_added", "roi_created", "roi_selected", "roi_deselected",
    "roi_updated", "roi_edited", "roi_curve_edited", "roi_brush_edited", "roi_deleted",
    "roi_duplicated", "roi_exported", "roi_export_selection_updated",
    "roi_color_updated", "roi_visibility_updated", "roi_lock_updated",
    "roi_smoothed", "roi_simplified", "roi_holes_filled", "roi_split",
    "roi_same_label_merged", "rois_merged", "brush_selection_updated",
    "brush_committed", "viewport_changed",
    "geojson_imported", "class_export_rules_updated",
    "annotations_dirty", "annotations_saved",
    "annotation_history_updated", "annotation_history_cleared",
    "viewer_log_updated", "viewer_log_cleared", "viewer_log_exported",
    "annotation_spots_exported", "annotation_spots_updated",
    "measurement_added", "measurements_cleared",
    "trajectory_added", "trajectory_deleted", "trajectory_area_created", "trajectory_area_updated",
    "trajectories_cleared",
    "stain_updated", "image_transform_updated",
    "layer_added", "layer_removed", "layer_updated", "layer_visibility_updated",
    "layer_opacity_updated", "tile_grid_toggled",
    "multi_view_layout_updated", "multi_view_pane_replaced", "multi_view_sync_updated",
    "tile_preview_created", "tile_preview_cleared", "tile_preview_exported", "tiles_extracted",
    "channel_source_added", "channel_source_removed", "channel_updated",
    "artifact_detected", "artifact_flagged", "artifact_overlay_toggled",
    "artifact_sensitivity_updated", "artifacts_cleared",
    "grandqc_loaded", "grandqc_cleared",
    "kodama_cells_selected", "seurat_spots_selected", "seurat_gene_coloured",
    "seurat_cluster_coloured", "seurat_plot_scope_changed",
    "prediction_started", "prediction_finished", "prediction_failed",
    "prediction_cleared",
    "proximity_started", "proximity_finished", "proximity_failed",
    "proximity_cleared", "proximity_stats_started",
    "proximity_stats_finished", "proximity_stats_failed",
    "proximity_stats_cleared", "proximity_stats_exported",
    "trajectory_profile_started", "trajectory_profile_finished",
    "trajectory_profile_failed", "trajectory_profile_cleared",
    "ihc_intensity_measured",
    "segmentation_requested", "segmentation_started", "segmentation_progress",
    "segmentation_added", "segmentation_completed",
    "segmentation_finished", "segmentation_cleared", "segmentation_failed",
    "tiles_started", "tiles_finished", "tiles_failed", "conversion_started",
    "conversion_finished", "conversion_failed", "pyramid_started",
    "pyramid_finished", "pyramid_failed", "job_status",
    "job_failed", "r_add_rois", "r_add_segmentation", "r_add_layer",
    "r_set_layer_visible", "r_remove_layer", "r_add_channel_source",
    "r_remove_channel_source", "r_set_channel_settings",
    "r_restore_project_state"
  )
}

wsi_viewer_allowed_payload_fields <- function() {
  c(
    "event", "time", "sequence", "slide", "project", "selected_index",
    "selected_roi", "selected_rois", "selected_object", "rois",
    "segmentation", "layers", "measurements", "trajectories",
    "artifacts", "view", "annotations", "history", "logs", "stain",
    "channel_sources", "channel_settings",
    "tile_sources", "kodama_selection", "seurat_selection",
    "annotation_spots", "detail"
  )
}

wsi_viewer_validate_selected_object <- function(selected_object) {
  if (is.null(selected_object)) {
    return(invisible(NULL))
  }
  if (!is.list(selected_object) || is.data.frame(selected_object)) {
    wsi_abort("Viewer state payload `selected_object` must be a JSON object or null when supplied.")
  }
  object_type <- selected_object[["type", exact = TRUE]]
  if (!is.null(object_type)) {
    allowed_types <- c("annotation", "trajectory", "layer_object", "measurement")
    if (!is.character(object_type) || length(object_type) != 1L ||
        is.na(object_type) || !object_type %in% allowed_types) {
      wsi_abort(sprintf(
        "Viewer state payload `selected_object$type` must be one of: %s.",
        paste(allowed_types, collapse = ", ")
      ))
    }
  }
  for (field in c("index", "layer_index", "item_index")) {
    value <- selected_object[[field, exact = TRUE]]
    if (!is.null(value) &&
        (!is.numeric(value) || length(value) != 1L || is.na(value))) {
      wsi_abort(sprintf(
        "Viewer state payload `selected_object$%s` must be a single number when supplied.",
        field
      ))
    }
  }
  invisible(NULL)
}

wsi_viewer_validate_event <- function(event) {
  if (is.null(event)) {
    return("viewer_state")
  }
  if (!is.character(event) || length(event) != 1L || is.na(event) || !nzchar(event)) {
    wsi_abort("Viewer event must be a single non-empty string.")
  }
  event <- as.character(event)
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", event)) {
    wsi_abort(sprintf("Viewer event `%s` contains invalid characters.", event))
  }
  allowed <- wsi_viewer_allowed_events()
  if (!event %in% allowed) {
    wsi_abort(sprintf(
      "Unsupported viewer event `%s`. Expected one of: %s.",
      event,
      paste(allowed, collapse = ", ")
    ))
  }
  event
}

wsi_viewer_validate_state_payload <- function(payload) {
  if (!is.list(payload)) {
    wsi_abort("Viewer state payload must be a JSON object.")
  }
  unknown <- setdiff(names(payload), wsi_viewer_allowed_payload_fields())
  if (length(unknown)) {
    wsi_abort(sprintf(
      "Viewer state payload contains unsupported field%s: %s.",
      if (length(unknown) == 1L) "" else "s",
      paste(unknown, collapse = ", ")
    ))
  }
  payload$event <- wsi_viewer_validate_event(payload[["event", exact = TRUE]] %||% "viewer_state")
  sequence <- payload[["sequence", exact = TRUE]]
  if (!is.null(sequence) &&
      (!is.numeric(sequence) || length(sequence) != 1L || is.na(sequence))) {
    wsi_abort("Viewer state payload `sequence` must be a single number when supplied.")
  }
  time <- payload[["time", exact = TRUE]]
  if (!is.null(time) &&
      (!is.character(time) || length(time) != 1L || is.na(time))) {
    wsi_abort("Viewer state payload `time` must be a single string when supplied.")
  }
  for (field in c("rois", "selected_rois", "segmentation")) {
    value <- payload[[field, exact = TRUE]]
    if (!is.null(value) && is.list(value) && !is.null(value$type) &&
        !identical(value$type, "FeatureCollection")) {
      wsi_abort(sprintf("Viewer state payload `%s` must be a GeoJSON FeatureCollection.", field))
    }
  }
  selected <- payload[["selected_roi", exact = TRUE]]
  if (!is.null(selected) && is.list(selected) && !is.null(selected$type) &&
      !identical(selected$type, "Feature")) {
    wsi_abort("Viewer state payload `selected_roi` must be a GeoJSON Feature when supplied.")
  }
  wsi_viewer_validate_selected_object(payload[["selected_object", exact = TRUE]])
  payload
}

wsi_viewer_autosave_due <- function(state, force = FALSE) {
  autosave <- state$autosave %||% list(enabled = FALSE)
  if (!isTRUE(autosave$enabled)) {
    return(FALSE)
  }
  if (isTRUE(force) || is.null(autosave$last_save)) {
    return(TRUE)
  }
  interval <- as.numeric(autosave$interval %||% 5)
  last <- autosave$last_save
  if (is.character(last)) {
    last <- suppressWarnings(as.POSIXct(last))
  }
  is.na(last) || as.numeric(difftime(Sys.time(), last, units = "secs")) >= interval
}

wsi_viewer_autosave_save <- function(state, slide = NULL, force = FALSE,
                                     reason = "autosave") {
  if (!inherits(state, "wsi_viewer_state")) {
    return(invisible(NULL))
  }
  autosave <- state$autosave %||% list(enabled = FALSE)
  if (!isTRUE(autosave$enabled) || !wsi_viewer_autosave_due(state, force = force)) {
    return(invisible(NULL))
  }
  now <- Sys.time()
  project <- tryCatch(
    wsi_project(
      autosave$path,
      slide = slide,
      viewer_state = state,
      metadata = list(
        autosave = list(
          enabled = TRUE,
          reason = reason,
          last_event = state$last_event %||% NA_character_,
          selected_roi_id = if (inherits(state$selected_roi, "wsi_roi") && nrow(state$selected_roi)) {
            state$selected_roi$roi_id[[1L]]
          } else {
            NA_character_
          }
        )
      ),
      processing_provenance = list(
        autosave = TRUE,
        autosave_reason = reason,
        autosave_interval_seconds = autosave$interval %||% NA_real_
      ),
      overwrite = autosave$overwrite %||% TRUE
    ),
    error = function(err) err
  )
  if (inherits(project, "error")) {
    autosave$last_error <- conditionMessage(project)
    autosave$last_error_time <- format(now, "%Y-%m-%dT%H:%M:%OS%z")
    state$autosave <- autosave
    wsi_assign_viewer_state(state)
    return(invisible(NULL))
  }
  autosave$last_save <- now
  autosave$last_error <- NULL
  autosave$last_error_time <- NULL
  autosave$last_event <- state$last_event %||% reason
  autosave$last_project <- project$path %||% autosave$path
  autosave$count <- as.integer(autosave$count %||% 0L) + 1L
  state$autosave <- autosave
  wsi_assign_viewer_state(state)
  invisible(project)
}

wsi_viewer_queue_command <- function(state, type, payload = list()) {
  if (!inherits(state, "wsi_viewer_state")) {
    wsi_abort("`state` must be a `wsi_viewer_state` object.")
  }
  if (!is.character(type) || length(type) != 1L || is.na(type) || !nzchar(type)) {
    wsi_abort("Viewer command `type` must be a single non-empty string.")
  }
  allowed <- c(
    "job_update", "add_rois", "add_segmentation", "add_layer",
    "set_layer_visible", "remove_layer", "annotations_saved",
    "add_channel_source", "remove_channel_source", "set_channel_settings",
    "colour_spots_by_gene", "restore_project_state"
  )
  if (!type %in% allowed) {
    wsi_abort(sprintf(
      "Unsupported viewer command `%s`. Expected one of: %s.",
      type,
      paste(allowed, collapse = ", ")
    ))
  }
  state$command_sequence <- as.integer(state$command_sequence %||% 0L) + 1L
  command <- list(
    id = sprintf("cmd_%d", state$command_sequence),
    type = type,
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
    payload = payload %||% list()
  )
  state$commands[[length(state$commands) + 1L]] <- command
  wsi_viewer_send_ws(state, list(ok = TRUE, commands = list(command)))
  invisible(command)
}

wsi_viewer_take_commands <- function(state) {
  commands <- state$commands %||% list()
  state$commands <- list()
  commands
}

wsi_viewer_ws_json <- function(body) {
  jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
}

wsi_viewer_send_ws_one <- function(ws, body) {
  if (is.null(ws)) {
    return(FALSE)
  }
  ok <- tryCatch({
    ws$send(wsi_viewer_ws_json(body))
    TRUE
  }, error = function(err) FALSE)
  isTRUE(ok)
}

wsi_viewer_send_ws <- function(state, body) {
  clients <- state$ws_clients %||% list()
  if (!length(clients)) {
    return(invisible(FALSE))
  }
  keep <- rep(TRUE, length(clients))
  for (i in seq_along(clients)) {
    ws <- clients[[i]]$ws %||% clients[[i]]
    keep[[i]] <- wsi_viewer_send_ws_one(ws, body)
  }
  state$ws_clients <- clients[keep]
  invisible(any(keep))
}

wsi_viewer_event_aliases <- function(event) {
  switch(
    event,
    roi_added = "roi_created",
    segmentation_added = "segmentation_finished",
    segmentation_completed = "segmentation_finished",
    character()
  )
}

wsi_viewer_event_names <- function(event) {
  unique(c(event, wsi_viewer_event_aliases(event)))
}

wsi_viewer_callback_object <- function(callback_event, state, payload) {
  if (callback_event %in% c("roi_created", "roi_added", "roi_selected")) {
    return(state$selected_roi)
  }
  if (callback_event %in% c("segmentation_finished", "segmentation_added", "segmentation_completed")) {
    return(state$segmentation)
  }
  if (callback_event %in% c("layer_added", "layer_removed", "layer_visibility_updated", "r_add_layer")) {
    detail <- payload$detail %||% list()
    key <- detail$id %||% detail$name %||% payload$id %||% payload$name
    layer <- wsi_viewer_get_layer(state$layers, key)
    if (!is.null(layer)) {
      return(layer)
    }
    return(state$layers)
  }
  if (callback_event %in% c("measurement_added")) {
    if (nrow(state$measurements)) {
      return(utils::tail(state$measurements, 1L))
    }
    return(state$measurements)
  }
  if (callback_event %in% c("trajectory_added")) {
    if (nrow(state$trajectories %||% wsi_empty_trajectories())) {
      return(utils::tail(state$trajectories, 1L))
    }
    return(state$trajectories %||% wsi_empty_trajectories())
  }
  if (callback_event %in% c("trajectory_profile_finished")) {
    return(state$trajectory_profile %||% wsi_empty_trajectory_profile())
  }
  if (callback_event %in% c("annotation_spots_exported", "annotation_spots_updated")) {
    return(state$annotation_spots %||% wsi_empty_annotation_spots())
  }
  wsi_viewer_state(state)
}

wsi_viewer_callback_args <- function(callback, object, event, state, payload) {
  formals <- tryCatch(formals(callback), error = function(err) NULL)
  args <- list(object, event, state, payload)
  if (is.null(formals)) {
    return(list(object))
  }
  if (!length(formals)) {
    return(list())
  }
  if ("..." %in% names(formals)) {
    return(list(object, event = event, state = state, payload = payload))
  }
  unname(args[seq_len(min(length(formals), length(args)))])
}

wsi_dispatch_viewer_callbacks <- function(state, payload) {
  callbacks <- state$callbacks %||% list()
  if (!length(callbacks)) {
    return(invisible(state))
  }
  state$dispatching_callback <- TRUE
  on.exit({
    state$dispatching_callback <- FALSE
  }, add = TRUE)
  event <- as.character(state$last_event %||% payload[["event", exact = TRUE]] %||% "viewer_state")
  event_names <- wsi_viewer_event_names(event)
  keep <- rep(TRUE, length(callbacks))

  for (i in seq_along(callbacks)) {
    callback <- callbacks[[i]]
    if (!callback$event %in% event_names) {
      next
    }
    object <- wsi_viewer_callback_object(callback$event, state, payload)
    err <- tryCatch(
      {
        do.call(
          callback$callback,
          wsi_viewer_callback_args(callback$callback, object, event, state, payload)
        )
        NULL
      },
      error = function(err) err
    )
    if (!is.null(err)) {
      state$callback_errors[[length(state$callback_errors) + 1L]] <- list(
        id = callback$id,
        event = callback$event,
        message = conditionMessage(err),
        time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
      )
      wsi_warn(sprintf(
        "Viewer callback `%s` for event `%s` failed: %s",
        callback$id,
        callback$event,
        conditionMessage(err)
      ))
    }
    if (isTRUE(callback$once)) {
      keep[[i]] <- FALSE
    }
  }
  state$callbacks <- callbacks[keep]
  invisible(state)
}

wsi_empty_measurements <- function() {
  data.frame(
    id = character(),
    start_x = numeric(),
    start_y = numeric(),
    end_x = numeric(),
    end_y = numeric(),
    distance_px = numeric(),
    distance_um = numeric(),
    stringsAsFactors = FALSE
  )
}

wsi_empty_trajectories <- function() {
  out <- data.frame(
    id = character(),
    name = character(),
    n = integer(),
    control_count = integer(),
    point_count = integer(),
    length_px = numeric(),
    area_width_px = numeric(),
    area_roi_id = character(),
    created = character(),
    stringsAsFactors = FALSE
  )
  out$control_points <- I(list())
  out$points <- I(list())
  class(out) <- c("wsi_trajectories", class(out))
  out
}

wsi_viewer_points_from_payload <- function(points) {
  if (is.null(points) || !length(points)) {
    return(data.frame(x = numeric(), y = numeric()))
  }
  if (is.data.frame(points)) {
    if (!all(c("x", "y") %in% names(points))) {
      return(data.frame(x = numeric(), y = numeric()))
    }
    return(data.frame(
      x = suppressWarnings(as.numeric(points$x)),
      y = suppressWarnings(as.numeric(points$y)),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(points, function(point) {
    if (!is.list(point)) {
      return(NULL)
    }
    data.frame(
      x = suppressWarnings(as.numeric(point$x %||% NA_real_)),
      y = suppressWarnings(as.numeric(point$y %||% NA_real_)),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(x = numeric(), y = numeric()))
  }
  out <- do.call(rbind, rows)
  out[is.finite(out$x) & is.finite(out$y), , drop = FALSE]
}

wsi_trajectories_from_payload <- function(trajectories) {
  if (is.null(trajectories) || !length(trajectories)) {
    return(wsi_empty_trajectories())
  }
  if (is.data.frame(trajectories) && all(c("id", "name") %in% names(trajectories))) {
    out <- as.data.frame(trajectories, stringsAsFactors = FALSE)
    for (column in c("n", "control_count", "point_count")) {
      if (!column %in% names(out)) {
        out[[column]] <- NA_integer_
      }
      out[[column]] <- suppressWarnings(as.integer(out[[column]]))
    }
    if (!"length_px" %in% names(out)) {
      out$length_px <- NA_real_
    }
    out$length_px <- suppressWarnings(as.numeric(out$length_px))
    if (!"area_width_px" %in% names(out)) {
      out$area_width_px <- NA_real_
    }
    out$area_width_px <- suppressWarnings(as.numeric(out$area_width_px))
    if (!"area_roi_id" %in% names(out)) {
      out$area_roi_id <- NA_character_
    }
    out$area_roi_id <- as.character(out$area_roi_id)
    if (!"created" %in% names(out)) {
      out$created <- NA_character_
    }
    if (!"control_points" %in% names(out)) {
      out$control_points <- I(rep(list(data.frame(x = numeric(), y = numeric())), nrow(out)))
    } else if (!inherits(out$control_points, "AsIs")) {
      out$control_points <- I(as.list(out$control_points))
    }
    if (!"points" %in% names(out)) {
      out$points <- I(rep(list(data.frame(x = numeric(), y = numeric())), nrow(out)))
    } else if (!inherits(out$points, "AsIs")) {
      out$points <- I(as.list(out$points))
    }
    out <- out[, c("id", "name", "n", "control_count", "point_count", "length_px", "area_width_px", "area_roi_id", "created", "control_points", "points"), drop = FALSE]
    class(out) <- c("wsi_trajectories", setdiff(class(out), "wsi_trajectories"))
    return(out)
  }
  entries <- if (is.list(trajectories)) trajectories else as.list(trajectories)
  rows <- vector("list", length(entries))
  control_points <- vector("list", length(entries))
  points <- vector("list", length(entries))
  keep <- logical(length(entries))
  for (i in seq_along(entries)) {
    entry <- entries[[i]]
    if (!is.list(entry)) {
      next
    }
    control <- wsi_viewer_points_from_payload(entry$control_points %||% entry$controls)
    sampled <- wsi_viewer_points_from_payload(entry$points %||% entry$xy)
    rows[[i]] <- data.frame(
      id = as.character(entry$id %||% sprintf("trajectory_%d", i)),
      name = as.character(entry$name %||% sprintf("Trajectory %d", i)),
      n = suppressWarnings(as.integer(entry$n %||% nrow(sampled))),
      control_count = nrow(control),
      point_count = nrow(sampled),
      length_px = suppressWarnings(as.numeric(entry$length_px %||% NA_real_)),
      area_width_px = suppressWarnings(as.numeric(entry$area_width_px %||% NA_real_)),
      area_roi_id = as.character(entry$area_roi_id %||% NA_character_),
      created = as.character(entry$created %||% NA_character_),
      stringsAsFactors = FALSE
    )
    control_points[[i]] <- control
    points[[i]] <- sampled
    keep[[i]] <- TRUE
  }
  if (!any(keep)) {
    return(wsi_empty_trajectories())
  }
  out <- do.call(rbind, rows[keep])
  out$control_points <- I(control_points[keep])
  out$points <- I(points[keep])
  class(out) <- c("wsi_trajectories", class(out))
  out
}

wsi_trajectory_points_payload <- function(points) {
  if (is.null(points)) {
    return(list())
  }
  points <- as.data.frame(points, stringsAsFactors = FALSE)
  if (!all(c("x", "y") %in% names(points)) || !nrow(points)) {
    return(list())
  }
  lapply(seq_len(nrow(points)), function(i) {
    list(
      x = unname(as.numeric(points$x[[i]])),
      y = unname(as.numeric(points$y[[i]]))
    )
  })
}

wsi_trajectories_to_payload <- function(trajectories) {
  trajectories <- wsi_trajectories_from_payload(trajectories)
  if (!nrow(trajectories)) {
    return(list())
  }
  lapply(seq_len(nrow(trajectories)), function(i) {
    list(
      id = trajectories$id[[i]],
      name = trajectories$name[[i]],
      n = trajectories$n[[i]],
      length_px = trajectories$length_px[[i]],
      area_width_px = trajectories$area_width_px[[i]],
      area_roi_id = trajectories$area_roi_id[[i]],
      created = trajectories$created[[i]],
      control_points = wsi_trajectory_points_payload(trajectories$control_points[[i]]),
      points = wsi_trajectory_points_payload(trajectories$points[[i]])
    )
  })
}

wsi_empty_roi_summary <- function() {
  data.frame(
    roi_id = character(),
    roi_name = character(),
    roi_class = character(),
    object_type = character(),
    xmin = numeric(),
    ymin = numeric(),
    xmax = numeric(),
    ymax = numeric(),
    area_px2 = numeric(),
    area_um2 = numeric(),
    area_mm2 = numeric(),
    percent_total_area = numeric(),
    cell_count = integer(),
    cells_per_px2 = numeric(),
    cells_per_mm2 = numeric(),
    stringsAsFactors = FALSE
  )
}

wsi_empty_cell_summary <- function() {
  data.frame(
    cell_id = character(),
    cell_name = character(),
    cell_class = character(),
    source = character(),
    x = numeric(),
    y = numeric(),
    area_px2 = numeric(),
    roi_id = character(),
    roi_name = character(),
    roi_class = character(),
    inside_roi = logical(),
    distance_to_roi_boundary_px = numeric(),
    distance_to_roi_boundary_um = numeric(),
    stringsAsFactors = FALSE
  )
}

wsi_empty_class_summary <- function() {
  data.frame(
    class = character(),
    area_px2 = numeric(),
    roi_count = integer(),
    percent_area = numeric(),
    area_um2 = numeric(),
    area_mm2 = numeric(),
    cell_count = integer(),
    cells_per_mm2 = numeric(),
    cells_per_px2 = numeric(),
    stringsAsFactors = FALSE
  )
}

wsi_viewer_merge_ihc_roi_summary <- function(summary, ihc_summary) {
  if (!is.data.frame(summary) || !nrow(summary) ||
      !is.data.frame(ihc_summary) || !nrow(ihc_summary) ||
      !"roi_id" %in% names(ihc_summary)) {
    return(summary)
  }
  cols <- setdiff(names(ihc_summary), c("roi_name", "roi_class"))
  merge(summary, ihc_summary[, cols, drop = FALSE], by = "roi_id", all.x = TRUE, sort = FALSE)
}

wsi_viewer_merge_ihc_class_summary <- function(summary, ihc_class_summary) {
  if (!is.data.frame(summary) || !nrow(summary) ||
      !is.data.frame(ihc_class_summary) || !nrow(ihc_class_summary) ||
      !"class" %in% names(ihc_class_summary)) {
    return(summary)
  }
  merge(summary, ihc_class_summary, by = "class", all.x = TRUE, sort = FALSE)
}

wsi_viewer_measurement_column <- function(name) {
  name <- trimws(as.character(name %||% "measurement"))
  if (!nzchar(name)) {
    name <- "measurement"
  }
  name <- gsub("[^A-Za-z0-9]+", "_", name)
  name <- gsub("^_+|_+$", "", name)
  paste0("measurement_", name)
}

wsi_viewer_measurement_values <- function(measurements) {
  if (is.null(measurements) || !length(measurements)) {
    return(list())
  }
  out <- list()
  if (!is.null(names(measurements)) && any(nzchar(names(measurements)))) {
    for (name in names(measurements)) {
      value <- measurements[[name]]
      if (is.list(value) && !is.null(value$value)) {
        value <- value$value
      }
      if (length(value) == 1L && !is.list(value)) {
        out[[wsi_viewer_measurement_column(name)]] <- value
      }
    }
    return(out)
  }
  for (entry in measurements) {
    if (!is.list(entry)) {
      next
    }
    name <- entry$name %||% entry$key %||% entry$id %||% NULL
    value <- entry$value %||% entry$measurement %||% NULL
    if (!is.null(name) && length(value) == 1L && !is.list(value)) {
      out[[wsi_viewer_measurement_column(name)]] <- value
    }
  }
  out
}

wsi_viewer_measurement_table <- function(measurements) {
  n <- length(measurements %||% list())
  if (!n) {
    return(data.frame())
  }
  rows <- lapply(measurements, wsi_viewer_measurement_values)
  cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  if (!length(cols)) {
    return(data.frame(row.names = seq_len(n)))
  }
  out <- as.data.frame(matrix(NA_real_, nrow = n, ncol = length(cols)))
  names(out) <- cols
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    for (col in names(row)) {
      value <- row[[col]]
      out[[col]][[i]] <- suppressWarnings(as.numeric(value))
      if (is.na(out[[col]][[i]]) && !is.na(value)) {
        out[[col]] <- as.character(out[[col]])
        out[[col]][[i]] <- as.character(value)
      }
    }
  }
  out
}

wsi_viewer_roi_centroid <- function(rois, index) {
  properties <- if ("properties" %in% names(rois)) rois$properties[[index]] else list()
  centroid <- properties$centroid %||% properties$center %||% NULL
  if (is.list(centroid) && all(c("x", "y") %in% names(centroid))) {
    xy <- c(as.numeric(centroid$x), as.numeric(centroid$y))
    if (all(is.finite(xy))) {
      return(xy)
    }
  }
  c(
    mean(c(rois$xmin[[index]], rois$xmax[[index]])),
    mean(c(rois$ymin[[index]], rois$ymax[[index]]))
  )
}

wsi_viewer_cell_points <- function(segmentation) {
  if (!inherits(segmentation, "wsi_roi") || !nrow(segmentation)) {
    return(NULL)
  }
  points <- t(vapply(seq_len(nrow(segmentation)), function(i) {
    wsi_viewer_roi_centroid(segmentation, i)
  }, numeric(2)))
  colnames(points) <- c("x", "y")
  points
}

wsi_viewer_source_column <- function(rois, index) {
  props <- if ("properties" %in% names(rois)) rois$properties[[index]] else list()
  source <- props$source %||% props$source_file %||% NA_character_
  as.character(source %||% NA_character_)
}

wsi_viewer_is_segmentation_roi <- function(rois, index) {
  source <- tolower(wsi_viewer_source_column(rois, index))
  class <- tolower(as.character(if ("class" %in% names(rois)) rois$class[[index]] else ""))
  object_type <- tolower(as.character(if ("object_type" %in% names(rois)) rois$object_type[[index]] else ""))
  grepl("stardist|segmentation|cellpose", source) ||
    class %in% c("cell", "cells", "nucleus", "nuclei") ||
    object_type %in% c("detection", "cell", "nucleus")
}

wsi_viewer_annotation_rois <- function(rois) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    return(wsi_empty_roi())
  }
  keep <- !vapply(seq_len(nrow(rois)), function(i) wsi_viewer_is_segmentation_roi(rois, i), logical(1))
  out <- rois[keep, , drop = FALSE]
  class(out) <- unique(c("wsi_roi", class(out)))
  out
}

wsi_viewer_cell_summary <- function(segmentation, rois = NULL, pixel_size = NULL) {
  if (!inherits(segmentation, "wsi_roi") || !nrow(segmentation)) {
    return(wsi_empty_cell_summary())
  }
  points <- wsi_viewer_cell_points(segmentation)
  px <- wsi_pixel_size_xy(pixel_size)
  rows <- lapply(seq_len(nrow(segmentation)), function(i) {
    point <- points[i, ]
    area_px2 <- tryCatch(wsi_roi_area_px(segmentation, i), error = function(err) NA_real_)
    roi_index <- NA_integer_
    inside <- FALSE
    distance_px <- NA_real_
    if (inherits(rois, "wsi_roi") && nrow(rois)) {
      inside_hits <- which(vapply(seq_len(nrow(rois)), function(j) {
        tryCatch(wsi_point_in_roi(point, rois, j), error = function(err) FALSE)
      }, logical(1)))
      if (length(inside_hits)) {
        roi_index <- inside_hits[[1L]]
        inside <- TRUE
        distance_px <- tryCatch(wsi_point_roi_boundary_distance(point, rois, roi_index), error = function(err) NA_real_)
      } else {
        distances <- vapply(seq_len(nrow(rois)), function(j) {
          tryCatch(wsi_point_roi_boundary_distance(point, rois, j), error = function(err) Inf)
        }, numeric(1))
        if (length(distances) && any(is.finite(distances))) {
          roi_index <- which.min(distances)
          distance_px <- distances[[roi_index]]
        }
      }
    }
    data.frame(
      cell_id = segmentation$roi_id[[i]] %||% sprintf("cell_%d", i),
      cell_name = segmentation$name[[i]] %||% sprintf("cell_%d", i),
      cell_class = wsi_roi_class(segmentation$class[[i]] %||% "cell"),
      source = wsi_viewer_source_column(segmentation, i),
      x = point[["x"]],
      y = point[["y"]],
      area_px2 = area_px2,
      roi_id = if (is.na(roi_index)) NA_character_ else rois$roi_id[[roi_index]],
      roi_name = if (is.na(roi_index)) NA_character_ else rois$name[[roi_index]],
      roi_class = if (is.na(roi_index)) NA_character_ else wsi_roi_class(rois$class[[roi_index]]),
      inside_roi = inside,
      distance_to_roi_boundary_px = distance_px,
      distance_to_roi_boundary_um = if (is.null(px) || is.na(distance_px)) NA_real_ else distance_px * mean(px),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  measurement_cols <- wsi_viewer_measurement_table(segmentation$measurements %||% list())
  if (ncol(measurement_cols)) {
    out <- cbind(out, measurement_cols)
  }
  out
}

wsi_viewer_roi_summary <- function(rois, segmentation = NULL, pixel_size = NULL) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    return(wsi_empty_roi_summary())
  }
  summary <- wsi_roi_measurement_table(rois, pixel_size = pixel_size)
  cells <- wsi_viewer_cell_points(segmentation)
  if (!is.null(cells) && nrow(cells)) {
    density <- measure_cell_density(as.data.frame(cells), rois, pixel_size = pixel_size)
    summary <- merge(
      summary,
      density[, c("roi_id", "cell_count", "cells_per_px2", "cells_per_mm2"), drop = FALSE],
      by = "roi_id",
      all.x = TRUE,
      sort = FALSE
    )
    summary$cell_count[is.na(summary$cell_count)] <- 0L
  } else {
    summary$cell_count <- 0L
    summary$cells_per_px2 <- ifelse(summary$area_px2 > 0, 0, NA_real_)
    summary$cells_per_mm2 <- NA_real_
  }
  measurement_cols <- wsi_viewer_measurement_table(rois$measurements %||% list())
  if (ncol(measurement_cols)) {
    summary <- cbind(summary, measurement_cols)
  }
  summary
}

wsi_viewer_class_summary <- function(rois, segmentation = NULL, pixel_size = NULL) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    return(wsi_empty_class_summary())
  }
  cells <- wsi_viewer_cell_points(segmentation)
  summary <- summarise_rois(
    rois,
    cells = if (!is.null(cells) && nrow(cells)) as.data.frame(cells) else NULL,
    pixel_size = pixel_size
  )
  if (!"cell_count" %in% names(summary)) {
    summary$cell_count <- 0L
    summary$cells_per_mm2 <- NA_real_
    summary$cells_per_px2 <- ifelse(summary$area_px2 > 0, 0, NA_real_)
  }
  summary
}

wsi_viewer_update_measurement_tables <- function(state) {
  if (!inherits(state, "wsi_viewer_state")) {
    return(invisible(NULL))
  }
  pixel_size <- state$pixel_size %||% NULL
  if (!is.null(pixel_size)) {
    numeric_pixel_size <- suppressWarnings(as.numeric(unlist(pixel_size, use.names = FALSE)))
    if (!length(numeric_pixel_size) || anyNA(numeric_pixel_size) || any(!is.finite(numeric_pixel_size))) {
      pixel_size <- NULL
    }
  }
  annotation_rois <- wsi_viewer_annotation_rois(state$rois)
  state$cell_summary <- wsi_viewer_cell_summary(state$segmentation, annotation_rois, pixel_size = pixel_size)
  state$roi_summary <- wsi_viewer_roi_summary(annotation_rois, state$segmentation, pixel_size = pixel_size)
  state$class_summary <- wsi_viewer_class_summary(annotation_rois, state$segmentation, pixel_size = pixel_size)
  state$roi_summary <- wsi_viewer_merge_ihc_roi_summary(state$roi_summary, state$ihc_summary)
  state$class_summary <- wsi_viewer_merge_ihc_class_summary(state$class_summary, state$ihc_class_summary)
  invisible(state)
}

wsi_measurements_from_payload <- function(measures) {
  if (is.null(measures) || !length(measures)) {
    return(wsi_empty_measurements())
  }
  rows <- lapply(seq_along(measures), function(i) {
    m <- measures[[i]]
    start <- m$start %||% list()
    end <- m$end %||% list()
    data.frame(
      id = as.character(m$id %||% sprintf("measure_%d", i)),
      start_x = as.numeric(start$x %||% NA_real_),
      start_y = as.numeric(start$y %||% NA_real_),
      end_x = as.numeric(end$x %||% NA_real_),
      end_y = as.numeric(end$y %||% NA_real_),
      distance_px = as.numeric(m$distance_px %||% NA_real_),
      distance_um = as.numeric(m$distance_um %||% NA_real_),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

wsi_empty_annotation_history <- function() {
  out <- data.frame(
    id = character(),
    time = character(),
    action = character(),
    label = character(),
    stringsAsFactors = FALSE
  )
  out$detail <- I(list())
  class(out) <- c("wsi_annotation_history", class(out))
  out
}

wsi_annotation_history_from_payload <- function(history) {
  if (is.null(history) || !length(history)) {
    return(wsi_empty_annotation_history())
  }
  if (is.data.frame(history)) {
    out <- as.data.frame(history, stringsAsFactors = FALSE)
    for (column in c("id", "time", "action", "label")) {
      if (!column %in% names(out)) {
        out[[column]] <- NA_character_
      }
      out[[column]] <- as.character(out[[column]])
    }
    if (!"detail" %in% names(out)) {
      out$detail <- I(rep(list(list()), nrow(out)))
    } else if (!inherits(out$detail, "AsIs")) {
      out$detail <- I(as.list(out$detail))
    }
    out <- out[, c("id", "time", "action", "label", "detail"), drop = FALSE]
    class(out) <- c("wsi_annotation_history", setdiff(class(out), "wsi_annotation_history"))
    return(out)
  }
  entries <- if (is.list(history)) history else as.list(history)
  rows <- lapply(seq_along(entries), function(i) {
    entry <- entries[[i]]
    if (!is.list(entry)) {
      entry <- list(label = as.character(entry))
    }
    data.frame(
      id = as.character(entry$id %||% sprintf("history_%d", i)),
      time = as.character(entry$time %||% NA_character_),
      action = as.character(entry$action %||% "annotation_changed"),
      label = as.character(entry$label %||% entry$action %||% "Annotation changed"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$detail <- I(lapply(entries, function(entry) {
    if (is.list(entry)) entry$detail %||% list() else list()
  }))
  class(out) <- c("wsi_annotation_history", class(out))
  out
}

wsi_empty_viewer_logs <- function() {
  out <- data.frame(
    id = character(),
    time = character(),
    level = character(),
    message = character(),
    source = character(),
    stringsAsFactors = FALSE
  )
  out$detail <- I(list())
  class(out) <- c("wsi_viewer_logs", class(out))
  out
}

wsi_viewer_logs_from_payload <- function(logs) {
  if (is.null(logs) || !length(logs)) {
    return(wsi_empty_viewer_logs())
  }
  if (is.data.frame(logs)) {
    out <- as.data.frame(logs, stringsAsFactors = FALSE)
    for (column in c("id", "time", "level", "message", "source")) {
      if (!column %in% names(out)) {
        out[[column]] <- NA_character_
      }
      out[[column]] <- as.character(out[[column]])
    }
    if (!"detail" %in% names(out)) {
      out$detail <- I(rep(list(list()), nrow(out)))
    } else if (!inherits(out$detail, "AsIs")) {
      out$detail <- I(as.list(out$detail))
    }
    out <- out[, c("id", "time", "level", "message", "source", "detail"), drop = FALSE]
    class(out) <- c("wsi_viewer_logs", setdiff(class(out), "wsi_viewer_logs"))
    return(out)
  }
  entries <- if (is.list(logs)) logs else as.list(logs)
  rows <- lapply(seq_along(entries), function(i) {
    entry <- entries[[i]]
    if (!is.list(entry)) {
      entry <- list(message = as.character(entry))
    }
    data.frame(
      id = as.character(entry$id %||% sprintf("log_%d", i)),
      time = as.character(entry$time %||% NA_character_),
      level = as.character(entry$level %||% "info"),
      message = as.character(entry$message %||% ""),
      source = as.character(entry$source %||% "viewer"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$detail <- I(lapply(entries, function(entry) {
    if (is.list(entry)) entry$detail %||% list() else list()
  }))
  class(out) <- c("wsi_viewer_logs", class(out))
  out
}

wsi_empty_annotation_spots <- function() {
  out <- data.frame(
    annotation_index = integer(),
    annotation_id = character(),
    annotation_name = character(),
    annotation_class = character(),
    spot_id = character(),
    spot_label = character(),
    spot_x = numeric(),
    spot_y = numeric(),
    spot_layer_id = character(),
    spot_layer_name = character(),
    project_image = character(),
    project_section = character(),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_annotation_spots", class(out))
  out
}

wsi_payload_value <- function(row, name, default = NA_character_) {
  if (!is.list(row) || is.null(row[[name]])) {
    return(default)
  }
  value <- row[[name]]
  if (length(value) < 1L) {
    return(default)
  }
  first <- value[[1L]]
  if (is.null(first) || length(first) < 1L) {
    return(default)
  }
  if (is.atomic(first) && length(first) == 1L && is.na(first)) {
    return(default)
  }
  first
}

wsi_payload_character <- function(row, name, default = NA_character_) {
  as.character(wsi_payload_value(row, name, default = default))
}

wsi_payload_numeric <- function(row, name) {
  value <- suppressWarnings(as.numeric(wsi_payload_value(row, name, default = NA_real_)))
  if (length(value) < 1L) {
    return(NA_real_)
  }
  value[[1L]]
}

wsi_payload_integer <- function(row, name) {
  value <- suppressWarnings(as.integer(wsi_payload_value(row, name, default = NA_integer_)))
  if (length(value) < 1L) {
    return(NA_integer_)
  }
  value[[1L]]
}

wsi_annotation_spots_from_payload <- function(x) {
  if (is.null(x) || !length(x)) {
    return(wsi_empty_annotation_spots())
  }
  columns <- names(wsi_empty_annotation_spots())
  if (is.data.frame(x)) {
    out <- as.data.frame(x, stringsAsFactors = FALSE)
    for (column in setdiff(columns, names(out))) {
      out[[column]] <- NA
    }
    out <- out[, columns, drop = FALSE]
  } else {
    rows <- if (is.list(x) && !is.null(names(x)) && any(names(x) %in% columns)) {
      list(x)
    } else {
      as.list(x)
    }
    out <- do.call(rbind, lapply(rows, function(row) {
      data.frame(
        annotation_index = wsi_payload_integer(row, "annotation_index"),
        annotation_id = wsi_payload_character(row, "annotation_id"),
        annotation_name = wsi_payload_character(row, "annotation_name"),
        annotation_class = wsi_payload_character(row, "annotation_class"),
        spot_id = wsi_payload_character(row, "spot_id"),
        spot_label = wsi_payload_character(row, "spot_label"),
        spot_x = wsi_payload_numeric(row, "spot_x"),
        spot_y = wsi_payload_numeric(row, "spot_y"),
        spot_layer_id = wsi_payload_character(row, "spot_layer_id"),
        spot_layer_name = wsi_payload_character(row, "spot_layer_name"),
        project_image = wsi_payload_character(row, "project_image"),
        project_section = wsi_payload_character(row, "project_section"),
        stringsAsFactors = FALSE
      )
    }))
  }
  out$annotation_index <- suppressWarnings(as.integer(out$annotation_index))
  for (column in setdiff(columns, c("annotation_index", "spot_x", "spot_y"))) {
    out[[column]] <- as.character(out[[column]])
  }
  out$spot_x <- suppressWarnings(as.numeric(out$spot_x))
  out$spot_y <- suppressWarnings(as.numeric(out$spot_y))
  out <- out[, columns, drop = FALSE]
  class(out) <- c("wsi_annotation_spots", setdiff(class(out), "wsi_annotation_spots"))
  out
}

wsi_rois_from_payload <- function(geojson) {
  if (is.null(geojson)) {
    return(wsi_empty_roi())
  }
  wsi_roi_from_geojson(geojson)
}

wsi_selected_roi_from_payload <- function(feature) {
  if (is.null(feature)) {
    return(NULL)
  }
  wsi_roi_from_geojson(list(type = "FeatureCollection", features = list(feature)))
}

wsi_selected_rois_from_payload <- function(selected_rois = NULL, selected_roi = NULL) {
  if (!is.null(selected_rois)) {
    rois <- wsi_rois_from_payload(selected_rois)
    if (inherits(rois, "wsi_roi") && nrow(rois)) {
      return(rois)
    }
  }
  selected <- wsi_selected_roi_from_payload(selected_roi)
  if (inherits(selected, "wsi_roi") && nrow(selected)) {
    return(selected)
  }
  wsi_empty_roi()
}

wsi_viewer_job_record <- function(job, status = NULL, message = NULL,
                                  progress = NULL, log = NULL) {
  meta <- tryCatch(job$metadata(), error = function(err) {
    list(
      id = job$id %||% NA_character_,
      name = job$name %||% "wsiTools job",
      pid = tryCatch(job$pid(), error = function(e) NA_integer_),
      status = tryCatch(job$status(), error = function(e) "failed"),
      display_status = "failed",
      progress = NA_real_,
      progress_available = FALSE,
      message = conditionMessage(err),
      started = job$started %||% NA,
      finished = NA,
      log = conditionMessage(err)
    )
  })
  raw_status <- as.character(status %||% meta$status %||% "running")[[1L]]
  display_status <- wsi_job_display_status(raw_status)
  progress <- progress %||% meta$progress %||% NA_real_
  progress <- suppressWarnings(as.numeric(progress))
  if (identical(display_status, "completed")) {
    progress <- 100
  } else if (identical(display_status, "queued") && !is.finite(progress)) {
    progress <- 0
  }
  log <- as.character(log %||% meta$log %||% character())
  message <- as.character(message %||% meta$message %||% utils::tail(log[nzchar(log)], 1L) %||% "")
  list(
    id = as.character(meta$id %||% job$id %||% ""),
    name = as.character(meta$name %||% job$name %||% "wsiTools job"),
    pid = suppressWarnings(as.integer(meta$pid %||% NA_integer_)),
    status = display_status,
    raw_status = raw_status,
    progress = if (is.finite(progress)) max(0, min(100, progress)) else NA_real_,
    progress_available = isTRUE(meta$progress_available) || is.finite(progress),
    message = message[[1L]] %||% "",
    log = utils::tail(log, 40L),
    started = as.character(meta$started %||% NA),
    updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
    finished = as.character(meta$finished %||% NA)
  )
}

wsi_viewer_jobs_table <- function(jobs) {
  jobs <- jobs %||% list()
  if (!length(jobs)) {
    return(data.frame(
      id = character(),
      name = character(),
      status = character(),
      progress = numeric(),
      message = character(),
      updated = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    id = vapply(jobs, function(job) as.character(job$id %||% ""), character(1)),
    name = vapply(jobs, function(job) as.character(job$name %||% ""), character(1)),
    status = vapply(jobs, function(job) as.character(job$status %||% ""), character(1)),
    progress = vapply(jobs, function(job) as.numeric(job$progress %||% NA_real_), numeric(1)),
    message = vapply(jobs, function(job) as.character(job$message %||% ""), character(1)),
    updated = vapply(jobs, function(job) as.character(job$updated %||% ""), character(1)),
    stringsAsFactors = FALSE
  )
}

wsi_viewer_state_set_job <- function(state, job, status = NULL, message = NULL,
                                     progress = NULL, log = NULL,
                                     queue_command = TRUE) {
  if (!inherits(state, "wsi_viewer_state") || is.null(job)) {
    return(invisible(NULL))
  }
  record <- wsi_viewer_job_record(
    job = job,
    status = status,
    message = message,
    progress = progress,
    log = log
  )
  if (!nzchar(record$id)) {
    return(invisible(NULL))
  }
  state$jobs <- state$jobs %||% list()
  state$jobs[[record$id]] <- record
  if (isTRUE(queue_command)) {
    wsi_viewer_queue_command(state, "job_update", list(job = record))
  }
  wsi_assign_viewer_state(state)
  invisible(record)
}

wsi_assign_viewer_state <- function(state) {
  envir <- state$export_envir
  name <- state$export_name
  if (!is.environment(envir) || is.null(name) || !nzchar(name)) {
    return(invisible(state))
  }
  assign(name, state, envir = envir)
  assign(paste0(name, "_rois"), state$rois, envir = envir)
  assign(paste0(name, "_measurements"), state$measurements, envir = envir)
  assign(paste0(name, "_trajectories"), state$trajectories %||% wsi_empty_trajectories(), envir = envir)
  assign(paste0(name, "_roi_summary"), state$roi_summary, envir = envir)
  assign(paste0(name, "_cell_summary"), state$cell_summary, envir = envir)
  assign(paste0(name, "_class_summary"), state$class_summary, envir = envir)
  assign(paste0(name, "_ihc_summary"), state$ihc_summary %||% wsi_empty_ihc_intensity_summary("roi"), envir = envir)
  assign(paste0(name, "_ihc_class_summary"), state$ihc_class_summary %||% wsi_empty_ihc_intensity_summary("class"), envir = envir)
  assign(paste0(name, "_segmentation"), state$segmentation, envir = envir)
  assign(paste0(name, "_layers"), state$layers, envir = envir)
  assign(paste0(name, "_project"), state$project %||% list(), envir = envir)
  assign(paste0(name, "_project_snapshot"), state$project_snapshot %||% NULL, envir = envir)
  assign(paste0(name, "_channel_sources"), state$channel_sources %||% list(), envir = envir)
  assign(paste0(name, "_channel_settings"), state$channel_settings %||% wsi_empty_channel_settings(), envir = envir)
  assign(paste0(name, "_tile_sources"), state$tile_sources %||% list(), envir = envir)
  assign(paste0(name, "_tile_preview"), state$tile_preview %||% wsi_empty_tile_preview(), envir = envir)
  assign(paste0(name, "_prediction"), state$prediction %||% wsi_empty_prediction_result(), envir = envir)
  assign(paste0(name, "_proximity"), state$proximity %||% wsi_empty_proximity_result(), envir = envir)
  assign(paste0(name, "_proximity_stats"), state$proximity_stats %||% wsi_empty_proximity_stats_result(), envir = envir)
  assign(paste0(name, "_trajectory_profile"), state$trajectory_profile %||% wsi_empty_trajectory_profile(), envir = envir)
  assign(paste0(name, "_annotations"), state$annotations, envir = envir)
  assign(paste0(name, "_history"), state$history, envir = envir)
  assign(paste0(name, "_logs"), state$logs %||% wsi_empty_viewer_logs(), envir = envir)
  assign(paste0(name, "_autosave"), wsi_viewer_autosave_status(state), envir = envir)
  assign(paste0(name, "_jobs"), wsi_viewer_jobs_table(state$jobs), envir = envir)
  assign(paste0(name, "_selected_roi"), state$selected_roi, envir = envir)
  assign(paste0(name, "_selected_rois"), state$selected_rois, envir = envir)
  assign(paste0(name, "_selected_object"), state$selected_object %||% NULL, envir = envir)
  assign(paste0(name, "_last_segmentation"), state$last_segmentation, envir = envir)
  assign(paste0(name, "_kodama_selection"), state$kodama_selection %||% list(labels = character(), count = 0L, matched_count = 0L), envir = envir)
  assign(paste0(name, "_seurat_selection"), state$seurat_selection %||% list(labels = character(), count = 0L, matched_count = 0L), envir = envir)
  assign(paste0(name, "_annotation_spots"), state$annotation_spots %||% wsi_empty_annotation_spots(), envir = envir)
  assign(paste0(name, "_last_event"), state$last_payload, envir = envir)
  invisible(state)
}

wsi_viewer_state_apply <- function(state, payload) {
  if (!inherits(state, "wsi_viewer_state")) {
    wsi_abort("`state` must be a `wsi_viewer_state` object.")
  }
  if (!is.list(payload)) {
    wsi_abort("Viewer state payload must be a JSON object.")
  }
  payload <- wsi_viewer_validate_state_payload(payload)

  state$rois <- wsi_rois_from_payload(payload[["rois", exact = TRUE]])
  state$measurements <- wsi_measurements_from_payload(payload[["measurements", exact = TRUE]])
  state$trajectories <- wsi_trajectories_from_payload(payload[["trajectories", exact = TRUE]])
  state$segmentation <- wsi_rois_from_payload(payload[["segmentation", exact = TRUE]])
  state$layers <- wsi_viewer_update_layers_from_payload(state$layers, payload[["layers", exact = TRUE]])
  state$project <- payload[["project", exact = TRUE]] %||% state$project %||% list()
  detail <- payload[["detail", exact = TRUE]]
  if (is.list(detail) && !is.null(detail$project_snapshot)) {
    state$project_snapshot <- detail$project_snapshot
  }
  if (is.list(detail) && !is.null(detail$tile_preview)) {
    state$tile_preview <- tryCatch(
      wsi_spatial_tile_payload_grid(detail$tile_preview),
      error = function(err) state$tile_preview %||% wsi_empty_tile_preview()
    )
  }
  if (is.list(detail) && !is.null(detail$prediction) && is.data.frame(detail$prediction)) {
    state$prediction <- detail$prediction
  }
  if (is.list(detail) && !is.null(detail$proximity) && is.data.frame(detail$proximity)) {
    state$proximity <- detail$proximity
  }
  if (is.list(detail) && !is.null(detail$proximity_stats) && is.data.frame(detail$proximity_stats)) {
    state$proximity_stats <- detail$proximity_stats
  }
  if (is.list(detail) && !is.null(detail$trajectory_profile)) {
    state$trajectory_profile <- wsi_trajectory_profile_from_payload(detail$trajectory_profile)
  }
  state$selected_roi <- wsi_selected_roi_from_payload(payload[["selected_roi", exact = TRUE]])
  state$selected_rois <- wsi_selected_rois_from_payload(
    payload[["selected_rois", exact = TRUE]],
    payload[["selected_roi", exact = TRUE]]
  )
  state$selected_object <- payload[["selected_object", exact = TRUE]] %||% NULL
  state$view <- payload[["view", exact = TRUE]] %||% list()
  state$stain <- payload[["stain", exact = TRUE]] %||% NULL
  if (!is.null(payload[["channel_sources", exact = TRUE]])) {
    state$channel_sources <- wsi_channel_sources_payload(payload[["channel_sources", exact = TRUE]])
  }
  if (!is.null(payload[["channel_settings", exact = TRUE]])) {
    state$channel_settings <- wsi_channel_settings_from_payload(payload[["channel_settings", exact = TRUE]])
  } else if (!is.null(state$stain$channels)) {
    state$channel_settings <- wsi_channel_settings_from_payload(state$stain$channels)
  }
  if (!is.null(payload[["tile_sources", exact = TRUE]])) {
    state$tile_sources <- payload[["tile_sources", exact = TRUE]]
  }
  state$kodama_selection <- payload[["kodama_selection", exact = TRUE]] %||%
    state$kodama_selection %||% list(labels = character(), count = 0L, matched_count = 0L)
  state$seurat_selection <- payload[["seurat_selection", exact = TRUE]] %||%
    state$seurat_selection %||% list(labels = character(), count = 0L, matched_count = 0L)
  state$annotation_spots <- wsi_annotation_spots_from_payload(payload[["annotation_spots", exact = TRUE]])
  state$annotations <- payload[["annotations", exact = TRUE]] %||% list(dirty = FALSE, dirty_reason = "")
  state$history <- wsi_annotation_history_from_payload(payload[["history", exact = TRUE]])
  state$logs <- wsi_viewer_logs_from_payload(payload[["logs", exact = TRUE]])
  wsi_viewer_update_measurement_tables(state)
  state$last_event <- as.character(payload[["event", exact = TRUE]] %||% "viewer_state")
  if (identical(state$last_event, "prediction_cleared")) {
    state$prediction <- wsi_empty_prediction_result()
  }
  if (identical(state$last_event, "proximity_cleared")) {
    state$proximity <- wsi_empty_proximity_result()
    state$proximity_stats <- wsi_empty_proximity_stats_result()
  }
  if (identical(state$last_event, "proximity_stats_cleared")) {
    state$proximity_stats <- wsi_empty_proximity_stats_result()
  }
  if (identical(state$last_event, "trajectory_profile_cleared")) {
    state$trajectory_profile <- wsi_empty_trajectory_profile()
  }
  if (startsWith(state$last_event, "segmentation")) {
    state$last_segmentation <- payload[["detail", exact = TRUE]] %||% list()
  }
  state$last_payload <- payload
  state$last_sync <- Sys.time()

  event <- list(
    event = state$last_event,
    time = as.character(payload[["time", exact = TRUE]] %||% format(state$last_sync, "%Y-%m-%dT%H:%M:%OS%z")),
    roi_count = nrow(state$rois),
    measurement_count = nrow(state$measurements),
    trajectory_count = nrow(state$trajectories %||% wsi_empty_trajectories()),
    segmentation_count = nrow(state$segmentation),
    selected_roi_count = nrow(state$selected_rois),
    roi_summary_count = nrow(state$roi_summary),
    cell_summary_count = nrow(state$cell_summary),
    history_count = nrow(state$history),
    log_count = nrow(state$logs %||% wsi_empty_viewer_logs()),
    annotation_spot_count = nrow(state$annotation_spots %||% wsi_empty_annotation_spots()),
    tile_preview_count = nrow(state$tile_preview %||% wsi_empty_tile_preview()),
    prediction_count = nrow(state$prediction %||% wsi_empty_prediction_result()),
    proximity_count = nrow(state$proximity %||% wsi_empty_proximity_result()),
    proximity_stats_count = nrow(state$proximity_stats %||% wsi_empty_proximity_stats_result()),
    trajectory_profile_count = nrow(state$trajectory_profile %||% wsi_empty_trajectory_profile()),
    layer_count = length(state$layers %||% list()),
    channel_count = nrow(state$channel_settings %||% wsi_empty_channel_settings())
  )
  state$events[[length(state$events) + 1L]] <- event
  max_events <- state$max_events %||% 1000L
  if (length(state$events) > max_events) {
    state$events <- utils::tail(state$events, max_events)
  }

  wsi_assign_viewer_state(state)
  wsi_dispatch_viewer_callbacks(state, payload)
  invisible(state)
}

wsi_viewer_state_record_event <- function(state, event, detail = list()) {
  if (!inherits(state, "wsi_viewer_state")) {
    return(invisible(NULL))
  }
  event <- wsi_viewer_validate_event(event %||% "viewer_state")
  now <- Sys.time()
  payload <- list(
    event = event,
    time = format(now, "%Y-%m-%dT%H:%M:%OS%z"),
    selected_roi = if (inherits(state$selected_roi, "wsi_roi") && nrow(state$selected_roi)) {
      wsi_viewer_rois_to_geojson(state$selected_roi)$features[[1L]]
    } else {
      NULL
    },
    selected_rois = if (inherits(state$selected_rois, "wsi_roi") && nrow(state$selected_rois)) {
      wsi_viewer_rois_to_geojson(state$selected_rois)
    } else {
      NULL
    },
    selected_object = state$selected_object %||% NULL,
    detail = detail %||% list()
  )
  state$last_event <- event
  state$last_payload <- payload
  state$last_sync <- now
  wsi_viewer_update_measurement_tables(state)
  entry <- list(
    event = event,
    time = payload$time,
    roi_count = if (inherits(state$rois, "wsi_roi")) nrow(state$rois) else 0L,
    measurement_count = if (is.data.frame(state$measurements)) nrow(state$measurements) else 0L,
    segmentation_count = if (inherits(state$segmentation, "wsi_roi")) nrow(state$segmentation) else 0L,
    selected_roi_count = if (inherits(state$selected_rois, "wsi_roi")) nrow(state$selected_rois) else 0L,
    roi_summary_count = if (is.data.frame(state$roi_summary)) nrow(state$roi_summary) else 0L,
    cell_summary_count = if (is.data.frame(state$cell_summary)) nrow(state$cell_summary) else 0L,
    history_count = if (is.data.frame(state$history)) nrow(state$history) else 0L,
    annotation_spot_count = if (is.data.frame(state$annotation_spots)) nrow(state$annotation_spots) else 0L,
    tile_preview_count = if (is.data.frame(state$tile_preview)) nrow(state$tile_preview) else 0L,
    prediction_count = if (is.data.frame(state$prediction)) nrow(state$prediction) else 0L,
    proximity_count = if (is.data.frame(state$proximity)) nrow(state$proximity) else 0L,
    proximity_stats_count = if (is.data.frame(state$proximity_stats)) nrow(state$proximity_stats) else 0L,
    trajectory_profile_count = if (is.data.frame(state$trajectory_profile)) nrow(state$trajectory_profile) else 0L,
    layer_count = length(state$layers %||% list()),
    channel_count = nrow(state$channel_settings %||% wsi_empty_channel_settings())
  )
  if (length(detail %||% list())) {
    entry$detail <- detail
  }
  state$events[[length(state$events) + 1L]] <- entry
  max_events <- state$max_events %||% 1000L
  if (length(state$events) > max_events) {
    state$events <- utils::tail(state$events, max_events)
  }
  wsi_assign_viewer_state(state)
  invisible(state)
}

wsi_viewer_state_set_selected_roi <- function(state, roi, event = "segmentation_requested",
                                              detail = list()) {
  if (!inherits(state, "wsi_viewer_state") || !inherits(roi, "wsi_roi") || !nrow(roi)) {
    return(invisible(NULL))
  }
  selected <- roi[1L, , drop = FALSE]
  class(selected) <- unique(c("wsi_roi", class(selected)))
  state$selected_roi <- selected
  state$selected_rois <- selected
  state$selected_object <- list(
    type = "annotation",
    index = 0,
    id = selected$roi_id[[1L]] %||% NA_character_,
    name = selected$name[[1L]] %||% selected$roi_id[[1L]] %||% NA_character_
  )
  wsi_viewer_state_record_event(
    state,
    event,
    utils::modifyList(
      list(roi_id = selected$roi_id[[1L]] %||% NA_character_),
      detail %||% list(),
      keep.null = TRUE
    )
  )
}

wsi_viewer_state_add_segmentation_result <- function(state, result, cell_radius = 8) {
  if (!inherits(state, "wsi_viewer_state") || is.null(result$segmentation)) {
    return(invisible(NULL))
  }
  segmentation <- tryCatch(
    wsi_viewer_coerce_segmentation(result$segmentation, radius = cell_radius),
    error = function(err) NULL
  )
  added <- 0L
  if (inherits(segmentation, "wsi_roi") && nrow(segmentation)) {
    state$segmentation <- wsi_viewer_bind_rois(state$segmentation, segmentation)
    added <- nrow(segmentation)
  }
  wsi_viewer_update_measurement_tables(state)
  detail <- list(
    added = added,
    engine = result$engine %||% NULL,
    crop = result$crop %||% result$input %||% NULL,
    output = result$output %||% NULL,
    slide_output = result$slide_output %||% NULL,
    roi_id = result$roi_id %||% NULL,
    bbox = wsi_named_numeric_list(result$bbox),
    status = result$status %||% "complete",
    segmentation_type = if (!is.null(result$segmentation)) class(result$segmentation)[[1L]] else NULL
  )
  state$last_segmentation <- detail
  wsi_viewer_state_record_event(state, "segmentation_finished", detail)
}

wsi_viewer_state_response <- function(state, dequeue_commands = TRUE) {
  commands <- if (isTRUE(dequeue_commands)) {
    wsi_viewer_take_commands(state)
  } else {
    state$commands %||% list()
  }
  list(
    ok = TRUE,
    event = state$last_event,
    roi_count = nrow(state$rois),
    measurement_count = nrow(state$measurements),
    roi_summary_count = nrow(state$roi_summary),
    cell_summary_count = nrow(state$cell_summary),
    segmentation_count = nrow(state$segmentation),
    selected_roi_count = nrow(state$selected_rois),
    layer_count = length(state$layers %||% list()),
    channel_count = nrow(state$channel_settings %||% wsi_empty_channel_settings()),
    history_count = nrow(state$history),
    tile_preview_count = nrow(state$tile_preview %||% wsi_empty_tile_preview()),
    prediction_count = nrow(state$prediction %||% wsi_empty_prediction_result()),
    proximity_count = nrow(state$proximity %||% wsi_empty_proximity_result()),
    proximity_stats_count = nrow(state$proximity_stats %||% wsi_empty_proximity_stats_result()),
    trajectory_profile_count = nrow(state$trajectory_profile %||% wsi_empty_trajectory_profile()),
    annotations_dirty = isTRUE((state$annotations %||% list())$dirty),
    last_sync = as.character(state$last_sync),
    autosave = wsi_viewer_autosave_status(state),
    jobs = unname(state$jobs %||% list()),
    commands = commands
  )
}

wsi_viewer_rois_to_geojson <- function(rois) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  features <- lapply(seq_len(nrow(rois)), function(i) {
    feature <- wsi_roi_feature_for_write(rois, i)
    feature$type <- "Feature"
    feature$id <- wsi_geojson_scalar(wsi_roi_column_value(rois, "roi_id", i), default = as.character(i))
    feature$properties <- wsi_roi_properties_for_write(rois, i)
    feature$geometry <- wsi_roi_geometry_for_write(rois, i)
    feature
  })
  wsi_geojson_for_rois(rois, features)
}

wsi_viewer_coerce_rois <- function(rois, arg = "rois") {
  if (inherits(rois, "wsi_roi")) {
    return(rois)
  }
  if (is.character(rois) && length(rois) == 1L && !is.na(rois) && nzchar(rois)) {
    return(read_geojson(rois))
  }
  if (is.list(rois) && !is.data.frame(rois)) {
    return(wsi_roi_from_geojson(rois))
  }
  wsi_abort(sprintf("`%s` must be a `wsi_roi` object, GeoJSON list, or GeoJSON file path.", arg))
}

wsi_viewer_empty_like <- function(example, n) {
  if (is.list(example) && !is.data.frame(example)) {
    return(I(rep(list(NULL), n)))
  }
  if (is.logical(example)) {
    return(rep(NA, n))
  }
  if (is.numeric(example) || is.integer(example)) {
    return(rep(NA_real_, n))
  }
  rep(NA_character_, n)
}

wsi_viewer_bind_rois <- function(x, y) {
  if (is.null(x) || !inherits(x, "wsi_roi") || !nrow(x)) {
    return(y)
  }
  if (is.null(y) || !inherits(y, "wsi_roi") || !nrow(y)) {
    return(x)
  }
  cols <- union(names(x), names(y))
  add_missing <- function(data, template) {
    for (col in setdiff(cols, names(data))) {
      source <- if (col %in% names(template)) template[[col]] else character()
      data[[col]] <- wsi_viewer_empty_like(source, nrow(data))
    }
    data[cols]
  }
  out <- rbind(
    add_missing(as.data.frame(x), y),
    add_missing(as.data.frame(y), x)
  )
  class(out) <- unique(c("wsi_roi", setdiff(class(y), "data.frame"), setdiff(class(x), "data.frame"), "data.frame"))
  out
}

wsi_viewer_selected_tail <- function(rois) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    return(NULL)
  }
  out <- rois[nrow(rois), , drop = FALSE]
  class(out) <- unique(c("wsi_roi", class(out)))
  out
}

wsi_viewer_coerce_segmentation <- function(segmentation, radius = 8) {
  if (inherits(segmentation, "wsi_roi")) {
    return(segmentation)
  }
  if (is.character(segmentation) && length(segmentation) == 1L &&
      !is.na(segmentation) && nzchar(segmentation)) {
    return(wsi_segmentation_to_rois(import_segmentation(segmentation), radius = radius))
  }
  if (is.list(segmentation) && !is.data.frame(segmentation)) {
    return(wsi_roi_from_geojson(segmentation))
  }
  if (is.data.frame(segmentation)) {
    data <- segmentation
    cols <- wsi_centroid_columns(data)
    if (is.null(cols)) {
      wsi_abort("Segmentation data frames must contain `x`/`y` or `centroid_x`/`centroid_y` columns.")
    }
    data$x <- as.numeric(data[[cols[["x"]]]])
    data$y <- as.numeric(data[[cols[["y"]]]])
    class(data) <- unique(c("wsi_segmentation_centroids", class(data)))
    return(wsi_segmentation_to_rois(data, radius = radius))
  }
  wsi_abort("`segmentation` must be GeoJSON-like ROI data, a centroid table, or an imported segmentation object.")
}

wsi_viewer_layer_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(trimws(name))) {
    wsi_abort("Layer `name` must be a single non-empty string.")
  }
  trimws(name)
}

wsi_viewer_layer_id <- function(name) {
  id <- tolower(gsub("[^A-Za-z0-9]+", "_", wsi_viewer_layer_name(name)))
  id <- gsub("^_+|_+$", "", id)
  if (!nzchar(id)) {
    id <- sprintf("layer_%s", as.integer(Sys.time()))
  }
  id
}

wsi_viewer_layer_colour <- function(colour = NULL, fallback = "#38bdf8") {
  value <- colour %||% fallback
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    value <- fallback
  }
  ok <- tryCatch(
    {
      grDevices::col2rgb(value)
      TRUE
    },
    error = function(err) FALSE
  )
  if (isTRUE(ok)) {
    return(value)
  }
  fallback
}

wsi_viewer_layer_extent <- function(extent = NULL, slide = NULL, ncol = NULL, nrow = NULL) {
  if (is.null(extent)) {
    if (!is.null(slide) && inherits(slide, "wsi_slide")) {
      return(list(
        xmin = 0,
        ymin = 0,
        xmax = unname(as.numeric(slide$dimensions[["width"]])),
        ymax = unname(as.numeric(slide$dimensions[["height"]]))
      ))
    }
    return(list(
      xmin = 0,
      ymin = 0,
      xmax = as.numeric(ncol %||% 1),
      ymax = as.numeric(nrow %||% 1)
    ))
  }
  if (!is.numeric(extent) || anyNA(extent) || !all(is.finite(extent))) {
    wsi_abort("Layer `extent` must be numeric and finite.")
  }
  nm <- names(extent) %||% character()
  if (all(c("xmin", "ymin", "xmax", "ymax") %in% nm)) {
    value <- as.numeric(extent[c("xmin", "ymin", "xmax", "ymax")])
    return(list(xmin = value[[1L]], ymin = value[[2L]], xmax = value[[3L]], ymax = value[[4L]]))
  }
  if (all(c("x", "y", "width", "height") %in% nm)) {
    x <- as.numeric(extent[["x"]])
    y <- as.numeric(extent[["y"]])
    return(list(xmin = x, ymin = y, xmax = x + as.numeric(extent[["width"]]), ymax = y + as.numeric(extent[["height"]])))
  }
  if (length(extent) == 4L) {
    value <- as.numeric(extent)
    return(list(xmin = value[[1L]], ymin = value[[2L]], xmax = value[[3L]], ymax = value[[4L]]))
  }
  wsi_abort("Layer `extent` must contain `xmin`, `ymin`, `xmax`, `ymax` or `x`, `y`, `width`, `height`.")
}

wsi_viewer_bbox_ring <- function(xmin, ymin, xmax, ymax) {
  list(
    list(x = xmin, y = ymin),
    list(x = xmax, y = ymin),
    list(x = xmax, y = ymax),
    list(x = xmin, y = ymax),
    list(x = xmin, y = ymin)
  )
}

wsi_viewer_recolour_items <- function(items, colour, fill_alpha = 0.18) {
  override <- !is.null(colour)
  colour <- wsi_viewer_layer_colour(colour, fallback = "#38bdf8")
  lapply(items, function(item) {
    if (isTRUE(override) || is.null(item$colour) || is.na(item$colour) || !nzchar(item$colour)) {
      item$colour <- colour
    }
    if (isTRUE(override) || is.null(item$fill) || is.na(item$fill) || !nzchar(item$fill)) {
      item$fill <- wsi_viewer_hex_to_rgba(item$colour, alpha = fill_alpha)
    }
    item
  })
}

wsi_viewer_layer_grid_items <- function(grid, colour = NULL, fill_alpha = 0.03) {
  needed <- c("x", "y", "width", "height")
  if (!is.data.frame(grid) || !all(needed %in% names(grid))) {
    wsi_abort("Tile-grid layers must be a data frame with `x`, `y`, `width`, and `height` columns.")
  }
  colour <- wsi_viewer_layer_colour(colour, fallback = "#facc15")
  downsample <- if ("downsample" %in% names(grid)) as.numeric(grid$downsample) else rep(1, nrow(grid))
  ids <- if ("tile_id" %in% names(grid)) as.character(grid$tile_id) else sprintf("tile_%05d", seq_len(nrow(grid)))
  classes <- if ("class" %in% names(grid)) as.character(grid$class) else rep("tile", nrow(grid))
  lapply(seq_len(nrow(grid)), function(i) {
    x <- as.numeric(grid$x[[i]])
    y <- as.numeric(grid$y[[i]])
    w <- as.numeric(grid$width[[i]]) * downsample[[i]]
    h <- as.numeric(grid$height[[i]]) * downsample[[i]]
    ring <- wsi_viewer_bbox_ring(x, y, x + w, y + h)
    list(
      id = ids[[i]],
      name = ids[[i]],
      label = ids[[i]],
      class = classes[[i]],
      visible = TRUE,
      locked = TRUE,
      geometry_type = "Polygon",
      source = "tile grid",
      drawable = TRUE,
      point_count = 4L,
      area = w * h,
      bbox = list(xmin = x, ymin = y, xmax = x + w, ymax = y + h),
      colour = colour,
      fill = wsi_viewer_hex_to_rgba(colour, alpha = fill_alpha),
      rings = list(ring)
    )
  })
}

wsi_viewer_layer_points_items <- function(points, colour = NULL, radius = 6) {
  if (!is.data.frame(points) || !all(c("x", "y") %in% names(points))) {
    wsi_abort("Point layers must be a data frame with `x` and `y` columns.")
  }
  radius <- wsi_check_scalar_number(radius, "radius", allow_zero = FALSE)
  colour <- wsi_viewer_layer_colour(colour, fallback = "#38bdf8")
  ids <- if ("id" %in% names(points)) as.character(points$id) else sprintf("point_%05d", seq_len(nrow(points)))
  source_labels <- if ("label" %in% names(points)) as.character(points$label) else ids
  classes <- if ("class" %in% names(points)) as.character(points$class) else rep("point", nrow(points))
  point_colours <- if ("colour" %in% names(points)) {
    as.character(points$colour)
  } else if ("color" %in% names(points)) {
    as.character(points$color)
  } else {
    rep(NA_character_, nrow(points))
  }
  lapply(seq_len(nrow(points)), function(i) {
    x <- as.numeric(points$x[[i]])
    y <- as.numeric(points$y[[i]])
    point_colour <- wsi_viewer_layer_colour(point_colours[[i]], fallback = colour)
    item <- list(
      id = ids[[i]],
      name = ids[[i]],
      label = ids[[i]],
      class = classes[[i]],
      source_label = source_labels[[i]],
      type = "point",
      x = x,
      y = y,
      radius = radius,
      source = "points",
      colour = point_colour,
      fill = wsi_viewer_hex_to_rgba(point_colour, alpha = 0.35)
    )
    scope_columns <- c(
      "project_key", "wsi_project_key", "project_image", "project_section",
      "image_id", "section_id", "sample_id", "project_image_index",
      "project_section_index", "original_id", "feature_id"
    )
    for (column in intersect(scope_columns, names(points))) {
      value <- points[[column]][[i]]
      item[[column]] <- if (is.numeric(value) || is.integer(value)) {
        unname(value)
      } else {
        as.character(value %||% "")
      }
    }
    item$project_scoped <- any(c(
      "project_key", "wsi_project_key", "project_image", "project_section",
      "image_id", "section_id", "sample_id", "project_image_index"
    ) %in% names(item))
    item
  })
}

wsi_viewer_matrix_values <- function(x, max_cells = 250000L) {
  mat <- as.matrix(x)
  if (!(is.numeric(mat) || is.logical(mat))) {
    wsi_abort("Heatmap and mask layers must be numeric or logical matrices.")
  }
  if (length(mat) > max_cells) {
    wsi_abort(sprintf(
      "Layer matrix has %s cells; downsample it before adding it to the viewer, or increase `max_cells`.",
      format(length(mat), big.mark = ",", scientific = FALSE)
    ))
  }
  storage.mode(mat) <- "double"
  list(
    values = unname(lapply(seq_len(nrow(mat)), function(i) unname(as.numeric(mat[i, ])))),
    nrow = nrow(mat),
    ncol = ncol(mat),
    min = suppressWarnings(min(mat, na.rm = TRUE)),
    max = suppressWarnings(max(mat, na.rm = TRUE))
  )
}

wsi_viewer_layer_payload <- function(name, data, type = c("auto", "rois", "segmentation",
                                                         "points", "tile_grid", "heatmap",
                                                         "mask", "image"),
                                     slide = NULL, visible = TRUE, opacity = 1,
                                     colour = NULL, radius = 8, extent = NULL,
                                     replace = TRUE, max_cells = 250000L) {
  name <- wsi_viewer_layer_name(name)
  type <- match.arg(type)
  if (!is.logical(visible) || length(visible) != 1L || is.na(visible)) {
    wsi_abort("Layer `visible` must be `TRUE` or `FALSE`.")
  }
  opacity <- wsi_check_scalar_number(opacity, "opacity")
  opacity <- max(0, min(1, opacity))
  layer_colour <- wsi_viewer_layer_colour(colour, fallback = "#38bdf8")
  id <- wsi_viewer_layer_id(name)

  if (identical(type, "auto")) {
    if (inherits(data, "wsi_tissue_mask")) {
      type <- "mask"
    } else if (inherits(data, "wsi_roi")) {
      type <- "rois"
    } else if (is.data.frame(data) && all(c("x", "y", "width", "height") %in% names(data))) {
      type <- "tile_grid"
    } else if (is.data.frame(data) && !is.null(wsi_centroid_columns(data))) {
      type <- "segmentation"
    } else if (is.matrix(data) && (is.numeric(data) || is.logical(data))) {
      type <- "heatmap"
    } else if (inherits(data, "raster") || (is.array(data) && length(dim(data)) >= 3L)) {
      type <- "image"
    } else if (is.character(data) && length(data) == 1L && nzchar(data)) {
      ext <- tolower(tools::file_ext(data))
      type <- if (ext %in% c("geojson", "json")) "rois" else "image"
    } else {
      type <- "points"
    }
  }

  layer <- list(
    id = id,
    name = name,
    type = type,
    visible = visible,
    opacity = opacity,
    colour = layer_colour,
    replace = isTRUE(replace),
    count = 0L
  )

  if (identical(type, "rois")) {
    rois <- wsi_viewer_coerce_rois(data, arg = "data")
    items <- wsi_viewer_recolour_items(wsi_viewer_roi_features(rois), colour = colour, fill_alpha = 0.16)
    layer$items <- items
    layer$count <- length(items)
  } else if (identical(type, "segmentation")) {
    rois <- wsi_viewer_coerce_segmentation(data, radius = radius)
    items <- wsi_viewer_recolour_items(wsi_viewer_roi_features(rois), colour = colour %||% "#38bdf8", fill_alpha = 0.12)
    layer$type <- "vector"
    layer$source_type <- "segmentation"
    layer$items <- items
    layer$count <- length(items)
  } else if (identical(type, "tile_grid")) {
    items <- wsi_viewer_layer_grid_items(data, colour = colour %||% "#facc15")
    layer$type <- "vector"
    layer$source_type <- "tile_grid"
    layer$items <- items
    layer$count <- length(items)
  } else if (identical(type, "points")) {
    items <- wsi_viewer_layer_points_items(data, colour = colour, radius = radius)
    layer$type <- "vector"
    layer$source_type <- "points"
    layer$items <- items
    layer$count <- length(items)
  } else if (identical(type, "mask")) {
    mat <- if (inherits(data, "wsi_tissue_mask")) data$mask else data
    values <- wsi_viewer_matrix_values(mat, max_cells = max_cells)
    if (inherits(data, "wsi_tissue_mask")) {
      extent <- extent %||% c(
        xmin = 0,
        ymin = 0,
        xmax = ncol(data$mask) * as.numeric(data$scale_x %||% 1),
        ymax = nrow(data$mask) * as.numeric(data$scale_y %||% 1)
      )
    }
    layer$type <- "heatmap"
    layer$source_type <- "mask"
    layer$values <- values$values
    layer$nrow <- values$nrow
    layer$ncol <- values$ncol
    layer$min <- 0
    layer$max <- 1
    layer$extent <- wsi_viewer_layer_extent(extent, slide = slide, ncol = values$ncol, nrow = values$nrow)
    layer$count <- values$nrow * values$ncol
    layer$opacity <- opacity %||% 0.35
  } else if (identical(type, "heatmap")) {
    values <- wsi_viewer_matrix_values(data, max_cells = max_cells)
    layer$values <- values$values
    layer$nrow <- values$nrow
    layer$ncol <- values$ncol
    layer$min <- if (is.finite(values$min)) values$min else 0
    layer$max <- if (is.finite(values$max)) values$max else 1
    layer$extent <- wsi_viewer_layer_extent(extent, slide = slide, ncol = values$ncol, nrow = values$nrow)
    layer$count <- values$nrow * values$ncol
  } else if (identical(type, "image")) {
    if (is.character(data) && length(data) == 1L) {
      path <- wsi_validate_input_path(data)
      ext <- tolower(tools::file_ext(path))
      mime <- switch(ext, jpg = "image/jpeg", jpeg = "image/jpeg", gif = "image/gif",
                     webp = "image/webp", tif = "image/tiff", tiff = "image/tiff",
                     "image/png")
      layer$data_uri <- wsi_image_data_uri(path, mime = mime)
    } else if (inherits(data, "raster") || is.array(data)) {
      layer$data_uri <- wsi_array_data_uri(data)
    } else {
      wsi_abort("Image layers must be image paths, rasters, or arrays.")
    }
    layer$extent <- wsi_viewer_layer_extent(extent, slide = slide)
    layer$count <- 1L
  }

  structure(layer, class = c("wsi_viewer_layer", "list"))
}

wsi_viewer_layer_key <- function(layer) {
  layer$id %||% wsi_viewer_layer_id(layer$name %||% "layer")
}

wsi_viewer_get_layer <- function(layers, key) {
  if (is.null(key) || !length(key)) {
    return(NULL)
  }
  key <- as.character(key[[1L]])
  for (layer in layers %||% list()) {
    if (identical(as.character(layer$id %||% ""), key) ||
        identical(as.character(layer$name %||% ""), key)) {
      return(layer)
    }
  }
  NULL
}

wsi_viewer_set_layer <- function(layers, layer) {
  key <- wsi_viewer_layer_key(layer)
  layers <- layers %||% list()
  idx <- match(key, vapply(layers, wsi_viewer_layer_key, character(1)))
  if (is.na(idx)) {
    layers[[length(layers) + 1L]] <- layer
  } else {
    layers[[idx]] <- layer
  }
  names(layers) <- vapply(layers, wsi_viewer_layer_key, character(1))
  layers
}

wsi_viewer_layer_summary <- function(layers) {
  layers <- layers %||% list()
  if (!length(layers)) {
    return(data.frame(id = character(), name = character(), type = character(),
                      visible = logical(), opacity = numeric(), count = integer(),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    id = vapply(layers, function(layer) as.character(layer$id %||% ""), character(1)),
    name = vapply(layers, function(layer) as.character(layer$name %||% ""), character(1)),
    type = vapply(layers, function(layer) as.character(layer$source_type %||% layer$type %||% ""), character(1)),
    visible = vapply(layers, function(layer) isTRUE(layer$visible), logical(1)),
    opacity = vapply(layers, function(layer) as.numeric(layer$opacity %||% NA_real_), numeric(1)),
    count = vapply(layers, function(layer) as.integer(layer$count %||% 0L), integer(1)),
    stringsAsFactors = FALSE
  )
}

wsi_viewer_update_layers_from_payload <- function(layers, payload_layers) {
  if (is.null(payload_layers) || !length(payload_layers)) {
    return(layers %||% list())
  }
  out <- layers %||% list()
  for (summary in payload_layers) {
    if (!is.list(summary)) {
      next
    }
    key <- summary$id %||% summary$name
    layer <- wsi_viewer_get_layer(out, key)
    if (is.null(layer)) {
      next
    }
    if (!is.null(summary$visible)) {
      layer$visible <- isTRUE(summary$visible)
    }
    if (!is.null(summary$opacity)) {
      layer$opacity <- max(0, min(1, as.numeric(summary$opacity)))
    }
    out <- wsi_viewer_set_layer(out, layer)
  }
  out
}

wsi_viewer_session_pump <- function(session, timeout = 0L) {
  if (inherits(session, "wsi_viewer_session") && requireNamespace("httpuv", quietly = TRUE)) {
    try(httpuv::service(as.integer(timeout)), silent = TRUE)
  }
  invisible(session)
}

wsi_viewer_session_slide_input <- function(slide, require_path = FALSE) {
  path <- slide$path %||% NULL
  if (is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path) && file.exists(path)) {
    return(path)
  }
  if (isTRUE(require_path)) {
    wsi_abort("This async operation needs a file-backed slide path.")
  }
  slide
}

wsi_viewer_session_selected_roi <- function(session, roi = NULL, service = TRUE) {
  if (is.null(roi)) {
    selected <- session$get_selected_roi(service = service)
    if (inherits(selected, "wsi_roi") && nrow(selected)) {
      return(selected[1L, , drop = FALSE])
    }
    selected <- session$get_selected_rois(service = service)
    if (inherits(selected, "wsi_roi") && nrow(selected)) {
      return(selected[1L, , drop = FALSE])
    }
    wsi_abort("Select or draw an ROI in the viewer before starting selected-ROI analysis.")
  }
  roi <- wsi_viewer_coerce_rois(roi)
  if (!nrow(roi)) {
    wsi_abort("`roi` does not contain any regions.")
  }
  if (nrow(roi) > 1L) {
    wsi_warn("Multiple ROIs were supplied; using the first ROI for this operation.")
  }
  roi[1L, , drop = FALSE]
}

wsi_viewer_session_selected_rois <- function(session, roi = NULL, service = TRUE) {
  if (is.null(roi)) {
    selected <- session$get_selected_rois(service = service)
    if (inherits(selected, "wsi_roi") && nrow(selected)) {
      return(selected)
    }
    return(NULL)
  }
  roi <- wsi_viewer_coerce_rois(roi)
  if (!nrow(roi)) {
    return(NULL)
  }
  roi
}

wsi_viewer_session_register_job <- function(session, job) {
  session$jobs <- session$jobs %||% list()
  session$jobs[[job$id]] <- job
  wsi_viewer_state_set_job(
    session$state,
    job,
    status = "queued",
    message = sprintf("%s queued.", job$name),
    progress = 0
  )
  wsi_viewer_state_set_job(
    session$state,
    job,
    status = "running",
    message = sprintf("%s running.", job$name)
  )
  invisible(job)
}

wsi_viewer_session_jobs_table <- function(jobs) {
  jobs <- jobs %||% list()
  if (!length(jobs)) {
    return(data.frame(
      id = character(),
      name = character(),
      pid = integer(),
      status = character(),
      progress = numeric(),
      message = character(),
      started = as.POSIXct(character()),
      updated = character(),
      stringsAsFactors = FALSE
    ))
  }
  meta <- lapply(jobs, function(job) {
    tryCatch(job$metadata(), error = function(err) list(
      id = job$id %||% "",
      name = job$name %||% "wsiTools job",
      pid = NA_integer_,
      display_status = "failed",
      progress = NA_real_,
      message = conditionMessage(err),
      started = job$started %||% NA,
      updated = as.character(Sys.time())
    ))
  })
  data.frame(
    id = vapply(meta, function(x) as.character(x$id %||% ""), character(1)),
    name = vapply(meta, function(x) as.character(x$name %||% ""), character(1)),
    pid = vapply(meta, function(x) as.integer(x$pid %||% NA_integer_), integer(1)),
    status = vapply(meta, function(x) as.character(x$display_status %||% wsi_job_display_status(x$status)), character(1)),
    progress = vapply(meta, function(x) as.numeric(x$progress %||% NA_real_), numeric(1)),
    message = vapply(meta, function(x) as.character(x$message %||% ""), character(1)),
    started = as.POSIXct(vapply(meta, function(x) as.character(x$started %||% NA), character(1))),
    updated = vapply(meta, function(x) as.character(x$updated %||% ""), character(1)),
    stringsAsFactors = FALSE
  )
}

wsi_viewer_session_record_job_failure <- function(session, job, error, event = "job_failed",
                                                  service = TRUE) {
  wsi_viewer_state_set_job(
    session$state,
    job,
    status = "failed",
    message = conditionMessage(error),
    log = c(job$log(n = 40L), conditionMessage(error))
  )
  detail <- list(
    job_id = job$id,
    job_name = job$name,
    status = "failed",
    message = conditionMessage(error)
  )
  wsi_viewer_state_record_event(session$state, event, detail)
  if (isTRUE(service)) {
    wsi_viewer_session_pump(session, 0L)
  }
  invisible(detail)
}

wsi_viewer_session_add_segmentation_job_result <- function(session, result,
                                                           cell_radius = 8,
                                                           name = "Async cell segmentation",
                                                           job = NULL,
                                                           service = TRUE) {
  segmentation <- NULL
  added <- 0L
  if (!is.null(result$segmentation)) {
    segmentation <- tryCatch(
      wsi_viewer_coerce_segmentation(result$segmentation, radius = cell_radius),
      error = function(err) {
        wsi_warn(sprintf("Could not convert async segmentation result for viewer overlay: %s", conditionMessage(err)))
        NULL
      }
    )
    if (inherits(segmentation, "wsi_roi") && nrow(segmentation)) {
      session$state$segmentation <- wsi_viewer_bind_rois(session$state$segmentation, segmentation)
      added <- nrow(segmentation)
      wsi_viewer_queue_command(
        session$state,
        "add_segmentation",
        list(geojson = wsi_viewer_rois_to_geojson(segmentation), name = name)
      )
    }
  }

  detail <- list(
    added = added,
    engine = result$engine %||% NULL,
    crop = result$crop %||% result$input %||% NULL,
    output = result$output %||% NULL,
    slide_output = result$slide_output %||% NULL,
    roi_id = result$roi_id %||% NULL,
    bbox = wsi_named_numeric_list(result$bbox),
    status = result$status %||% "complete",
    segmentation_type = if (!is.null(result$segmentation)) class(result$segmentation)[[1L]] else NULL,
    async = TRUE
  )
  if (!is.null(job)) {
    detail$job_id <- job$id
    detail$job_name <- job$name
  }
  session$state$last_segmentation <- detail
  wsi_viewer_update_measurement_tables(session$state)
  if (!is.null(job)) {
    wsi_viewer_state_set_job(
      session$state,
      job,
      status = "finished",
      progress = 100,
      message = sprintf("Imported %s cell%s.", added, if (added == 1L) "" else "s")
    )
  }
  wsi_viewer_state_record_event(session$state, "segmentation_finished", detail)
  if (isTRUE(service)) {
    wsi_viewer_session_pump(session, 0L)
  }
  invisible(detail)
}

wsi_viewer_session_record_async_result <- function(session, event, job, result,
                                                   detail = list(), service = TRUE) {
  wsi_viewer_state_set_job(
    session$state,
    job,
    status = "finished",
    progress = 100,
    message = sprintf("%s completed.", job$name)
  )
  detail <- utils::modifyList(
    list(
      job_id = job$id,
      job_name = job$name,
      status = "complete"
    ),
    detail %||% list(),
    keep.null = TRUE
  )
  wsi_viewer_state_record_event(session$state, event, detail)
  if (isTRUE(service)) {
    wsi_viewer_session_pump(session, 0L)
  }
  invisible(result)
}

wsi_viewer_session_collect_jobs <- function(session) {
  jobs <- session$jobs %||% list()
  if (!length(jobs)) {
    return(invisible(session))
  }
  for (job in jobs) {
    status <- tryCatch(job$status(), error = function(err) {
      wsi_viewer_session_record_job_failure(session, job, err, service = FALSE)
      "failed"
    })
    if (!identical(status, "failed")) {
      wsi_viewer_state_set_job(
        session$state,
        job,
        status = status,
        queue_command = !identical(wsi_job_display_status(status), (session$state$jobs[[job$id]] %||% list())$status)
      )
    }
  }
  invisible(session)
}

wsi_viewer_session_capabilities <- function(session) {
  slide <- session$slide
  slide_path <- slide$path %||% NA_character_
  file_backed <- is.character(slide_path) && length(slide_path) == 1L &&
    !is.na(slide_path) && nzchar(slide_path) && file.exists(slide_path)
  backend <- slide$backend %||% NA_character_
  region_read <- file_backed && (
    wsi_has_vips() ||
      (identical(backend, "openslide") && wsi_command_exists("openslide-write-png"))
  )
  tiled_viewer <- file_backed && wsi_has_vips()
  dynamic_tile_server <- live_bridge <- inherits(session$state, "wsi_viewer_state") && !is.null(session$url)
  httpuv_ready <- requireNamespace("httpuv", quietly = TRUE)
  async_ready <- wsi_has_callr()

  out <- data.frame(
    capability = c(
      "session_api",
      "static_viewer",
      "live_r_sync",
      "autosave_project",
      "http_service",
      "thumbnail_viewer",
      "tiled_viewer",
      "dynamic_tile_server",
      "region_export",
      "tile_grid",
      "tile_preview_layer",
      "tile_export",
      "conversion",
      "pyramid_export",
      "geojson_roundtrip",
      "annotation_editing",
      "r_controlled_layers",
      "channel_tile_layers",
      "cellphenotyper_cell_overlays",
      "measurements",
      "async_jobs"
    ),
    available = c(
      TRUE,
      TRUE,
      live_bridge,
      live_bridge && isTRUE((session$state$autosave %||% list())$enabled),
      httpuv_ready,
      TRUE,
      tiled_viewer,
      dynamic_tile_server && region_read,
      region_read,
      TRUE,
      TRUE,
      TRUE,
      region_read,
      wsi_has_vips(),
      wsi_has_vips(),
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      async_ready
    ),
    backend = c(
      "wsi_viewer_session",
      "base R/html",
      "httpuv",
      "wsi_project",
      "httpuv",
      "base R/html",
      "libvips",
      "httpuv + region backend",
      if (wsi_has_vips()) "libvips" else "openslide-write-png",
      "base R",
      "base R + viewer layer",
      if (wsi_has_vips()) "libvips" else "openslide-write-png",
      "libvips",
      "libvips",
      "jsonlite",
      "viewer JavaScript",
      "viewer state bridge",
      "OpenSeadragon tiled layers",
      "CellPhenotyper/imported cell tables",
      "base R",
      "callr"
    ),
    notes = c(
      "R methods expose get/add/list/callback/project operations.",
      "A static HTML viewer can be written without OpenSlide or libvips.",
      "Browser edits can sync back into this R session when the live bridge is running.",
      "Live viewer state can be autosaved into a .wsiproject folder.",
      "Local HTTP servicing for live viewer state and dynamic tile endpoints.",
      "Thumbnail mode is available for mock slides and file-backed slides.",
      "Deep Zoom tiled viewing requires a file-backed slide and libvips.",
      "Live on-demand tiles are served from cached region reads; static Deep Zoom remains available.",
      "Exporting ROI crops or regions requires libvips or openslide-write-png.",
      "Coordinate-only tile grids do not read image pixels.",
      "Tile previews are pushed into the viewer as locked coordinate overlays before export.",
      "Saving tile images requires region export capability.",
      "Format conversion requires libvips.",
      "Pyramidal TIFF/OME-TIFF export requires libvips.",
      "QuPath-compatible GeoJSON import/export is handled in R.",
      "Polygon drawing, brush editing, labels, colors, and manager controls are viewer-side features.",
      "R can push ROI, CellPhenotyper cell, tile-grid, point, heatmap, mask, and image layers.",
      "R can push precomputed or live channel tile sources and update visibility/opacity/color/contrast.",
      "Cell segmentation is expected to come from CellPhenotyper projects or R-supplied cell layers.",
      "Distance, ROI/cell/class summaries, and density tables are kept in R data frames.",
      "Non-blocking jobs require the suggested callr package."
    ),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_viewer_capabilities", class(out))
  out
}

wsi_attach_viewer_session_methods <- function(session) {
  self <- NULL
  session$jobs <- session$jobs %||% list()

  session$on <- function(event, callback, once = FALSE) {
    if (!is.character(event) || length(event) != 1L || is.na(event) || !nzchar(event)) {
      wsi_abort("`event` must be a single non-empty event name.")
    }
    if (!is.function(callback)) {
      wsi_abort("`callback` must be a function.")
    }
    if (!is.logical(once) || length(once) != 1L || is.na(once)) {
      wsi_abort("`once` must be `TRUE` or `FALSE`.")
    }
    self$state$callback_sequence <- as.integer(self$state$callback_sequence %||% 0L) + 1L
    id <- sprintf("callback_%d", self$state$callback_sequence)
    self$state$callbacks[[length(self$state$callbacks) + 1L]] <- list(
      id = id,
      event = event,
      callback = callback,
      once = once
    )
    invisible(id)
  }
  session$off <- function(event = NULL, id = NULL) {
    callbacks <- self$state$callbacks %||% list()
    if (!length(callbacks)) {
      return(invisible(self))
    }
    keep <- rep(TRUE, length(callbacks))
    if (!is.null(event)) {
      if (!is.character(event) || anyNA(event) || !length(event)) {
        wsi_abort("`event` must be `NULL` or character event name(s).")
      }
      keep <- keep & !vapply(callbacks, function(x) x$event %in% event, logical(1))
    }
    if (!is.null(id)) {
      if (!is.character(id) || anyNA(id) || !length(id)) {
        wsi_abort("`id` must be `NULL` or character callback id(s).")
      }
      keep <- keep & !vapply(callbacks, function(x) x$id %in% id, logical(1))
    }
    if (is.null(event) && is.null(id)) {
      keep[] <- FALSE
    }
    self$state$callbacks <- callbacks[keep]
    invisible(self)
  }
  session$list_callbacks <- function() {
    callbacks <- self$state$callbacks %||% list()
    if (!length(callbacks)) {
      return(data.frame(id = character(), event = character(), once = logical()))
    }
    data.frame(
      id = vapply(callbacks, `[[`, character(1), "id"),
      event = vapply(callbacks, `[[`, character(1), "event"),
      once = vapply(callbacks, function(x) isTRUE(x$once), logical(1)),
      stringsAsFactors = FALSE
    )
  }
  session$get_callback_errors <- function() {
    self$state$callback_errors %||% list()
  }
  session$capabilities <- function() {
    wsi_viewer_session_capabilities(self)
  }
  session$get_state <- function(service = TRUE) {
    if (isTRUE(service) && !isTRUE(self$state$dispatching_callback)) {
      wsi_viewer_session_pump(self, 0L)
    }
    wsi_viewer_state(self)
  }
  session$get_rois <- function(service = TRUE) {
    session$get_state(service = service)$rois
  }
  session$get_selected_roi <- function(service = TRUE) {
    session$get_state(service = service)$selected_roi
  }
  session$get_selected_rois <- function(service = TRUE) {
    session$get_state(service = service)$selected_rois
  }
  session$get_selected_object <- function(service = TRUE) {
    session$get_state(service = service)$selected_object
  }
  session$get_measurements <- function(service = TRUE) {
    session$get_state(service = service)$measurements
  }
  session$get_trajectories <- function(service = TRUE) {
    session$get_state(service = service)$trajectories
  }
  session$get_roi_summary <- function(service = TRUE) {
    session$get_state(service = service)$roi_summary
  }
  session$get_cell_summary <- function(service = TRUE) {
    session$get_state(service = service)$cell_summary
  }
  session$get_class_summary <- function(service = TRUE) {
    session$get_state(service = service)$class_summary
  }
  session$get_ihc_summary <- function(service = TRUE) {
    session$get_state(service = service)$ihc_summary
  }
  session$get_ihc_class_summary <- function(service = TRUE) {
    session$get_state(service = service)$ihc_class_summary
  }
  session$get_segmentation <- function(service = TRUE) {
    session$get_state(service = service)$segmentation
  }
  session$get_layers <- function(service = TRUE) {
    session$get_state(service = service)$layers
  }
  session$get_channel_sources <- function(service = TRUE) {
    session$get_state(service = service)$channel_sources
  }
  session$get_channel_settings <- function(service = TRUE) {
    session$get_state(service = service)$channel_settings
  }
  session$get_kodama_selection <- function(service = TRUE) {
    session$get_state(service = service)$kodama_selection
  }
  session$get_annotation_spots <- function(service = TRUE) {
    session$get_state(service = service)$annotation_spots
  }
  session$get_spot_annotation_table <- function(service = TRUE) {
    session$get_annotation_spots(service = service)
  }
  session$get_selected_spots <- function(service = TRUE) {
    selection <- session$get_state(service = service)$seurat_selection %||%
      list(labels = character(), count = 0L, matched_count = 0L)
    labels <- as.character(selection$labels %||% character())
    labels <- labels[nzchar(labels) & !is.na(labels)]
    out <- data.frame(
      spot_id = labels,
      spot_label = labels,
      selected = rep(TRUE, length(labels)),
      stringsAsFactors = FALSE
    )
    attr(out, "count") <- as.integer(selection$count %||% length(labels))
    attr(out, "matched_count") <- as.integer(selection$matched_count %||% NA_integer_)
    class(out) <- c("wsi_selected_spots", class(out))
    out
  }
  session$list_layers <- function(service = TRUE) {
    wsi_viewer_layer_summary(session$get_layers(service = service))
  }
  session$get_events <- function(service = TRUE) {
    session$get_state(service = service)$events
  }
  session$get_history <- function(service = TRUE) {
    session$get_state(service = service)$history
  }
  session$get_logs <- function(service = TRUE) {
    session$get_state(service = service)$logs
  }
  session$get_tile_preview <- function(service = TRUE) {
    session$get_state(service = service)$tile_preview
  }
  session$get_prediction <- function(service = TRUE) {
    session$get_state(service = service)$prediction
  }
  session$get_proximity <- function(service = TRUE) {
    session$get_state(service = service)$proximity
  }
  session$get_proximity_stats <- function(service = TRUE) {
    session$get_state(service = service)$proximity_stats
  }
  session$get_trajectory_profile <- function(service = TRUE) {
    session$get_state(service = service)$trajectory_profile
  }
  session$colour_spots_by_gene <- function(gene, service = TRUE) {
    if (!is.character(gene) || length(gene) != 1L || is.na(gene) || !nzchar(trimws(gene))) {
      wsi_abort("`gene` must be a single non-empty gene name.")
    }
    gene <- trimws(gene)
    self$state$last_event <- "r_colour_spots_by_gene"
    self$state$last_sync <- Sys.time()
    wsi_viewer_queue_command(
      self$state,
      "colour_spots_by_gene",
      list(gene = gene)
    )
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$color_spots_by_gene <- session$colour_spots_by_gene
  session$add_rois <- function(rois, name = "R session", service = TRUE) {
    rois <- wsi_viewer_coerce_rois(rois)
    if (!nrow(rois)) {
      return(invisible(self))
    }
    self$state$rois <- wsi_viewer_bind_rois(self$state$rois, rois)
    self$state$selected_roi <- wsi_viewer_selected_tail(self$state$rois)
    self$state$selected_rois <- self$state$selected_roi %||% wsi_empty_roi()
    self$state$last_event <- "r_add_rois"
    self$state$last_sync <- Sys.time()
    wsi_viewer_update_measurement_tables(self$state)
    wsi_viewer_queue_command(
      self$state,
      "add_rois",
      list(geojson = wsi_viewer_rois_to_geojson(rois), name = name)
    )
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$add_segmentation <- function(cells, radius = 8, name = "R segmentation", service = TRUE) {
    segmentation <- wsi_viewer_coerce_segmentation(cells, radius = radius)
    if (!nrow(segmentation)) {
      return(invisible(self))
    }
    self$state$segmentation <- wsi_viewer_bind_rois(self$state$segmentation, segmentation)
    self$state$last_event <- "r_add_segmentation"
    self$state$last_segmentation <- list(added = nrow(segmentation), source = name, status = "queued")
    self$state$last_sync <- Sys.time()
    wsi_viewer_update_measurement_tables(self$state)
    wsi_viewer_queue_command(
      self$state,
      "add_segmentation",
      list(geojson = wsi_viewer_rois_to_geojson(segmentation), name = name)
    )
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$add_layer <- function(name, data, type = c("auto", "rois", "segmentation",
                                                    "points", "tile_grid", "heatmap",
                                                    "mask", "image"),
                                visible = TRUE, opacity = 1, colour = NULL,
                                radius = 8, extent = NULL, replace = TRUE,
                                max_cells = 250000L, service = TRUE) {
    layer <- wsi_viewer_layer_payload(
      name = name,
      data = data,
      type = type,
      slide = self$slide,
      visible = visible,
      opacity = opacity,
      colour = colour,
      radius = radius,
      extent = extent,
      replace = replace,
      max_cells = max_cells
    )
    self$state$layers <- wsi_viewer_set_layer(self$state$layers, layer)
    self$state$last_event <- "r_add_layer"
    self$state$last_sync <- Sys.time()
    wsi_viewer_queue_command(
      self$state,
      "add_layer",
      list(layer = layer)
    )
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$set_layer_visible <- function(name, visible = TRUE, service = TRUE) {
    name <- wsi_viewer_layer_name(name)
    if (!is.logical(visible) || length(visible) != 1L || is.na(visible)) {
      wsi_abort("`visible` must be `TRUE` or `FALSE`.")
    }
    layer <- wsi_viewer_get_layer(self$state$layers, name)
    if (is.null(layer)) {
      wsi_abort(sprintf("Layer `%s` was not found.", name))
    }
    layer$visible <- visible
    self$state$layers <- wsi_viewer_set_layer(self$state$layers, layer)
    self$state$last_event <- "r_set_layer_visible"
    self$state$last_sync <- Sys.time()
    wsi_viewer_queue_command(
      self$state,
      "set_layer_visible",
      list(id = layer$id, name = layer$name, visible = visible)
    )
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$remove_layer <- function(name, service = TRUE) {
    name <- wsi_viewer_layer_name(name)
    layer <- wsi_viewer_get_layer(self$state$layers, name)
    if (is.null(layer)) {
      wsi_abort(sprintf("Layer `%s` was not found.", name))
    }
    keep <- !vapply(self$state$layers, function(x) {
      identical(as.character(x$id %||% ""), as.character(layer$id %||% "")) ||
        identical(as.character(x$name %||% ""), as.character(layer$name %||% ""))
    }, logical(1))
    self$state$layers <- self$state$layers[keep]
    self$state$last_event <- "r_remove_layer"
    self$state$last_sync <- Sys.time()
    wsi_viewer_queue_command(
      self$state,
      "remove_layer",
      list(id = layer$id, name = layer$name)
    )
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$add_channel_source <- function(source, service = TRUE) {
    source <- wsi_channel_source_payload(source)
    sources <- self$state$channel_sources %||% list()
    keys <- vapply(sources, function(x) as.character(x$id %||% ""), character(1))
    idx <- match(source$id, keys)
    if (is.na(idx)) {
      sources[[length(sources) + 1L]] <- source
    } else {
      sources[[idx]] <- source
    }
    names(sources) <- vapply(sources, function(x) as.character(x$id %||% ""), character(1))
    self$state$channel_sources <- sources
    self$state$channel_settings <- wsi_channel_settings_from_sources(sources)
    self$state$last_event <- "r_add_channel_source"
    self$state$last_sync <- Sys.time()
    wsi_viewer_queue_command(
      self$state,
      "add_channel_source",
      list(source = source)
    )
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$remove_channel_source <- function(id, service = TRUE) {
    id <- wsi_channel_source_id(id)
    sources <- self$state$channel_sources %||% list()
    keep <- !vapply(sources, function(x) identical(as.character(x$id %||% ""), id), logical(1))
    if (all(keep)) {
      wsi_abort(sprintf("Channel source `%s` was not found.", id))
    }
    self$state$channel_sources <- sources[keep]
    settings <- self$state$channel_settings %||% wsi_empty_channel_settings()
    if (is.data.frame(settings) && nrow(settings) && "id" %in% names(settings)) {
      settings <- settings[settings$id != id, , drop = FALSE]
      class(settings) <- c("wsi_channel_settings", setdiff(class(settings), "wsi_channel_settings"))
    }
    self$state$channel_settings <- settings
    self$state$last_event <- "r_remove_channel_source"
    self$state$last_sync <- Sys.time()
    wsi_viewer_queue_command(
      self$state,
      "remove_channel_source",
      list(id = id)
    )
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$set_channel_settings <- function(id, visible = NULL, opacity = NULL,
                                           colour = NULL, gain = NULL,
                                           contrast_min = NULL,
                                           contrast_max = NULL,
                                           selected_values = NULL,
                                           service = TRUE) {
    id <- wsi_channel_source_id(id)
    patch <- list(
      visible = visible,
      opacity = opacity,
      colour = colour,
      gain = gain,
      contrast_min = contrast_min,
      contrast_max = contrast_max,
      selected_values = selected_values
    )
    patch <- patch[!vapply(patch, is.null, logical(1))]
    self$state$channel_settings <- wsi_channel_update_one(self$state$channel_settings, id, patch)
    sources <- self$state$channel_sources %||% list()
    if (length(sources)) {
      sources <- lapply(sources, function(source) {
        if (identical(as.character(source$id %||% ""), id)) {
          utils::modifyList(source, patch, keep.null = TRUE)
        } else {
          source
        }
      })
      self$state$channel_sources <- sources
    }
    self$state$last_event <- "r_set_channel_settings"
    self$state$last_sync <- Sys.time()
    wsi_viewer_queue_command(
      self$state,
      "set_channel_settings",
      list(id = id, settings = patch)
    )
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$set_channel_visible <- function(id, visible = TRUE, service = TRUE) {
    if (!is.logical(visible) || length(visible) != 1L || is.na(visible)) {
      wsi_abort("`visible` must be `TRUE` or `FALSE`.")
    }
    self$set_channel_settings(id, visible = visible, service = service)
  }
  session$set_channel_opacity <- function(id, opacity = 1, service = TRUE) {
    self$set_channel_settings(id, opacity = wsi_channel_opacity(opacity), service = service)
  }
  session$set_channel_colour <- function(id, colour, service = TRUE) {
    self$set_channel_settings(id, colour = wsi_colour_to_hex(colour, "colour"), service = service)
  }
  session$set_channel_contrast <- function(id, contrast_min = 0,
                                           contrast_max = 1, gain = NULL,
                                           service = TRUE) {
    contrast <- wsi_channel_contrast(contrast_min, contrast_max)
    self$set_channel_settings(
      id,
      gain = gain,
      contrast_min = unname(contrast[["min"]]),
      contrast_max = unname(contrast[["max"]]),
      service = service
    )
  }
  session$preview_tiles <- function(roi = NULL,
                                    layer = TRUE,
                                    layer_name = "Tile preview",
                                    visible = TRUE,
                                    colour = "#facc15",
                                    opacity = 0.85,
                                    service = TRUE,
                                    ...) {
    selected <- wsi_viewer_session_selected_rois(self, roi = roi, service = service)
    args <- list(...)
    args$save_images <- FALSE
    args$output_dir <- NULL
    if (!is.null(selected)) {
      args$roi <- selected
    }
    preview <- do.call(extract_tiles, c(list(image = self$slide), args))
    preview <- wsi_tile_preview(preview)
    self$state$tile_preview <- preview
    if (isTRUE(layer)) {
      self$add_layer(
        layer_name,
        preview,
        type = "tile_grid",
        visible = visible,
        opacity = opacity,
        colour = colour,
        service = FALSE
      )
    }
    wsi_viewer_state_record_event(
      self$state,
      "tile_preview_created",
      list(
        tile_count = nrow(preview),
        roi_count = if (is.null(selected)) 0L else nrow(selected),
        layer_name = if (isTRUE(layer)) layer_name else NULL
      )
    )
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    preview
  }
  session$clear_tile_preview <- function(layer_name = "Tile preview",
                                         remove_layer = TRUE,
                                         service = TRUE) {
    self$state$tile_preview <- wsi_empty_tile_preview()
    if (isTRUE(remove_layer)) {
      layer <- wsi_viewer_get_layer(self$state$layers, wsi_viewer_layer_name(layer_name))
      if (!is.null(layer)) {
        self$remove_layer(layer$name, service = FALSE)
      }
    }
    wsi_viewer_state_record_event(
      self$state,
      "tile_preview_cleared",
      list(layer_name = layer_name)
    )
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$extract_tile_preview <- function(output_dir,
                                           format = c("png", "jpeg", "tiff"),
                                           overwrite = FALSE,
                                           manifest_file = NULL,
                                           service = TRUE) {
    format <- match.arg(format)
    preview <- self$get_tile_preview(service = service)
    if (!is.data.frame(preview) || !nrow(preview)) {
      wsi_abort("No tile preview is available. Run `viewer$preview_tiles()` before exporting previewed tiles.")
    }
    manifest <- wsi_export_tiles(
      self$slide,
      preview,
      output_dir = output_dir,
      format = format,
      overwrite = overwrite
    )
    class(manifest) <- c("wsi_tile_manifest", setdiff(class(manifest), "wsi_tile_manifest"))
    wsi_write_tile_manifest_file(manifest, manifest_file, overwrite = overwrite)
    spot_index_file <- NULL
    if ("spot_id" %in% names(manifest)) {
      spot_index_file <- file.path(output_dir, "spot_tile_index.csv")
      wsi_write_spatial_tile_index_file(manifest, spot_index_file, overwrite = overwrite)
    }
    wsi_viewer_state_record_event(
      self$state,
      "tile_preview_exported",
      list(
        tile_count = nrow(manifest),
        output_dir = output_dir,
        manifest_file = manifest_file %||% NA_character_,
        spot_index_file = spot_index_file %||% NA_character_,
        format = format
      )
    )
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    manifest
  }
  session$extract_preview_tiles <- session$extract_tile_preview
  session$run_preview_tiles <- session$extract_tile_preview
  session$measure_ihc_intensity <- function(stains,
                                            roi = NULL,
                                            image_origin = c(x = 0, y = 0),
                                            dab_channel = NULL,
                                            hematoxylin_channel = NULL,
                                            dab_threshold = 0.1,
                                            pixel_size = NULL,
                                            max_pixels = 5e6,
                                            service = TRUE) {
    selected <- wsi_viewer_session_selected_rois(self, roi = roi, service = service)
    if (is.null(selected)) {
      selected <- wsi_viewer_annotation_rois(self$state$rois)
    }
    if (!inherits(selected, "wsi_roi") || !nrow(selected)) {
      wsi_abort("No ROI is available for IHC measurement. Draw/select an ROI or pass `roi`.")
    }
    if (is.null(pixel_size)) {
      pixel_size <- self$state$pixel_size %||% NULL
    }
    report <- measure_ihc_intensity(
      stains,
      rois = selected,
      image_origin = image_origin,
      dab_channel = dab_channel,
      hematoxylin_channel = hematoxylin_channel,
      dab_threshold = dab_threshold,
      pixel_size = pixel_size,
      by = "both",
      max_pixels = max_pixels
    )
    self$state$ihc_summary <- report$roi_summary
    self$state$ihc_class_summary <- report$class_summary
    wsi_viewer_update_measurement_tables(self$state)
    wsi_viewer_state_record_event(
      self$state,
      "ihc_intensity_measured",
      list(
        roi_count = nrow(report$roi_summary),
        class_count = nrow(report$class_summary),
        dab_threshold = dab_threshold
      )
    )
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    report
  }
  session$get_jobs <- function() {
    self$jobs %||% list()
  }
  session$list_jobs <- function() {
    wsi_viewer_session_collect_jobs(self)
    wsi_viewer_session_jobs_table(self$jobs)
  }
  session$get_job <- function(id) {
    if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
      wsi_abort("`id` must be a single non-empty job id.")
    }
    job <- (self$jobs %||% list())[[id]]
    if (is.null(job)) {
      wsi_abort(sprintf("Viewer job `%s` was not found.", id))
    }
    job
  }
  session$get_job_progress <- function(id = NULL, service = TRUE) {
    if (isTRUE(service)) {
      wsi_viewer_session_collect_jobs(self)
    }
    if (is.null(id)) {
      return(wsi_viewer_jobs_table(self$state$jobs))
    }
    if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
      wsi_abort("`id` must be `NULL` or a single non-empty job id.")
    }
    record <- (self$state$jobs %||% list())[[id]]
    if (is.null(record)) {
      wsi_abort(sprintf("Viewer job `%s` was not found.", id))
    }
    record
  }
  session$get_job_log <- function(id, n = 40L, service = TRUE) {
    if (isTRUE(service)) {
      wsi_viewer_session_collect_jobs(self)
    }
    job <- (self$jobs %||% list())[[id]]
    if (!is.null(job)) {
      return(job$log(n = n))
    }
    record <- session$get_job_progress(id = id, service = FALSE)
    utils::tail(as.character(record$log %||% character()), as.integer(n))
  }
  session$run_segmentation_async <- function(roi = NULL,
                                             output_dir = "wsi_stardist_viewer_async",
                                             engine = c("stardist_he", "stardist_ihc", "mesmer_dapi"),
                                             cell_radius = 8,
                                             service = TRUE,
                                             update_viewer = TRUE,
                                             ...) {
    engine <- wsi_cell_segmentation_engine(engine)
    selected <- wsi_viewer_session_selected_rois(self, roi = roi, service = service)
    if (is.null(selected) || !inherits(selected, "wsi_roi") || !nrow(selected)) {
      wsi_abort("No selected ROI is available for cell segmentation. Draw/select an ROI or pass `roi`.")
    }
    job <- wsi_cell_segment_roi_async(
      image = wsi_viewer_session_slide_input(self$slide),
      roi = selected,
      output_dir = output_dir,
      engine = engine,
      ...
    )
    wsi_viewer_session_register_job(self, job)
    wsi_viewer_state_record_event(
      self$state,
      "segmentation_started",
      list(
        job_id = job$id,
        job_name = job$name,
        engine = engine,
        roi_count = nrow(selected),
        async = TRUE
      )
    )
    job$then(function(result, job) {
      if (isTRUE(update_viewer)) {
        wsi_viewer_session_add_segmentation_job_result(
          self,
          result,
          cell_radius = cell_radius,
          job = job,
          service = service
        )
      } else {
        wsi_viewer_session_record_async_result(
          self,
          "segmentation_finished",
          job,
          result,
          detail = list(engine = result$engine %||% engine, async = TRUE),
          service = service
        )
      }
    })
    job$catch(function(error, job) {
      wsi_viewer_session_record_job_failure(self, job, error, event = "segmentation_failed", service = service)
    })
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    job
  }
  session$run_tiles_async <- function(roi = NULL,
                                      layer = TRUE,
                                      layer_name = "Async tile grid",
                                      service = TRUE,
                                      ...) {
    selected <- wsi_viewer_session_selected_rois(self, roi = roi, service = service)
    args <- wsi_async_job_args(...)
    if (!is.null(selected)) {
      args$roi <- selected
    }
    job <- do.call(
      wsi_extract_tiles_async,
      c(list(image = wsi_viewer_session_slide_input(self$slide)), args)
    )
    wsi_viewer_session_register_job(self, job)
    wsi_viewer_state_record_event(
      self$state,
      "tiles_started",
      list(job_id = job$id, job_name = job$name, roi_count = if (is.null(selected)) 0L else nrow(selected), async = TRUE)
    )
    job$then(function(result, job) {
      if (isTRUE(layer) && is.data.frame(result)) {
        self$add_layer(layer_name, result, type = "tile_grid", service = FALSE)
      }
      wsi_viewer_session_record_async_result(
        self,
        "tiles_finished",
        job,
        result,
        detail = list(tile_count = if (is.data.frame(result)) nrow(result) else NA_integer_, async = TRUE),
        service = service
      )
    })
    job$catch(function(error, job) {
      wsi_viewer_session_record_job_failure(self, job, error, event = "tiles_failed", service = service)
    })
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    job
  }
  session$run_conversion_async <- function(output, input = NULL, service = TRUE, ...) {
    input <- input %||% wsi_viewer_session_slide_input(self$slide, require_path = TRUE)
    job <- wsi_convert_async(input = input, output = output, ...)
    wsi_viewer_session_register_job(self, job)
    wsi_viewer_state_record_event(
      self$state,
      "conversion_started",
      list(job_id = job$id, job_name = job$name, input = input, output = output, async = TRUE)
    )
    job$then(function(result, job) {
      wsi_viewer_session_record_async_result(
        self,
        "conversion_finished",
        job,
        result,
        detail = list(input = input, output = output, async = TRUE),
        service = service
      )
    })
    job$catch(function(error, job) {
      wsi_viewer_session_record_job_failure(self, job, error, event = "conversion_failed", service = service)
    })
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    job
  }
  session$run_pyramid_async <- function(output, input = NULL, service = TRUE, ...) {
    input <- input %||% wsi_viewer_session_slide_input(self$slide, require_path = TRUE)
    job <- wsi_pyramid_async(input = input, output = output, ...)
    wsi_viewer_session_register_job(self, job)
    wsi_viewer_state_record_event(
      self$state,
      "pyramid_started",
      list(job_id = job$id, job_name = job$name, input = input, output = output, async = TRUE)
    )
    job$then(function(result, job) {
      wsi_viewer_session_record_async_result(
        self,
        "pyramid_finished",
        job,
        result,
        detail = list(input = input, output = output, async = TRUE),
        service = service
      )
    })
    job$catch(function(error, job) {
      wsi_viewer_session_record_job_failure(self, job, error, event = "pyramid_failed", service = service)
    })
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    job
  }
  session$restore_project_state <- function(project, service = TRUE) {
    restore_project_state(self, project, service = service)
  }
  session$save_project <- function(path, ..., overwrite = FALSE, service = TRUE) {
    project <- wsi_project(path, slide = self$slide, viewer_state = self, ..., overwrite = overwrite)
    self$state$annotations <- list(dirty = FALSE, dirty_reason = "project_saved")
    self$state$last_event <- "project_saved"
    self$state$last_sync <- Sys.time()
    wsi_viewer_queue_command(
      self$state,
      "annotations_saved",
      list(reason = "project_saved", path = path)
    )
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    project
  }
  session$autosave_start <- function(path = NULL, interval = 5,
                                     overwrite = TRUE, service = TRUE) {
    self$state$autosave <- wsi_viewer_autosave_config(
      autosave = TRUE,
      path = path,
      interval = interval,
      overwrite = overwrite,
      name = self$name
    )
    project <- wsi_viewer_autosave_save(
      self$state,
      slide = self$slide,
      force = TRUE,
      reason = "autosave_started"
    )
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(project %||% self)
  }
  session$autosave_stop <- function(service = TRUE) {
    self$state$autosave$enabled <- FALSE
    self$state$autosave$stopped_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
    wsi_assign_viewer_state(self$state)
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(self)
  }
  session$autosave_now <- function(reason = "manual_autosave", service = TRUE) {
    project <- wsi_viewer_autosave_save(
      self$state,
      slide = self$slide,
      force = TRUE,
      reason = reason
    )
    if (isTRUE(service)) {
      wsi_viewer_session_pump(self, 0L)
    }
    invisible(project %||% self)
  }
  session$autosave_status <- function() {
    wsi_viewer_autosave_status(self$state)
  }
  session$service <- function(timeout = 100L) {
    wsi_viewer_service(self, timeout = timeout)
    wsi_viewer_session_collect_jobs(self)
    invisible(self)
  }
  session$stop <- function() {
    wsi_viewer_stop(self)
    invisible(self)
  }

  self <- session
  session
}

wsi_viewer_image_export_region <- function(payload, slide) {
  if (!is.list(payload)) {
    wsi_abort("Image export payload must be a JSON object.")
  }
  unknown <- setdiff(names(payload), c(
    "scope", "format", "region", "output_dir", "filename", "overwrite",
    "selected_roi", "annotation_count", "viewport"
  ))
  if (length(unknown)) {
    wsi_abort(sprintf(
      "Unsupported image export field%s: %s.",
      if (length(unknown) == 1L) "" else "s",
      paste(unknown, collapse = ", ")
    ))
  }
  region <- payload$region %||% NULL
  if (!is.list(region)) {
    wsi_abort("Image export requires a `region` object with x, y, width, height, and optional level.")
  }
  unknown_region <- setdiff(names(region), c("x", "y", "width", "height", "level"))
  if (length(unknown_region)) {
    wsi_abort(sprintf(
      "Unsupported image export region field%s: %s.",
      if (length(unknown_region) == 1L) "" else "s",
      paste(unknown_region, collapse = ", ")
    ))
  }
  level <- region$level %||% 0
  wsi_validate_region(
    slide,
    x = region$x,
    y = region$y,
    width = region$width,
    height = region$height,
    level = level
  )
}

wsi_viewer_image_export_filename <- function(payload, image_format, scope = "viewport") {
  ext <- wsi_format_extension(image_format)
  filename <- payload$filename %||% NULL
  if (is.null(filename) || !is.character(filename) || length(filename) != 1L ||
      is.na(filename) || !nzchar(trimws(filename))) {
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- sprintf("wsiTools_%s_%s.%s", wsi_safe_id(scope, "export"), stamp, ext)
  }
  filename <- basename(trimws(filename))
  if (!grepl(sprintf("\\.%s$", ext), filename, ignore.case = TRUE)) {
    filename <- paste0(tools::file_path_sans_ext(filename), ".", ext)
  }
  filename
}

wsi_viewer_image_export_response <- function(slide, payload, state = NULL,
                                             output_dir = getwd(),
                                             max_pixels = 50000000) {
  wsi_check_slide(slide)
  region <- wsi_viewer_image_export_region(payload, slide)
  format <- as.character(payload$format %||% "tiff")
  if (identical(format, "jpg")) {
    format <- "jpeg"
  }
  format <- match.arg(format, c("png", "jpeg", "tiff"))
  scope <- as.character(payload$scope %||% "viewport")
  if (!identical(scope, "selected_roi") && !identical(scope, "viewport")) {
    wsi_abort("`scope` must be `viewport` or `selected_roi` for image export.")
  }
  max_pixels <- as.numeric(wsi_check_scalar_number(max_pixels, "image_export_max_pixels", allow_zero = FALSE))
  pixels <- as.numeric(region$width) * as.numeric(region$height)
  if (is.finite(max_pixels) && pixels > max_pixels) {
    wsi_abort(sprintf(
      "Image export region is too large (%s pixels). Zoom in, select a smaller ROI, or increase `image_export_max_pixels` in `wsi_viewer_live()`.",
      base::format(round(pixels), big.mark = ",", scientific = FALSE, trim = TRUE)
    ))
  }
  payload_dir <- payload$output_dir %||% NULL
  if (is.character(payload_dir) && length(payload_dir) == 1L &&
      !is.na(payload_dir) && nzchar(trimws(payload_dir))) {
    output_dir <- trimws(payload_dir)
  }
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    wsi_abort("`image_export_dir` must be a single non-empty directory path.")
  }
  output_dir <- path.expand(output_dir)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_dir)) {
    wsi_abort(sprintf("Could not create image export directory: %s", output_dir))
  }
  overwrite <- isTRUE(payload$overwrite)
  filename <- wsi_viewer_image_export_filename(payload, image_format = format, scope = scope)
  output <- file.path(output_dir, filename)
  output <- wsi_validate_output_path(output, overwrite = overwrite)
  wsi_export_region(
    slide,
    x = region$x,
    y = region$y,
    width = region$width,
    height = region$height,
    level = region$level,
    output = output,
    format = format,
    overwrite = overwrite
  )
  output <- normalizePath(output, winslash = "/", mustWork = FALSE)
  result <- list(
    ok = TRUE,
    file = output,
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    format = format,
    scope = scope,
    region = list(
      x = region$x,
      y = region$y,
      width = region$width,
      height = region$height,
      level = region$level,
      downsample = region$downsample
    )
  )
  if (inherits(state, "wsi_viewer_state")) {
    wsi_viewer_state_record_event(
      state,
      "image_exported",
      list(
        file = output,
        format = format,
        scope = scope,
        width = region$width,
        height = region$height,
        level = region$level
      )
    )
  }
  result
}

wsi_start_viewer_state_server <- function(state, slide = NULL,
                                          host = "127.0.0.1", port = 8788,
                                          path = "/viewer-state", max_tries = 20L,
	                                          tile_sources = list(),
	                                          tile_path = "/tiles",
	                                          seurat = NULL,
	                                          seurat_gene_path = "/seurat-gene",
	                                          spatial_tile_path = "/spatial-tiles",
	                                          image_export_path = "/image-export",
	                                          image_export_dir = getwd(),
	                                          image_export_max_pixels = 50000000,
	                                          prediction_context = NULL,
	                                          prediction_path = "/prediction",
	                                          proximity_context = NULL,
	                                          proximity_path = "/proximity") {
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    wsi_abort(
      "Live viewer state sync requires the optional package `httpuv`.",
      class = "wsi_missing_dependency"
    )
  }
  port <- as.integer(wsi_check_scalar_number(port, "port", allow_zero = FALSE))
  max_tries <- as.integer(wsi_check_scalar_number(max_tries, "max_tries", allow_zero = TRUE))
  if (!startsWith(path, "/")) {
    path <- paste0("/", path)
  }
	  if (!startsWith(seurat_gene_path, "/")) {
	    seurat_gene_path <- paste0("/", seurat_gene_path)
	  }
	  if (!startsWith(spatial_tile_path, "/")) {
	    spatial_tile_path <- paste0("/", spatial_tile_path)
	  }
	  if (!startsWith(image_export_path, "/")) {
	    image_export_path <- paste0("/", image_export_path)
	  }
	  if (!startsWith(prediction_path, "/")) {
	    prediction_path <- paste0("/", prediction_path)
	  }
	  if (!startsWith(proximity_path, "/")) {
	    proximity_path <- paste0("/", proximity_path)
	  }
	  tile_path <- wsi_dynamic_tile_route(tile_path)
  if (inherits(tile_sources, "wsi_dynamic_tile_source")) {
    tile_sources <- list(tile_sources)
  }
  if (!is.list(tile_sources)) {
    wsi_abort("`tile_sources` must be a list of dynamic tile sources.")
  }
  if (length(tile_sources)) {
    names(tile_sources) <- vapply(tile_sources, function(source) {
      if (!inherits(source, "wsi_dynamic_tile_source")) {
        wsi_abort("`tile_sources` entries must be `wsi_dynamic_tile_source` objects.")
      }
      as.character(source$id %||% "")
    }, character(1))
  }

  viewer_state_response <- function(body, dequeue_commands = TRUE) {
    if (!nzchar(body)) {
      wsi_abort("Viewer state request body was empty.")
    }
    payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)
    if (is.list(payload) && identical(payload$type %||% NULL, "viewer_state") && is.list(payload$payload)) {
      payload <- payload$payload
    }
    wsi_viewer_state_apply(state, payload)
    wsi_viewer_autosave_save(state, slide = slide, reason = state$last_event %||% "viewer_state")
    wsi_viewer_state_response(state, dequeue_commands = dequeue_commands)
  }

	  seurat_gene_response <- function(req) {
    method <- req$REQUEST_METHOD %||% "GET"
    source_name <- if (!is.null(seurat) && inherits(seurat, "wsi_seurat_spatial")) {
      as.character(seurat$source_name %||% "spatial object")
    } else {
      "spatial object"
    }
    if (!wsi_seurat_live_gene_available(seurat)) {
      return(wsi_http_json_response(
        status = 404L,
        body = list(error = sprintf("No live %s expression source is attached to this viewer.", source_name))
      ))
    }
    gene <- NULL
    if (identical(method, "GET")) {
      query <- wsi_http_query_params(req$QUERY_STRING %||% "")
      gene <- query$gene %||% query$q %||% NULL
    } else if (identical(method, "POST")) {
      body <- wsi_http_request_body(req)
      payload <- if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else list()
      if (!is.list(payload)) {
        return(wsi_http_json_response(status = 400L, body = list(error = "Spatial gene request must be a JSON object.")))
      }
      unknown <- setdiff(names(payload), c("gene", "q"))
      if (length(unknown)) {
        return(wsi_http_json_response(
          status = 400L,
          body = list(error = sprintf("Unsupported spatial gene request field%s: %s.", if (length(unknown) == 1L) "" else "s", paste(unknown, collapse = ", ")))
        ))
      }
      gene <- payload$gene %||% payload$q %||% NULL
    } else {
      return(wsi_http_json_response(status = 405L, body = list(error = "Use GET or POST for spatial gene expression lookup.")))
    }
    if (is.null(gene) || !is.character(gene) || length(gene) != 1L || is.na(gene) || !nzchar(trimws(gene))) {
      return(wsi_http_json_response(status = 400L, body = list(error = "Provide a single non-empty `gene` value.")))
    }
    tryCatch(
      wsi_http_json_response(body = wsi_seurat_dynamic_gene_payload(seurat, trimws(gene))),
      error = function(err) {
        wsi_http_json_response(status = 404L, body = list(error = conditionMessage(err), gene = trimws(gene)))
      }
    )
	  }

	  spatial_tile_response <- function(req) {
	    method <- req$REQUEST_METHOD %||% "GET"
	    if (!identical(method, "POST")) {
	      return(wsi_http_json_response(status = 405L, body = list(error = "Use POST for spatial spot tile export.")))
	    }
	    if (is.null(slide)) {
	      return(wsi_http_json_response(status = 404L, body = list(error = "No slide is attached to this live viewer session.")))
	    }
	    tryCatch({
	      body <- wsi_http_request_body(req)
	      payload <- if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else list()
	      result <- wsi_spatial_tile_export_response(slide, payload, state = state)
	      response <- wsi_viewer_state_response(state)
	      response$spatial_tiles <- result
	      response$tile_count <- result$tile_count
	      response$output_dir <- result$output_dir
	      response$manifest_file <- result$manifest_file
	      response$spot_index_file <- result$spot_index_file
	      wsi_http_json_response(body = response)
	    }, error = function(err) {
	      wsi_http_json_response(status = 500L, body = list(ok = FALSE, error = conditionMessage(err)))
	    })
	  }

	  image_export_response <- function(req) {
	    method <- req$REQUEST_METHOD %||% "GET"
	    if (!identical(method, "POST")) {
	      return(wsi_http_json_response(status = 405L, body = list(error = "Use POST for viewer image export.")))
	    }
	    if (is.null(slide)) {
	      return(wsi_http_json_response(status = 404L, body = list(error = "No slide is attached to this live viewer session.")))
	    }
	    tryCatch({
	      body <- wsi_http_request_body(req)
	      payload <- if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else list()
	      result <- wsi_viewer_image_export_response(
	        slide,
	        payload,
	        state = state,
	        output_dir = image_export_dir,
	        max_pixels = image_export_max_pixels
	      )
	      response <- wsi_viewer_state_response(state)
	      response$image_export <- result
	      wsi_http_json_response(body = response)
	    }, error = function(err) {
	      wsi_http_json_response(status = 500L, body = list(ok = FALSE, error = conditionMessage(err)))
	    })
	  }

	  prediction_response <- function(req) {
	    method <- req$REQUEST_METHOD %||% "GET"
	    if (!identical(method, "POST")) {
	      return(wsi_http_json_response(status = 405L, body = list(error = "Use POST for PLS-LDA prediction.")))
	    }
	    context <- prediction_context %||% list()
	    if (is.null(context$spatial) && !is.null(seurat)) {
	      context$spatial <- seurat
	    }
	    if (!wsi_prediction_context_enabled(context)) {
	      return(wsi_http_json_response(
	        status = 404L,
	        body = list(error = "No live Seurat/Giotto/SpatialExperiment or CellPhenotyper prediction source is attached to this viewer.")
	      ))
	    }
	    tryCatch({
	      body <- wsi_http_request_body(req)
	      payload <- if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else list()
	      response <- wsi_prediction_response(context = context, state = state, payload = payload)
	      wsi_http_json_response(body = response)
	    }, error = function(err) {
	      wsi_viewer_state_record_event(state, "prediction_failed", list(error = conditionMessage(err)))
	      wsi_http_json_response(status = 500L, body = list(ok = FALSE, error = conditionMessage(err)))
	    })
	  }

	  proximity_response <- function(req) {
	    method <- req$REQUEST_METHOD %||% "GET"
	    if (!identical(method, "POST")) {
	      return(wsi_http_json_response(status = 405L, body = list(error = "Use POST for proximity analysis.")))
	    }
	    context <- proximity_context %||% prediction_context %||% list()
	    if (is.null(context$spatial) && !is.null(seurat)) {
	      context$spatial <- seurat
	    }
	    if (!wsi_prediction_context_enabled(context)) {
	      return(wsi_http_json_response(
	        status = 404L,
	        body = list(error = "No live Seurat/Giotto/SpatialExperiment or CellPhenotyper point source is attached to this viewer.")
	      ))
	    }
	    tryCatch({
	      body <- wsi_http_request_body(req)
	      payload <- if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else list()
	      response <- wsi_proximity_response(context = context, state = state, payload = payload)
	      wsi_http_json_response(body = response)
	    }, error = function(err) {
	      payload <- get0("payload", ifnotfound = list())
	      action <- if (is.list(payload)) tolower(as.character(payload$action %||% "")) else ""
	      event <- if (action %in% c("stats", "statistics")) "proximity_stats_failed" else "proximity_failed"
	      wsi_viewer_state_record_event(state, event, list(error = conditionMessage(err)))
	      wsi_http_json_response(status = 500L, body = list(ok = FALSE, error = conditionMessage(err)))
	    })
	  }

	  app <- list(
	    call = function(req) {
      method <- req$REQUEST_METHOD %||% "GET"
      request_path <- req$PATH_INFO %||% "/"
      if (identical(method, "OPTIONS")) {
        return(wsi_http_json_response(status = 204L, body = ""))
      }
	      if (identical(request_path, seurat_gene_path)) {
	        return(seurat_gene_response(req))
	      }
	      if (identical(request_path, spatial_tile_path)) {
	        return(spatial_tile_response(req))
	      }
	      if (identical(request_path, image_export_path)) {
	        return(image_export_response(req))
	      }
	      if (identical(request_path, prediction_path)) {
	        return(prediction_response(req))
	      }
	      if (identical(request_path, proximity_path)) {
	        return(proximity_response(req))
	      }
      tile_request <- wsi_dynamic_tile_parse(request_path, route = tile_path)
      if (!is.null(tile_request)) {
        if (!identical(method, "GET")) {
          return(wsi_http_json_response(status = 405L, body = list(error = "Use GET for viewer tiles.")))
        }
        source <- tile_sources[[tile_request$slide_id]]
        if (is.null(source)) {
          return(wsi_http_json_response(status = 404L, body = list(error = "Unknown slide tile source.")))
        }
        return(tryCatch(
          wsi_dynamic_tile_response(
            source,
            level = tile_request$level,
            col = tile_request$x,
            row = tile_request$y,
            format = tile_request$format,
            settings = wsi_dynamic_tile_query_settings(req$QUERY_STRING %||% "")
          ),
          error = function(err) {
            status <- if (inherits(err, "wsi_region_out_of_bounds")) 404L else 500L
            wsi_http_json_response(status = status, body = list(error = conditionMessage(err)))
          }
        ))
      }
      if (!identical(request_path, path)) {
        return(wsi_http_json_response(status = 404L, body = list(error = "Not found.")))
      }
      if (identical(method, "GET")) {
        return(wsi_http_json_response(body = wsi_viewer_state_response(state)))
      }
      if (!identical(method, "POST")) {
        return(wsi_http_json_response(status = 405L, body = list(error = "Use POST with viewer state JSON.")))
      }

      tryCatch({
        body <- wsi_http_request_body(req)
        wsi_http_json_response(body = viewer_state_response(body))
      }, error = function(err) {
        wsi_http_json_response(status = 500L, body = list(error = conditionMessage(err)))
      })
    },
    onWSOpen = function(ws) {
      request_path <- tryCatch(ws$request$PATH_INFO %||% "/", error = function(err) "/")
      if (!identical(request_path, path)) {
        try(ws$close(), silent = TRUE)
        return(invisible(NULL))
      }
      state$ws_sequence <- as.integer(state$ws_sequence %||% 0L) + 1L
      client_id <- sprintf("ws_%d", state$ws_sequence)
      state$ws_clients[[client_id]] <- list(id = client_id, ws = ws, opened = Sys.time())
      wsi_viewer_send_ws_one(ws, utils::modifyList(
        wsi_viewer_state_response(state, dequeue_commands = FALSE),
        list(transport = "websocket", websocket_id = client_id),
        keep.null = TRUE
      ))
      ws$onMessage(function(binary, message) {
        response <- tryCatch({
          if (isTRUE(binary)) {
            wsi_abort("Binary WebSocket messages are not supported by the viewer state bridge.")
          }
          viewer_state_response(message)
        }, error = function(err) {
          list(ok = FALSE, error = conditionMessage(err), transport = "websocket")
        })
        wsi_viewer_send_ws_one(ws, response)
      })
      ws$onClose(function() {
        clients <- state$ws_clients %||% list()
        clients[[client_id]] <- NULL
        state$ws_clients <- clients
      })
    }
  )

  last_error <- NULL
  for (candidate in seq.int(port, port + max_tries)) {
    server <- try(httpuv::startServer(host, candidate, app), silent = TRUE)
    if (!inherits(server, "try-error")) {
      url <- sprintf("http://%s:%d%s", host, candidate, path)
      ws_url <- sprintf("ws://%s:%d%s", host, candidate, path)
      return(list(
        server = server,
        host = host,
        port = candidate,
        path = path,
        url = url,
        ws_url = ws_url,
	        tile_path = tile_path,
	        seurat_gene_path = seurat_gene_path,
	        spatial_tile_path = spatial_tile_path,
	        image_export_path = image_export_path,
	        prediction_path = prediction_path,
	        proximity_path = proximity_path,
	        seurat_gene_url = if (wsi_seurat_live_gene_available(seurat)) sprintf("http://%s:%d%s", host, candidate, seurat_gene_path) else NULL,
	        spatial_tile_export_url = sprintf("http://%s:%d%s", host, candidate, spatial_tile_path),
	        image_export_url = if (!is.null(slide)) sprintf("http://%s:%d%s", host, candidate, image_export_path) else NULL,
	        prediction_url = if (wsi_prediction_context_enabled(prediction_context %||% list(spatial = seurat))) sprintf("http://%s:%d%s", host, candidate, prediction_path) else NULL,
	        proximity_url = if (wsi_prediction_context_enabled(proximity_context %||% prediction_context %||% list(spatial = seurat))) sprintf("http://%s:%d%s", host, candidate, proximity_path) else NULL,
	        tile_sources = tile_sources
	      ))
    }
    last_error <- conditionMessage(attr(server, "condition"))
  }
  wsi_abort(sprintf("Could not start live viewer state server near port %d: %s", port, last_error %||% "unknown error"))
}

wsi_project_tile_record_from_dynamic <- function(source, base_url = NULL, index = 1L) {
  metadata <- wsi_dynamic_tile_metadata(source, base_url = base_url)
  source_metadata <- source$metadata %||% list()
  slide <- source$slide %||% NULL
  path <- source_metadata$path %||% source$path %||% if (!is.null(slide)) slide$path else ""
  backend <- source_metadata$backend %||%
    if (!is.null(slide)) slide$backend else metadata$kind %||% "dynamic"
  mpp <- if (!is.null(source_metadata$mpp)) {
    source_metadata$mpp
  } else if (!is.null(slide)) {
    wsi_viewer_mpp_payload(tryCatch(wsi_mpp(slide), error = function(err) NULL))
  } else {
    NULL
  }
  objective_power <- if (!is.null(source_metadata$objective_power)) {
    source_metadata$objective_power
  } else if (!is.null(slide)) {
    wsi_viewer_objective_power_payload(
      tryCatch(wsi_objective_power(slide), error = function(err) NULL)
    )
  } else {
    NULL
  }

  list(
    id = as.character(source_metadata$project_item_id %||% source_metadata$id %||% metadata$id),
    label = as.character(source_metadata$label %||% source_metadata$name %||% metadata$name %||% sprintf("Image %d", index)),
    path = as.character(path %||% ""),
    backend = as.character(backend %||% "dynamic"),
    type = as.character(source_metadata$type %||% metadata$kind %||% "slide"),
    status = as.character(source_metadata$status %||% "live tiles"),
    message = as.character(source_metadata$message %||% "Full-resolution tiles are served on demand by the live R session."),
    width = unname(as.numeric(metadata$width)),
    height = unname(as.numeric(metadata$height)),
    tile_url_base = metadata$tile_url_base,
    tile_url_template = metadata$tile_url_template,
    tile_url_style = metadata$tile_url_style,
    tile_format = metadata$tile_format,
    tile_size = metadata$tile_size,
    tile_overlap = metadata$tile_overlap,
    min_level = metadata$min_level,
    max_level = metadata$max_level,
    image_data_uri = NULL,
    navigator_image_data_uri = NULL,
    sections = list(),
    mpp = mpp,
    objective_power = objective_power,
    active = isTRUE(source_metadata$active)
  )
}

wsi_project_images_with_dynamic_tiles <- function(project_images = NULL,
                                                  dynamic_sources = list(),
                                                  base_url = NULL) {
  if (!length(dynamic_sources)) {
    return(project_images)
  }
  records <- lapply(seq_along(dynamic_sources), function(i) {
    wsi_project_tile_record_from_dynamic(dynamic_sources[[i]], base_url = base_url, index = i)
  })
  if (is.null(project_images)) {
    return(records)
  }
  existing <- if (is.list(project_images) && !is.data.frame(project_images)) {
    project_images
  } else {
    as.list(project_images)
  }
  existing_ids <- vapply(existing, function(item) {
    if (is.list(item)) {
      as.character(item$id %||% item$path %||% "")
    } else {
      as.character(item %||% "")
    }
  }, character(1))
  existing_ids <- existing_ids[nzchar(existing_ids)]
  for (record in records) {
    record_ids <- c(record$id %||% "", record$path %||% "")
    if (any(nzchar(record_ids) & record_ids %in% existing_ids)) {
      next
    }
    existing[[length(existing) + 1L]] <- record
  }
  existing
}

#' Start a live viewer session that syncs back to R
#'
#' Opens an interactive viewer with a local WebSocket bridge and an HTTP polling
#' fallback. Browser-side
#' annotations, imported GeoJSON, edited ROI labels/classes, distance
#' measurements, and segmentation overlays are posted back to the current R
#' session. ROI area, cell-density, cell, stain-measurement, and class-summary
#' tables are refreshed as ordinary R data frames whenever ROI or segmentation
#' state changes. The returned object is a live session with convenience methods
#' such as `capabilities()`, `get_rois()`, `get_selected_roi()`, `get_selected_rois()`,
#' `get_selected_object()`, `get_selected_spots()`,
#' `get_spot_annotation_table()`,
#' `get_measurements()`, `get_roi_summary()`, `get_cell_summary()`, `get_class_summary()`,
#' `get_ihc_summary()`, `get_ihc_class_summary()`, `get_segmentation()`,
#' `get_layers()`, `get_annotation_spots()`, `get_history()`, `get_logs()`, `get_tile_preview()`,
#' `colour_spots_by_gene()`,
#' `add_rois()`, `add_segmentation()`, `measure_ihc_intensity()`,
#' `add_layer()`, `set_layer_visible()`, `preview_tiles()`,
#' `extract_tile_preview()`, `list_jobs()`,
#' `run_tiles_async()`,
#' `run_conversion_async()`, `run_pyramid_async()`, and `save_project()`.
#' Async methods require the suggested `callr` package and return `wsi_job`
#' objects with `status()` and `result()` methods. The state object
#' is an environment, so it is updated in place as new events arrive. Selected
#' ROI StarDist/Cellpose launching is no longer provided by wsiTools; run cell
#' segmentation separately with CellPhenotyper and open the resulting project or
#' cell overlays in the viewer.
#'
#' This live bridge is optional and requires the suggested `httpuv` package.
#' The ordinary [wsi_viewer()] remains a static HTML viewer for file-only use.
#'
#' @param slide A `wsi_slide` object.
#' @param ... Additional arguments passed to [wsi_viewer()].
#' @param name Name assigned in `envir` for the live state object. Companion
#'   objects named `<name>_rois`, `<name>_measurements`,
#'   `<name>_roi_summary`, `<name>_cell_summary`, `<name>_class_summary`,
#'   `<name>_ihc_summary`, `<name>_ihc_class_summary`,
#'   `<name>_segmentation`, `<name>_layers`, `<name>_selected_roi`,
#'   `<name>_selected_rois`, `<name>_selected_object`,
#'   `<name>_annotation_spots`,
#'   `<name>_history`, `<name>_logs`, `<name>_tile_preview`,
#'   `<name>_last_segmentation`, and `<name>_last_event` are refreshed after
#'   every browser sync or R-side measurement update.
#' @param envir Environment where live state objects are assigned.
#' @param host,port,path Local HTTP/WebSocket address used for browser-to-R
#'   sync.
#' @param max_tries Number of subsequent ports to try if `port` is busy.
#' @param transport Live browser-to-R transport. `"auto"` enables WebSocket
#'   sync with HTTP polling fallback, `"websocket"` requests WebSocket first,
#'   and `"polling"` disables the WebSocket URL in the generated viewer.
#' @param dynamic_tiles Whether live tiled mode should use the local `httpuv`
#'   dynamic tile server instead of precomputing Deep Zoom tiles. Static Deep
#'   Zoom generation in [wsi_viewer()] is unchanged.
#' @param dynamic_tile_format,dynamic_tile_cache_dir,dynamic_tile_path Format,
#'   cache directory, and HTTP route for on-demand live tiles.
#' @param seurat_gene_path Local HTTP route used by live Seurat viewers to
#'   retrieve one gene at a time from the active R session.
#' @param spatial_tile_path Local HTTP route used by live Seurat, Giotto, and
#'   SpatialExperiment viewers to export spot-centered image tiles from R.
#' @param image_export_path,image_export_dir,image_export_max_pixels Local HTTP
#'   route, default output directory, and maximum region size for viewer image
#'   export. The viewer can export the visible viewport or selected annotation
#'   bounding box as PNG/JPEG/TIFF through R.
#' @param prediction_path Local HTTP route used by live spatial and
#'   CellPhenotyper viewers to run optional `fastPLS` PLS-LDA prediction from
#'   selected annotation-defined training/test sets.
#' @param prediction_context Internal live prediction sources. Advanced users
#'   can pass `wsi_prediction_context()`; ordinary Seurat/Giotto/
#'   SpatialExperiment and CellPhenotyper viewer helpers set this automatically.
#' @param proximity_path Local HTTP route used by live spatial and
#'   CellPhenotyper viewers to calculate nearest-neighbour proximity from
#'   spots/cells inside one annotation to spots/cells inside another annotation.
#' @param proximity_context Internal live proximity sources. Advanced users can
#'   pass `wsi_proximity_context()`; ordinary Seurat/Giotto/SpatialExperiment
#'   and CellPhenotyper viewer helpers set this automatically.
#' @param project_tile_sources Optional dynamic tile sources used only by
#'   Project-panel image/section entries. These sources are served by the live
#'   tile server but are not exposed as Stains/channel layers.
#' @param wait If `TRUE`, service the HTTP bridge until interrupted. This is
#'   the most reliable mode for plain R sessions. Press Esc or Ctrl+C to return
#'   to the console; synced objects remain in `envir`.
#' @param open Whether to open the viewer with [utils::browseURL()].
#' @param autosave Whether to periodically save the live viewer state to a
#'   `.wsiproject` folder. May also be a single path, equivalent to setting
#'   `autosave = TRUE` and `autosave_path` to that value.
#' @param autosave_path Project directory used for autosave. If `autosave =
#'   TRUE` and this is `NULL`, a directory named
#'   `<name>_autosave.wsiproject` is used in the working directory.
#' @param autosave_interval Seconds between browser-to-R autosave syncs.
#' @param autosave_overwrite Whether autosave may update an existing
#'   `.wsiproject` index and sidecar files.
#' @param stardist Logical retained for old scripts. If `TRUE`, wsiTools starts
#'   an optional selected-ROI cell-segmentation endpoint for StarDist H&E,
#'   StarDist IHC, and Mesmer DAPI presets.
#' @param stardist_output_dir,stardist_host,stardist_port,stardist_path,stardist_max_tries
#'   Segmentation endpoint settings.
#' @param stardist_model,stardist_command,stardist_args,stardist_output_type,stardist_prob_thresh,stardist_nms_thresh,stardist_level,stardist_crop_format,stardist_backend,stardist_cell_radius,stardist_overwrite
#'   StarDist defaults for selected-ROI segmentation.
#' @param segmentation_engines,segmentation_default_engine Engine presets shown
#'   in the viewer Cells menu when `stardist = TRUE`.
#' @param stardist_ihc_model Optional StarDist model for IHC segmentation.
#' @param mesmer_command,mesmer_args Optional Mesmer command and arguments.
#' @param segmentation_tiles_x,segmentation_tiles_y,segmentation_min_area Optional
#'   command-template placeholders used by tiled external wrappers to reduce RAM.
#' @param segmentation_nuclear_channel,segmentation_membrane_channel Channel
#'   placeholders for mIHC/DAPI wrappers.
#' @param segmentation_keras_home,segmentation_pretrained_zip Optional model
#'   cache/model placeholders for external wrappers.
#'
#' @return A `wsi_viewer_session` object, invisibly. The object keeps the
#'   bridge fields (`url`, `html`, `state`) and provides live-session helper
#'   methods:
#'   `on()`, `off()`, `list_callbacks()`, `get_state()`, `get_rois()`, `get_selected_roi()`, `get_selected_rois()`,
#'   `get_selected_object()`,
#'   `get_measurements()`, `get_roi_summary()`, `get_cell_summary()`,
#'   `get_class_summary()`, `get_ihc_summary()`, `get_ihc_class_summary()`,
#'   `get_segmentation()`, `get_layers()`, `get_annotation_spots()`,
#'   `get_history()`, `get_logs()`, `get_tile_preview()`, `get_prediction()`, `get_proximity()`,
#'   `list_layers()`, `get_events()`, `add_rois()`, `add_segmentation()`,
#'   `add_layer()`, `set_layer_visible()`, `remove_layer()`,
#'   `measure_ihc_intensity()`,
#'   `preview_tiles()`, `clear_tile_preview()`, `extract_tile_preview()`,
#'   `save_project()`,
#'   `service()`, and `stop()`.
#' @export
#'
#' @examples
#' \dontrun{
#' slide <- wsi_open("sample.svs")
#' session <- wsi_viewer_live(slide, mode = "tiles", wait = FALSE)
#'
#' # After drawing in the browser and stopping the live loop:
#' session$get_rois()
#' session$get_selected_rois()
#' session$get_measurements()
#' session$get_roi_summary()
#' session$get_cell_summary()
#' session$get_class_summary()
#' session$get_ihc_summary()
#' session$get_layers()
#' session$get_history()
#' session$get_logs()
#' session$save_project("case_001.wsiproject")
#'
#' # Autosave the viewer state every 5 seconds:
#' session <- wsi_viewer_live(
#'   slide,
#'   mode = "tiles",
#'   autosave_path = "case_001_autosave.wsiproject",
#'   autosave_interval = 5,
#'   wait = FALSE
#' )
#'
#' # Push R-controlled overlays into the open viewer:
#' session$add_layer("tumour ROIs", session$get_rois())
#' session$add_layer("DAB intensity", matrix(runif(100), nrow = 10), opacity = 0.4)
#' session$set_layer_visible("DAB intensity", TRUE)
#'
#' # Preview candidate tiles in the viewer before writing image files:
#' preview <- session$preview_tiles(tile_size = 512, stride = 512)
#' manifest <- session$extract_tile_preview("confirmed_tiles")
#'
#' # Register callbacks before interacting with the browser:
#' session$on("roi_created", function(roi) print(roi))
#' session$on("roi_selected", function(roi) {
#'   crop <- export_roi_crop(slide, roi)
#' })
#' session$on("segmentation_finished", function(cells) {
#'   print(summarise_rois(session$get_rois(), cells))
#' })
#' }
wsi_viewer_session <- function(slide, ..., name = "wsi_viewer_live_state",
                               envir = parent.frame(), host = "127.0.0.1",
                               port = 8788, path = "/viewer-state",
                               max_tries = 20L,
                               transport = c("auto", "websocket", "polling"),
                               dynamic_tiles = FALSE,
                               dynamic_tile_format = c("png", "jpg", "jpeg"),
	                               dynamic_tile_cache_dir = NULL,
	                               dynamic_tile_path = "/tiles",
	                               seurat_gene_path = "/seurat-gene",
	                               spatial_tile_path = "/spatial-tiles",
	                               image_export_path = "/image-export",
	                               image_export_dir = getwd(),
	                               image_export_max_pixels = 50000000,
	                               prediction_path = "/prediction",
	                               prediction_context = NULL,
	                               proximity_path = "/proximity",
	                               proximity_context = NULL,
	                               project_tile_sources = NULL,
                               wait = interactive(),
                               open = interactive(),
                               autosave = !is.null(autosave_path),
                               autosave_path = NULL,
                               autosave_interval = 5,
                               autosave_overwrite = TRUE,
                               stardist = FALSE,
                               stardist_output_dir = "wsi_stardist_viewer",
                               stardist_host = host,
                               stardist_port = 8787,
                               stardist_path = "/segment",
                               stardist_max_tries = max_tries,
                               stardist_model = "2D_versatile_he",
                               stardist_command = NULL,
                               stardist_args = NULL,
                               stardist_output_type = c("auto", "geojson", "csv", "mask"),
                               stardist_prob_thresh = NULL,
                               stardist_nms_thresh = NULL,
                               stardist_level = 0,
                               stardist_crop_format = c("png", "tiff", "jpeg"),
                               stardist_backend = c("auto", "vips", "openslide"),
                               stardist_cell_radius = 8,
                               stardist_overwrite = TRUE,
                               segmentation_engines = c("stardist_he", "stardist_ihc", "mesmer_dapi"),
                               segmentation_default_engine = "stardist_he",
                               stardist_ihc_model = NULL,
                               mesmer_command = NULL,
                               mesmer_args = NULL,
                               segmentation_tiles_x = NULL,
                               segmentation_tiles_y = NULL,
                               segmentation_min_area = NULL,
                               segmentation_nuclear_channel = "DAPI",
                               segmentation_membrane_channel = NULL,
                               segmentation_keras_home = NULL,
                               segmentation_pretrained_zip = NULL) {
  if (!is.logical(stardist) || length(stardist) != 1L || is.na(stardist)) {
    wsi_abort("`stardist` must be `TRUE` or `FALSE`.")
  }
  transport <- match.arg(transport)
  if (!is.logical(dynamic_tiles) || length(dynamic_tiles) != 1L || is.na(dynamic_tiles)) {
    wsi_abort("`dynamic_tiles` must be `TRUE` or `FALSE`.")
  }
  dynamic_tile_format <- wsi_dynamic_tile_format(dynamic_tile_format)
  stardist_output_type <- match.arg(stardist_output_type)
  stardist_crop_format <- match.arg(stardist_crop_format)
  stardist_backend <- match.arg(stardist_backend)
  autosave_config <- wsi_viewer_autosave_config(
    autosave = autosave,
    path = autosave_path,
    interval = autosave_interval,
    overwrite = autosave_overwrite,
    name = name
  )

  state <- wsi_new_viewer_state(name = name, envir = envir)
  state$autosave <- autosave_config
  state$pixel_size <- tryCatch(wsi_mpp(slide), error = function(err) NULL)
  wsi_viewer_update_measurement_tables(state)
  wsi_assign_viewer_state(state)

  dots <- list(...)
  live_seurat <- dots$seurat %||% NULL
  live_prediction_context <- prediction_context %||% list()
  if (is.null(live_prediction_context$spatial) && !is.null(live_seurat)) {
    live_prediction_context$spatial <- live_seurat
  }
  live_proximity_context <- proximity_context %||% live_prediction_context
  if (is.null(live_proximity_context$spatial) && !is.null(live_seurat)) {
    live_proximity_context$spatial <- live_seurat
  }
  requested_channel_sources <- dots$channel_sources %||% NULL
  dynamic_project_sources <- wsi_dynamic_channel_sources(project_tile_sources)
  dynamic_source <- NULL
  if (isTRUE(dynamic_tiles)) {
    dynamic_source <- wsi_dynamic_tile_source(
      slide,
      slide_id = wsi_safe_id(name, "slide"),
      format = dynamic_tile_format,
      cache_dir = dynamic_tile_cache_dir,
      route = dynamic_tile_path
    )
  }
  requested_stain <- dots$stain %||% "none"
  dynamic_stain_sources <- NULL
  if (isTRUE(dynamic_tiles) && requested_stain %in% c("he", "ihc")) {
    dynamic_stain_sources <- tryCatch(
      wsi_stain_channel_sources(
        slide = slide,
        stain = requested_stain,
        channels = dots$channels %||% NULL,
        source_prefix = wsi_safe_id(name, "slide"),
        tile_size = if (!is.null(dynamic_source)) dynamic_source$tile_size else dots$tile_size %||% 512,
        tile_overlap = if (!is.null(dynamic_source)) dynamic_source$tile_overlap else 1,
        format = "png",
        cache_dir = if (!is.null(dynamic_source)) dynamic_source$cache_dir else dynamic_tile_cache_dir,
        route = dynamic_tile_path
      ),
      error = function(err) {
        wsi_warn(paste0(
          "Could not start tiled stain-channel sources: ",
          conditionMessage(err),
          ". Browser-side stain controls will remain available as a fallback."
        ))
        NULL
      }
    )
    if (!is.null(dynamic_stain_sources)) {
      requested_channel_sources <- wsi_channel_sources_combine(
        requested_channel_sources,
        dynamic_stain_sources
      )
    }
  }
  dynamic_channel_sources <- wsi_dynamic_channel_sources(requested_channel_sources)
  all_dynamic_sources <- c(
    if (is.null(dynamic_source)) list() else list(dynamic_source),
    dynamic_channel_sources,
    dynamic_project_sources
  )

  bridge <- wsi_start_viewer_state_server(
    state = state,
    slide = slide,
    host = host,
    port = port,
    path = path,
    max_tries = max_tries,
	    tile_sources = all_dynamic_sources,
	    tile_path = dynamic_tile_path,
	    seurat = live_seurat,
	    seurat_gene_path = seurat_gene_path,
	    spatial_tile_path = spatial_tile_path,
	    image_export_path = image_export_path,
	    image_export_dir = image_export_dir,
	    image_export_max_pixels = image_export_max_pixels,
	    prediction_context = live_prediction_context,
	    prediction_path = prediction_path,
	    proximity_context = live_proximity_context,
	    proximity_path = proximity_path
	  )

  stardist_bridge <- NULL
  session_ready <- FALSE
  on.exit({
    if (!isTRUE(session_ready)) {
      if (!is.null(stardist_bridge)) {
        try(httpuv::stopServer(stardist_bridge$server), silent = TRUE)
      }
      try(httpuv::stopServer(bridge$server), silent = TRUE)
      if (length(all_dynamic_sources)) {
        lapply(all_dynamic_sources, wsi_dynamic_tile_cleanup)
      }
    }
  }, add = TRUE)

  base_url <- sprintf("http://%s:%d", bridge$host, bridge$port)
  if (length(dynamic_project_sources)) {
    dots$project_images <- wsi_project_images_with_dynamic_tiles(
      dots$project_images %||% NULL,
      dynamic_project_sources,
      base_url = base_url
    )
  }
  if (!is.null(dynamic_source)) {
    metadata <- wsi_dynamic_tile_metadata(dynamic_source, base_url = base_url)
    cache_key <- as.character(metadata$cache_key %||% "")
    if (nzchar(cache_key)) {
      separator <- if (grepl("\\?", metadata$tile_url_template)) "&" else "?"
      metadata$tile_url_template <- paste0(
        metadata$tile_url_template,
        separator,
        "v=",
        utils::URLencode(cache_key, reserved = TRUE)
      )
    }
    state$tile_sources <- list(dynamic = metadata)
    dots$mode <- dots$mode %||% "tiles"
    dots$tile_url_base <- metadata$tile_url_base
    dots$tile_url_template <- metadata$tile_url_template
    dots$tile_url_style <- metadata$tile_url_style
    dots$tile_size <- metadata$tile_size
    dots$tile_format <- metadata$tile_format
    dots$max_level <- metadata$max_level
    dots$tile_overlap <- metadata$tile_overlap
    dots$tile_sources <- state$tile_sources
    dots$tile_source_label <- "dynamic tile server"
  }
  if (length(requested_channel_sources)) {
    dots$channel_sources <- wsi_live_channel_sources(requested_channel_sources, base_url = base_url)
  }
  if (isTRUE(stardist)) {
    stardist_bridge <- wsi_stardist_server(
      image = slide,
      output_dir = stardist_output_dir,
      host = stardist_host,
      port = stardist_port,
      path = stardist_path,
      max_tries = stardist_max_tries,
      engines = segmentation_engines,
      default_engine = segmentation_default_engine,
      model = stardist_model,
      command = stardist_command,
      args = stardist_args,
      stardist_ihc_model = stardist_ihc_model,
      mesmer_command = mesmer_command,
      mesmer_args = mesmer_args,
      output_type = stardist_output_type,
      prob_thresh = stardist_prob_thresh,
      nms_thresh = stardist_nms_thresh,
      tiles_x = segmentation_tiles_x,
      tiles_y = segmentation_tiles_y,
      min_area = segmentation_min_area,
      nuclear_channel = segmentation_nuclear_channel,
      membrane_channel = segmentation_membrane_channel,
      keras_home = segmentation_keras_home,
      pretrained_zip = segmentation_pretrained_zip,
      overwrite = stardist_overwrite,
      level = stardist_level,
      crop_format = stardist_crop_format,
      backend = stardist_backend,
      cell_radius = stardist_cell_radius,
      state = state,
      wait = FALSE
    )
    dots$segmentation_run_url <- stardist_bridge$url
    dots$segmentation_engines <- segmentation_engines
    dots$segmentation_default_engine <- segmentation_default_engine
  }
  dots$viewer_state_url <- bridge$url
  dots$viewer_state_ws_url <- if (identical(transport, "polling")) NULL else bridge$ws_url
	  dots$viewer_transport <- transport
	  dots$seurat_gene_url <- bridge$seurat_gene_url %||% NULL
	  dots$spatial_tile_export_url <- bridge$spatial_tile_export_url %||% NULL
	  dots$image_export_url <- bridge$image_export_url %||% NULL
	  dots$prediction_url <- bridge$prediction_url %||% NULL
	  dots$proximity_url <- bridge$proximity_url %||% NULL
  if (!is.null(dots$channel_sources)) {
    state$channel_sources <- wsi_channel_sources_payload(dots$channel_sources)
    state$channel_settings <- wsi_channel_settings_from_sources(state$channel_sources)
  }
  dots$autosave_enabled <- isTRUE(state$autosave$enabled)
  dots$autosave_interval <- state$autosave$interval %||% autosave_interval
  dots$autosave_path <- state$autosave$path %||% NULL
  dots$open <- open
  dots$slide <- slide
  html <- do.call(wsi_viewer, dots)

  session <- structure(
    c(
      bridge,
      list(
        state = state,
        slide = slide,
        html = html,
        name = name,
        envir = envir,
        stardist_server = stardist_bridge,
        transport = transport,
        dynamic_tile_source = dynamic_source,
        dynamic_tile_sources = all_dynamic_sources,
        dynamic_tile_cache_dir = if (!is.null(dynamic_source)) dynamic_source$cache_dir else NULL,
        dynamic_channel_cache_dirs = unique(vapply(dynamic_channel_sources, function(x) as.character(x$cache_dir %||% ""), character(1))),
        dynamic_project_cache_dirs = unique(vapply(dynamic_project_sources, function(x) as.character(x$cache_dir %||% ""), character(1)))
      )
    ),
    class = "wsi_viewer_session"
  )
  session <- wsi_attach_viewer_session_methods(session)
  if (isTRUE(state$autosave$enabled)) {
    wsi_viewer_autosave_save(state, slide = slide, force = TRUE, reason = "session_started")
  }
  session_ready <- TRUE

  message("wsiTools live viewer written to ", html)
  if (!isTRUE(open)) {
    message("Open the viewer manually from Rscript/batch sessions: ", wsi_file_url(html))
  }
  message("wsiTools live viewer sync listening at ", bridge$url)
  if (identical(transport, "polling")) {
    message("WebSocket sync disabled; HTTP polling is active.")
  } else {
    message("WebSocket sync available at ", bridge$ws_url, " with HTTP polling fallback.")
  }
  if (!is.null(dynamic_source)) {
    message("Dynamic tile server active at ", wsi_dynamic_tile_metadata(dynamic_source, base_url = sprintf("http://%s:%d", bridge$host, bridge$port))$tile_url_base)
  }
  if (length(dynamic_channel_sources)) {
    message("Dynamic channel tile overlays active: ", length(dynamic_channel_sources), " channel", if (length(dynamic_channel_sources) == 1L) "" else "s")
  }
  if (length(dynamic_project_sources)) {
    message("Dynamic project tile sources active: ", length(dynamic_project_sources), " tissue/section source", if (length(dynamic_project_sources) == 1L) "" else "s")
  }
  if (!is.null(bridge$seurat_gene_url)) {
    source_name <- as.character((live_seurat %||% list())$source_name %||% "spatial")
    message("Live ", source_name, " gene lookup active at ", bridge$seurat_gene_url)
  }
  if (!is.null(bridge$image_export_url)) {
    message("Live viewport/ROI image export active at ", bridge$image_export_url)
  }
  message("Browser edits update `", name, "` and companion objects in the chosen R environment.")
  if (isTRUE(wait)) {
    message("Press Ctrl+C or Esc to stop the live sync loop and return to R.")
    on.exit({
      if (!is.null(stardist_bridge)) {
        try(httpuv::stopServer(stardist_bridge$server), silent = TRUE)
      }
      try(httpuv::stopServer(bridge$server), silent = TRUE)
      if (length(all_dynamic_sources)) {
        lapply(all_dynamic_sources, wsi_dynamic_tile_cleanup)
      }
    }, add = TRUE)
    tryCatch(
      repeat {
        httpuv::service(100)
        wsi_viewer_session_collect_jobs(session)
      },
      interrupt = function(e) NULL
    )
  }

  invisible(session)
}

#' @rdname wsi_viewer_session
#' @export
wsi_viewer_live <- wsi_viewer_session

#' @rdname wsi_viewer_session
#' @keywords internal
wsi_viewer_stardist <- function(slide, ..., stardist = TRUE) {
  wsi_viewer_session(slide, ..., stardist = stardist)
}

#' Read live viewer state
#'
#' @param x A `wsi_viewer_session` or `wsi_viewer_state` object.
#'
#' @return A list containing ROIs, distance measurements, ROI/cell/class summary
#'   tables, trajectories, segmentation overlays, selected ROI(s), the selected
#'   viewer object, R-controlled viewer layers, previewed tile coordinates,
#'   last segmentation run metadata,
#'   KODAMA selected-cell labels, annotation history, view/stain settings,
#'   autosave status, and event history.
#' @export
wsi_viewer_state <- function(x) {
  state <- if (inherits(x, "wsi_viewer_session")) {
    x$state
  } else {
    x
  }
  if (!inherits(state, "wsi_viewer_state")) {
    wsi_abort("`x` must be a `wsi_viewer_session` or `wsi_viewer_state` object.")
  }
  list(
    rois = state$rois,
    measurements = state$measurements,
    trajectories = state$trajectories %||% wsi_empty_trajectories(),
    roi_summary = state$roi_summary,
    cell_summary = state$cell_summary,
    class_summary = state$class_summary,
    ihc_summary = state$ihc_summary %||% wsi_empty_ihc_intensity_summary("roi"),
    ihc_class_summary = state$ihc_class_summary %||% wsi_empty_ihc_intensity_summary("class"),
    segmentation = state$segmentation,
    layers = state$layers %||% list(),
    project = state$project %||% list(),
    project_snapshot = state$project_snapshot %||% NULL,
    channel_sources = state$channel_sources %||% list(),
    channel_settings = state$channel_settings %||% wsi_empty_channel_settings(),
    tile_sources = state$tile_sources %||% list(),
    kodama_selection = state$kodama_selection %||% list(labels = character(), count = 0L, matched_count = 0L),
    seurat_selection = state$seurat_selection %||% list(labels = character(), count = 0L, matched_count = 0L),
    annotation_spots = state$annotation_spots %||% wsi_empty_annotation_spots(),
    tile_preview = state$tile_preview %||% wsi_empty_tile_preview(),
    prediction = state$prediction %||% wsi_empty_prediction_result(),
    proximity = state$proximity %||% wsi_empty_proximity_result(),
    proximity_stats = state$proximity_stats %||% wsi_empty_proximity_stats_result(),
    trajectory_profile = state$trajectory_profile %||% wsi_empty_trajectory_profile(),
    selected_roi = state$selected_roi,
    selected_rois = state$selected_rois,
    selected_object = state$selected_object %||% NULL,
    last_segmentation = state$last_segmentation,
    view = state$view,
    stain = state$stain,
    annotations = state$annotations %||% list(dirty = FALSE, dirty_reason = ""),
    history = state$history %||% wsi_empty_annotation_history(),
    logs = state$logs %||% wsi_empty_viewer_logs(),
    autosave = wsi_viewer_autosave_status(state),
    jobs = wsi_viewer_jobs_table(state$jobs),
    job_details = state$jobs %||% list(),
    events = state$events,
    pending_commands = state$commands %||% list(),
    last_event = state$last_event,
    last_payload = state$last_payload,
    last_sync = state$last_sync
  )
}

#' @rdname wsi_viewer_state
#' @export
viewer_state <- wsi_viewer_state

#' Service or stop a live viewer session
#'
#' @param session A `wsi_viewer_session` object.
#' @param timeout Milliseconds to service pending HTTP events.
#'
#' @return `session`, invisibly.
#' @export
wsi_viewer_service <- function(session, timeout = 100L) {
  if (!inherits(session, "wsi_viewer_session")) {
    wsi_abort("`session` must be a `wsi_viewer_session` object.")
  }
  timeout <- as.integer(wsi_check_scalar_number(timeout, "timeout", allow_zero = TRUE))
  httpuv::service(timeout)
  invisible(session)
}

#' @rdname wsi_viewer_service
#' @export
wsi_viewer_stop <- function(session) {
  if (!inherits(session, "wsi_viewer_session")) {
    wsi_abort("`session` must be a `wsi_viewer_session` object.")
  }
  if (!is.null(session$stardist_server)) {
    try(httpuv::stopServer(session$stardist_server$server), silent = TRUE)
  }
  try(httpuv::stopServer(session$server), silent = TRUE)
  if (length(session$dynamic_tile_sources %||% list())) {
    lapply(session$dynamic_tile_sources, wsi_dynamic_tile_cleanup)
  } else if (!is.null(session$dynamic_tile_source)) {
    wsi_dynamic_tile_cleanup(session$dynamic_tile_source)
  }
  invisible(session)
}

#' @export
print.wsi_viewer_state <- function(x, ...) {
  cat("<wsi_viewer_state>\n")
  cat(sprintf("  ROIs: %d\n", nrow(x$rois)))
  cat(sprintf("  selected ROIs: %d\n", nrow(x$selected_rois)))
  cat(sprintf("  measurements: %d\n", nrow(x$measurements)))
  cat(sprintf("  trajectories: %d\n", nrow(x$trajectories %||% wsi_empty_trajectories())))
  cat(sprintf("  ROI summary rows: %d\n", nrow(x$roi_summary)))
  cat(sprintf("  cell summary rows: %d\n", nrow(x$cell_summary)))
  cat(sprintf("  segmentation overlays: %d\n", nrow(x$segmentation)))
  cat(sprintf("  layers: %d\n", length(x$layers %||% list())))
  cat(sprintf("  annotation-spot associations: %d\n", nrow(x$annotation_spots %||% wsi_empty_annotation_spots())))
  cat(sprintf("  tile preview: %d\n", nrow(x$tile_preview %||% wsi_empty_tile_preview())))
  cat(sprintf("  history entries: %d\n", nrow(x$history %||% wsi_empty_annotation_history())))
  cat(sprintf("  last event: %s\n", x$last_event %||% "none"))
  invisible(x)
}

#' @export
print.wsi_viewer_capabilities <- function(x, ...) {
  cat("<wsi_viewer_capabilities>\n")
  available <- sum(as.logical(x$available), na.rm = TRUE)
  cat(sprintf("  available: %d/%d\n", available, nrow(x)))
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

#' @export
print.wsi_viewer_session <- function(x, ...) {
  cat("<wsi_viewer_session>\n")
  cat(sprintf("  url: %s\n", x$url))
  cat(sprintf("  websocket: %s\n", x$ws_url %||% "none"))
  cat(sprintf("  transport: %s\n", x$transport %||% "auto"))
  if (!is.null(x$dynamic_tile_cache_dir)) {
    cat(sprintf("  dynamic tile cache: %s\n", x$dynamic_tile_cache_dir))
  }
  channel_caches <- x$dynamic_channel_cache_dirs %||% character()
  channel_caches <- channel_caches[nzchar(channel_caches)]
  if (length(channel_caches)) {
    cat(sprintf("  dynamic channel caches: %s\n", paste(channel_caches, collapse = ", ")))
  }
  cat(sprintf("  html: %s\n", x$html))
  cat(sprintf("  state: %s\n", x$name))
  if (!is.null(x$stardist_server)) {
    cat(sprintf("  stardist: %s\n", x$stardist_server$url))
  }
  cat("  methods: capabilities(), on(), get_rois(), get_selected_roi(), get_selected_rois(), get_selected_object(), get_selected_spots(), get_spot_annotation_table(), get_measurements(), get_trajectories(), get_roi_summary(), get_cell_summary(), get_ihc_summary(), get_segmentation(), get_layers(), get_channel_settings(), get_kodama_selection(), get_annotation_spots(), get_history(), get_logs(), get_tile_preview(), get_prediction(), get_proximity(), get_proximity_stats(), get_trajectory_profile(), colour_spots_by_gene(), add_rois(), add_layer(), add_channel_source(), measure_ihc_intensity(), preview_tiles(), extract_tile_preview(), list_jobs(), run_tiles_async(), save_project(), autosave_start()\n")
  cat("  stop with: wsi_viewer_stop(x)\n")
  invisible(x)
}
