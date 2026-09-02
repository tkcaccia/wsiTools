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
  state$analysis_rois <- wsi_empty_roi()
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
  state$trajectory_correlations <- wsi_empty_trajectory_correlations()
  state$selected_roi <- NULL
  state$selected_rois <- wsi_empty_roi()
  state$selected_object <- NULL
  state$last_segmentation <- NULL
  state$pixel_size <- NULL
  state$view <- list()
  state$stain <- NULL
  state$channel_sources <- list()
  state$annotation_masks <- list()
  state$channel_settings <- wsi_empty_channel_settings()
  state$tile_sources <- list()
  state$kodama_selection <- list(labels = character(), count = 0L, matched_count = 0L)
  state$seurat_selection <- list(labels = character(), count = 0L, matched_count = 0L)
  state$performance <- list()
  state$annotation_spots <- wsi_empty_annotation_spots()
  state$spatial_registration <- wsi_empty_spatial_registration()
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
  # The native renderer polls a read-only state snapshot. This counter changes
  # only when a drawable/shared overlay state changes, not for camera events.
  state$native_renderer_revision <- 0L
  # Native WGPU can keep several project images open simultaneously. Browser
  # state is scoped in JavaScript; native switches need a compact R-side vault
  # so editable project data does not bleed from one source into another.
  state$native_active_source_id <- NULL
  state$native_project_states <- list()
  state$export_name <- name
  state$export_envir <- envir
  state$max_events <- as.integer(max_events)
  class(state) <- c("wsi_viewer_state", "environment")
  wsi_assign_viewer_state(state)
  state
}

wsi_native_project_state_snapshot <- function(state) {
  list(
    rois = state$rois,
    measurements = state$measurements,
    trajectories = state$trajectories,
    segmentation = state$segmentation,
    layers = state$layers,
    selected_roi = state$selected_roi,
    selected_rois = state$selected_rois,
    selected_object = state$selected_object,
    annotation_spots = state$annotation_spots,
    annotations = state$annotations,
    history = state$history,
    channel_settings = state$channel_settings,
    stain = state$stain
  )
}

wsi_native_project_state_restore <- function(state, snapshot = NULL) {
  snapshot <- snapshot %||% list()
  state$rois <- snapshot$rois %||% wsi_empty_roi()
  state$measurements <- snapshot$measurements %||% wsi_empty_measurements()
  state$trajectories <- snapshot$trajectories %||% wsi_empty_trajectories()
  state$segmentation <- snapshot$segmentation %||% wsi_empty_roi()
  state$layers <- snapshot$layers %||% list()
  state$selected_roi <- snapshot$selected_roi %||% NULL
  state$selected_rois <- snapshot$selected_rois %||% wsi_empty_roi()
  state$selected_object <- snapshot$selected_object %||% NULL
  state$annotation_spots <- snapshot$annotation_spots %||% wsi_empty_annotation_spots()
  state$annotations <- snapshot$annotations %||% list(dirty = FALSE, dirty_reason = "")
  state$history <- snapshot$history %||% wsi_empty_annotation_history()
  state$channel_settings <- snapshot$channel_settings %||% wsi_empty_channel_settings()
  state$stain <- snapshot$stain %||% NULL
  wsi_viewer_update_measurement_tables(state)
  invisible(state)
}

wsi_native_project_state_activate <- function(state, source_id) {
  source_id <- as.character(source_id %||% "")
  if (!length(source_id) || is.na(source_id[[1L]]) || !nzchar(source_id[[1L]])) {
    return(FALSE)
  }
  source_id <- source_id[[1L]]
  previous <- as.character(state$native_active_source_id %||% "")
  previous <- if (length(previous)) previous[[1L]] else ""
  if (identical(previous, source_id)) {
    return(FALSE)
  }
  # The first native selection adopts the viewer's already-initialized state
  # (for example ROIs supplied when the live session was created), rather than
  # replacing it with an empty project slot.
  if (!nzchar(previous)) {
    state$native_active_source_id <- source_id
    return(TRUE)
  }
  if (nzchar(previous)) {
    state$native_project_states[[previous]] <- wsi_native_project_state_snapshot(state)
  }
  wsi_native_project_state_restore(state, state$native_project_states[[source_id]] %||% NULL)
  state$native_active_source_id <- source_id
  TRUE
}

wsi_viewer_decimate_dense_ring <- function(ring, max_points = 500L) {
  if (!is.list(ring)) {
    return(ring)
  }
  n <- length(ring)
  max_points <- suppressWarnings(as.numeric(max_points %||% 500L))
  if (!is.finite(max_points) || max_points < 8L || n <= max_points) {
    return(ring)
  }
  max_points <- as.integer(max_points)
  point_xy <- function(point) {
    c(
      suppressWarnings(as.numeric(point$x %||% point[[1L]] %||% NA_real_)),
      suppressWarnings(as.numeric(point$y %||% point[[2L]] %||% NA_real_))
    )
  }
  closed <- n > 2L && isTRUE(all.equal(point_xy(ring[[1L]]), point_xy(ring[[n]]), tolerance = 1e-8))
  core_n <- if (closed) n - 1L else n
  target <- if (closed) max_points - 1L else max_points
  idx <- unique(pmax(1L, pmin(core_n, round(seq(1, core_n, length.out = target)))))
  out <- ring[idx]
  if (closed && length(out)) {
    out[[length(out) + 1L]] <- out[[1L]]
  }
  out
}

wsi_viewer_decimate_dense_ring_groups <- function(ring_groups, max_points_per_roi = 1200L) {
  if (!length(ring_groups)) {
    return(ring_groups)
  }
  ring_count <- sum(vapply(ring_groups, length, integer(1)))
  if (!ring_count) {
    return(ring_groups)
  }
  max_points_per_roi <- suppressWarnings(as.numeric(max_points_per_roi %||% 1200L))
  if (!is.finite(max_points_per_roi) || max_points_per_roi <= 0) {
    return(ring_groups)
  }
  per_ring <- max(16L, floor(as.integer(max_points_per_roi) / ring_count))
  lapply(ring_groups, function(group) {
    lapply(group, wsi_viewer_decimate_dense_ring, max_points = per_ring)
  })
}

wsi_viewer_dense_point_xy <- function(point) {
  c(
    suppressWarnings(as.numeric(point$x %||% point[[1L]] %||% NA_real_)),
    suppressWarnings(as.numeric(point$y %||% point[[2L]] %||% NA_real_))
  )
}

wsi_viewer_dense_point <- function(x, y) {
  list(x = unname(as.numeric(x)), y = unname(as.numeric(y)))
}

wsi_viewer_clip_dense_ring_edge <- function(points, edge, value) {
  if (length(points) < 2L) {
    return(points)
  }
  inside <- function(point) {
    xy <- wsi_viewer_dense_point_xy(point)
    if (any(!is.finite(xy))) {
      return(FALSE)
    }
    switch(
      edge,
      xmin = xy[[1L]] >= value,
      xmax = xy[[1L]] <= value,
      ymin = xy[[2L]] >= value,
      ymax = xy[[2L]] <= value,
      FALSE
    )
  }
  intersect_point <- function(start, end) {
    a <- wsi_viewer_dense_point_xy(start)
    b <- wsi_viewer_dense_point_xy(end)
    if (any(!is.finite(c(a, b)))) {
      return(end)
    }
    if (edge %in% c("xmin", "xmax")) {
      denom <- b[[1L]] - a[[1L]]
      if (abs(denom) < 1e-12) {
        return(wsi_viewer_dense_point(value, b[[2L]]))
      }
      t <- (value - a[[1L]]) / denom
      return(wsi_viewer_dense_point(value, a[[2L]] + t * (b[[2L]] - a[[2L]])))
    }
    denom <- b[[2L]] - a[[2L]]
    if (abs(denom) < 1e-12) {
      return(wsi_viewer_dense_point(b[[1L]], value))
    }
    t <- (value - a[[2L]]) / denom
    wsi_viewer_dense_point(a[[1L]] + t * (b[[1L]] - a[[1L]]), value)
  }
  out <- list()
  previous <- points[[length(points)]]
  previous_inside <- inside(previous)
  for (current in points) {
    current_inside <- inside(current)
    if (isTRUE(current_inside)) {
      if (!isTRUE(previous_inside)) {
        out[[length(out) + 1L]] <- intersect_point(previous, current)
      }
      out[[length(out) + 1L]] <- current
    } else if (isTRUE(previous_inside)) {
      out[[length(out) + 1L]] <- intersect_point(previous, current)
    }
    previous <- current
    previous_inside <- current_inside
  }
  out
}

wsi_viewer_clip_dense_ring_to_rect <- function(ring, bounds) {
  if (!is.list(ring) || length(ring) < 4L) {
    return(list())
  }
  bounds <- suppressWarnings(as.numeric(bounds[c("xmin", "ymin", "xmax", "ymax")]))
  names(bounds) <- c("xmin", "ymin", "xmax", "ymax")
  if (any(!is.finite(bounds)) || bounds[["xmax"]] <= bounds[["xmin"]] ||
      bounds[["ymax"]] <= bounds[["ymin"]]) {
    return(ring)
  }
  points <- ring[vapply(ring, function(point) {
    xy <- wsi_viewer_dense_point_xy(point)
    all(is.finite(xy))
  }, logical(1))]
  if (length(points) < 4L) {
    return(list())
  }
  first <- wsi_viewer_dense_point_xy(points[[1L]])
  last <- wsi_viewer_dense_point_xy(points[[length(points)]])
  if (isTRUE(all.equal(first, last, tolerance = 1e-8))) {
    points <- points[-length(points)]
  }
  for (edge in c("xmin", "xmax", "ymin", "ymax")) {
    points <- wsi_viewer_clip_dense_ring_edge(points, edge, bounds[[edge]])
    if (length(points) < 3L) {
      return(list())
    }
  }
  points[[length(points) + 1L]] <- points[[1L]]
  points
}

wsi_viewer_clip_dense_ring_groups_to_rect <- function(ring_groups, bounds) {
  if (!length(ring_groups)) {
    return(ring_groups)
  }
  out <- list()
  for (group in ring_groups) {
    if (!length(group)) {
      next
    }
    clipped <- lapply(group, wsi_viewer_clip_dense_ring_to_rect, bounds = bounds)
    clipped <- clipped[vapply(clipped, length, integer(1)) >= 4L]
    if (length(clipped)) {
      out[[length(out) + 1L]] <- clipped
    }
  }
  out
}

wsi_viewer_dense_roi_features <- function(roi,
                                          fill_alpha = 0.22,
                                          colour = "#F97316",
                                          source_name = "Cell annotation",
                                          bounds_only = FALSE,
                                          max_points_per_roi = 1200L,
                                          clip_bounds = NULL) {
  if (!inherits(roi, "wsi_roi") || !nrow(roi)) {
    return(list())
  }
  features <- vector("list", nrow(roi))
  class_colour_lookup <- wsi_viewer_roi_class_colour_lookup(roi)
  for (i in seq_len(nrow(roi))) {
    geometry_type <- as.character(roi$geometry_type[[i]] %||% "Polygon")
    if (isTRUE(bounds_only)) {
      xmin <- unname(as.numeric(roi$xmin[[i]]))
      ymin <- unname(as.numeric(roi$ymin[[i]]))
      xmax <- unname(as.numeric(roi$xmax[[i]]))
      ymax <- unname(as.numeric(roi$ymax[[i]]))
      if (all(is.finite(c(xmin, ymin, xmax, ymax))) && xmax > xmin && ymax > ymin) {
        geometry_type <- "Polygon"
        ring_groups <- list(list(list(
          list(x = xmin, y = ymin),
          list(x = xmax, y = ymin),
          list(x = xmax, y = ymax),
          list(x = xmin, y = ymax),
          list(x = xmin, y = ymin)
        )))
      } else {
        ring_groups <- list()
      }
    } else {
      ring_groups <- wsi_viewer_roi_ring_groups(geometry_type, roi$coordinates[[i]])
      if (!is.null(clip_bounds)) {
        ring_groups <- wsi_viewer_clip_dense_ring_groups_to_rect(ring_groups, clip_bounds)
      }
      ring_groups <- wsi_viewer_decimate_dense_ring_groups(
        ring_groups,
        max_points_per_roi = max_points_per_roi
      )
    }
    rings <- if (length(ring_groups)) ring_groups[[1L]] else list()
    add_groups <- if (length(ring_groups) > 1L) ring_groups[-1L] else list()
    drawable <- length(ring_groups) > 0L
    roi_class <- as.character(roi$class[[i]] %||% "cell")
    if (!nzchar(roi_class) || is.na(roi_class)) {
      roi_class <- "cell"
    }
    class_colour <- wsi_viewer_lookup_class_colour(class_colour_lookup, roi_class, colour)
    item_colour <- if ("color" %in% names(roi)) {
      wsi_viewer_roi_colour(roi, i, class_colour)
    } else {
      class_colour
    }
    point_count <- if (drawable) {
      sum(vapply(unlist(ring_groups, recursive = FALSE), length, integer(1)))
    } else {
      0L
    }
    features[[i]] <- list(
      id = as.character(roi$roi_id[[i]] %||% i),
      name = as.character(roi$name[[i]] %||% roi$roi_id[[i]] %||% paste0("cell_", i)),
      label = as.character(roi$name[[i]] %||% roi$roi_id[[i]] %||% paste0("cell_", i)),
      class = roi_class,
      visible = TRUE,
      locked = FALSE,
      geometry_type = geometry_type,
      source = source_name,
      drawable = drawable,
      dense_geometry = TRUE,
      point_count = point_count,
      area = if (drawable) wsi_viewer_ring_groups_area(ring_groups) else NA_real_,
      bbox = list(
        xmin = unname(as.numeric(roi$xmin[[i]])),
        ymin = unname(as.numeric(roi$ymin[[i]])),
        xmax = unname(as.numeric(roi$xmax[[i]])),
        ymax = unname(as.numeric(roi$ymax[[i]]))
      ),
      colour = item_colour,
      original_colour = item_colour,
      fill = wsi_viewer_hex_to_rgba(item_colour, alpha = fill_alpha),
      rings = rings,
      add_groups = add_groups,
      properties = list(wsiTools_dense_geometry = TRUE)
    )
  }
  features
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
    "roi_updated", "roi_label_updated", "roi_edited", "roi_curve_edited", "roi_brush_edited", "roi_deleted",
    "roi_duplicated", "roi_exported", "roi_export_selection_updated",
    "roi_color_updated", "roi_visibility_updated", "roi_lock_updated",
    "roi_smoothed", "roi_simplified", "roi_holes_filled", "roi_split",
    "roi_same_label_merged", "rois_merged", "brush_selection_updated",
    "brush_committed", "annotation_mask_updated", "viewport_changed",
    "geojson_imported", "geojson_mask_overlay_created", "class_export_rules_updated",
    "annotations_dirty", "annotations_saved",
    "annotation_history_updated", "annotation_history_cleared",
    "annotation_undo", "annotation_redo",
    "viewer_log_updated", "viewer_log_cleared", "viewer_log_exported",
    "annotation_spots_exported", "annotation_spots_updated",
    "measurement_added", "measurement_deleted", "measurements_cleared",
    "trajectory_added", "trajectory_deleted", "trajectory_area_created", "trajectory_area_updated",
    "trajectories_cleared",
    "stain_updated", "image_transform_updated",
    "layer_added", "layer_removed", "layer_updated", "layer_visibility_updated",
    "layer_opacity_updated", "tile_grid_toggled",
    "multi_view_layout_updated", "multi_view_pane_replaced", "multi_view_sync_updated",
    "performance_updated",
    "tile_preview_created", "tile_preview_cleared", "tile_preview_exported", "tiles_extracted",
    "channel_source_added", "channel_source_removed", "channel_updated",
    "artifact_detected", "artifact_flagged", "artifact_overlay_toggled",
    "artifact_sensitivity_updated", "artifacts_cleared",
    "grandqc_loaded", "grandqc_cleared", "kodama_loaded", "kodama_cleared",
    "kodama_cells_selected", "seurat_spots_selected", "seurat_gene_coloured",
    "seurat_cluster_coloured", "seurat_plot_scope_changed",
    "spatial_registration_updated", "spatial_registration_saved",
    "spatial_object_save_requested",
    "prediction_started", "prediction_finished", "prediction_failed",
    "prediction_cleared",
    "proximity_started", "proximity_finished", "proximity_failed",
    "proximity_cleared", "proximity_stats_started",
    "proximity_stats_finished", "proximity_stats_failed",
    "proximity_stats_cleared", "proximity_stats_exported",
    "trajectory_profile_started", "trajectory_profile_finished",
    "trajectory_profile_failed", "trajectory_profile_cleared", "trajectory_profile_exported",
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
    "channel_sources", "annotation_masks", "channel_settings",
    "tile_sources", "kodama_selection", "seurat_selection",
    "annotation_spots", "performance", "detail"
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

wsi_viewer_queue_command <- function(state, type, payload = list(), send_ws = TRUE) {
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
  if (isTRUE(send_ws)) {
    wsi_viewer_send_ws(state, list(ok = TRUE, commands = list(command)))
  }
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

wsi_empty_spatial_registration <- function() {
  out <- data.frame(
    source = character(),
    layer_id = character(),
    layer_name = character(),
    item_index = integer(),
    id = character(),
    label = character(),
    x = numeric(),
    y = numeric(),
    original_x = numeric(),
    original_y = numeric(),
    changed = logical(),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_spatial_registration", class(out))
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

wsi_payload_logical <- function(row, name) {
  value <- wsi_payload_value(row, name, default = NA)
  if (is.logical(value)) {
    return(isTRUE(value[[1L]]))
  }
  if (is.numeric(value)) {
    return(!is.na(value[[1L]]) && value[[1L]] != 0)
  }
  value <- tolower(as.character(value[[1L]] %||% ""))
  value %in% c("true", "t", "1", "yes", "y")
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

wsi_spatial_registration_from_payload <- function(x) {
  if (is.null(x) || !length(x)) {
    return(wsi_empty_spatial_registration())
  }
  if (is.list(x) && !is.null(x$coordinates)) {
    x <- x$coordinates
  }
  columns <- names(wsi_empty_spatial_registration())
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
    if (!length(rows)) {
      return(wsi_empty_spatial_registration())
    }
    out <- do.call(rbind, lapply(rows, function(row) {
      data.frame(
        source = wsi_payload_character(row, "source"),
        layer_id = wsi_payload_character(row, "layer_id"),
        layer_name = wsi_payload_character(row, "layer_name"),
        item_index = wsi_payload_integer(row, "item_index"),
        id = wsi_payload_character(row, "id"),
        label = wsi_payload_character(row, "label"),
        x = wsi_payload_numeric(row, "x"),
        y = wsi_payload_numeric(row, "y"),
        original_x = wsi_payload_numeric(row, "original_x"),
        original_y = wsi_payload_numeric(row, "original_y"),
        changed = wsi_payload_logical(row, "changed"),
        stringsAsFactors = FALSE
      )
    }))
  }
  out$item_index <- suppressWarnings(as.integer(out$item_index))
  for (column in c("source", "layer_id", "layer_name", "id", "label")) {
    out[[column]] <- as.character(out[[column]])
  }
  for (column in c("x", "y", "original_x", "original_y")) {
    out[[column]] <- suppressWarnings(as.numeric(out[[column]]))
  }
  out$changed <- as.logical(out$changed)
  out <- out[, columns, drop = FALSE]
  class(out) <- c("wsi_spatial_registration", setdiff(class(out), "wsi_spatial_registration"))
  out
}

wsi_spatial_object_save_format <- function(format) {
  format <- tolower(as.character(format %||% "rds")[[1L]])
  if (format %in% c("associations_csv", "annotation_associations_csv")) {
    format <- "annotation_csv"
  }
  if (!format %in% c("rds", "csv", "annotation_csv")) {
    wsi_abort("Spatial object save `format` must be `rds`, `csv`, or `annotation_csv`.")
  }
  format
}

wsi_spatial_object_save_path <- function(path, format) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
    wsi_abort("Choose or type an output path before saving the spatial object.")
  }
  path <- path.expand(trimws(path))
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(parent)) {
    wsi_abort(sprintf("Could not create output directory: %s", parent))
  }
  ext <- tolower(tools::file_ext(path))
  expected <- if (format %in% c("csv", "annotation_csv")) "csv" else "rds"
  if (!nzchar(ext)) {
    path <- paste0(path, ".", expected)
  } else if (!identical(ext, expected)) {
    wsi_abort(sprintf("Output path must end in .%s for %s export.", expected, toupper(format)))
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

wsi_spatial_object_overwrite <- function(value) {
  if (is.null(value)) {
    return(FALSE)
  }
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    wsi_abort("Spatial object save `overwrite` must be one TRUE or FALSE value.")
  }
  isTRUE(value)
}

wsi_spatial_object_registration_table <- function(payload) {
  reg <- wsi_spatial_registration_from_payload(payload$spatial_registration %||% payload)
  reg <- reg[is.finite(reg$x) & is.finite(reg$y) & nzchar(reg$id), , drop = FALSE]
  row.names(reg) <- NULL
  reg
}

wsi_spatial_object_payload_source <- function(spatial) {
  if (inherits(spatial, "wsi_seurat_spatial") || inherits(spatial, "wsi_spatial_object")) {
    return(spatial)
  }
  if (is.list(spatial) && inherits(spatial$spatial, "wsi_seurat_spatial")) {
    return(spatial$spatial)
  }
  spatial
}

wsi_spatial_object_from_source <- function(spatial) {
  source <- wsi_spatial_object_payload_source(spatial)
  if (is.list(source) && !is.null(source$expression_source$object)) {
    return(source$expression_source$object)
  }
  if (is.list(source) && !is.null(source$object)) {
    return(source$object)
  }
  source
}

wsi_spatial_object_set_slot <- function(object, slot_name, value) {
  if (isS4(object) && slot_name %in% methods::slotNames(object)) {
    methods::slot(object, slot_name) <- value
    return(object)
  }
  if (is.list(object)) {
    object[[slot_name]] <- value
    return(object)
  }
  attr(object, slot_name) <- value
  object
}

wsi_spatial_object_metadata <- function(object) {
  meta <- tryCatch(wsi_seurat_slot(object, "meta.data"), error = function(err) NULL)
  if (is.null(meta) && is.list(object)) {
    meta <- object$meta.data %||% object$meta_data %||% object$metadata %||% NULL
  }
  if (is.null(meta) || !is.data.frame(meta)) {
    return(NULL)
  }
  as.data.frame(meta, stringsAsFactors = FALSE)
}

wsi_spatial_object_table_ids <- function(tab) {
  candidates <- c(
    "barcode", "barcodes", "cell", "cells", "cell_id", "cellid",
    "spot", "spot_id", "feature_id", "id"
  )
  ids <- rownames(tab) %||% as.character(seq_len(nrow(tab)))
  for (candidate in candidates) {
    if (candidate %in% names(tab)) {
      ids <- as.character(tab[[candidate]])
      break
    }
  }
  ids
}

wsi_spatial_object_registration_index <- function(tab, registration) {
  ids <- wsi_spatial_object_table_ids(tab)
  idx <- match(ids, as.character(registration$id))
  if (!any(!is.na(idx)) && nrow(tab) == nrow(registration)) {
    idx <- seq_len(nrow(tab))
  }
  idx
}

wsi_spatial_object_update_coordinate_frame <- function(coords, registration) {
  if (is.null(coords) || !nrow(coords)) {
    return(list(coordinates = coords, matched = 0L))
  }
  original_class <- class(coords)
  out <- as.data.frame(coords, stringsAsFactors = FALSE)
  idx <- wsi_spatial_object_registration_index(out, registration)
  hit <- !is.na(idx)
  matched <- sum(hit)
  if (!matched) {
    return(list(coordinates = coords, matched = 0L))
  }
  x_values <- registration$x[idx[hit]]
  y_values <- registration$y[idx[hit]]
  x_columns <- intersect(c("x", "image_x", "imagecol", "col", "pxl_col_in_fullres", "slide_x"), names(out))
  y_columns <- intersect(c("y", "image_y", "imagerow", "row", "pxl_row_in_fullres", "slide_y"), names(out))
  if (!length(x_columns)) {
    out$x <- NA_real_
    x_columns <- "x"
  } else if (!"x" %in% names(out)) {
    out$x <- NA_real_
    x_columns <- unique(c(x_columns, "x"))
  }
  if (!length(y_columns)) {
    out$y <- NA_real_
    y_columns <- "y"
  } else if (!"y" %in% names(out)) {
    out$y <- NA_real_
    y_columns <- unique(c(y_columns, "y"))
  }
  for (column in x_columns) {
    out[[column]][hit] <- x_values
  }
  for (column in y_columns) {
    out[[column]][hit] <- y_values
  }
  class(out) <- if (inherits(coords, "data.frame")) original_class else class(out)
  list(coordinates = out, matched = matched)
}

wsi_spatial_object_update_image_coordinates <- function(object, spatial, registration) {
  images <- tryCatch(wsi_seurat_slot(object, "images"), error = function(err) NULL)
  if (is.null(images) || !is.list(images) || !length(images)) {
    return(list(object = object, matched = 0L, images = character()))
  }
  source <- wsi_spatial_object_payload_source(spatial)
  image_name <- if (is.list(source)) as.character(source$image_name %||% "") else ""
  image_names <- names(images)
  targets <- if (nzchar(image_name) && image_name %in% image_names) image_name else image_names
  matched <- 0L
  updated_names <- character()
  for (name in targets) {
    image_obj <- images[[name]]
    coords <- tryCatch(wsi_seurat_slot(image_obj, "coordinates"), error = function(err) NULL)
    if (is.null(coords) || !nrow(coords)) {
      next
    }
    updated <- wsi_spatial_object_update_coordinate_frame(coords, registration)
    if (updated$matched > 0L) {
      image_obj <- wsi_spatial_object_set_slot(image_obj, "coordinates", updated$coordinates)
      images[[name]] <- image_obj
      matched <- matched + updated$matched
      updated_names <- c(updated_names, name)
    }
  }
  if (length(updated_names)) {
    object <- wsi_spatial_object_set_slot(object, "images", images)
  }
  list(object = object, matched = matched, images = unique(updated_names))
}

wsi_spatial_object_with_registration <- function(spatial, registration) {
  object <- wsi_spatial_object_from_source(spatial)
  if (is.null(object)) {
    wsi_abort("No live spatial object is attached to this viewer session.")
  }
  registration <- as.data.frame(registration, stringsAsFactors = FALSE)
  row.names(registration) <- NULL
  class(registration) <- c("wsi_spatial_registration", setdiff(class(registration), "wsi_spatial_registration"))
  updated <- object
  meta <- wsi_spatial_object_metadata(updated)
  matched <- 0L
  if (!is.null(meta) && nrow(meta) && nrow(registration)) {
    idx <- wsi_spatial_object_registration_index(meta, registration)
    matched <- sum(!is.na(idx))
    meta$registered_x <- NA_real_
    meta$registered_y <- NA_real_
    meta$wsi_registered_x <- NA_real_
    meta$wsi_registered_y <- NA_real_
    meta$wsi_registration_changed <- FALSE
    meta$wsi_registration_source <- NA_character_
    if (matched > 0L) {
      hit <- !is.na(idx)
      meta$registered_x[hit] <- registration$x[idx[hit]]
      meta$registered_y[hit] <- registration$y[idx[hit]]
      meta$wsi_registered_x[hit] <- registration$x[idx[hit]]
      meta$wsi_registered_y[hit] <- registration$y[idx[hit]]
      meta$wsi_registration_changed[hit] <- as.logical(registration$changed[idx[hit]])
      meta$wsi_registration_source[hit] <- as.character(registration$source[idx[hit]])
    }
    updated <- wsi_spatial_object_set_slot(updated, "meta.data", meta)
  }
  image_update <- wsi_spatial_object_update_image_coordinates(updated, spatial, registration)
  updated <- image_update$object
  misc <- tryCatch(wsi_seurat_slot(updated, "misc"), error = function(err) NULL)
  if (is.null(misc) || !is.list(misc)) {
    misc <- list()
  }
  misc$wsiTools <- misc$wsiTools %||% list()
  misc$wsiTools$spatial_registration <- registration
  misc$wsiTools$spatial_registration_saved_at <- Sys.time()
  misc$wsiTools$spatial_registration_image_coordinates <- image_update$images
  updated <- wsi_spatial_object_set_slot(updated, "misc", misc)
  attr(updated, "wsi_spatial_registration") <- registration
  list(object = updated, matched = matched, image_coordinate_matched = image_update$matched)
}

wsi_spatial_object_annotation_rois <- function(payload, state = NULL) {
  geojson <- payload$annotations %||% payload$rois %||% NULL
  rois <- if (!is.null(geojson)) {
    wsi_roi_from_geojson(geojson)
  } else if (!is.null(state) && inherits(state, "wsi_viewer_state") &&
             inherits(state$analysis_rois, "wsi_roi") && nrow(state$analysis_rois)) {
    state$analysis_rois
  } else if (!is.null(state) && inherits(state, "wsi_viewer_state") &&
             inherits(state$rois, "wsi_roi")) {
    state$rois
  } else {
    wsi_empty_roi()
  }
  if (!nrow(rois) && !is.null(state) && inherits(state, "wsi_viewer_state") &&
      inherits(state$analysis_rois, "wsi_roi") && nrow(state$analysis_rois)) {
    rois <- state$analysis_rois
  }
  if (!nrow(rois)) {
    return(rois)
  }
  area <- tolower(as.character(rois$geometry_type %||% "")) %in%
    c("polygon", "multipolygon")
  out <- rois[area, , drop = FALSE]
  class(out) <- unique(c("wsi_roi", class(out)))
  out
}

wsi_spatial_object_annotation_association <- function(registration, rois) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    return(NULL)
  }
  points <- data.frame(
    point_id = as.character(registration$id),
    point_label = as.character(registration$label),
    x = as.numeric(registration$x),
    y = as.numeric(registration$y),
    stringsAsFactors = FALSE
  )
  wsi_associate_annotations(points, rois, engine = "auto")
}

wsi_spatial_object_annotation_export <- function(association,
                                                 unassigned = "Unassigned") {
  if (is.null(association)) {
    return(NULL)
  }
  out <- as.data.frame(association, stringsAsFactors = FALSE)
  category <- as.character(out$annotation_class)
  missing <- is.na(category) | !nzchar(category)
  category[missing] <- as.character(out$annotation_name[missing])
  missing <- is.na(category) | !nzchar(category)
  category[missing] <- as.character(out$annotation_id[missing])
  missing <- is.na(category) | !nzchar(category)
  category[missing] <- unassigned
  out$annotation_class <- category
  class(out) <- c(
    "wsi_annotation_association",
    setdiff(class(out), "wsi_annotation_association")
  )
  out
}

wsi_spatial_object_with_annotations <- function(object, association, rois,
                                                unassigned = "Unassigned") {
  association <- wsi_spatial_object_annotation_export(association, unassigned = unassigned)
  if (is.null(association)) {
    return(list(object = object, matched = 0L, assigned = 0L, unassigned = 0L))
  }
  meta <- wsi_spatial_object_metadata(object)
  if (is.null(meta) || !nrow(meta)) {
    wsi_abort("Could not add annotation assignments because the spatial object has no cell metadata table.")
  }
  idx <- match(wsi_spatial_object_table_ids(meta), as.character(association$point_id))
  hit <- !is.na(idx)
  if (!any(hit) && nrow(meta) == nrow(association)) {
    idx <- seq_len(nrow(meta))
    hit <- rep(TRUE, nrow(meta))
  }
  annotation <- rep(NA_character_, nrow(meta))
  annotation_id <- rep(NA_character_, nrow(meta))
  annotation_name <- rep(NA_character_, nrow(meta))
  annotation_index <- rep(NA_integer_, nrow(meta))
  if (any(hit)) {
    source_index <- idx[hit]
    annotation[hit] <- as.character(association$annotation_class[source_index])
    annotation_id[hit] <- as.character(association$annotation_id[source_index])
    annotation_name[hit] <- as.character(association$annotation_name[source_index])
    annotation_index[hit] <- suppressWarnings(as.integer(association$annotation_index[source_index]))
  }
  meta$wsi_annotation <- annotation
  meta$wsi_annotation_id <- annotation_id
  meta$wsi_annotation_name <- annotation_name
  meta$wsi_annotation_index <- annotation_index
  object <- wsi_spatial_object_set_slot(object, "meta.data", meta)

  assigned <- hit & !is.na(annotation_id) & nzchar(annotation_id)
  misc <- tryCatch(wsi_seurat_slot(object, "misc"), error = function(err) NULL)
  if (is.null(misc) || !is.list(misc)) {
    misc <- list()
  }
  misc$wsiTools <- misc$wsiTools %||% list()
  misc$wsiTools$annotation_association <- list(
    assigned_at = Sys.time(),
    cells = sum(hit),
    assigned = sum(assigned),
    unassigned = sum(hit) - sum(assigned),
    area_annotations = nrow(rois),
    coordinate_space = "level0_slide_pixels",
    metadata_columns = c(
      "wsi_annotation", "wsi_annotation_id",
      "wsi_annotation_name", "wsi_annotation_index"
    )
  )
  misc$wsiTools$annotation_rois <- wsi_viewer_rois_to_geojson(rois)
  object <- wsi_spatial_object_set_slot(object, "misc", misc)
  list(
    object = object,
    matched = sum(hit),
    assigned = sum(assigned),
    unassigned = sum(hit) - sum(assigned)
  )
}

wsi_spatial_object_annotation_spots <- function(association, registration) {
  association <- wsi_spatial_object_annotation_export(association)
  if (is.null(association) || !nrow(association)) {
    return(wsi_empty_annotation_spots())
  }
  assigned <- !is.na(association$annotation_id) & nzchar(association$annotation_id)
  association <- association[assigned, , drop = FALSE]
  if (!nrow(association)) {
    return(wsi_empty_annotation_spots())
  }
  idx <- match(as.character(association$point_id), as.character(registration$id))
  out <- data.frame(
    annotation_index = suppressWarnings(as.integer(association$annotation_index)),
    annotation_id = as.character(association$annotation_id),
    annotation_name = as.character(association$annotation_name),
    annotation_class = as.character(association$annotation_class),
    spot_id = as.character(association$point_id),
    spot_label = as.character(association$point_label),
    spot_x = as.numeric(association$x),
    spot_y = as.numeric(association$y),
    spot_layer_id = ifelse(is.na(idx), NA_character_, as.character(registration$layer_id[idx])),
    spot_layer_name = ifelse(is.na(idx), NA_character_, as.character(registration$layer_name[idx])),
    project_image = NA_character_,
    project_section = NA_character_,
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_annotation_spots", class(out))
  out
}

wsi_native_spatial_registration_from_state <- function(state, payload) {
  if (is.null(state) || !inherits(state, "wsi_viewer_state")) {
    wsi_abort("Native spatial registration requires a live viewer state.")
  }
  source_id <- as.character(payload$native_wgpu_point_source_id %||% "")
  transform <- payload$native_wgpu_spatial_transform %||% list()
  if (!nzchar(source_id) || !is.list(transform)) {
    return(wsi_empty_spatial_registration())
  }
  layers <- state$layers %||% list()
  layer <- Filter(function(candidate) identical(as.character(candidate$id %||% ""), source_id), layers)
  if (!length(layer)) return(wsi_empty_spatial_registration())
  layer <- layer[[1L]]
  items <- layer$items %||% list()
  if (is.data.frame(items)) items <- lapply(seq_len(nrow(items)), function(i) as.list(items[i, , drop = FALSE]))
  value <- function(name, default) {
    out <- suppressWarnings(as.numeric(transform[[name]] %||% default))
    if (!length(out) || !is.finite(out[[1L]])) default else out[[1L]]
  }
  flag <- function(name) isTRUE(transform[[name]] %||% FALSE)
  scale_x <- value("scale_x", 1); scale_y <- value("scale_y", 1)
  offset_x <- value("offset_x", 0); offset_y <- value("offset_y", 0)
  rotation <- value("rotation_degrees", 0) * pi / 180
  centre_x <- value("center_x", 0); centre_y <- value("center_y", 0)
  flip_h <- flag("flip_horizontal"); flip_v <- flag("flip_vertical")
  rows <- lapply(seq_along(items), function(index) {
    item <- items[[index]]
    if (!is.list(item)) return(NULL)
    x <- suppressWarnings(as.numeric(item$x %||% item$slide_x %||% NA_real_))
    y <- suppressWarnings(as.numeric(item$y %||% item$slide_y %||% NA_real_))
    id <- as.character(item$id %||% item$barcode %||% item$label %||% "")
    if (!is.finite(x) || !is.finite(y) || !nzchar(id)) return(NULL)
    dx <- x - centre_x; dy <- y - centre_y
    if (flip_h) dx <- -dx
    if (flip_v) dy <- -dy
    dx <- dx * scale_x; dy <- dy * scale_y
    if (rotation != 0) {
      cos_r <- cos(rotation); sin_r <- sin(rotation)
      next_x <- dx * cos_r + dy * sin_r
      dy <- -dx * sin_r + dy * cos_r
      dx <- next_x
    }
    data.frame(
      source = as.character(layer$source_type %||% layer$type %||% "spatial"),
      layer_id = source_id, layer_name = as.character(layer$name %||% source_id),
      item_index = as.integer(index - 1L), id = id,
      label = as.character(item$label %||% item$barcode %||% id),
      x = centre_x + dx + offset_x, y = centre_y + dy + offset_y,
      original_x = x, original_y = y,
      changed = TRUE, stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(wsi_empty_spatial_registration())
  out <- do.call(rbind, rows)
  class(out) <- c("wsi_spatial_registration", class(out))
  out
}

wsi_spatial_object_save_response <- function(spatial, payload, state = NULL) {
  if (!is.list(payload)) {
    wsi_abort("Spatial object save request must be a JSON object.")
  }
  unknown <- setdiff(
    names(payload),
    c(
      "format", "output", "path", "file", "overwrite",
      "spatial_registration", "annotations", "rois",
      "native_wgpu_spatial_transform", "native_wgpu_point_source_id"
    )
  )
  if (length(unknown)) {
    wsi_abort(sprintf(
      "Unsupported spatial object save field%s: %s.",
      if (length(unknown) == 1L) "" else "s",
      paste(unknown, collapse = ", ")
    ))
  }
  format <- wsi_spatial_object_save_format(payload$format %||% "rds")
  output <- wsi_spatial_object_save_path(payload$output %||% payload$path %||% payload$file, format)
  overwrite <- wsi_spatial_object_overwrite(payload$overwrite)
  if (file.exists(output) && !overwrite) {
    wsi_abort(
      sprintf(
        "The output file already exists: %s. Confirm replacement before saving, or choose a different name.",
        output
      ),
      class = "wsi_overwrite_required"
    )
  }
  registration <- if (!is.null(payload$native_wgpu_spatial_transform)) {
    wsi_native_spatial_registration_from_state(state, payload)
  } else {
    wsi_spatial_object_registration_table(payload)
  }
  if (!nrow(registration)) {
    wsi_abort("No registered spatial coordinates were supplied by the viewer.")
  }
  if (!is.null(state) && inherits(state, "wsi_viewer_state")) {
    state$spatial_registration <- registration
  }
  rois <- wsi_spatial_object_annotation_rois(payload, state = state)
  association <- if (nrow(rois)) {
    wsi_spatial_object_annotation_association(registration, rois)
  } else {
    NULL
  }
  association_export <- wsi_spatial_object_annotation_export(association)
  association_count <- if (is.null(association_export)) 0L else nrow(association_export)
  assigned_count <- if (is.null(association_export)) {
    0L
  } else {
    sum(!is.na(association_export$annotation_id) & nzchar(association_export$annotation_id))
  }
  if (!is.null(state) && inherits(state, "wsi_viewer_state")) {
    state$annotation_spots <- wsi_spatial_object_annotation_spots(association, registration)
  }
  image_coordinate_matched <- NA_integer_
  if (identical(format, "csv")) {
    utils::write.csv(registration, output, row.names = FALSE)
    matched <- NA_integer_
  } else if (identical(format, "annotation_csv")) {
    if (is.null(association_export) || !nrow(rois)) {
      wsi_abort("Draw or import at least one area annotation before exporting coordinate-to-annotation assignments.")
    }
    utils::write.csv(association_export, output, row.names = FALSE)
    matched <- association_count
  } else {
    updated <- wsi_spatial_object_with_registration(spatial, registration)
    if (!is.null(association_export)) {
      annotated <- wsi_spatial_object_with_annotations(updated$object, association, rois)
      updated$object <- annotated$object
      association_count <- annotated$matched
      assigned_count <- annotated$assigned
    }
    saveRDS(updated$object, output)
    matched <- updated$matched
    image_coordinate_matched <- updated$image_coordinate_matched
  }
  if (!is.null(state) && inherits(state, "wsi_viewer_state")) {
    wsi_viewer_state_record_event(
      state,
      "spatial_object_save_requested",
      list(
        file = output, format = format, count = nrow(registration),
        matched = matched,
        image_coordinate_matched = image_coordinate_matched %||% NA_integer_,
        annotation_count = nrow(rois),
        association_count = association_count,
        assigned_count = assigned_count
      )
    )
    wsi_assign_viewer_state(state)
  }
  list(
    ok = TRUE,
    file = output,
    format = format,
    overwrite = overwrite,
    count = nrow(registration),
    matched = matched,
    image_coordinate_matched = image_coordinate_matched %||% NA_integer_,
    annotation_count = nrow(rois),
    association_count = association_count,
    assigned_count = assigned_count,
    unassigned_count = association_count - assigned_count
  )
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
  assign(paste0(name, "_annotation_masks"), state$annotation_masks %||% list(), envir = envir)
  assign(paste0(name, "_channel_settings"), state$channel_settings %||% wsi_empty_channel_settings(), envir = envir)
  assign(paste0(name, "_tile_sources"), state$tile_sources %||% list(), envir = envir)
  assign(paste0(name, "_tile_preview"), state$tile_preview %||% wsi_empty_tile_preview(), envir = envir)
  assign(paste0(name, "_prediction"), state$prediction %||% wsi_empty_prediction_result(), envir = envir)
  assign(paste0(name, "_proximity"), state$proximity %||% wsi_empty_proximity_result(), envir = envir)
  assign(paste0(name, "_proximity_stats"), state$proximity_stats %||% wsi_empty_proximity_stats_result(), envir = envir)
  assign(paste0(name, "_trajectory_profile"), state$trajectory_profile %||% wsi_empty_trajectory_profile(), envir = envir)
  assign(paste0(name, "_trajectory_correlations"), state$trajectory_correlations %||% wsi_empty_trajectory_correlations(), envir = envir)
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
  assign(paste0(name, "_spatial_registration"), state$spatial_registration %||% wsi_empty_spatial_registration(), envir = envir)
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
  native_source_id <- payload$detail$native_wgpu_source_id %||% NULL
  native_overlay_fields <- c(
    "rois", "selected_roi", "selected_rois", "trajectories", "measurements", "layers",
    "stain", "channel_sources", "channel_settings", "annotation_masks"
  )
  native_overlay_changed <- any(vapply(native_overlay_fields, function(field) {
    !is.null(payload[[field, exact = TRUE]])
  }, logical(1)))
  if (identical(payload$event, "project_image_selected") &&
      is.character(native_source_id) && length(native_source_id) == 1L &&
      !is.na(native_source_id) && nzchar(native_source_id)) {
    if (isTRUE(wsi_native_project_state_activate(state, native_source_id))) {
      native_overlay_changed <- TRUE
    }
  }
  native_roi <- payload$detail$native_wgpu_roi %||% NULL
  native_trajectory <- payload$detail$native_wgpu_trajectory %||% NULL
  native_delete_roi_id <- payload$detail$native_wgpu_delete_roi_id %||% NULL
  native_delete_trajectory_id <- payload$detail$native_wgpu_delete_trajectory_id %||% NULL
  native_geojson_features <- payload$detail$native_wgpu_geojson_features %||% NULL
  native_annotation_snapshot <- payload$detail$native_wgpu_annotations %||% NULL
  native_trajectory_snapshot <- payload$detail$native_wgpu_trajectories %||% NULL
  native_segmentation_path <- payload$detail$native_wgpu_segmentation_path %||% NULL
  native_measurement <- payload$detail$native_wgpu_measurement %||% NULL
  native_delete_measurement_id <- payload$detail$native_wgpu_delete_measurement_id %||% NULL
  native_stain_mode <- payload$detail$native_wgpu_stain_mode %||% NULL
  native_stain_base_visible <- payload$detail$native_wgpu_base_visible %||% NULL
  native_stain_base_opacity <- payload$detail$native_wgpu_base_opacity %||% NULL
  if (!is.null(native_roi)) {
    native_overlay_changed <- TRUE
  }
  if (identical(payload$event, "measurement_added") && is.list(native_measurement)) {
    native_overlay_changed <- TRUE
  }
  if (is.character(native_delete_measurement_id) && length(native_delete_measurement_id) == 1L &&
      !is.na(native_delete_measurement_id) && nzchar(native_delete_measurement_id)) {
    native_overlay_changed <- TRUE
  }
  if (identical(payload$event, "stain_updated") && is.character(native_stain_mode) &&
      length(native_stain_mode) == 1L && !is.na(native_stain_mode) && nzchar(native_stain_mode)) {
    state$stain <- state$stain %||% list()
    state$stain$native_wgpu_stain_mode <- tolower(native_stain_mode)
    if (is.logical(native_stain_base_visible) && length(native_stain_base_visible) == 1L && !is.na(native_stain_base_visible)) {
      state$stain$native_wgpu_base_visible <- isTRUE(native_stain_base_visible)
    }
    opacity <- suppressWarnings(as.numeric(native_stain_base_opacity))
    if (length(opacity) == 1L && is.finite(opacity)) {
      state$stain$native_wgpu_base_opacity <- max(0, min(1, opacity))
    }
  }
  if (!is.null(native_trajectory)) {
    native_overlay_changed <- TRUE
  }
  if (payload$event %in% c("trajectories_cleared", "measurements_cleared")) {
    native_overlay_changed <- TRUE
  }
  if (is.character(native_delete_roi_id) && length(native_delete_roi_id) == 1L &&
      !is.na(native_delete_roi_id) && nzchar(native_delete_roi_id)) {
    native_overlay_changed <- TRUE
  }
  if (is.character(native_delete_trajectory_id) && length(native_delete_trajectory_id) == 1L &&
      !is.na(native_delete_trajectory_id) && nzchar(native_delete_trajectory_id)) {
    native_overlay_changed <- TRUE
  }
  if (identical(payload$event, "geojson_imported") && is.list(native_geojson_features) && length(native_geojson_features)) {
    native_overlay_changed <- TRUE
  }
  if (identical(payload$event, "segmentation_added") &&
      is.character(native_segmentation_path) && length(native_segmentation_path) == 1L &&
      !is.na(native_segmentation_path) && nzchar(native_segmentation_path)) {
    native_overlay_changed <- TRUE
  }
  if (payload$event %in% c("annotation_undo", "annotation_redo", "rois_merged", "roi_split") &&
      is.list(native_annotation_snapshot) && is.list(native_trajectory_snapshot)) {
    native_overlay_changed <- TRUE
  }

  # Browser full snapshots continue to replace their respective fields. Native
  # WGPU events are intentionally compact (for example, camera-only), so an
  # omitted field must mean "leave unchanged" rather than "erase state".
  if (!is.null(payload[["rois", exact = TRUE]])) {
    state$rois <- wsi_rois_from_payload(payload[["rois", exact = TRUE]])
  }
  # Native undo/redo restores compact Feature and trajectory snapshots through
  # the same validated bridge used for single-ROI edits. R remains the source
  # of truth before the renderer requests its next state snapshot.
  if (payload$event %in% c("annotation_undo", "annotation_redo", "rois_merged", "roi_split") &&
      is.list(native_annotation_snapshot) && is.list(native_trajectory_snapshot)) {
    state$rois <- wsi_rois_from_payload(list(
      type = "FeatureCollection",
      features = native_annotation_snapshot
    ))
    state$trajectories <- wsi_trajectories_from_payload(native_trajectory_snapshot)
    state$selected_roi <- wsi_empty_roi()
    state$selected_rois <- wsi_empty_roi()
  }
  # Native WGPU authoring sends one typed Feature at a time. Append or replace
  # it server-side so the renderer never needs to download every ROI merely to
  # preserve existing annotations.
  if (!is.null(native_roi) && payload$event %in% c("roi_created", "roi_updated", "roi_edited")) {
    native_rois <- wsi_rois_from_payload(list(type = "FeatureCollection", features = list(native_roi)))
    if (nrow(native_rois)) {
      existing <- state$rois %||% wsi_empty_roi()
      match_index <- match(native_rois$roi_id[[1L]], existing$roi_id)
      if (is.na(match_index)) {
        state$rois <- rbind(existing, native_rois)
      } else {
        existing[match_index, ] <- native_rois[1L, ]
        state$rois <- existing
      }
      state$selected_roi <- native_rois[1L, , drop = FALSE]
      state$selected_rois <- native_rois[1L, , drop = FALSE]
    }
  }
  # Native GeoJSON import is transmitted as a bounded Feature array. Keep the
  # browser and R semantics aligned by appending imported objects while R stays
  # authoritative for geometry conversion, class metadata, and persistence.
  if (identical(payload$event, "geojson_imported") && is.list(native_geojson_features) && length(native_geojson_features)) {
    imported <- wsi_rois_from_payload(list(type = "FeatureCollection", features = native_geojson_features))
    if (nrow(imported)) {
      existing <- state$rois %||% wsi_empty_roi()
      for (i in seq_len(nrow(imported))) {
        roi_id <- imported$roi_id[[i]]
        match_index <- match(roi_id, existing$roi_id)
        if (is.na(match_index)) {
          existing <- rbind(existing, imported[i, , drop = FALSE])
        } else {
          existing[match_index, ] <- imported[i, ]
        }
      }
      state$rois <- existing
      state$selected_roi <- imported[nrow(imported), , drop = FALSE]
      state$selected_rois <- state$selected_roi
    }
  }
  # Native WGPU deletion sends only the ROI identifier. R remains the
  # authoritative owner of the complete annotation collection.
  if (identical(payload$event, "roi_deleted") &&
      is.character(native_delete_roi_id) && length(native_delete_roi_id) == 1L &&
      !is.na(native_delete_roi_id) && nzchar(native_delete_roi_id)) {
    existing <- state$rois %||% wsi_empty_roi()
    if (nrow(existing)) {
      state$rois <- existing[existing$roi_id != native_delete_roi_id, , drop = FALSE]
    }
    selected_id <- state$selected_roi$roi_id %||% NA_character_
    if (length(selected_id) && identical(as.character(selected_id[[1L]]), native_delete_roi_id)) {
      state$selected_roi <- wsi_empty_roi()
      state$selected_rois <- wsi_empty_roi()
    }
  }
  if (!is.null(payload[["measurements", exact = TRUE]])) {
    state$measurements <- wsi_measurements_from_payload(payload[["measurements", exact = TRUE]])
  }
  if (identical(payload$event, "measurements_cleared")) {
    state$measurements <- wsi_empty_measurements()
  }
  if (identical(payload$event, "measurement_added") && is.list(native_measurement)) {
    added_measurement <- wsi_measurements_from_payload(list(native_measurement))
    if (nrow(added_measurement)) {
      existing <- state$measurements %||% wsi_empty_measurements()
      match_index <- match(added_measurement$id[[1L]], existing$id)
      if (is.na(match_index)) state$measurements <- rbind(existing, added_measurement)
      else { existing[match_index, ] <- added_measurement[1L, ]; state$measurements <- existing }
    }
  }
  if (identical(payload$event, "measurement_deleted") &&
      is.character(native_delete_measurement_id) && length(native_delete_measurement_id) == 1L &&
      !is.na(native_delete_measurement_id) && nzchar(native_delete_measurement_id)) {
    existing <- state$measurements %||% wsi_empty_measurements()
    if (nrow(existing)) {
      state$measurements <- existing[existing$id != native_delete_measurement_id, , drop = FALSE]
    }
  }
  if (!is.null(payload[["trajectories", exact = TRUE]])) {
    state$trajectories <- wsi_trajectories_from_payload(payload[["trajectories", exact = TRUE]])
  }
  if (identical(payload$event, "trajectories_cleared")) {
    state$trajectories <- wsi_empty_trajectories()
  }
  # Like native ROIs, a native WGPU trajectory is transmitted as one compact
  # record. Append/replace it on the R side rather than asking the renderer to
  # round-trip every saved trajectory on each edit.
  if (!is.null(native_trajectory) && identical(payload$event, "trajectory_added")) {
    native_trajectories <- wsi_trajectories_from_payload(list(native_trajectory))
    if (nrow(native_trajectories)) {
      existing <- state$trajectories %||% wsi_empty_trajectories()
      match_index <- match(native_trajectories$id[[1L]], existing$id)
      if (is.na(match_index)) {
        state$trajectories <- rbind(existing, native_trajectories)
      } else {
        existing[match_index, ] <- native_trajectories[1L, ]
        state$trajectories <- existing
      }
    }
  }
  # Native WGPU trajectory deletion follows the same compact, id-only pattern
  # as ROI deletion. The R session remains responsible for all trajectory data.
  if (identical(payload$event, "trajectory_deleted") &&
      is.character(native_delete_trajectory_id) && length(native_delete_trajectory_id) == 1L &&
      !is.na(native_delete_trajectory_id) && nzchar(native_delete_trajectory_id)) {
    existing <- state$trajectories %||% wsi_empty_trajectories()
    if (nrow(existing)) {
      state$trajectories <- existing[existing$id != native_delete_trajectory_id, , drop = FALSE]
    }
  }
  if (!is.null(payload[["segmentation", exact = TRUE]])) {
    state$segmentation <- wsi_rois_from_payload(payload[["segmentation", exact = TRUE]])
  }
  if (!is.null(payload[["layers", exact = TRUE]])) {
    state$layers <- wsi_viewer_update_layers_from_payload(state$layers, payload[["layers", exact = TRUE]])
  }
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
  if (is.list(detail) && is.data.frame(detail$trajectory_correlations)) {
    state$trajectory_correlations <- detail$trajectory_correlations
  }
  if (is.list(detail) && !is.null(detail$spatial_registration)) {
    state$spatial_registration <- wsi_spatial_registration_from_payload(detail$spatial_registration)
  }
  if (!is.null(payload[["selected_roi", exact = TRUE]]) ||
      !is.null(payload[["selected_rois", exact = TRUE]])) {
    state$selected_roi <- wsi_selected_roi_from_payload(payload[["selected_roi", exact = TRUE]])
    state$selected_rois <- wsi_selected_rois_from_payload(
      payload[["selected_rois", exact = TRUE]],
      payload[["selected_roi", exact = TRUE]]
    )
  }
  if (!is.null(payload[["selected_object", exact = TRUE]])) {
    state$selected_object <- payload[["selected_object", exact = TRUE]]
  }
  if (!is.null(payload[["view", exact = TRUE]])) {
    state$view <- payload[["view", exact = TRUE]]
  }
  if (!is.null(payload[["stain", exact = TRUE]])) {
    state$stain <- payload[["stain", exact = TRUE]]
  }
  if (!is.null(payload[["channel_sources", exact = TRUE]])) {
    state$channel_sources <- wsi_channel_sources_payload(payload[["channel_sources", exact = TRUE]])
  }
  state$annotation_masks <- payload[["annotation_masks", exact = TRUE]] %||%
    state$annotation_masks %||% list()
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
  state$performance <- payload[["performance", exact = TRUE]] %||%
    state$performance %||% list()
  if (!is.null(payload[["annotation_spots", exact = TRUE]])) {
    state$annotation_spots <- wsi_annotation_spots_from_payload(payload[["annotation_spots", exact = TRUE]])
  }
  if (!is.null(payload[["annotations", exact = TRUE]])) {
    state$annotations <- payload[["annotations", exact = TRUE]]
  }
  if (!is.null(payload[["history", exact = TRUE]])) {
    state$history <- wsi_annotation_history_from_payload(payload[["history", exact = TRUE]])
  }
  if (!is.null(payload[["logs", exact = TRUE]])) {
    state$logs <- wsi_viewer_logs_from_payload(payload[["logs", exact = TRUE]])
  }
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
    state$trajectory_correlations <- wsi_empty_trajectory_correlations()
  }
  if (startsWith(state$last_event, "segmentation")) {
    state$last_segmentation <- payload[["detail", exact = TRUE]] %||% list()
  }
  state$last_payload <- payload
  state$last_sync <- Sys.time()
  if (isTRUE(native_overlay_changed)) {
    state$native_renderer_revision <- as.integer(state$native_renderer_revision %||% 0L) + 1L
  }

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
    annotation_mask_count = length(state$annotation_masks %||% list()),
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
    trajectory_correlation_count = if (is.data.frame(state$trajectory_correlations)) nrow(state$trajectory_correlations) else 0L,
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
    trajectory_correlation_count = nrow(state$trajectory_correlations %||% wsi_empty_trajectory_correlations()),
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
  session$get_annotation_masks <- function(service = TRUE) {
    session$get_state(service = service)$annotation_masks
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
  session$get_spatial_registration <- function(service = TRUE) {
    session$get_state(service = service)$spatial_registration %||% wsi_empty_spatial_registration()
  }
  session$get_performance <- function(service = TRUE) {
    session$get_state(service = service)$performance %||% list()
  }
  session$get_spot_annotation_table <- function(service = TRUE) {
    session$get_annotation_spots(service = service)
  }
  session$get_annotation_spot_matrix <- function(service = TRUE,
                                                 by = c("annotation", "class"),
                                                 include_unassigned = FALSE) {
    wsi_annotation_association_matrix(
      session$get_annotation_spots(service = service),
      by = by,
      include_unassigned = include_unassigned
    )
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
  session$get_trajectory_correlations <- function(service = TRUE) {
    session$get_state(service = service)$trajectory_correlations
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

wsi_viewer_geojson_mask_response <- function(slide, payload, state = NULL,
                                             output_dir = getwd(),
                                             output_html = NULL) {
  if (is.null(slide)) {
    wsi_abort("No slide is attached to this live viewer session.")
  }
  if (!is.list(payload)) {
    wsi_abort("GeoJSON mask overlay request must be a JSON object.")
  }
  unknown <- setdiff(names(payload), c(
    "geojson", "file", "name", "id", "downsample", "label_by",
    "opacity", "visible", "rebuild", "smooth", "smooth_iterations",
    "smooth_max_vertices", "display_min_zoom", "min_display_zoom", "min_zoom"
  ))
  if (length(unknown)) {
    wsi_abort(sprintf(
      "Unsupported GeoJSON mask field%s: %s.",
      if (length(unknown) == 1L) "" else "s",
      paste(unknown, collapse = ", ")
    ))
  }
  geojson <- payload$geojson %||% NULL
  if (is.null(geojson) || !is.list(geojson)) {
    wsi_abort("GeoJSON mask overlay requires a `geojson` FeatureCollection.")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_dir)) {
    wsi_abort(sprintf("Could not create GeoJSON mask output directory: %s", output_dir))
  }
  file_label <- as.character(payload$file %||% payload$name %||% "imported_cells")
  id <- wsi_safe_id(as.character(payload$id %||% file_label), fallback = "imported_cells")
  work_dir <- file.path(output_dir, id)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  geojson_file <- file.path(work_dir, paste0(id, ".geojson"))
  jsonlite::write_json(geojson, geojson_file, auto_unbox = TRUE, null = "null")
  downsample <- suppressWarnings(as.numeric(payload$downsample %||% 4))
  if (!is.finite(downsample) || downsample < 1) {
    downsample <- 4
  }
  label_by <- as.character(payload$label_by %||% "class")
  if (!label_by %in% c("class", "name", "roi_id", "index", "constant")) {
    label_by <- "class"
  }
  opacity <- suppressWarnings(as.numeric(payload$opacity %||% 0.45))
  if (!is.finite(opacity)) {
    opacity <- 0.45
  }
  display_min_zoom <- suppressWarnings(as.numeric(
    payload$display_min_zoom %||% payload$min_display_zoom %||% payload$min_zoom %||% 5
  ))
  if (!is.finite(display_min_zoom) || display_min_zoom < 0) {
    display_min_zoom <- 5
  }
  source_result <- wsi_geojson_mask_channel_source(
    geojson = geojson_file,
    slide = slide,
    output_dir = work_dir,
    name = as.character(payload$name %||% sprintf("Mask: %s", basename(file_label))),
    id = id,
    output_html = output_html,
    downsample = downsample,
    label_by = label_by,
    visible = isTRUE(payload$visible %||% TRUE),
    opacity = opacity,
    rebuild = isTRUE(payload$rebuild),
    overwrite = TRUE,
    display_min_zoom = display_min_zoom,
    smooth = isTRUE(payload$smooth %||% TRUE),
    smooth_iterations = as.integer(payload$smooth_iterations %||% 1L),
    smooth_max_vertices = as.integer(payload$smooth_max_vertices %||% 4000L)
  )
  source <- wsi_channel_source_payload(source_result$source)
  if (inherits(state, "wsi_viewer_state")) {
    sources <- state$channel_sources %||% list()
    keys <- vapply(sources, function(x) as.character(x$id %||% ""), character(1))
    idx <- match(source$id, keys)
    if (is.na(idx)) {
      sources[[length(sources) + 1L]] <- source
    } else {
      sources[[idx]] <- source
    }
    names(sources) <- vapply(sources, function(x) as.character(x$id %||% ""), character(1))
    state$channel_sources <- sources
    state$channel_settings <- wsi_channel_settings_from_sources(sources)
    wsi_viewer_state_record_event(
      state,
      "geojson_mask_overlay_created",
      list(file = file_label, id = source$id, labels = length(source$metadata$legend %||% list()))
    )
  }
  list(
    ok = TRUE,
    source = source,
    mask = source_result$mask,
    tiles = source_result$tiles,
    file = file_label
  )
}

wsi_start_viewer_state_server <- function(state, slide = NULL,
                                          host = "127.0.0.1", port = 8788,
                                          path = "/viewer-state", max_tries = 20L,
	                                          tile_sources = list(),
                                          tile_path = "/tiles",
                                          seurat = NULL,
	                                          cellphenotyper = NULL,
	                                          seurat_gene_path = "/seurat-gene",
	                                          spatial_tile_path = "/spatial-tiles",
	                                          spatial_object_save_path = "/spatial-object-save",
	                                          image_export_path = "/image-export",
	                                          image_export_dir = getwd(),
	                                          image_export_max_pixels = 50000000,
	                                          geojson_mask_path = "/geojson-mask-overlay",
	                                          geojson_mask_dir = file.path(tempdir(), "wsiTools_geojson_masks"),
	                                          output_html = NULL,
	                                          dense_geojson_context = NULL,
	                                          dense_geojson_path = "/dense-geojson",
	                                          prediction_context = NULL,
	                                          prediction_path = "/prediction",
                                          proximity_context = NULL,
                                          proximity_path = "/proximity",
                                          native_renderer_path = "/native-renderer",
                                          native_state_path = "/native-renderer-state",
                                          native_points_path = "/native-points") {
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
	  if (!startsWith(spatial_object_save_path, "/")) {
	    spatial_object_save_path <- paste0("/", spatial_object_save_path)
	  }
  if (!startsWith(image_export_path, "/")) {
    image_export_path <- paste0("/", image_export_path)
  }
  if (!startsWith(geojson_mask_path, "/")) {
    geojson_mask_path <- paste0("/", geojson_mask_path)
  }
  if (!startsWith(dense_geojson_path, "/")) {
    dense_geojson_path <- paste0("/", dense_geojson_path)
  }
	  if (!startsWith(prediction_path, "/")) {
	    prediction_path <- paste0("/", prediction_path)
	  }
	  if (!startsWith(proximity_path, "/")) {
	    proximity_path <- paste0("/", proximity_path)
	  }
	  if (!startsWith(native_renderer_path, "/")) {
	    native_renderer_path <- paste0("/", native_renderer_path)
	  }
	  if (!startsWith(native_state_path, "/")) {
	    native_state_path <- paste0("/", native_state_path)
	  }
	  if (!startsWith(native_points_path, "/")) {
	    native_points_path <- paste0("/", native_points_path)
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
      native_project_path <- state$last_payload$detail$native_wgpu_project_path %||% NULL
      native_project_open_path <- state$last_payload$detail$native_wgpu_project_open_path %||% NULL
      native_association_csv_path <- state$last_payload$detail$native_wgpu_annotation_csv_path %||% NULL
      native_spatial_object_path <- state$last_payload$detail$native_wgpu_spatial_object_path %||% NULL
      native_geojson_path <- state$last_payload$detail$native_wgpu_geojson_path %||% NULL
      native_segmentation_path <- state$last_payload$detail$native_wgpu_segmentation_path %||% NULL
      native_grandqc_items <- state$last_payload$detail$native_wgpu_grandqc_items %||% NULL
      native_kodama_items <- state$last_payload$detail$native_wgpu_kodama_items %||% NULL
      native_grandqc_property <- function(roi) {
        props <- roi$properties[[1L]] %||% list()
        identical(as.character(props$source_menu %||% ""), "GrandQC")
      }
      if (identical(state$last_event, "grandqc_cleared")) {
        existing <- state$rois %||% wsi_empty_roi()
        removed <- if (nrow(existing)) sum(vapply(seq_len(nrow(existing)), function(i) {
          native_grandqc_property(existing[i, , drop = FALSE])
        }, logical(1))) else 0L
        if (removed) {
          keep <- !vapply(seq_len(nrow(existing)), function(i) {
            native_grandqc_property(existing[i, , drop = FALSE])
          }, logical(1))
          state$rois <- existing[keep, , drop = FALSE]
        }
        wsi_viewer_state_record_event(
          state, "grandqc_cleared",
          list(renderer = "native_wgpu", removed = as.integer(removed), ok = TRUE)
        )
      }
      if (identical(state$last_event, "grandqc_loaded") &&
          is.list(native_grandqc_items) && length(native_grandqc_items)) {
        imported_result <- tryCatch({
          existing <- state$rois %||% wsi_empty_roi()
          if (nrow(existing)) {
            keep <- !vapply(seq_len(nrow(existing)), function(i) {
              native_grandqc_property(existing[i, , drop = FALSE])
            }, logical(1))
            existing <- existing[keep, , drop = FALSE]
          }
          total <- 0L
          for (item in native_grandqc_items) {
            item <- item %||% list()
            item_path <- path.expand(as.character(item$path %||% ""))
            if (!nzchar(item_path) || !file.exists(item_path) || dir.exists(item_path)) {
              wsi_abort(sprintf("GrandQC GeoJSON file does not exist: %s", item_path))
            }
            imported <- read_geojson(item_path)
            if (!nrow(imported)) next
            grandqc_id <- as.character(item$id %||% tools::file_path_sans_ext(basename(item_path)))
            grandqc_label <- as.character(item$label %||% basename(item_path))
            imported$properties <- I(lapply(imported$properties, function(props) {
              props <- props %||% list()
              props$source_menu <- "GrandQC"
              props$grandqc_id <- grandqc_id
              props$grandqc_path <- normalizePath(item_path, winslash = "/", mustWork = FALSE)
              props
            }))
            empty_class <- is.na(imported$class) | !nzchar(as.character(imported$class)) |
              tolower(as.character(imported$class)) == "annotation"
            imported$class[empty_class] <- "artifact"
            empty_name <- is.na(imported$name) | !nzchar(as.character(imported$name))
            imported$name[empty_name] <- grandqc_label
            existing <- rbind(existing, imported)
            total <- total + nrow(imported)
          }
          state$rois <- existing
          if (total) {
            state$selected_roi <- existing[nrow(existing), , drop = FALSE]
            state$selected_rois <- state$selected_roi
          }
          list(count = as.integer(total), files = length(native_grandqc_items))
        }, error = function(err) err)
        wsi_viewer_state_record_event(
          state, "grandqc_loaded",
          list(
            renderer = "native_wgpu",
            count = if (inherits(imported_result, "error")) 0L else imported_result$count,
            files = if (inherits(imported_result, "error")) 0L else imported_result$files,
            ok = !inherits(imported_result, "error"),
            error = if (inherits(imported_result, "error")) conditionMessage(imported_result) else NULL
          )
        )
      }
      native_kodama_property <- function(roi) {
        props <- roi$properties[[1L]] %||% list()
        identical(as.character(props$source_menu %||% ""), "KODAMA")
      }
      if (identical(state$last_event, "kodama_cleared")) {
        existing <- state$rois %||% wsi_empty_roi()
        removed <- if (nrow(existing)) sum(vapply(seq_len(nrow(existing)), function(i) {
          native_kodama_property(existing[i, , drop = FALSE])
        }, logical(1))) else 0L
        if (removed) {
          keep <- !vapply(seq_len(nrow(existing)), function(i) {
            native_kodama_property(existing[i, , drop = FALSE])
          }, logical(1))
          state$rois <- existing[keep, , drop = FALSE]
        }
        wsi_viewer_state_record_event(
          state, "kodama_cleared",
          list(renderer = "native_wgpu", removed = as.integer(removed), ok = TRUE)
        )
      }
      if (identical(state$last_event, "kodama_loaded") &&
          is.list(native_kodama_items) && length(native_kodama_items)) {
        imported_result <- tryCatch({
          existing <- state$rois %||% wsi_empty_roi()
          if (nrow(existing)) {
            keep <- !vapply(seq_len(nrow(existing)), function(i) {
              native_kodama_property(existing[i, , drop = FALSE])
            }, logical(1))
            existing <- existing[keep, , drop = FALSE]
          }
          total <- 0L
          for (item in native_kodama_items) {
            item <- item %||% list()
            item_path <- path.expand(as.character(item$path %||% ""))
            if (!nzchar(item_path) || !file.exists(item_path) || dir.exists(item_path)) {
              wsi_abort(sprintf("KODAMA GeoJSON file does not exist: %s", item_path))
            }
            imported <- read_geojson(item_path)
            if (!nrow(imported)) next
            shift_dx <- suppressWarnings(as.numeric(item$shift_dx %||% 0))
            shift_dy <- suppressWarnings(as.numeric(item$shift_dy %||% 0))
            if (!is.finite(shift_dx)) shift_dx <- 0
            if (!is.finite(shift_dy)) shift_dy <- 0
            if (shift_dx != 0 || shift_dy != 0) {
              imported <- wsi_translate_rois(imported, dx = shift_dx, dy = shift_dy)
            }
            kodama_id <- as.character(item$id %||% tools::file_path_sans_ext(basename(item_path)))
            kodama_label <- as.character(item$label %||% basename(item_path))
            kodama_profile <- as.character(item$profile %||% "")
            imported$properties <- I(lapply(imported$properties, function(props) {
              props <- props %||% list()
              props$source_menu <- "KODAMA"
              props$kodama_id <- kodama_id
              props$kodama_profile <- kodama_profile
              props$kodama_path <- normalizePath(item_path, winslash = "/", mustWork = FALSE)
              props
            }))
            empty_class <- is.na(imported$class) | !nzchar(as.character(imported$class)) |
              tolower(as.character(imported$class)) == "annotation"
            imported$class[empty_class] <- "kodama"
            empty_name <- is.na(imported$name) | !nzchar(as.character(imported$name))
            imported$name[empty_name] <- kodama_label
            existing <- rbind(existing, imported)
            total <- total + nrow(imported)
          }
          state$rois <- existing
          if (total) {
            state$selected_roi <- existing[nrow(existing), , drop = FALSE]
            state$selected_rois <- state$selected_roi
          }
          list(count = as.integer(total), files = length(native_kodama_items))
        }, error = function(err) err)
        wsi_viewer_state_record_event(
          state, "kodama_loaded",
          list(
            renderer = "native_wgpu",
            count = if (inherits(imported_result, "error")) 0L else imported_result$count,
            files = if (inherits(imported_result, "error")) 0L else imported_result$files,
            ok = !inherits(imported_result, "error"),
            error = if (inherits(imported_result, "error")) conditionMessage(imported_result) else NULL
          )
        )
      }
      if (identical(state$last_event, "segmentation_added") &&
          is.character(native_segmentation_path) && length(native_segmentation_path) == 1L &&
          !is.na(native_segmentation_path) && nzchar(native_segmentation_path)) {
        segmentation_path <- path.expand(native_segmentation_path)
        imported_segmentation <- tryCatch({
          if (!file.exists(segmentation_path) || dir.exists(segmentation_path)) {
            wsi_abort(sprintf("Selected cell segmentation file does not exist: %s", segmentation_path))
          }
          segmentation_type <- wsi_segmentation_type(segmentation_path, "auto")
          raw_segmentation <- if (identical(segmentation_type, "mask")) {
            import_segmentation(segmentation_path, mask_as_rois = TRUE)
          } else {
            import_segmentation(segmentation_path)
          }
          imported <- wsi_viewer_coerce_segmentation(raw_segmentation)
          if (!inherits(imported, "wsi_roi") || !nrow(imported)) {
            wsi_abort("The selected segmentation contains no supported cell objects.")
          }
          if (nrow(imported) > 10000L) {
            source_id <- paste0(
              "native_segmentation_",
              wsi_safe_id(tools::file_path_sans_ext(basename(segmentation_path)), "cells"),
              "_", as.integer(Sys.time())
            )
            source <- list(
              id = source_id,
              name = basename(segmentation_path),
              path = normalizePath(segmentation_path, winslash = "/", mustWork = FALSE),
              target_source_id = as.character(state$native_active_source_id %||% ""),
              rois = imported,
              source_type = "cell_segmentation",
              kind = "cell_segmentation",
              visible = TRUE,
              opacity = 0.92,
              colour = "#F97316",
              fill_alpha = 0.18,
              line_width = 1.8,
              min_zoom = 1,
              full_resolution_zoom = 3,
              max_points_per_roi = 24000L,
              total_count = nrow(imported),
              bbox_index = wsi_bbox_index_create(imported)
            )
            dense_geojson_context$sources[[source_id]] <- source
            state$layers <- wsi_viewer_update_layers_from_payload(
              state$layers,
              list(list(
                id = source_id,
                name = source$name,
                type = "vector",
                source_type = source$source_type,
                visible = TRUE,
                opacity = source$opacity,
                colour = source$colour,
                metadata = list(viewport_only = TRUE, total_count = nrow(imported))
              ))
            )
            list(mode = "dense", count = nrow(imported), source_id = source_id)
          } else {
            state$segmentation <- wsi_viewer_bind_rois(state$segmentation, imported)
            list(mode = "editable", count = nrow(imported), source_id = NULL)
          }
        }, error = function(err) err)
        wsi_viewer_state_record_event(
          state,
          "segmentation_added",
          list(
            renderer = "native_wgpu",
            path = normalizePath(segmentation_path, winslash = "/", mustWork = FALSE),
            mode = if (inherits(imported_segmentation, "error")) NULL else imported_segmentation$mode,
            count = if (inherits(imported_segmentation, "error")) 0L else imported_segmentation$count,
            source_id = if (inherits(imported_segmentation, "error")) NULL else imported_segmentation$source_id,
            ok = !inherits(imported_segmentation, "error"),
            error = if (inherits(imported_segmentation, "error")) conditionMessage(imported_segmentation) else NULL
          )
        )
      }
      if (identical(state$last_event, "geojson_imported") &&
          is.character(native_geojson_path) && length(native_geojson_path) == 1L &&
          !is.na(native_geojson_path) && nzchar(native_geojson_path)) {
        geojson_path <- path.expand(native_geojson_path)
        extension <- tolower(tools::file_ext(geojson_path))
        imported_result <- tryCatch({
          if (!extension %in% c("geojson", "json")) {
            wsi_abort("Native GeoJSON import accepts only .geojson or .json files.")
          }
          if (!file.exists(geojson_path) || dir.exists(geojson_path)) {
            wsi_abort(sprintf("Selected GeoJSON file does not exist: %s", geojson_path))
          }
          imported <- read_geojson(geojson_path)
          if (!nrow(imported)) {
            wsi_abort("The selected GeoJSON file contains no supported features.")
          }
          # Large cell/segmentation files stay in R and are served only for the
          # visible viewport. Tissue-scale imports remain normal editable ROIs.
          if (nrow(imported) > 10000L) {
            source_id <- paste0(
              "native_dense_",
              wsi_safe_id(tools::file_path_sans_ext(basename(geojson_path)), "geojson"),
              "_", as.integer(Sys.time())
            )
            source <- list(
              id = source_id,
              name = basename(geojson_path),
              path = normalizePath(geojson_path, winslash = "/", mustWork = FALSE),
              target_source_id = as.character(state$native_active_source_id %||% ""),
              rois = imported,
              source_type = "cell_segmentation",
              kind = "cell_segmentation",
              visible = TRUE,
              opacity = 0.92,
              colour = "#F97316",
              fill_alpha = 0.18,
              line_width = 1.8,
              min_zoom = 1,
              full_resolution_zoom = 3,
              max_points_per_roi = 24000L,
              total_count = nrow(imported),
              bbox_index = wsi_bbox_index_create(imported)
            )
            dense_geojson_context$sources[[source_id]] <- source
            state$layers <- wsi_viewer_update_layers_from_payload(
              state$layers,
              list(list(
                id = source_id,
                name = source$name,
                type = "vector",
                source_type = source$source_type,
                visible = TRUE,
                opacity = source$opacity,
                colour = source$colour,
                metadata = list(viewport_only = TRUE, total_count = nrow(imported))
              ))
            )
            list(mode = "dense", count = nrow(imported), source_id = source_id)
          } else {
            existing <- state$rois %||% wsi_empty_roi()
            for (i in seq_len(nrow(imported))) {
              roi_id <- imported$roi_id[[i]]
              match_index <- match(roi_id, existing$roi_id)
              if (is.na(match_index)) existing <- rbind(existing, imported[i, , drop = FALSE])
              else existing[match_index, ] <- imported[i, ]
            }
            state$rois <- existing
            state$selected_roi <- imported[nrow(imported), , drop = FALSE]
            state$selected_rois <- state$selected_roi
            list(mode = "editable", count = nrow(imported), source_id = NULL)
          }
        }, error = function(err) err)
        wsi_viewer_state_record_event(
          state,
          "geojson_imported",
          list(
            renderer = "native_wgpu",
            path = normalizePath(geojson_path, winslash = "/", mustWork = FALSE),
            mode = if (inherits(imported_result, "error")) NULL else imported_result$mode,
            count = if (inherits(imported_result, "error")) 0L else imported_result$count,
            source_id = if (inherits(imported_result, "error")) NULL else imported_result$source_id,
            ok = !inherits(imported_result, "error"),
            error = if (inherits(imported_result, "error")) conditionMessage(imported_result) else NULL
          )
        )
      }
    if (identical(state$last_event, "project_save_requested") &&
        is.character(native_project_path) && length(native_project_path) == 1L &&
        !is.na(native_project_path) && nzchar(native_project_path)) {
      saved_project <- tryCatch(
        wsi_project(
          native_project_path,
          slide = slide,
          viewer_state = state,
          overwrite = TRUE
        ),
        error = function(err) err
      )
      if (inherits(saved_project, "error")) {
        wsi_viewer_state_record_event(
          state,
          "project_saved",
          list(renderer = "native_wgpu", path = native_project_path, ok = FALSE,
               error = conditionMessage(saved_project))
        )
      } else {
        state$annotations <- list(dirty = FALSE, dirty_reason = "project_saved")
        wsi_viewer_state_record_event(
          state,
          "project_saved",
          list(renderer = "native_wgpu", path = saved_project$path, ok = TRUE)
        )
      }
    }
    if (identical(state$last_event, "project_opened") &&
        is.character(native_project_open_path) && length(native_project_open_path) == 1L &&
        !is.na(native_project_open_path) && nzchar(native_project_open_path)) {
      restored_project <- tryCatch({
        project_path <- path.expand(native_project_open_path)
        if (!dir.exists(project_path)) {
          wsi_abort(sprintf("Selected project directory does not exist: %s", project_path))
        }
        # `restore_project_state()` only needs the live state container when
        # service is disabled; this handler runs before the public session
        # object is assembled, so a compact session-shaped wrapper is enough.
        native_session <- structure(list(state = state), class = "wsi_viewer_session")
        restore_project_state(native_session, project_path, service = FALSE)
        normalizePath(project_path, winslash = "/", mustWork = TRUE)
      }, error = function(err) err)
      wsi_viewer_state_record_event(
        state,
        "project_opened",
        list(
          renderer = "native_wgpu",
          path = if (inherits(restored_project, "error")) native_project_open_path else restored_project,
          ok = !inherits(restored_project, "error"),
          error = if (inherits(restored_project, "error")) conditionMessage(restored_project) else NULL
        )
      )
    }
      if (identical(state$last_event, "annotation_spots_exported") &&
        is.character(native_association_csv_path) && length(native_association_csv_path) == 1L &&
        !is.na(native_association_csv_path) && nzchar(native_association_csv_path)) {
      export_result <- tryCatch({
        utils::write.csv(
          state$annotation_spots %||% wsi_empty_annotation_spots(),
          native_association_csv_path,
          row.names = FALSE
        )
        TRUE
      }, error = function(err) err)
      wsi_viewer_state_record_event(
        state,
        "annotation_spots_exported",
        list(
          renderer = "native_wgpu",
          path = native_association_csv_path,
          count = nrow(state$annotation_spots %||% wsi_empty_annotation_spots()),
          ok = !inherits(export_result, "error"),
          error = if (inherits(export_result, "error")) conditionMessage(export_result) else NULL
        )
        )
      }
      if (identical(state$last_event, "spatial_object_save_requested") &&
          !is.null(seurat) && is.character(native_spatial_object_path) &&
          length(native_spatial_object_path) == 1L && !is.na(native_spatial_object_path) &&
          nzchar(native_spatial_object_path)) {
        native_result <- tryCatch(
          wsi_spatial_object_save_response(
            seurat,
            list(
              format = "rds", output = native_spatial_object_path, overwrite = TRUE,
              native_wgpu_point_source_id = state$last_payload$detail$native_wgpu_point_source_id %||% "",
              native_wgpu_spatial_transform = state$last_payload$detail$native_wgpu_spatial_transform %||% list()
            ),
            state = state
          ),
          error = function(err) err
        )
        wsi_viewer_state_record_event(
          state,
          "spatial_registration_saved",
          list(
            renderer = "native_wgpu", path = native_spatial_object_path,
            ok = !inherits(native_result, "error"),
            count = if (inherits(native_result, "error")) 0L else native_result$count %||% 0L,
            error = if (inherits(native_result, "error")) conditionMessage(native_result) else NULL
          )
        )
      }
      wsi_viewer_autosave_save(state, slide = slide, reason = state$last_event %||% "viewer_state")
    wsi_viewer_state_response(state, dequeue_commands = dequeue_commands)
  }

  native_point_layers <- function() {
    layers <- state$layers %||% list()
    Filter(function(layer) {
      if (!is.list(layer) || !length(layer$items %||% list())) return(FALSE)
      source_type <- tolower(as.character(layer$source_type %||% layer$type %||% ""))
      # Do not accidentally marshal polygon-heavy tissue/cell geometry through
      # the point endpoint. Point layers have an explicit point source type or
      # point-like records with finite x/y coordinates.
      point_type <- grepl("point|spot|seurat|spatial|cellphenotyper|proximity|prediction", source_type)
      first_item <- (layer$items %||% list())[[1L]] %||% list()
      point_item <- is.list(first_item) && is.finite(suppressWarnings(as.numeric(first_item$x %||% first_item$slide_x %||% NA_real_))) &&
        is.finite(suppressWarnings(as.numeric(first_item$y %||% first_item$slide_y %||% NA_real_)))
      isTRUE(point_type || point_item)
    }, layers)
  }

  native_point_sources <- function() {
    lapply(native_point_layers(), function(layer) {
      metadata <- layer$metadata %||% list()
      target_fields <- c(
        "target_id", "target_project_id", "target_project_item_id",
        "target_project_image_id", "target_item_id", "target_path",
        "target_source_path", "target_tile_source_id", "base_id", "base_path",
        "base_slide_path", "slide_path", "project_item_id", "project_image_id", "item_id",
        "tile_source_id"
      )
      target_ids <- unlist(c(layer[target_fields], metadata[target_fields]), use.names = FALSE)
      target_ids <- unique(as.character(target_ids[!is.na(target_ids) & nzchar(as.character(target_ids))]))
      list(
        id = as.character(layer$id %||% ""),
        name = as.character(layer$name %||% layer$id %||% "Spatial points"),
        source_type = as.character(layer$source_type %||% layer$type %||% "points"),
        visible = isTRUE(layer$visible %||% TRUE),
        opacity = suppressWarnings(as.numeric(layer$opacity %||% 1)),
        colour = as.character(layer$colour %||% layer$color %||% "#38bdf8"),
        radius = suppressWarnings(as.numeric(layer$radius %||% 6)),
        count = suppressWarnings(as.integer(layer$total_count %||% layer$count %||% length(layer$items %||% list()))),
        target_ids = target_ids
      )
    })
  }

  native_layer_summaries <- function(layers = state$layers) {
    lapply(layers %||% list(), function(layer) {
      if (!is.list(layer)) return(list())
      layer[c("id", "name", "type", "source_type", "visible", "opacity", "colour", "color", "radius", "count", "total_count", "metadata")]
    })
  }

  native_points_response <- function(req) {
    method <- req$REQUEST_METHOD %||% "GET"
    if (!identical(method, "POST")) {
      return(wsi_http_json_response(status = 405L, body = list(error = "Use POST for native viewport points.")))
    }
    tryCatch({
      body <- wsi_http_request_body(req)
      payload <- if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else list()
      source_id <- as.character(payload$source_id %||% "")
      candidates <- native_point_layers()
      matches <- which(vapply(candidates, function(x) identical(as.character(x$id %||% ""), source_id), logical(1)))
      if (!length(matches)) {
        return(wsi_http_json_response(status = 404L, body = list(ok = FALSE, error = "Unknown native point layer.")))
      }
      layer <- candidates[[matches[[1L]]]]
      items <- layer$items %||% list()
      if (is.data.frame(items)) items <- lapply(seq_len(nrow(items)), function(i) as.list(items[i, , drop = FALSE]))
      # Prediction remains R-side.  Recolour the existing viewport point layer
      # by its compact id lookup instead of sending a second full-slide layer
      # to the native renderer.
      prediction <- state$prediction %||% wsi_empty_prediction_result()
      prediction_colours <- character()
      if (is.data.frame(prediction) && nrow(prediction) && "id" %in% names(prediction)) {
        palette <- wsi_prediction_palette(as.character(prediction$predicted %||% character()))
        ids <- as.character(prediction$id)
        values <- unname(palette[as.character(prediction$predicted)])
        keep <- !is.na(ids) & nzchar(ids) & !is.na(values) & nzchar(values)
        prediction_colours <- stats::setNames(as.character(values[keep]), ids[keep])
      }
      xmin <- suppressWarnings(as.numeric(payload$xmin %||% -Inf))
      ymin <- suppressWarnings(as.numeric(payload$ymin %||% -Inf))
      xmax <- suppressWarnings(as.numeric(payload$xmax %||% Inf))
      ymax <- suppressWarnings(as.numeric(payload$ymax %||% Inf))
      zoom <- suppressWarnings(as.numeric(payload$zoom %||% 1))
      max_items <- suppressWarnings(as.integer(payload$max_items %||% 50000L))
      if (!is.finite(max_items) || max_items < 100L) max_items <- 50000L
      # Native WGPU sends only a compact global coordinate transform with each
      # viewport request.  Apply it here, before clipping, so registration does
      # not require transferring or duplicating a full spatial point table.
      transform <- payload$spatial_transform %||% list()
      transform_value <- function(name, default) {
        value <- suppressWarnings(as.numeric(transform[[name]] %||% default))
        if (!length(value) || !is.finite(value[[1L]])) default else value[[1L]]
      }
      transform_flag <- function(name) isTRUE(transform[[name]] %||% FALSE)
      scale_x <- transform_value("scale_x", 1)
      scale_y <- transform_value("scale_y", 1)
      offset_x <- transform_value("offset_x", 0)
      offset_y <- transform_value("offset_y", 0)
      rotation <- transform_value("rotation_degrees", 0) * pi / 180
      flip_h <- transform_flag("flip_horizontal")
      flip_v <- transform_flag("flip_vertical")
      centre_x <- transform_value("center_x", 0)
      centre_y <- transform_value("center_y", 0)
      if (identical(as.character(payload$action %||% ""), "trajectory_profile")) {
        profile_result <- tryCatch({
          trajectory <- payload$trajectory %||% list()
          trajectory_points <- trajectory$points %||% trajectory$coordinates %||% list()
          trajectory_points <- lapply(trajectory_points, function(point) {
            if (is.list(point)) c(x = as.numeric(point$x %||% point[[1L]] %||% NA_real_), y = as.numeric(point$y %||% point[[2L]] %||% NA_real_))
            else as.numeric(point)
          })
          trajectory_points <- Filter(function(point) length(point) >= 2L && all(is.finite(point[1:2])), trajectory_points)
          if (length(trajectory_points) < 2L) wsi_abort("Trajectory profiling needs a path with at least two points.")
          trajectory_matrix <- do.call(rbind, lapply(trajectory_points, function(point) point[1:2]))
          dx <- diff(trajectory_matrix[, 1L]); dy <- diff(trajectory_matrix[, 2L])
          segment_length <- sqrt(dx * dx + dy * dy)
          valid_segment <- is.finite(segment_length) & segment_length > 0
          if (!any(valid_segment)) wsi_abort("Trajectory profiling needs a non-zero path length.")
          starts <- trajectory_matrix[-nrow(trajectory_matrix), , drop = FALSE]
          starts <- starts[valid_segment, , drop = FALSE]
          dx <- dx[valid_segment]; dy <- dy[valid_segment]; segment_length <- segment_length[valid_segment]
          cumulative <- c(0, cumsum(segment_length))
          total_length <- sum(segment_length)
          bins <- suppressWarnings(as.integer(payload$bins %||% 20L))
          bins <- max(2L, min(200L, if (is.finite(bins)) bins else 20L))
          width <- suppressWarnings(as.numeric(payload$width_px %||% 250))
          width <- max(1, if (is.finite(width)) width else 250)
          feature <- as.character(payload$feature %||% "count")
          project_point <- function(item) {
            x <- suppressWarnings(as.numeric(item$x %||% item$slide_x %||% NA_real_))
            y <- suppressWarnings(as.numeric(item$y %||% item$slide_y %||% NA_real_))
            if (!is.finite(x) || !is.finite(y) || identical(item$visible, FALSE)) return(NULL)
            px <- x - centre_x; py <- y - centre_y
            if (flip_h) px <- -px
            if (flip_v) py <- -py
            px <- px * scale_x; py <- py * scale_y
            if (rotation != 0) {
              cos_r <- cos(rotation); sin_r <- sin(rotation)
              rotated_x <- px * cos_r + py * sin_r
              py <- -px * sin_r + py * cos_r; px <- rotated_x
            }
            x <- centre_x + px + offset_x; y <- centre_y + py + offset_y
            best_distance <- Inf; best_along <- 0
            for (j in seq_along(segment_length)) {
              vx <- x - starts[j, 1L]; vy <- y - starts[j, 2L]
              t <- max(0, min(1, (vx * dx[j] + vy * dy[j]) / (segment_length[j] * segment_length[j])))
              qx <- starts[j, 1L] + t * dx[j]; qy <- starts[j, 2L] + t * dy[j]
              distance <- sqrt((x - qx)^2 + (y - qy)^2)
              if (distance < best_distance) { best_distance <- distance; best_along <- cumulative[j] + t * segment_length[j] }
            }
            if (!is.finite(best_distance) || best_distance > width / 2) return(NULL)
            value <- if (identical(feature, "count")) 1 else item[[feature]] %||% NA
            list(id = as.character(item$id %||% item$barcode %||% item$label %||% ""), value = value, along = best_along, distance = best_distance)
          }
          included <- Filter(Negate(is.null), lapply(items, project_point))
          if (!length(included)) wsi_abort("No points fell within the selected trajectory width.")
          bin_index <- vapply(included, function(point) min(bins, max(1L, floor(point$along / total_length * bins) + 1L)), integer(1))
          numeric_values <- vapply(included, function(point) {
            value <- point$value %||% NA
            suppressWarnings(as.numeric(value[[1L]] %||% NA_real_))
          }, numeric(1))
          is_numeric <- identical(feature, "count") || sum(is.finite(numeric_values)) >= max(2L, floor(length(included) / 2L))
          rows <- lapply(seq_len(bins), function(bin) {
            hit <- which(bin_index == bin); values <- if (is_numeric) numeric_values[hit] else as.character(vapply(included[hit], function(point) point$value[[1L]] %||% "", character(1)))
            valid <- if (is_numeric) values[is.finite(values)] else values[nzchar(values)]
            dominant <- if (!is_numeric && length(valid)) names(sort(table(valid), decreasing = TRUE))[[1L]] else ""
            list(trajectory_id = as.character(trajectory$id %||% ""), trajectory_name = as.character(trajectory$name %||% "Trajectory"), source_id = source_id, source_name = as.character(layer$name %||% source_id), feature = feature, feature_type = if (is_numeric) "numeric" else "category", category = if (is_numeric) "" else dominant, bin = bin, bin_start_px = (bin - 1) / bins * total_length, bin_end_px = bin / bins * total_length, distance_px = ((bin - .5) / bins) * total_length, distance_fraction = (bin - .5) / bins, width_px = width, total_length_px = total_length, count = length(hit), mean = if (is_numeric && length(valid)) mean(valid) else NA_real_, median = if (is_numeric && length(valid)) stats::median(valid) else NA_real_, min = if (is_numeric && length(valid)) min(valid) else NA_real_, max = if (is_numeric && length(valid)) max(valid) else NA_real_, sd = if (is_numeric && length(valid) > 1L) stats::sd(valid) else NA_real_, dominant = dominant, dominant_count = if (nzchar(dominant)) sum(valid == dominant) else 0L, fraction = length(hit) / length(included), project_image = as.character(state$project$active_image %||% ""), project_section = as.character(state$project$active_section %||% ""))
          })
          palette <- grDevices::colorRampPalette(c("#2563eb", "#facc15", "#dc2626"))(101L)
          colours <- stats::setNames(vapply(included, function(point) palette[[min(101L, max(1L, floor(point$along / total_length * 100) + 1L))]], character(1)), vapply(included, `[[`, character(1), "id"))
          list(rows = rows, colours = as.list(colours), count = length(included))
        }, error = function(err) err)
        if (inherits(profile_result, "error")) return(wsi_http_json_response(status = 400L, body = list(ok = FALSE, error = conditionMessage(profile_result))))
        return(wsi_http_json_response(body = list(ok = TRUE, trajectory_profile = profile_result$rows, colours = profile_result$colours, count = profile_result$count)))
      }
      points <- lapply(items, function(item) {
        if (!is.list(item)) return(NULL)
        x <- suppressWarnings(as.numeric(item$x %||% item$slide_x %||% NA_real_))
        y <- suppressWarnings(as.numeric(item$y %||% item$slide_y %||% NA_real_))
        if (is.finite(x) && is.finite(y)) {
          dx <- x - centre_x
          dy <- y - centre_y
          if (flip_h) dx <- -dx
          if (flip_v) dy <- -dy
          dx <- dx * scale_x
          dy <- dy * scale_y
          if (rotation != 0) {
            cos_r <- cos(rotation); sin_r <- sin(rotation)
            rotated_x <- dx * cos_r + dy * sin_r
            dy <- -dx * sin_r + dy * cos_r
            dx <- rotated_x
          }
          x <- centre_x + dx + offset_x
          y <- centre_y + dy + offset_y
        }
        if (!is.finite(x) || !is.finite(y) || x < xmin || x > xmax || y < ymin || y > ymax || identical(item$visible, FALSE)) return(NULL)
        point_id <- as.character(item$id %||% item$barcode %||% item$label %||% "")
        prediction_colour <- unname(prediction_colours[point_id])
        if (!length(prediction_colour) || is.na(prediction_colour) || !nzchar(prediction_colour)) {
          prediction_colour <- NULL
        }
        list(
          x = x, y = y,
          radius = suppressWarnings(as.numeric(item$radius %||% layer$radius %||% 6)),
          colour = as.character(prediction_colour %||% item$colour %||% item$color %||% layer$colour %||% "#38bdf8"),
          id = point_id,
          cluster_values = item$cluster_values %||% item$clusters %||% list()
        )
      })
      points <- Filter(Negate(is.null), points)
      # Same progressive intent as the browser: all local points when close,
      # then a stable deterministic subset for overview navigation.
      stride <- if (is.finite(zoom) && zoom >= 5) 1L else if (is.finite(zoom) && zoom >= 2) 2L else 10L
      if (length(points) > max_items) stride <- max(stride, ceiling(length(points) / max_items))
      if (stride > 1L && length(points)) points <- points[seq.int(1L, length(points), by = stride)]
      wsi_http_json_response(body = list(
        ok = TRUE, source_id = source_id, total = length(items), represented = length(points),
        stride = stride, points = points
      ))
    }, error = function(err) {
      wsi_http_json_response(status = 500L, body = list(ok = FALSE, error = conditionMessage(err)))
    })
  }

  # This intentionally contains only tile geometry and identifiers. The native
  # desktop renderer receives the same compact source contract as the browser;
  # pixels still arrive only through the validated tile route below.
  native_renderer_manifest <- function() {
    sources <- lapply(tile_sources, function(source) {
      objective_power <- suppressWarnings(as.numeric(
        source$objective_power %||% source$objective %||% source$magnification %||%
          source$metadata$objective_power %||% source$slide$objective_power %||% NA_real_
      ))
      list(
        id = as.character(source$id %||% ""),
        width = as.numeric(source$width %||% source$slide$dimensions[["width"]] %||% NA_real_),
        height = as.numeric(source$height %||% source$slide$dimensions[["height"]] %||% NA_real_),
        tile_size = as.integer(source$tile_size %||% 512L),
        tile_overlap = as.integer(source$tile_overlap %||% 0L),
        tile_format = as.character(source$tile_format %||% "jpg"),
        min_level = as.integer(source$min_level %||% 0L),
        max_level = as.integer(source$max_level %||% 0L),
        label = as.character(source$name %||% source$label %||% source$id %||% "slide"),
        mpp = wsi_viewer_mpp_payload(
          source$mpp %||% source$pixel_size %||% source$metadata$mpp %||%
            source$metadata$pixel_size %||% source$slide$mpp %||% NULL
        ),
        objective_power = if (is.finite(objective_power) && objective_power > 0) objective_power else NULL
      )
    })
    sources <- sources[vapply(sources, function(source) {
      nzchar(source$id) && is.finite(source$width) && is.finite(source$height) &&
        source$width > 0 && source$height > 0
    }, logical(1))]
    dense_sources <- dense_geojson_sources()
    dense_sources <- lapply(dense_sources, function(source) {
      list(
        id = as.character(source$id %||% ""),
        name = as.character(source$name %||% "Dense annotation"),
        source_type = as.character(source$source_type %||% source$kind %||% "annotation"),
        visible = isTRUE(source$visible %||% TRUE),
        min_zoom = suppressWarnings(as.numeric(source$min_zoom %||% 0)),
        colour = as.character(source$colour %||% "#F97316")
      )
    })
    dense_sources <- dense_sources[vapply(dense_sources, function(source) nzchar(source$id), logical(1))]
    # Native WGPU rendering uses the existing authenticated/session-scoped tile
    # route. Only advertise channel sources backed by that route: arbitrary
    # file:// channels remain browser-only rather than making the native client
    # guess how a local static pyramid should be opened.
    dynamic_ids <- vapply(tile_sources, function(source) {
      as.character(source$id %||% "")
    }, character(1))
    channel_sources <- state$channel_sources %||% list()
    channel_sources <- lapply(channel_sources, function(source) {
      metadata <- source$metadata %||% list()
      target_fields <- c(
        "target_id", "target_project_id", "target_project_item_id",
        "target_project_image_id", "target_item_id", "target_path",
        "target_source_path", "target_tile_source_id", "base_id", "base_path",
        "base_slide_path", "slide_path", "project_item_id", "project_image_id", "item_id",
        "tile_source_id"
      )
      target_ids <- unlist(c(source[target_fields], metadata[target_fields]), use.names = FALSE)
      target_ids <- unique(as.character(target_ids[!is.na(target_ids) & nzchar(as.character(target_ids))]))
      list(
        id = as.character(source$id %||% ""),
        width = suppressWarnings(as.numeric(source$width %||% NA_real_)),
        height = suppressWarnings(as.numeric(source$height %||% NA_real_)),
        tile_size = as.integer(source$tile_size %||% 512L),
        tile_overlap = as.integer(source$tile_overlap %||% 0L),
        tile_format = as.character(source$tile_format %||% "png"),
        min_level = as.integer(source$min_level %||% 0L),
        max_level = suppressWarnings(as.integer(source$max_level %||% NA_integer_)),
        label = as.character(source$name %||% source$id %||% "channel"),
        source_type = as.character(source$type %||% "dynamic"),
        visible = isTRUE(source$visible %||% TRUE),
        opacity = suppressWarnings(as.numeric(source$opacity %||% 1)),
        colour = as.character(source$colour %||% source$color %||% "#ffffff"),
        gain = suppressWarnings(as.numeric(source$gain %||% source$strength %||% 1)),
        contrast_min = suppressWarnings(as.numeric(source$contrast_min %||% 0)),
        contrast_max = suppressWarnings(as.numeric(source$contrast_max %||% 1)),
        target_ids = target_ids
      )
    })
    channel_sources <- channel_sources[vapply(channel_sources, function(source) {
      nzchar(source$id) && source$id %in% dynamic_ids &&
        is.finite(source$width) && is.finite(source$height) &&
        source$width > 0 && source$height > 0 &&
        is.finite(source$max_level) && source$max_level >= source$min_level
    }, logical(1))]
    list(
      protocol = "wsiTools-native-renderer/v1",
      tile_route = tile_path,
      state_route = path,
	      state_snapshot_route = native_state_path,
      dense_geojson_route = dense_geojson_path,
      native_points_route = native_points_path,
      spatial_gene_route = seurat_gene_path,
      prediction_route = prediction_path,
      prediction_enabled = wsi_prediction_context_enabled(
        prediction_context %||% list(spatial = seurat)
      ),
      prediction = {
        prediction_context_native <- prediction_context %||% list(spatial = seurat)
        spatial_native <- prediction_context_native$spatial %||% prediction_context_native$seurat %||% NULL
        cells_native <- prediction_context_native$cellphenotyper_project %||% prediction_context_native$cellphenotyper %||% NULL
        wsi_prediction_config(
          seurat = if (inherits(spatial_native, "wsi_seurat_spatial") || inherits(spatial_native, "wsi_spatial_object")) wsi_viewer_seurat_config(spatial_native) else list(enabled = FALSE),
          cellphenotyper = if (inherits(cells_native, "wsi_cellphenotyper_project")) wsi_viewer_cellphenotyper_config(cells_native) else list(enabled = FALSE)
        )
      },
      # Keep analytical requests on the same local, validated R bridge used
      # by the browser viewer.  The native client never receives an R console.
      proximity_route = proximity_path,
      image_export_route = image_export_path,
      proximity_enabled = wsi_prediction_context_enabled(
        proximity_context %||% prediction_context %||% list(spatial = seurat)
      ),
      segmentation_run_url = as.character(state$native_segmentation_run_url %||% ""),
      spatial_clusters = if (inherits(seurat, "wsi_seurat_spatial") || inherits(seurat, "wsi_spatial_object")) {
        seurat$clusters %||% wsi_spatial_cluster_config(seurat$cluster_values %||% data.frame())
      } else list(enabled = FALSE, fields = list(), default_field = NULL),
      # Reduction plots are sampled by wsi_viewer_seurat_config(). The native
      # renderer receives IDs and two-dimensional coordinates only; expression
      # matrices and full spatial objects remain in the R session.
      spatial = if (inherits(seurat, "wsi_seurat_spatial") || inherits(seurat, "wsi_spatial_object")) {
        wsi_viewer_seurat_config(seurat)
      } else list(enabled = FALSE, plots = list(), spot_count = 0L),
      cellphenotyper = if (inherits(cellphenotyper, "wsi_cellphenotyper_project") || is.list(cellphenotyper)) {
        wsi_viewer_cellphenotyper_config(cellphenotyper)
      } else list(enabled = FALSE),
	      source_count = length(sources),
	      sources = sources,
	      dense_sources = dense_sources,
	      point_sources = native_point_sources(),
	      channel_sources = channel_sources,
      capabilities = list(
        tiles = TRUE,
        full_resolution_level = TRUE,
        project_state = TRUE,
        typed_events = TRUE,
        native_controls = c(
          "navigation", "roi_selection", "polygon_draft", "dense_overlays",
          "annotation_fills", "native_panels", "dynamic_channel_layers", "viewport_points",
          "measurements", "gpu_stain_display", "selected_roi_segmentation",
          "proximity_analysis", "pls_lda_prediction", "annotation_association",
          "spatial_registration_global"
        )
      )
    )
  }

  # A read-only snapshot for the native renderer. It intentionally excludes
  # dense segmentation/cell geometry: those are fetched per viewport through
  # the existing dense GeoJSON route once the native overlay renderer requests
  # them. This avoids turning a state refresh into a whole-slide transfer.
  native_renderer_state <- function(source_id = NULL) {
    source_id <- as.character(source_id %||% "")
    source_id <- if (length(source_id)) source_id[[1L]] else ""
    active_source_id <- as.character(state$native_active_source_id %||% "")
    active_source_id <- if (length(active_source_id)) active_source_id[[1L]] else ""
    snapshot <- if (nzchar(source_id) && !identical(source_id, active_source_id)) {
      state$native_project_states[[source_id]] %||% list()
    } else {
      list()
    }
    using_snapshot <- length(snapshot) > 0L
    rois <- if (using_snapshot) snapshot$rois else state$rois
    rois <- rois %||% wsi_empty_roi()
    limit <- 5000L
    listed <- min(nrow(rois), limit)
    visible_rois <- if (listed) rois[seq_len(listed), , drop = FALSE] else rois
    selected <- (if (using_snapshot) snapshot$selected_roi else state$selected_roi) %||% wsi_empty_roi()
    segmentation <- (if (using_snapshot) snapshot$segmentation else state$segmentation) %||% wsi_empty_roi()
    segmentation_limit <- 5000L
    segmentation_listed <- min(nrow(segmentation), segmentation_limit)
    visible_segmentation <- if (segmentation_listed) segmentation[seq_len(segmentation_listed), , drop = FALSE] else segmentation
    list(
      protocol = "wsiTools-native-renderer-state/v1",
      source_id = if (nzchar(source_id)) source_id else active_source_id,
      event = state$last_event %||% "viewer_state",
      revision = as.integer(state$native_renderer_revision %||% 0L),
      annotations = wsi_viewer_rois_to_geojson(visible_rois),
      annotations_total = nrow(rois),
      annotations_truncated = nrow(rois) > listed,
      selected_roi = if (nrow(selected)) wsi_viewer_rois_to_geojson(selected)$features[[1L]] else NULL,
      segmentation = wsi_viewer_rois_to_geojson(visible_segmentation),
      segmentation_total = nrow(segmentation),
      segmentation_truncated = nrow(segmentation) > segmentation_listed,
      trajectories = wsi_trajectories_to_payload((if (using_snapshot) snapshot$trajectories else state$trajectories) %||% wsi_empty_trajectories()),
      measurements = {
        measures <- (if (using_snapshot) snapshot$measurements else state$measurements) %||% wsi_empty_measurements()
        if (is.data.frame(measures) && nrow(measures)) lapply(seq_len(nrow(measures)), function(i) as.list(measures[i, , drop = FALSE])) else list()
      },
      layers = native_layer_summaries(if (using_snapshot) snapshot$layers %||% list() else state$layers),
      channel_settings = (if (using_snapshot) snapshot$channel_settings else state$channel_settings) %||% wsi_empty_channel_settings(),
      stain = (if (using_snapshot) snapshot$stain else state$stain) %||% list(),
      dense_sources = lapply(Filter(function(source) {
        target <- as.character(source$target_source_id %||% "")
        !length(target) || !nzchar(target[[1L]]) || identical(target[[1L]], if (nzchar(source_id)) source_id else active_source_id)
      }, dense_geojson_sources()), function(source) list(
        id = as.character(source$id %||% ""),
        name = as.character(source$name %||% "Dense annotation"),
        source_type = as.character(source$source_type %||% source$kind %||% "annotation"),
        visible = isTRUE(source$visible %||% TRUE),
        min_zoom = suppressWarnings(as.numeric(source$min_zoom %||% 0)),
        colour = as.character(source$colour %||% "#F97316")
      )),
      project = state$project %||% list(),
      view = state$view %||% list()
    )
  }

  dense_geojson_sources <- function() {
    if (is.environment(dense_geojson_context)) {
      sources <- dense_geojson_context$sources %||% list()
    } else if (is.list(dense_geojson_context)) {
      sources <- dense_geojson_context$sources %||% dense_geojson_context
    } else {
      sources <- list()
    }
    sources[vapply(sources, function(source) {
      is.list(source) && (
        (inherits(source$rois, "wsi_roi") && nrow(source$rois) > 0L) ||
          (is.character(source$static_url) && length(source$static_url) == 1L &&
             !is.na(source$static_url) && nzchar(source$static_url))
      )
    }, logical(1))]
  }

  dense_geojson_payload <- function(payload) {
    if (!is.list(payload)) {
      wsi_abort("Dense GeoJSON viewport request must be a JSON object.")
    }
    unknown <- setdiff(
      names(payload),
      c("source_id", "id", "xmin", "ymin", "xmax", "ymax", "zoom", "limit")
    )
    if (length(unknown)) {
      wsi_abort(sprintf(
        "Unsupported dense GeoJSON request field%s: %s.",
        if (length(unknown) == 1L) "" else "s",
        paste(unknown, collapse = ", ")
      ))
    }
    sources <- dense_geojson_sources()
    if (!length(sources)) {
      return(list(ok = TRUE, loaded = FALSE, retry_after_ms = 1500L, sources = list()))
    }
    source_id <- as.character(payload$source_id %||% payload$id %||% "")
    source_names <- names(sources)
    if (!nzchar(source_id)) {
      source_id <- source_names[[1L]]
    }
    source <- sources[[source_id]]
    if (is.null(source)) {
      return(list(
        ok = FALSE,
        loaded = FALSE,
        error = sprintf("Dense GeoJSON source not found: %s", source_id),
        sources = source_names
      ))
    }
    static_url <- as.character(source$static_url %||% "")
    if (length(static_url) == 1L && !is.na(static_url) && nzchar(static_url)) {
      return(list(
        ok = TRUE,
        loaded = TRUE,
        source_id = as.character(source$id %||% source_id),
        sources = source_names,
        static_url = static_url,
        static_source = list(
          name = as.character(source$name %||% "Tissue annotation"),
          kind = as.character(source$kind %||% "tissue"),
          source_type = as.character(source$source_type %||% "annotation"),
          visible = isTRUE(source$visible %||% TRUE),
          opacity = suppressWarnings(as.numeric(source$opacity %||% 0.86)),
          colour = as.character(source$colour %||% "#22C55E"),
          fill_alpha = suppressWarnings(as.numeric(source$fill_alpha %||% 0.16)),
          line_width = suppressWarnings(as.numeric(source$line_width %||% 2.2)),
          full_resolution_zoom = suppressWarnings(as.numeric(source$full_resolution_zoom %||% 3)),
          total_count = suppressWarnings(as.integer(source$total_count %||% NA_integer_))
        )
      ))
    }
    rois <- source$rois
    bounds <- vapply(c("xmin", "ymin", "xmax", "ymax"), function(field) {
      suppressWarnings(as.numeric(payload[[field]] %||% NA_real_))
    }, numeric(1))
    if (any(!is.finite(bounds)) || bounds[["xmax"]] <= bounds[["xmin"]] ||
        bounds[["ymax"]] <= bounds[["ymin"]]) {
      wsi_abort("Dense GeoJSON viewport request needs finite xmin, ymin, xmax, and ymax.")
    }
    zoom <- suppressWarnings(as.numeric(payload$zoom %||% NA_real_))
    source_min_zoom <- suppressWarnings(as.numeric(source$min_zoom %||% 0))
    if (!is.finite(source_min_zoom) || source_min_zoom < 0) {
      source_min_zoom <- 0
    }
    if (is.finite(zoom) && zoom < source_min_zoom) {
      source_type <- as.character(source$source_type %||% "cell_segmentation")
      source_id <- as.character(source$id %||% source_id)
      return(list(
        ok = TRUE,
        loaded = TRUE,
        source_id = source_id,
        sources = source_names,
        total_count = nrow(rois),
        viewport_count = 0L,
        returned_count = 0L,
        layer = list(
          id = source_id,
          name = as.character(source$name %||% "Cell annotation"),
          type = "vector",
          source_type = source_type,
          visible = isTRUE(source$visible %||% TRUE),
          opacity = suppressWarnings(as.numeric(source$opacity %||% 0.92)),
          colour = as.character(source$colour %||% "#F97316"),
          line_width = suppressWarnings(as.numeric(source$line_width %||% 1.8)),
          replace = TRUE,
          count = 0L,
          total_count = nrow(rois),
          viewport_count = 0L,
          items = list(),
          metadata = list(viewport_only = TRUE, below_min_zoom = TRUE, min_zoom = source_min_zoom, zoom = zoom)
        )
      ))
    }
    default_limit <- if (is.finite(zoom) && zoom >= 12) {
      12000L
    } else if (is.finite(zoom) && zoom >= 8) {
      8000L
    } else if (is.finite(zoom) && zoom >= 5) {
      4000L
    } else {
      1500L
    }
    limit <- suppressWarnings(as.integer(payload$limit %||% default_limit))
    if (!is.finite(limit) || limit < 100L) {
      limit <- default_limit
    }
    limit <- min(limit, 15000L)
    bbox_cols <- c("xmin", "ymin", "xmax", "ymax")
    if (!all(bbox_cols %in% names(rois))) {
      wsi_abort("Dense GeoJSON source is missing bounding-box columns.")
    }
    rxmin <- suppressWarnings(as.numeric(rois$xmin))
    rymin <- suppressWarnings(as.numeric(rois$ymin))
    rxmax <- suppressWarnings(as.numeric(rois$xmax))
    rymax <- suppressWarnings(as.numeric(rois$ymax))
    bbox_index <- source$bbox_index %||% NULL
    if (is.null(bbox_index)) {
      bbox_index <- wsi_bbox_index_create(rois)
      if (!is.null(bbox_index)) {
        source$bbox_index <- bbox_index
        if (is.environment(dense_geojson_context)) {
          dense_geojson_context$sources[[source_id]] <- source
        }
      }
    }
    idx <- wsi_bbox_index_query(
      bbox_index,
      bounds[["xmin"]],
      bounds[["ymin"]],
      bounds[["xmax"]],
      bounds[["ymax"]]
    )
    spatial_indexed <- !is.null(idx)
    if (is.null(idx)) {
      idx <- which(
        is.finite(rxmin) & is.finite(rymin) & is.finite(rxmax) & is.finite(rymax) &
          rxmin <= bounds[["xmax"]] & rxmax >= bounds[["xmin"]] &
          rymin <= bounds[["ymax"]] & rymax >= bounds[["ymin"]]
      )
    }
    viewport_count <- length(idx)
    sampled <- FALSE
    if (viewport_count > limit) {
      sampled <- TRUE
      hash <- (as.double(idx) * 1103515245 + 12345) %% 2147483647
      keep <- order(hash, method = "radix")[seq_len(limit)]
      idx <- idx[sort(keep)]
    }
    subset <- rois[idx, , drop = FALSE]
    viewport_width <- bounds[["xmax"]] - bounds[["xmin"]]
    viewport_height <- bounds[["ymax"]] - bounds[["ymin"]]
    broad_view <- is.finite(viewport_width) && is.finite(viewport_height) &&
      max(viewport_width, viewport_height) > 18000
    source_cap <- suppressWarnings(as.numeric(source$max_points_per_roi %||% 1200L))
    if (is.na(source_cap) || source_cap <= 0) {
      source_cap <- 1200L
    }
    full_resolution_zoom <- suppressWarnings(as.numeric(source$full_resolution_zoom %||% Inf))
    if (is.na(full_resolution_zoom) || full_resolution_zoom < 0) {
      full_resolution_zoom <- Inf
    }
    zoom_cap <- if (is.finite(zoom) && zoom >= full_resolution_zoom && zoom < 5) {
      2400L
    } else if (is.finite(zoom) && zoom >= full_resolution_zoom && zoom < 10) {
      6000L
    } else if (is.finite(zoom) && zoom >= full_resolution_zoom && zoom < 16) {
      12000L
    } else if (is.finite(zoom) && zoom >= full_resolution_zoom) {
      24000L
    } else if (!is.finite(zoom) || zoom < 1.5) {
      96L
    } else if (isTRUE(broad_view) || zoom < 5) {
      320L
    } else if (zoom < 10) {
      900L
    } else if (zoom < 16) {
      2200L
    } else {
      source_cap
    }
    max_points_per_roi <- min(source_cap, zoom_cap)
    if (is.finite(max_points_per_roi)) {
      max_points_per_roi <- max(32L, as.integer(max_points_per_roi))
    }
    source_type <- as.character(source$source_type %||% if (identical(source$kind %||% "", "tissue")) {
      "annotation"
    } else {
      "cell_segmentation"
    })
    source_name <- as.character(source$name %||% "Cell annotation")
    source_colour <- as.character(source$colour %||% "#F97316")
    source_fill_alpha <- suppressWarnings(as.numeric(source$fill_alpha %||% 0.22))
    decorate_items <- function(items) {
      lapply(items, function(item) {
        item$source_type <- source_type
        item$dense_geometry <- TRUE
        item$source <- source_name
        item
      })
    }
    bounds_only <- (!is.finite(zoom) || zoom < 1.05) && nrow(subset) > 0L
    clip_pad <- max(viewport_width, viewport_height) * 0.08
    if (!is.finite(clip_pad) || clip_pad < 0) {
      clip_pad <- 0
    }
    clip_bounds <- c(
      xmin = bounds[["xmin"]] - clip_pad,
      ymin = bounds[["ymin"]] - clip_pad,
      xmax = bounds[["xmax"]] + clip_pad,
      ymax = bounds[["ymax"]] + clip_pad
    )
    items <- wsi_viewer_dense_roi_features(
      subset,
      fill_alpha = source_fill_alpha,
      colour = source_colour,
      source_name = source_name,
      bounds_only = bounds_only,
      max_points_per_roi = max_points_per_roi,
      clip_bounds = if (isTRUE(bounds_only)) NULL else clip_bounds
    )
    items <- decorate_items(items)
    source_id <- as.character(source$id %||% source_id)
    layer <- list(
      id = source_id,
      name = as.character(source$name %||% "Cell annotation"),
      type = "vector",
      source_type = source_type,
      visible = isTRUE(source$visible %||% TRUE),
      opacity = suppressWarnings(as.numeric(source$opacity %||% 0.92)),
      colour = as.character(source$colour %||% "#F97316"),
      line_width = suppressWarnings(as.numeric(source$line_width %||% 1.8)),
      replace = TRUE,
      count = length(items),
      total_count = nrow(rois),
      viewport_count = viewport_count,
      items = items,
      metadata = list(
        viewport_only = TRUE,
        sampled = sampled,
        displayed_count = length(items),
        viewport_count = viewport_count,
        spatial_indexed = spatial_indexed,
        source_path = as.character(source$path %||% ""),
        geometry_lod = if (isTRUE(bounds_only)) "bounds" else "detail",
        max_points_per_roi = if (is.finite(max_points_per_roi)) max_points_per_roi else "full",
        full_resolution_zoom = if (is.finite(full_resolution_zoom)) full_resolution_zoom else NA_real_,
        zoom = zoom
      )
    )
    list(
      ok = TRUE,
      loaded = TRUE,
      source_id = source_id,
      sources = source_names,
      total_count = nrow(rois),
      viewport_count = viewport_count,
      returned_count = length(items),
      layer = layer
    )
  }

	  seurat_gene_response <- function(req) {
    method <- req$REQUEST_METHOD %||% "GET"
    feature_context <- proximity_context %||% prediction_context %||% list(spatial = seurat)
    has_feature_context <- wsi_prediction_context_enabled(feature_context %||% list())
    source_name <- if (!is.null(seurat) && inherits(seurat, "wsi_seurat_spatial")) {
      as.character(seurat$source_name %||% "spatial object")
    } else if (has_feature_context) {
      "live feature source"
    } else {
      "spatial object"
    }
    if (!wsi_seurat_live_gene_available(seurat) && !has_feature_context) {
      return(wsi_http_json_response(
        status = 404L,
        body = list(error = sprintf("No live %s expression source is attached to this viewer.", source_name))
      ))
    }
    gene <- NULL
    feature_source <- NULL
    point_source <- NULL
    reduction_dims <- NULL
    project_scope <- list()
    if (identical(method, "GET")) {
      query <- wsi_http_query_params(req$QUERY_STRING %||% "")
      gene <- query$gene %||% query$q %||% NULL
      feature_source <- query$feature_source %||% query$source %||% NULL
      point_source <- query$point_source %||% NULL
    } else if (identical(method, "POST")) {
      body <- wsi_http_request_body(req)
      payload <- if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else list()
      if (!is.list(payload)) {
        return(wsi_http_json_response(status = 400L, body = list(error = "Spatial gene request must be a JSON object.")))
      }
      unknown <- setdiff(names(payload), c(
        "gene", "q", "feature_source", "source", "point_source", "reduction_dims",
        "project_key", "project_image_index", "project_section_index",
        "project_image", "project_section"
      ))
      if (length(unknown)) {
        return(wsi_http_json_response(
          status = 400L,
          body = list(error = sprintf("Unsupported spatial gene request field%s: %s.", if (length(unknown) == 1L) "" else "s", paste(unknown, collapse = ", ")))
        ))
      }
      gene <- payload$gene %||% payload$q %||% NULL
      feature_source <- payload$feature_source %||% payload$source %||% NULL
      point_source <- payload$point_source %||% NULL
      reduction_dims <- payload$reduction_dims %||% NULL
      project_scope <- payload[c(
        "project_key", "project_image_index", "project_section_index",
        "project_image", "project_section"
      )]
    } else {
      return(wsi_http_json_response(status = 405L, body = list(error = "Use GET or POST for spatial gene expression lookup.")))
    }
    if (is.null(gene) || !is.character(gene) || length(gene) != 1L || is.na(gene) || !nzchar(trimws(gene))) {
      return(wsi_http_json_response(status = 400L, body = list(error = "Provide a single non-empty `gene` value.")))
    }
    tryCatch(
      wsi_http_json_response(body = {
        feature_source <- as.character(feature_source %||% "")
        point_source <- as.character(point_source %||% "")
        if (nzchar(feature_source) && !identical(feature_source, "auto") && has_feature_context) {
          wsi_prediction_feature_payload(
            feature_context,
            feature = trimws(gene),
            source_id = feature_source,
            point_source = if (nzchar(point_source)) point_source else NULL,
            reduction_dims = reduction_dims,
            project_scope = project_scope %||% list()
          )
        } else if (inherits(feature_context$spatial %||% NULL, "wsi_spatial_project")) {
          wsi_seurat_dynamic_gene_payload(
            feature_context$spatial,
            trimws(gene),
            project_scope = project_scope %||% list()
          )
        } else if (wsi_seurat_live_gene_available(seurat)) {
          wsi_seurat_dynamic_gene_payload(seurat, trimws(gene), project_scope = project_scope %||% list())
        } else {
          wsi_prediction_feature_payload(
            feature_context,
            feature = trimws(gene),
            source_id = "spatial:raw",
            point_source = if (nzchar(point_source)) point_source else NULL,
            reduction_dims = reduction_dims,
            project_scope = project_scope %||% list()
          )
        }
      }),
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

	  spatial_object_save_response <- function(req) {
	    method <- req$REQUEST_METHOD %||% "GET"
	    if (!identical(method, "POST")) {
	      return(wsi_http_json_response(status = 405L, body = list(error = "Use POST for spatial object save.")))
	    }
	    if (is.null(seurat)) {
	      return(wsi_http_json_response(status = 404L, body = list(error = "No live spatial object is attached to this viewer session.")))
	    }
	    tryCatch({
	      body <- wsi_http_request_body(req)
	      payload <- if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else list()
	      result <- wsi_spatial_object_save_response(seurat, payload, state = state)
	      response <- wsi_viewer_state_response(state)
	      response$spatial_object_save <- result
	      wsi_http_json_response(body = response)
	    }, error = function(err) {
	      overwrite_required <- inherits(err, "wsi_overwrite_required")
	      wsi_http_json_response(
	        status = if (overwrite_required) 409L else 500L,
	        body = list(
	          ok = FALSE,
	          error = conditionMessage(err),
	          overwrite_required = overwrite_required
	        )
	      )
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

	  geojson_mask_response <- function(req) {
	    method <- req$REQUEST_METHOD %||% "GET"
	    if (!identical(method, "POST")) {
	      return(wsi_http_json_response(status = 405L, body = list(error = "Use POST for GeoJSON mask overlay conversion.")))
	    }
	    if (is.null(slide)) {
	      return(wsi_http_json_response(status = 404L, body = list(error = "No slide is attached to this live viewer session.")))
	    }
	    tryCatch({
	      body <- wsi_http_request_body(req)
	      payload <- if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else list()
	      result <- wsi_viewer_geojson_mask_response(
	        slide,
	        payload,
	        state = state,
	        output_dir = geojson_mask_dir,
	        output_html = output_html %||% state$html %||% NULL
	      )
	      response <- wsi_viewer_state_response(state)
	      response$geojson_mask_overlay <- result
	      response$commands <- c(
	        response$commands %||% list(),
	        list(list(
	          id = paste0("geojson_mask_", result$source$id, "_", as.integer(Sys.time())),
	          type = "add_channel_source",
	          payload = list(source = result$source)
	        ))
	      )
	      wsi_http_json_response(body = response)
	    }, error = function(err) {
	      wsi_http_json_response(status = 500L, body = list(ok = FALSE, error = conditionMessage(err)))
	    })
	  }

	  dense_geojson_response <- function(req) {
	    method <- req$REQUEST_METHOD %||% "GET"
	    if (!identical(method, "POST")) {
	      return(wsi_http_json_response(status = 405L, body = list(error = "Use POST for dense GeoJSON viewport requests.")))
	    }
	    tryCatch({
	      body <- wsi_http_request_body(req)
	      payload <- if (nzchar(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else list()
	      wsi_http_json_response(body = dense_geojson_payload(payload))
	    }, error = function(err) {
	      wsi_http_json_response(status = 500L, body = list(ok = FALSE, loaded = FALSE, error = conditionMessage(err)))
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
	      event <- if (action %in% c("trajectory_profile_stats", "trajectory_correlations")) {
	        "trajectory_profile_failed"
	      } else if (action %in% c("stats", "statistics")) {
	        "proximity_stats_failed"
	      } else {
	        "proximity_failed"
	      }
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
	      if (identical(request_path, spatial_object_save_path)) {
	        return(spatial_object_save_response(req))
	      }
	      if (identical(request_path, image_export_path)) {
	        return(image_export_response(req))
	      }
	      if (identical(request_path, geojson_mask_path)) {
	        return(geojson_mask_response(req))
	      }
	      if (identical(request_path, dense_geojson_path)) {
	        return(dense_geojson_response(req))
	      }
	      if (identical(request_path, prediction_path)) {
	        return(prediction_response(req))
	      }
	      if (identical(request_path, proximity_path)) {
	        return(proximity_response(req))
	      }
	      if (identical(request_path, native_renderer_path)) {
	        if (!identical(method, "GET")) {
	          return(wsi_http_json_response(status = 405L, body = list(error = "Use GET for native renderer metadata.")))
	        }
	        return(wsi_http_json_response(body = native_renderer_manifest()))
	      }
      if (identical(request_path, native_state_path)) {
	        if (!identical(method, "GET")) {
	          return(wsi_http_json_response(status = 405L, body = list(error = "Use GET for native renderer state.")))
	        }
        query <- wsi_http_query_params(req$QUERY_STRING %||% "")
        return(wsi_http_json_response(body = native_renderer_state(query$source_id %||% NULL)))
	      }
	      if (identical(request_path, native_points_path)) {
	        return(native_points_response(req))
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
            settings = wsi_dynamic_tile_query_settings(req$QUERY_STRING %||% ""),
            request_etag = req$HTTP_IF_NONE_MATCH %||% NULL
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
	        spatial_object_save_path = spatial_object_save_path,
	        image_export_path = image_export_path,
	        geojson_mask_path = geojson_mask_path,
	        dense_geojson_path = dense_geojson_path,
	        prediction_path = prediction_path,
	        proximity_path = proximity_path,
	        native_renderer_path = native_renderer_path,
	        native_state_path = native_state_path,
	        native_points_path = native_points_path,
	        seurat_gene_url = if (wsi_seurat_live_gene_available(seurat) ||
            wsi_prediction_context_enabled(proximity_context %||% prediction_context %||% list(spatial = seurat))) {
            sprintf("http://%s:%d%s", host, candidate, seurat_gene_path)
          } else {
            NULL
          },
	        spatial_tile_export_url = sprintf("http://%s:%d%s", host, candidate, spatial_tile_path),
	        spatial_object_save_url = if (!is.null(seurat)) sprintf("http://%s:%d%s", host, candidate, spatial_object_save_path) else NULL,
	        image_export_url = if (!is.null(slide)) sprintf("http://%s:%d%s", host, candidate, image_export_path) else NULL,
	        geojson_mask_url = if (!is.null(slide)) sprintf("http://%s:%d%s", host, candidate, geojson_mask_path) else NULL,
	        dense_geojson_url = sprintf("http://%s:%d%s", host, candidate, dense_geojson_path),
	        prediction_url = if (wsi_prediction_context_enabled(prediction_context %||% list(spatial = seurat))) sprintf("http://%s:%d%s", host, candidate, prediction_path) else NULL,
	        proximity_url = if (wsi_prediction_context_enabled(proximity_context %||% prediction_context %||% list(spatial = seurat))) sprintf("http://%s:%d%s", host, candidate, proximity_path) else NULL,
	        native_renderer_url = sprintf("http://%s:%d%s", host, candidate, native_renderer_path),
	        native_state_url = sprintf("http://%s:%d%s", host, candidate, native_state_path),
	        native_points_url = sprintf("http://%s:%d%s", host, candidate, native_points_path),
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
    match_index <- which(vapply(existing, function(item) {
      if (!is.list(item)) {
        return(FALSE)
      }
      item_ids <- c(item$id %||% "", item$path %||% "")
      any(nzchar(record_ids) & nzchar(item_ids) & record_ids %in% item_ids)
    }, logical(1)))
    if (length(match_index)) {
      existing[[match_index[[1L]]]] <- utils::modifyList(
        existing[[match_index[[1L]]]],
        record,
        keep.null = TRUE
      )
      next
    }
    existing[[length(existing) + 1L]] <- record
    existing_ids <- c(existing_ids, record_ids[nzchar(record_ids)])
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
#' @param dynamic_tile_persistent_cache Keep generated dynamic tiles across
#'   viewer sessions in a fingerprinted, size-bounded cache. The desktop app
#'   enables this by default; the R API leaves it opt-in for compatibility.
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
	                               spatial_object_save_path = "/spatial-object-save",
	                               image_export_path = "/image-export",
	                               image_export_dir = getwd(),
	                               image_export_max_pixels = 50000000,
	                               geojson_mask_path = "/geojson-mask-overlay",
	                               geojson_mask_dir = file.path(tempdir(), "wsiTools_geojson_masks"),
	                               dense_geojson_path = "/dense-geojson",
	                               dense_geojson_context = NULL,
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
                               segmentation_pretrained_zip = NULL,
                               dynamic_tile_persistent_cache = FALSE) {
  if (!is.logical(stardist) || length(stardist) != 1L || is.na(stardist)) {
    wsi_abort("`stardist` must be `TRUE` or `FALSE`.")
  }
  transport <- match.arg(transport)
  if (!is.logical(dynamic_tiles) || length(dynamic_tiles) != 1L || is.na(dynamic_tiles)) {
    wsi_abort("`dynamic_tiles` must be `TRUE` or `FALSE`.")
  }
  if (!is.logical(dynamic_tile_persistent_cache) ||
      length(dynamic_tile_persistent_cache) != 1L ||
      is.na(dynamic_tile_persistent_cache)) {
    wsi_abort("`dynamic_tile_persistent_cache` must be `TRUE` or `FALSE`.")
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
  if (is.null(dense_geojson_context)) {
    dense_geojson_context <- new.env(parent = emptyenv())
    dense_geojson_context$sources <- list()
  } else if (is.environment(dense_geojson_context) && is.null(dense_geojson_context$sources)) {
    dense_geojson_context$sources <- list()
  }

  dots <- list(...)
  if (is.null(dots$output)) {
    dots$output <- tempfile(fileext = ".html")
    dots$overwrite <- TRUE
  }
  live_seurat <- dots$seurat %||% NULL
  live_prediction_context <- prediction_context %||% list()
  if (is.null(live_prediction_context$spatial) && !is.null(live_seurat)) {
    live_prediction_context$spatial <- live_seurat
  }
	  live_cellphenotyper <- dots$cellphenotyper %||% dots$cellphenotyper_project %||%
	    live_prediction_context$cellphenotyper_project %||% live_prediction_context$cellphenotyper %||% NULL
  live_proximity_context <- proximity_context %||% live_prediction_context
  if (is.null(live_proximity_context$spatial) && !is.null(live_seurat)) {
    live_proximity_context$spatial <- live_seurat
  }
  requested_channel_sources <- dots$channel_sources %||% NULL
  dynamic_project_sources <- wsi_dynamic_channel_sources(project_tile_sources)
  dynamic_source <- NULL
  if (isTRUE(dynamic_tiles)) {
    dots$tile_image_loader_limit <- dots$tile_image_loader_limit %||% 4L
    dots$tile_prefetch_margin <- dots$tile_prefetch_margin %||% 0L
    dots$tile_prefetch_cache_count <- dots$tile_prefetch_cache_count %||% 0L
    dots$tile_timeout_ms <- dots$tile_timeout_ms %||% 60000L
    dynamic_source <- wsi_dynamic_tile_source(
      slide,
      slide_id = wsi_safe_id(name, "slide"),
      format = dynamic_tile_format,
      cache_dir = dynamic_tile_cache_dir,
      route = dynamic_tile_path,
      persistent_cache = dynamic_tile_persistent_cache
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
        route = dynamic_tile_path,
        persistent_cache = dynamic_tile_persistent_cache
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
  if (length(all_dynamic_sources) &&
      identical(Sys.getenv("WSITOOLS_DYNAMIC_PREWARM_TILES", unset = "false"), "true")) {
    try(
      wsi_dynamic_prewarm_tiles(all_dynamic_sources, timeout_warning = FALSE),
      silent = TRUE
    )
  }

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
	    cellphenotyper = live_cellphenotyper,
	    seurat_gene_path = seurat_gene_path,
	    spatial_tile_path = spatial_tile_path,
	    spatial_object_save_path = spatial_object_save_path,
	    image_export_path = image_export_path,
	    image_export_dir = image_export_dir,
	    image_export_max_pixels = image_export_max_pixels,
	    geojson_mask_path = geojson_mask_path,
	    geojson_mask_dir = geojson_mask_dir,
	    dense_geojson_context = dense_geojson_context,
	    dense_geojson_path = dense_geojson_path,
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
    dots$channel_sources <- wsi_live_channel_sources(
      requested_channel_sources,
      base_url = base_url,
      output = dots$output
    )
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
    # The native WGPU manifest is served from the same R session. Store the
    # selected-ROI endpoint in state so the native Cells menu can request only
    # the three allowlisted engines without accepting arbitrary commands.
    state$native_segmentation_run_url <- stardist_bridge$url
  }
  dots$viewer_state_url <- bridge$url
  dots$viewer_state_ws_url <- if (identical(transport, "polling")) NULL else bridge$ws_url
	  dots$viewer_transport <- transport
	  dots$seurat_gene_url <- bridge$seurat_gene_url %||% NULL
	  dots$spatial_tile_export_url <- bridge$spatial_tile_export_url %||% NULL
	  dots$spatial_object_save_url <- bridge$spatial_object_save_url %||% NULL
	  dots$image_export_url <- bridge$image_export_url %||% NULL
	  dots$geojson_mask_url <- bridge$geojson_mask_url %||% NULL
	  dots$dense_geojson_url <- bridge$dense_geojson_url %||% NULL
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
  state$html <- html

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
        dense_geojson_context = dense_geojson_context,
        proximity_context = live_proximity_context,
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
    annotation_masks = state$annotation_masks %||% list(),
    channel_settings = state$channel_settings %||% wsi_empty_channel_settings(),
    tile_sources = state$tile_sources %||% list(),
    kodama_selection = state$kodama_selection %||% list(labels = character(), count = 0L, matched_count = 0L),
    seurat_selection = state$seurat_selection %||% list(labels = character(), count = 0L, matched_count = 0L),
    annotation_spots = state$annotation_spots %||% wsi_empty_annotation_spots(),
    spatial_registration = state$spatial_registration %||% wsi_empty_spatial_registration(),
    performance = state$performance %||% list(),
    tile_preview = state$tile_preview %||% wsi_empty_tile_preview(),
    prediction = state$prediction %||% wsi_empty_prediction_result(),
    proximity = state$proximity %||% wsi_empty_proximity_result(),
    proximity_stats = state$proximity_stats %||% wsi_empty_proximity_stats_result(),
    trajectory_profile = state$trajectory_profile %||% wsi_empty_trajectory_profile(),
    trajectory_correlations = state$trajectory_correlations %||% wsi_empty_trajectory_correlations(),
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
    native_active_source_id = state$native_active_source_id %||% NULL,
    native_project_states = state$native_project_states %||% list(),
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
  cat("  methods: capabilities(), on(), get_rois(), get_selected_roi(), get_selected_rois(), get_selected_object(), get_selected_spots(), get_spot_annotation_table(), get_annotation_spot_matrix(), get_spatial_registration(), get_performance(), get_measurements(), get_trajectories(), get_roi_summary(), get_cell_summary(), get_ihc_summary(), get_segmentation(), get_layers(), get_annotation_masks(), get_channel_settings(), get_kodama_selection(), get_annotation_spots(), get_history(), get_logs(), get_tile_preview(), get_prediction(), get_proximity(), get_proximity_stats(), get_trajectory_profile(), get_trajectory_correlations(), colour_spots_by_gene(), add_rois(), add_layer(), add_channel_source(), measure_ihc_intensity(), preview_tiles(), extract_tile_preview(), list_jobs(), run_tiles_async(), save_project(), autosave_start()\n")
  cat("  stop with: wsi_viewer_stop(x)\n")
  invisible(x)
}
