wsi_project_manifest_file <- function(path) {
  file.path(path, "project.json")
}

wsi_project_prepare_dir <- function(path, overwrite = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    wsi_abort("`path` must be a single non-empty project directory path.")
  }
  if (!dir.exists(path)) {
    if (!dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
      wsi_abort(sprintf("Could not create project directory: %s", path))
    }
  }
  manifest <- wsi_project_manifest_file(path)
  if (file.exists(manifest) && !isTRUE(overwrite)) {
    wsi_abort(
      sprintf("Project already exists and `overwrite = FALSE`: %s", manifest),
      class = "wsi_output_exists"
    )
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

wsi_project_rel <- function(file, root) {
  if (length(file) > 1L) {
    out <- vapply(file, wsi_project_rel, character(1), root = root)
    names(out) <- names(file)
    return(out)
  }
  file <- tryCatch(
    normalizePath(file, winslash = "/", mustWork = FALSE),
    error = function(err) file
  )
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (startsWith(file, prefix)) {
    return(substring(file, nchar(prefix) + 1L))
  }
  file
}

wsi_project_abs <- function(file, root) {
  if (is.null(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    return(NA_character_)
  }
  if (grepl("^([A-Za-z]:)?[/\\\\]", file)) {
    return(file)
  }
  file.path(root, file)
}

wsi_project_write_csv <- function(x, file, overwrite = FALSE) {
  file <- wsi_validate_output_path(file, overwrite = overwrite)
  utils::write.csv(as.data.frame(x), file, row.names = FALSE)
  invisible(file)
}

wsi_project_slide_info <- function(slide = NULL) {
  if (is.null(slide)) {
    return(NULL)
  }
  if (inherits(slide, "wsi_slide")) {
    return(list(
      path = slide$path %||% NA_character_,
      absolute_path = if (!is.null(slide$path) && file.exists(slide$path)) {
        normalizePath(slide$path, winslash = "/", mustWork = FALSE)
      } else {
        slide$path %||% NA_character_
      },
      backend = slide$backend %||% NA_character_,
      dimensions = as.list(slide$dimensions %||% c(width = NA_real_, height = NA_real_)),
      levels = as.data.frame(slide$levels %||% data.frame()),
      mpp = as.list(wsi_mpp(slide)),
      objective_power = wsi_objective_power(slide)
    ))
  }
  if (is.character(slide) && length(slide) == 1L && !is.na(slide) && nzchar(slide)) {
    return(list(
      path = slide,
      absolute_path = if (file.exists(slide)) normalizePath(slide, winslash = "/", mustWork = FALSE) else slide,
      backend = NA_character_,
      dimensions = list(width = NA_real_, height = NA_real_),
      levels = data.frame(),
      mpp = list(x = NA_real_, y = NA_real_),
      objective_power = NA_real_
    ))
  }
  wsi_abort("`slide` must be NULL, a file path, or a `wsi_slide` object.")
}

wsi_project_state <- function(viewer_state = NULL) {
  if (is.null(viewer_state)) {
    return(NULL)
  }
  if (inherits(viewer_state, "wsi_viewer_session") || inherits(viewer_state, "wsi_viewer_state")) {
    return(wsi_viewer_state(viewer_state))
  }
  if (is.list(viewer_state)) {
    return(viewer_state)
  }
  wsi_abort("`viewer_state` must be NULL, a viewer session/state, or a viewer-state list.")
}

wsi_project_stain_settings <- function(stain_settings = NULL, stains = NULL, viewer = NULL) {
  if (!is.null(stain_settings)) {
    return(stain_settings)
  }
  if (!is.null(viewer$stain)) {
    return(viewer$stain)
  }
  if (inherits(stains, "wsi_ihc_channels")) {
    return(list(
      type = "wsi_ihc_channels",
      channel_metadata = stains$channel_metadata %||% list(),
      stain_matrix = stains$stain_matrix %||% NULL
    ))
  }
  if (inherits(stains, "wsi_stain_channels")) {
    return(list(
      type = "wsi_stain_channels",
      channel_metadata = unclass(stains)
    ))
  }
  if (is.list(stains)) {
    return(stains)
  }
  NULL
}

wsi_project_write_rois <- function(rois, path, name, overwrite = FALSE) {
  if (is.null(rois) || !inherits(rois, "wsi_roi") || !nrow(rois)) {
    return(NULL)
  }
  file <- file.path(path, sprintf("%s.geojson", name))
  write_geojson(rois, file, overwrite = overwrite)
  list(type = "geojson", path = wsi_project_rel(file, path), count = nrow(rois))
}

wsi_project_write_measurements <- function(measurements, path, overwrite = FALSE) {
  if (is.null(measurements)) {
    return(NULL)
  }
  if (inherits(measurements, "wsi_measurement_report")) {
    dir <- file.path(path, "measurements")
    if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
      wsi_abort(sprintf("Could not create measurement directory: %s", dir))
    }
    report_files <- character()
    for (name in wsi_report_table_names(measurements)) {
      table <- measurements[[name]]
      if (!is.data.frame(table)) {
        next
      }
      file <- file.path(dir, sprintf("%s.csv", name))
      wsi_project_write_csv(table, file, overwrite = overwrite)
      report_files[[name]] <- file
    }
    return(list(
      type = "measurement_report",
      tables = as.list(wsi_project_rel(report_files, path))
    ))
  }
  if (is.data.frame(measurements)) {
    file <- file.path(path, "measurements.csv")
    wsi_project_write_csv(measurements, file, overwrite = overwrite)
    return(list(type = "csv", path = wsi_project_rel(file, path), count = nrow(measurements)))
  }
  if (is.list(measurements)) {
    dir <- file.path(path, "measurements")
    if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
      wsi_abort(sprintf("Could not create measurement directory: %s", dir))
    }
    files <- list()
    for (name in names(measurements)) {
      table <- measurements[[name]]
      if (!is.data.frame(table)) {
        next
      }
      file <- file.path(dir, sprintf("%s.csv", wsi_safe_id(name, "measurement")))
      wsi_project_write_csv(table, file, overwrite = overwrite)
      files[[name]] <- wsi_project_rel(file, path)
    }
    return(list(type = "tables", tables = files))
  }
  wsi_abort("`measurements` must be NULL, a data frame, a measurement report, or a named list of data frames.")
}

wsi_project_write_segmentation <- function(segmentation, path, overwrite = FALSE) {
  if (is.null(segmentation)) {
    return(NULL)
  }
  if (inherits(segmentation, "wsi_roi")) {
    file <- file.path(path, "segmentation.geojson")
    write_geojson(segmentation, file, overwrite = overwrite)
    return(list(type = "geojson", path = wsi_project_rel(file, path), count = nrow(segmentation)))
  }
  if (is.data.frame(segmentation)) {
    file <- file.path(path, "segmentation.csv")
    wsi_project_write_csv(segmentation, file, overwrite = overwrite)
    return(list(type = "csv", path = wsi_project_rel(file, path), count = nrow(segmentation)))
  }
  if (is.list(segmentation) && !is.null(segmentation$path)) {
    return(list(
      type = segmentation$type %||% "external",
      path = as.character(segmentation$path),
      source_file = attr(segmentation, "source_file", exact = TRUE) %||% NA_character_
    ))
  }
  wsi_abort("`segmentation` must be NULL, ROI GeoJSON-like data, centroid data, or a segmentation object.")
}

wsi_project_write_tile_manifest <- function(tile_manifest, path, overwrite = FALSE) {
  if (is.null(tile_manifest)) {
    return(NULL)
  }
  dir <- file.path(path, "tile_manifests")
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
    wsi_abort(sprintf("Could not create tile manifest directory: %s", dir))
  }
  manifests <- if (is.data.frame(tile_manifest)) {
    list(tile_manifest = tile_manifest)
  } else if (is.list(tile_manifest)) {
    tile_manifest
  } else {
    wsi_abort("`tile_manifest` must be NULL, a data frame, or a named list of data frames.")
  }
  files <- list()
  for (name in names(manifests)) {
    manifest <- manifests[[name]]
    if (!is.data.frame(manifest)) {
      next
    }
    file <- file.path(dir, sprintf("%s.csv", wsi_safe_id(name, "tile_manifest")))
    wsi_project_write_csv(manifest, file, overwrite = overwrite)
    files[[name]] <- list(path = wsi_project_rel(file, path), count = nrow(manifest))
  }
  files
}

wsi_project_viewer_summary <- function(viewer) {
  if (is.null(viewer)) {
    return(NULL)
  }
  # Native WGPU keeps editable state per project source. Capture the active
  # source as well as previously visited sources so saving from one pane does
  # not silently discard annotations made on another slide.
  native_states <- viewer$native_project_states %||% list()
  native_active_source <- as.character(viewer$native_active_source_id %||% "")
  native_active_source <- if (length(native_active_source)) native_active_source[[1L]] else ""
  if (nzchar(native_active_source) && exists("wsi_native_project_state_snapshot", mode = "function")) {
    native_states[[native_active_source]] <- wsi_native_project_state_snapshot(viewer)
  }
  list(
    view = viewer$view %||% list(),
    project = viewer$project_snapshot %||% viewer$project %||% NULL,
    stain = viewer$stain %||% NULL,
    selected_roi = viewer$selected_roi %||% NULL,
    selected_rois = viewer$selected_rois %||% NULL,
    trajectories = viewer$trajectories %||% wsi_empty_trajectories(),
    tile_preview = viewer$tile_preview %||% wsi_empty_tile_preview(),
    layers = wsi_viewer_layer_summary(viewer$layers %||% list()),
    layers_full = viewer$layers %||% list(),
    channel_sources = viewer$channel_sources %||% list(),
    channel_settings = viewer$channel_settings %||% wsi_empty_channel_settings(),
    tile_sources = viewer$tile_sources %||% list(),
    annotations = viewer$annotations %||% list(dirty = FALSE, dirty_reason = ""),
    history = viewer$history %||% wsi_empty_annotation_history(),
    last_segmentation = viewer$last_segmentation %||% NULL,
    last_event = viewer$last_event %||% NULL,
    last_sync = as.character(viewer$last_sync %||% NA_character_),
    events = viewer$events %||% list(),
    native_active_source_id = native_active_source,
    native_project_states = native_states
  )
}

wsi_project_package_version <- function(package) {
  tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(err) NA_character_
  )
}

wsi_project_processing_provenance <- function(provenance = list()) {
  if (is.null(provenance)) {
    provenance <- list()
  }
  if (!is.list(provenance)) {
    wsi_abort("`processing_provenance` must be a list.")
  }
  base <- list(
    saved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    os = Sys.info()[["sysname"]] %||% NA_character_,
    working_directory = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    package_versions = list(
      wsiTools = wsi_project_package_version("wsiTools"),
      jsonlite = wsi_project_package_version("jsonlite"),
      cli = wsi_project_package_version("cli")
    ),
    backend_capabilities = tryCatch(wsi_backends(), error = function(err) NULL)
  )
  utils::modifyList(base, provenance, keep.null = TRUE)
}

wsi_new_project_object <- function(slide = NULL, viewer_state = NULL, rois = NULL,
                                   measurements = NULL, segmentation = NULL,
                                   stain_settings = NULL, stains = NULL,
                                   tile_manifest = NULL, metadata = list(),
                                   processing_provenance = list(),
                                   path = NULL, manifest = NULL) {
  viewer <- wsi_project_state(viewer_state)
  rois <- rois %||% viewer$rois
  measurements <- measurements %||% viewer$measurements
  segmentation <- segmentation %||% viewer$segmentation
  stain_settings <- wsi_project_stain_settings(stain_settings, stains, viewer)
  slide_info <- wsi_project_slide_info(slide)
  slide_path <- slide_info$path %||% NA_character_

  project <- list(
    path = path,
    manifest = manifest,
    slide = if (inherits(slide, "wsi_slide")) slide else NULL,
    slide_path = slide_path,
    slide_info = slide_info,
    viewer_state = viewer,
    rois = rois,
    measurements = measurements,
    segmentation = segmentation,
    stain_settings = stain_settings,
    stains = stains,
    tile_manifest = tile_manifest,
    metadata = metadata %||% list(),
    processing_provenance = processing_provenance %||% list()
  )
  class(project) <- "wsi_project"
  project
}

wsi_project_slide_source <- function(project) {
  if (inherits(project$slide, "wsi_slide")) {
    return(project$slide)
  }
  slide_path <- project$slide_path %||% NULL
  if (is.character(slide_path) && length(slide_path) == 1L &&
      !is.na(slide_path) && nzchar(slide_path)) {
    return(slide_path)
  }
  NULL
}

#' Save a reproducible wsiTools project
#'
#' Creates an editable project object, or writes one directly when `path` is a
#' project directory. The project stores slide identity, viewer state, ROI
#' annotations, measurements, segmentation outputs, stain settings, tile
#' manifests, and processing provenance as a JSON index plus sidecar
#' GeoJSON/CSV files. Pixel data are not copied or loaded.
#'
#' @param path Project directory to create/update, a `wsi_slide` object, a live
#'   viewer session/state for an in-memory project, or `NULL`.
#' @param slide Optional slide path or `wsi_slide` object.
#' @param viewer_state Optional object from [wsi_viewer_state()] or a live
#'   viewer session/state. ROIs, measurements, segmentation overlays, view
#'   settings, and stain settings are inferred from this object when not
#'   supplied explicitly.
#' @param rois Optional `wsi_roi` annotations.
#' @param measurements Optional measurement data frame, measurement report, or
#'   named list of data frames.
#' @param segmentation Optional segmentation result as `wsi_roi`, centroid data
#'   frame, or imported segmentation object.
#' @param stain_settings Optional list of stain/viewer settings.
#' @param stains Optional `wsi_ihc_channels` or `wsi_stain_channels` object used
#'   to derive stain settings.
#' @param tile_manifest Optional tile manifest data frame or named list of
#'   manifests.
#' @param metadata Optional user metadata list.
#' @param processing_provenance Optional list describing processing steps,
#'   model versions, parameters, commands, or other reproducibility notes.
#' @param overwrite Whether to overwrite an existing project index and sidecar
#'   files created by wsiTools.
#'
#' @return A `wsi_project` object. If saved, the object contains the project
#'   index and loaded in-memory objects.
#' @export
#'
#' @examples
#' \dontrun{
#' slide <- wsi_open("sample.svs")
#' project <- wsi_project(slide)
#' project$rois <- viewer$get_rois()
#' project$measurements <- viewer$get_measurements()
#' project$segmentation <- viewer$get_segmentation()
#' wsi_save_project(project, "case_001.wsiproject")
#'
#' rois <- read_geojson("annotations.geojson")
#' tiles <- extract_tiles(slide, roi = rois, save_images = FALSE)
#' project <- wsi_project(
#'   "case_001.wsiproject",
#'   slide = slide,
#'   rois = rois,
#'   tile_manifest = tiles
#' )
#' wsi_close(slide)
#' reopened <- wsi_read_project("case_001.wsiproject")
#' }
wsi_project <- function(path = NULL, slide = NULL, viewer_state = NULL, rois = NULL,
                        measurements = NULL, segmentation = NULL,
                        stain_settings = NULL, stains = NULL,
                        tile_manifest = NULL, metadata = list(),
                        processing_provenance = list(),
                        overwrite = FALSE) {
  if (inherits(path, "wsi_project") && is.null(slide)) {
    return(path)
  }
  if ((inherits(path, "wsi_slide") || inherits(path, "wsi_viewer_session") ||
       inherits(path, "wsi_viewer_state")) && is.null(slide)) {
    if (inherits(path, "wsi_slide")) {
      slide <- path
    } else {
      viewer_state <- path
      if (inherits(path, "wsi_viewer_session") && inherits(path$slide, "wsi_slide")) {
        slide <- path$slide
      }
    }
    path <- NULL
  }
  project <- wsi_new_project_object(
    slide = slide,
    viewer_state = viewer_state,
    rois = rois,
    measurements = measurements,
    segmentation = segmentation,
    stain_settings = stain_settings,
    stains = stains,
    tile_manifest = tile_manifest,
    metadata = metadata,
    processing_provenance = processing_provenance
  )
  if (is.null(path)) {
    return(project)
  }
  wsi_save_project(project, path, overwrite = overwrite)
}

#' @rdname wsi_project
#' @param project A `wsi_project` object, or a compatible list.
#' @export
wsi_save_project <- function(project, path = NULL, overwrite = FALSE,
                             processing_provenance = NULL) {
  if (!inherits(project, "wsi_project")) {
    if (!is.list(project)) {
      wsi_abort("`project` must be a `wsi_project` object or compatible list.")
    }
    class(project) <- c("wsi_project", class(project))
  }
  path <- path %||% project$path
  path <- wsi_project_prepare_dir(path, overwrite = overwrite)
  viewer <- wsi_project_state(project$viewer_state)
  rois <- project$rois %||% viewer$rois
  measurements <- project$measurements %||% viewer$measurements
  segmentation <- project$segmentation %||% viewer$segmentation
  tile_manifest <- project$tile_manifest %||% project$tile_manifests
  stain_settings <- wsi_project_stain_settings(project$stain_settings, project$stains, viewer)
  metadata <- project$metadata %||% list()
  provenance <- processing_provenance %||% project$processing_provenance %||% project$provenance %||% list()
  files <- list(
    rois = wsi_project_write_rois(rois, path, "rois", overwrite = overwrite),
    measurements = wsi_project_write_measurements(measurements, path, overwrite = overwrite),
    segmentation = wsi_project_write_segmentation(segmentation, path, overwrite = overwrite),
    tile_manifests = wsi_project_write_tile_manifest(tile_manifest, path, overwrite = overwrite)
  )

  manifest <- list(
    schema = "wsiTools-project",
    schema_version = 1L,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
    package_version = tryCatch(
      as.character(utils::packageVersion("wsiTools")),
      error = function(err) NA_character_
    ),
    slide = project$slide_info %||% wsi_project_slide_info(wsi_project_slide_source(project)),
    viewer_state = wsi_project_viewer_summary(viewer),
    stain_settings = stain_settings,
    files = files,
    metadata = metadata,
    processing_provenance = wsi_project_processing_provenance(provenance)
  )
  jsonlite::write_json(
    manifest,
    wsi_project_manifest_file(path),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  project <- list(
    path = path,
    manifest = manifest,
    slide_path = manifest$slide$path %||% NA_character_,
    slide = project$slide %||% NULL,
    slide_info = manifest$slide,
    viewer_state = viewer,
    rois = rois,
    measurements = measurements,
    segmentation = segmentation,
    stain_settings = stain_settings,
    tile_manifest = tile_manifest,
    metadata = metadata,
    processing_provenance = manifest$processing_provenance
  )
  class(project) <- "wsi_project"
  project
}

#' @rdname wsi_project
#' @export
save_wsi_project <- wsi_save_project

wsi_read_project_table <- function(file) {
  data <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  data
}

wsi_read_project_measurements <- function(info, root) {
  if (is.null(info)) {
    return(NULL)
  }
  if (identical(info$type, "csv")) {
    return(wsi_read_project_table(wsi_project_abs(info$path, root)))
  }
  if (identical(info$type, "measurement_report") || identical(info$type, "tables")) {
    tables <- info$tables %||% list()
    out <- lapply(tables, function(file) wsi_read_project_table(wsi_project_abs(file, root)))
    if (identical(info$type, "measurement_report")) {
      out$files <- unlist(tables, use.names = TRUE)
      class(out) <- c("wsi_measurement_report", class(out))
    }
    return(out)
  }
  NULL
}

wsi_read_project_tile_manifests <- function(info, root) {
  if (is.null(info) || !length(info)) {
    return(NULL)
  }
  out <- lapply(info, function(entry) {
    data <- wsi_read_project_table(wsi_project_abs(entry$path, root))
    class(data) <- c("wsi_tile_manifest", class(data))
    data
  })
  if (length(out) == 1L && identical(names(out), "tile_manifest")) {
    return(out[[1L]])
  }
  out
}

wsi_read_project_segmentation <- function(info, root) {
  if (is.null(info)) {
    return(NULL)
  }
  file <- wsi_project_abs(info$path, root)
  if (identical(info$type, "geojson") && file.exists(file)) {
    return(import_segmentation(file, type = "geojson"))
  }
  if (identical(info$type, "csv") && file.exists(file)) {
    return(import_segmentation(file, type = "csv"))
  }
  info
}

#' Read a saved wsiTools project
#'
#' @param path Project directory created by [wsi_project()].
#' @param open_slide Whether to try reopening the recorded slide path.
#'
#' @return A `wsi_project` object.
#' @export
wsi_read_project <- function(path, open_slide = FALSE) {
  if (!dir.exists(path)) {
    wsi_abort(sprintf("Project directory does not exist: %s", path), class = "wsi_file_not_found")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  manifest_file <- wsi_project_manifest_file(path)
  if (!file.exists(manifest_file)) {
    wsi_abort(sprintf("Project manifest does not exist: %s", manifest_file), class = "wsi_file_not_found")
  }
  manifest <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)
  if (!identical(manifest$schema, "wsiTools-project")) {
    wsi_abort("Project manifest is not a wsiTools project.")
  }

  rois_info <- manifest$files$rois
  rois <- NULL
  if (!is.null(rois_info$path)) {
    rois <- read_geojson(wsi_project_abs(rois_info$path, path))
  }
  measurements <- wsi_read_project_measurements(manifest$files$measurements, path)
  segmentation <- wsi_read_project_segmentation(manifest$files$segmentation, path)
  tile_manifest <- wsi_read_project_tile_manifests(manifest$files$tile_manifests, path)
  slide_path <- manifest$slide$path %||% manifest$slide$absolute_path %||% NA_character_
  slide <- NULL
  if (isTRUE(open_slide) && !is.na(slide_path) && nzchar(slide_path) && file.exists(slide_path)) {
    slide <- wsi_open(slide_path)
  }

  project <- list(
    path = path,
    manifest = manifest,
    slide_path = slide_path,
    slide = slide,
    rois = rois,
    measurements = measurements,
    segmentation = segmentation,
    stain_settings = manifest$stain_settings,
    tile_manifest = tile_manifest,
    viewer_state = manifest$viewer_state,
    metadata = manifest$metadata,
    processing_provenance = manifest$processing_provenance %||% manifest$provenance %||% list()
  )
  class(project) <- "wsi_project"
  project
}

#' @rdname wsi_read_project
#' @export
read_wsi_project <- wsi_read_project

#' Restore saved project state into a live viewer
#'
#' Restores annotations, selected ROI IDs, layers, segmentation, measurements,
#' stain/channel settings, tile source metadata, viewport state, and history
#' from a `.wsiproject` object or directory into an existing
#' [wsi_viewer_live()] session. Pixel data are not copied or loaded.
#'
#' @param viewer A `wsi_viewer_session`.
#' @param project A `wsi_project` object or path to a `.wsiproject` directory.
#' @param service Whether to service pending live-viewer events after queuing
#'   the browser restore command.
#'
#' @return The live viewer session, invisibly.
#' @export
restore_project_state <- function(viewer, project, service = TRUE) {
  if (!inherits(viewer, "wsi_viewer_session")) {
    wsi_abort("`viewer` must be a `wsi_viewer_session` object.")
  }
  if (is.character(project) && length(project) == 1L && !is.na(project) && nzchar(project)) {
    project <- wsi_read_project(project)
  }
  if (!inherits(project, "wsi_project")) {
    wsi_abort("`project` must be a `wsi_project` object or project directory path.")
  }
  saved <- project$viewer_state %||% list()
  state <- viewer$state
  if (inherits(project$rois, "wsi_roi")) {
    state$rois <- project$rois
  }
  if (is.data.frame(project$measurements)) {
    state$measurements <- project$measurements
  }
  if (inherits(project$segmentation, "wsi_roi")) {
    state$segmentation <- project$segmentation
  }
  state$selected_roi <- saved$selected_roi %||% NULL
  state$selected_rois <- saved$selected_rois %||% wsi_empty_roi()
  state$trajectories <- wsi_trajectories_from_payload(saved$trajectories %||% NULL)
  state$project <- saved$project %||% state$project %||% list()
  state$project_snapshot <- saved$project %||% state$project_snapshot %||% NULL
  state$layers <- saved$layers_full %||% state$layers %||% list()
  state$tile_preview <- saved$tile_preview %||% state$tile_preview %||% wsi_empty_tile_preview()
  state$view <- saved$view %||% list()
  state$stain <- saved$stain %||% project$stain_settings %||% NULL
  state$channel_sources <- saved$channel_sources %||% list()
  state$channel_settings <- if (!is.null(saved$channel_settings)) {
    wsi_channel_settings_from_payload(saved$channel_settings)
  } else if (!is.null(state$stain$channels)) {
    wsi_channel_settings_from_payload(state$stain$channels)
  } else {
    wsi_empty_channel_settings()
  }
  state$tile_sources <- saved$tile_sources %||% list()
  state$annotations <- saved$annotations %||% list(dirty = FALSE, dirty_reason = "project_restored")
  state$history <- wsi_annotation_history_from_payload(saved$history %||% NULL)
  state$native_project_states <- saved$native_project_states %||% list()
  state$native_active_source_id <- saved$native_active_source_id %||% NULL
  active_native_source <- as.character(state$native_active_source_id %||% "")
  active_native_source <- if (length(active_native_source)) active_native_source[[1L]] else ""
  if (nzchar(active_native_source) && exists("wsi_native_project_state_restore", mode = "function")) {
    wsi_native_project_state_restore(
      state,
      state$native_project_states[[active_native_source]] %||% NULL
    )
  }
  state$last_segmentation <- saved$last_segmentation %||% NULL
  state$last_event <- "r_restore_project_state"
  state$last_sync <- Sys.time()
  wsi_viewer_update_measurement_tables(state)
  wsi_viewer_queue_command(
    state,
    "restore_project_state",
    list(
      rois = if (inherits(state$rois, "wsi_roi") && nrow(state$rois)) wsi_viewer_rois_to_geojson(state$rois) else NULL,
      trajectories = wsi_trajectories_to_payload(state$trajectories),
      segmentation = if (inherits(state$segmentation, "wsi_roi") && nrow(state$segmentation)) wsi_viewer_rois_to_geojson(state$segmentation) else NULL,
      channel_sources = state$channel_sources,
      channel_settings = state$channel_settings,
      view = state$view,
      stain = state$stain
    )
  )
  wsi_assign_viewer_state(state)
  if (isTRUE(service)) {
    wsi_viewer_session_pump(viewer, 0L)
  }
  invisible(viewer)
}

#' @export
print.wsi_project <- function(x, ...) {
  cat("<wsi_project>\n")
  cat(sprintf("  path: %s\n", x$path))
  cat(sprintf("  slide: %s\n", x$slide_path %||% NA_character_))
  if (inherits(x$rois, "wsi_roi")) {
    cat(sprintf("  ROIs: %d\n", nrow(x$rois)))
  }
  if (is.data.frame(x$measurements)) {
    cat(sprintf("  measurements: %d rows\n", nrow(x$measurements)))
  } else if (inherits(x$measurements, "wsi_measurement_report")) {
    cat("  measurements: report\n")
  }
  if (is.data.frame(x$segmentation)) {
    cat(sprintf("  segmentation: %d rows\n", nrow(x$segmentation)))
  }
  if (is.data.frame(x$tile_manifest)) {
    cat(sprintf("  tile manifest: %d rows\n", nrow(x$tile_manifest)))
  } else if (is.list(x$tile_manifest) && length(x$tile_manifest)) {
    cat(sprintf("  tile manifests: %d\n", length(x$tile_manifest)))
  }
  invisible(x)
}
