#' Open a multi-image pathology project viewer
#'
#' `wsi_viewer_project()` creates a lightweight interactive viewer with a
#' Project section in the left annotation panel. It is intended for workflows
#' where a case contains multiple images or multi-scene microscopy files, such
#' as CZI. The function keeps large source files out of R memory and uses only
#' preview images in the browser. Format-specific high-resolution access remains
#' delegated to optional runtime backends.
#' Browser annotations are stored separately for each project image/section, so
#' ROIs drawn on one tissue section do not appear on another section.
#'
#' CZI first visualization uses the direct native libCZI/libCZIAPI bridge when
#' available, but only after giving OpenSlide and libvips first refusal for CZI
#' variants they can already read. The older `aicspylibczi` bridge is used only
#' if the user explicitly sets `WSITOOLS_CZI_ALLOW_PYTHON=true`.
#' Bio-Formats command-line tools are used for metadata/conversion workflows.
#' For first visualization, wsiTools can also use a small optional Java helper
#' around Bio-Formats `ImageReader` region reads; it does not call `bfconvert`
#' automatically because batch conversion is too slow for interactive opening.
#'
#' @param images Character vector of image paths, `wsi_slide` objects, or a
#'   data frame/list of project image records.
#' @param output HTML file to write. When `NULL`, a temporary HTML file is used.
#' @param open Open the generated HTML file in a browser.
#' @param width Preview width used for openable files.
#'   Larger values provide sharper zoomable previews, at the cost of a larger
#'   self-contained HTML file. This is still a downsampled preview and does not
#'   load the full source image into R.
#' @param height Optional preview height.
#' @param title Viewer title.
#' @param overwrite Overwrite `output` if it already exists.
#' @param roi_class_presets ROI class presets used by the annotation UI.
#' @param czi_sections For CZI files, show detected scenes/sections separately
#'   in the Project panel. Set to `FALSE` to preview the whole CZI bounding box
#'   as a single image when the backend supports it.
#'
#' @return Path to the generated HTML file, invisibly when opened.
#' @export
#' @examples
#' \dontrun{
#' wsi_viewer_project(c("section_1.czi", "section_2.czi"))
#' }
wsi_viewer_project <- function(images, output = NULL, open = interactive(),
                               width = 1600, height = NULL,
                               title = "wsiTools project viewer",
                               overwrite = FALSE,
                               roi_class_presets = wsi_roi_class_presets(),
                               czi_sections = TRUE) {
  width <- as.integer(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  if (!is.null(height)) {
    height <- as.integer(wsi_check_scalar_number(height, "height", allow_zero = FALSE))
  }
  if (missing(images) || length(images) == 0L) {
    wsi_abort("`images` must contain at least one image path or slide object.")
  }

  if (is.null(output)) {
    output <- tempfile(fileext = ".html")
    overwrite <- TRUE
  }
  output <- wsi_validate_output_path(output, overwrite = overwrite)

  roi_class_presets <- wsi_normalize_roi_class_presets(roi_class_presets)
  items <- wsi_viewer_project_items(images, width = width, height = height, czi_sections = czi_sections)
  first <- items[[1L]]
  preview_uri <- first$image_data_uri %||%
    wsi_viewer_placeholder_data_uri(
      label = first$label %||% "Project image",
      message = first$message %||% "Preview unavailable",
      width = width,
      height = height %||% max(1L, round(width * 0.7))
    )
  first_width <- as.numeric(first$width %||% width)
  first_height <- as.numeric(first$height %||% (height %||% max(1L, round(width * 0.7))))
  items[[1L]]$active <- TRUE

  config <- list(
    title = title,
    subtitle = sprintf("%s image%s in project | preview mode", length(items), if (length(items) == 1L) "" else "s"),
    viewer_mode = "project",
    preference_key = "wsiTools.viewer.preferences.v1",
    slide_width = first_width,
    slide_height = first_height,
    mpp = NULL,
    image_data_uri = preview_uri,
    navigator_image_data_uri = preview_uri,
    annotation_filename = paste0(tools::file_path_sans_ext(basename(output)), "_annotations.geojson"),
    roi_class_presets = wsi_viewer_class_presets_payload(roi_class_presets),
    segmentation_run_url = NULL,
    viewer_state_url = NULL,
    autosave_enabled = FALSE,
    autosave_interval_ms = 5000L,
    autosave_path = NULL,
    stain = list(enabled = FALSE, label = "none", channels = list(), basis = list()),
    project = list(items = items, active_index = 0L),
    rois = list(),
    layers = list()
  )

  writeLines(wsi_viewer_html(config), output, useBytes = TRUE)
  if (isTRUE(open)) {
    utils::browseURL(wsi_file_url(output))
    return(invisible(output))
  }
  output
}

#' Open CZI files as a live full-resolution sectioned project viewer
#'
#' `wsi_viewer_czi_project_live()` is the full-resolution companion to
#' [wsi_viewer_project()] for Zeiss CZI files. It starts a local `httpuv`
#' bridge and serves each detected CZI scene/section as OpenSeadragon tiles
#' generated on demand by the optional native CZI backend. Only requested tiles
#' are read, so the complete CZI image is not loaded into R memory.
#'
#' The returned `wsi_viewer_session` must stay alive for the browser to request
#' tiles. Use `wait = TRUE` for a simple blocking live viewer, or keep the
#' returned session object in R and call [wsi_viewer_service()] from long-running
#' scripts when your R front-end does not service `httpuv` automatically.
#'
#' @param images Character vector of CZI file paths.
#' @param output HTML file to write. When `NULL`, a temporary HTML file is used.
#' @param open Open the generated HTML file in a browser.
#' @param width Preview width for the navigator/Project panel thumbnails.
#' @param title Viewer title.
#' @param overwrite Overwrite `output` if it already exists.
#' @param tile_size,tile_overlap OpenSeadragon tile size and overlap.
#' @param tile_format Dynamic tile format.
#' @param channel Zero-based CZI channel shown as the RGB base image.
#' @param sections Show detected CZI scenes/sections separately in the Project
#'   panel. Set to `FALSE` to expose the whole CZI bounding box as one tiled
#'   image.
#' @param host,port,path,max_tries Local HTTP/WebSocket bridge address.
#' @param transport Live browser-to-R transport.
#' @param tile_path HTTP route used for dynamic tiles.
#' @param cache_dir Optional dynamic tile cache directory.
#' @param name,envir Live state name and environment.
#' @param wait If `TRUE`, service the live bridge until interrupted.
#' @param roi_class_presets ROI class presets used by the annotation UI.
#'
#' @return A `wsi_viewer_session` object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' session <- wsi_viewer_czi_project_live(c("scene_1.czi", "scene_2.czi"))
#' }
wsi_viewer_czi_project_live <- function(images, output = NULL, open = interactive(),
                                        width = 1024,
                                        title = "wsiTools CZI full-resolution project viewer",
                                        overwrite = FALSE,
                                        tile_size = 512,
                                        tile_overlap = 1,
                                        tile_format = c("jpg", "png", "jpeg"),
                                        channel = 0,
                                        sections = TRUE,
                                        host = "127.0.0.1",
                                        port = 8798,
                                        path = "/viewer-state",
                                        max_tries = 20L,
                                        transport = c("auto", "websocket", "polling"),
                                        tile_path = "/tiles",
                                        cache_dir = NULL,
                                        name = "wsi_czi_project_live_state",
                                        envir = parent.frame(),
                                        wait = interactive(),
                                        roi_class_presets = wsi_roi_class_presets()) {
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    wsi_abort(
      "Full-resolution live CZI project viewing requires the optional package `httpuv`.",
      class = "wsi_missing_dependency"
    )
  }
  if (!wsi_has_native_czi()) {
    wsi_abort(
      paste(
        "Full-resolution CZI project viewing requires the optional native CZI backend.",
        "Run `wsi_install_native_czi(accept_license = TRUE, ask = FALSE, persist = TRUE)`, then restart R if needed.",
        sep = "\n"
      ),
      class = "wsi_backend_unavailable"
    )
  }
  if (missing(images) || !length(images)) {
    wsi_abort("`images` must contain at least one CZI file path.")
  }
  if (!is.character(images) || anyNA(images) || !all(nzchar(images))) {
    wsi_abort("`images` must be a character vector of CZI file paths.")
  }
  paths <- vapply(images, wsi_validate_input_path, character(1))
  non_czi <- paths[tolower(tools::file_ext(paths)) != "czi"]
  if (length(non_czi)) {
    wsi_abort(sprintf(
      "`wsi_viewer_czi_project_live()` currently accepts CZI files only. Non-CZI path: %s",
      non_czi[[1L]]
    ))
  }
  if (is.null(output)) {
    output <- tempfile(fileext = ".html")
    overwrite <- TRUE
  }
  output <- wsi_validate_output_path(output, overwrite = overwrite)
  width <- as.integer(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  tile_overlap <- as.integer(wsi_check_scalar_number(tile_overlap, "tile_overlap"))
  if (tile_overlap >= tile_size) {
    wsi_abort("`tile_overlap` must be smaller than `tile_size`.")
  }
  tile_format <- wsi_dynamic_tile_format(tile_format)
  channel <- as.integer(wsi_check_scalar_number(channel, "channel", allow_zero = TRUE))
  if (!is.logical(sections) || length(sections) != 1L || is.na(sections)) {
    wsi_abort("`sections` must be `TRUE` or `FALSE`.")
  }
  transport <- match.arg(transport)
  tile_path <- wsi_dynamic_tile_route(tile_path)
  roi_class_presets <- wsi_normalize_roi_class_presets(roi_class_presets)

  state <- wsi_new_viewer_state(name = name, envir = envir)
  shared_cache <- wsi_dynamic_tile_cache_dir(cache_dir)
  dynamic_sources <- list()

  items <- lapply(seq_along(paths), function(i) {
    built <- wsi_czi_live_project_item(
      paths[[i]],
      index = i,
      width = width,
      tile_size = tile_size,
      tile_overlap = tile_overlap,
      tile_format = tile_format,
      channel = channel,
      sections = sections,
      cache_dir = shared_cache,
      route = tile_path
    )
    dynamic_sources <<- c(dynamic_sources, built$sources)
    built$item
  })
  if (!length(items) || !length(dynamic_sources)) {
    wsi_abort("No full-resolution CZI sections could be prepared for viewing.")
  }
  names(dynamic_sources) <- vapply(dynamic_sources, function(source) source$id, character(1))

  bridge <- wsi_start_viewer_state_server(
    state = state,
    slide = NULL,
    host = host,
    port = port,
    path = path,
    max_tries = max_tries,
    tile_sources = dynamic_sources,
    tile_path = tile_path
  )
  session_ready <- FALSE
  on.exit({
    if (!isTRUE(session_ready)) {
      try(httpuv::stopServer(bridge$server), silent = TRUE)
      lapply(dynamic_sources, wsi_dynamic_tile_cleanup)
    }
  }, add = TRUE)

  base_url <- sprintf("http://%s:%d", bridge$host, bridge$port)
  tile_metadata <- lapply(dynamic_sources, wsi_dynamic_tile_metadata, base_url = base_url)
  names(tile_metadata) <- names(dynamic_sources)
  items <- wsi_czi_live_apply_tile_metadata(items, tile_metadata)
  items[[1L]]$active <- TRUE
  first_section <- (items[[1L]]$sections %||% list())[[1L]]
  if (is.null(first_section)) {
    wsi_abort("The first CZI image did not provide an openable section.")
  }
  state$tile_sources <- tile_metadata

  config <- list(
    title = title,
    subtitle = sprintf(
      "%d CZI file%s | full-resolution dynamic tiles | %s",
      length(items),
      if (length(items) == 1L) "" else "s",
      if (isTRUE(sections)) "sectioned scenes" else "whole CZI bounding boxes"
    ),
    viewer_mode = "tiles",
    preference_key = "wsiTools.viewer.preferences.v1",
    slide_width = first_section$width,
    slide_height = first_section$height,
    mpp = NULL,
    image_data_uri = first_section$image_data_uri %||% first_section$navigator_image_data_uri %||% NULL,
    navigator_image_data_uri = first_section$navigator_image_data_uri %||% first_section$image_data_uri %||% NULL,
    tile_size = first_section$tile_size,
    tile_format = first_section$tile_format,
    tile_overlap = first_section$tile_overlap,
    tile_url_base = first_section$tile_url_base,
    tile_url_template = first_section$tile_url_template,
    tile_url_style = first_section$tile_url_style,
    max_level = first_section$max_level,
    min_level = first_section$min_level %||% 0L,
    tile_prefetch_margin = 1L,
    tile_sources = tile_metadata,
    tile_source_label = "dynamic CZI tile server",
    annotation_filename = paste0(tools::file_path_sans_ext(basename(output)), "_annotations.geojson"),
    roi_class_presets = wsi_viewer_class_presets_payload(roi_class_presets),
    segmentation_run_url = NULL,
    viewer_state_url = bridge$url,
    viewer_state_ws_url = if (identical(transport, "polling")) NULL else bridge$ws_url,
    viewer_transport = transport,
    autosave_enabled = FALSE,
    autosave_interval_ms = 5000L,
    autosave_path = NULL,
    stain = list(enabled = FALSE, label = "none", channels = list(), basis = list()),
    base_layer = list(id = "base_image", name = "CZI RGB", visible = TRUE, opacity = 1),
    project = list(items = items, active_index = 0L),
    rois = list(),
    layers = list(),
    channel_sources = list()
  )

  writeLines(wsi_tiled_viewer_html(config), output, useBytes = TRUE)
  if (isTRUE(open)) {
    utils::browseURL(wsi_file_url(output))
  }

  session <- structure(
    c(
      bridge,
      list(
        state = state,
        slide = NULL,
        html = output,
        name = name,
        envir = envir,
        stardist_server = NULL,
        transport = transport,
        dynamic_tile_source = NULL,
        dynamic_tile_sources = dynamic_sources,
        dynamic_tile_cache_dir = shared_cache,
        dynamic_channel_cache_dirs = character()
      )
    ),
    class = "wsi_viewer_session"
  )
  session <- wsi_attach_viewer_session_methods(session)
  session_ready <- TRUE

  message("wsiTools full-resolution CZI project viewer written to ", output)
  message("Live tile server listening at ", paste0("http://", bridge$host, ":", bridge$port, tile_path))
  if (identical(transport, "polling")) {
    message("WebSocket sync disabled; HTTP polling is active.")
  } else {
    message("WebSocket sync available at ", bridge$ws_url, " with HTTP polling fallback.")
  }
  message("Keep this R session open while using the viewer; stop with `wsi_viewer_stop(session)`.")

  if (isTRUE(wait)) {
    message("Press Ctrl+C or Esc to stop the live tile server and return to R.")
    on.exit({
      try(httpuv::stopServer(bridge$server), silent = TRUE)
      lapply(dynamic_sources, wsi_dynamic_tile_cleanup)
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

wsi_czi_live_project_item <- function(path, index = 1L, width = 1024,
                                      tile_size = 512, tile_overlap = 1,
                                      tile_format = "jpg", channel = 0,
                                      sections = TRUE, cache_dir = NULL,
                                      route = "/tiles") {
  info <- wsi_native_czi_info(path)
  preview <- tryCatch(
    wsi_native_czi_project_preview(path, width = width, sections = sections),
    error = function(err) NULL
  )
  section_rows <- wsi_czi_live_section_rows(info, sections = sections)
  scene_meta <- wsi_native_czi_scene_metadata(info$metadata_xml %||% NA_character_)
  previews <- preview$sections %||% list()
  pyramid_factors <- wsi_czi_pyramid_factors(info$pyramid_json %||% NA_character_)

  item_sections <- vector("list", nrow(section_rows))
  sources <- vector("list", nrow(section_rows))
  for (i in seq_len(nrow(section_rows))) {
    scene_row <- section_rows[i, , drop = FALSE]
    scene_index <- as.integer(scene_row$scene[[1L]])
    label <- wsi_czi_live_section_label(scene_row, scene_meta, i)
    source_id <- sprintf("czi_%d_scene_%s", index, scene_index)
    source <- wsi_dynamic_czi_section_tile_source(
      path,
      section = scene_row,
      source_id = source_id,
      name = label,
      tile_size = tile_size,
      tile_overlap = tile_overlap,
      format = tile_format,
      cache_dir = cache_dir,
      route = route,
      channel = channel,
      pyramid_factors = pyramid_factors,
      metadata = list(
        source_path = path,
        scene = scene_index,
        channel = channel,
        backend = "native_czi"
      )
    )
    preview_section <- wsi_czi_live_match_preview(previews, scene_index, i)
    placeholder <- wsi_viewer_placeholder_data_uri(
      label = label,
      message = "Full-resolution CZI tiles are served live from R.",
      width = min(width, 1200L),
      height = max(240L, round(min(width, 1200L) * as.numeric(scene_row$height[[1L]]) / max(1, as.numeric(scene_row$width[[1L]]))))
    )
    item_sections[[i]] <- list(
      id = sprintf("scene_%s", scene_index),
      label = label,
      scene = scene_index,
      x = as.numeric(scene_row$x[[1L]]),
      y = as.numeric(scene_row$y[[1L]]),
      width = as.numeric(scene_row$width[[1L]]),
      height = as.numeric(scene_row$height[[1L]]),
      status = "full-resolution dynamic tiles",
      message = "Tiles are generated from native CZI region reads and cached on demand.",
      image_data_uri = preview_section$image_data_uri %||% placeholder,
      navigator_image_data_uri = preview_section$navigator_image_data_uri %||% preview_section$image_data_uri %||% placeholder,
      content_bbox = preview_section$content_bbox %||% NULL,
      tile_source_id = source$id
    )
    sources[[i]] <- source
  }
  first <- item_sections[[1L]]
  item <- list(
    id = sprintf("project_czi_%d", index),
    label = basename(path),
    path = path,
    backend = "native_czi",
    type = "czi",
    width = first$width,
    height = first$height,
    status = "full-resolution tiled",
    message = "CZI scenes are shown as live OpenSeadragon tile sources.",
    image_data_uri = first$image_data_uri,
    navigator_image_data_uri = first$navigator_image_data_uri,
    sections = item_sections,
    active = FALSE
  )
  list(item = item, sources = sources)
}

wsi_czi_live_section_rows <- function(info, sections = TRUE) {
  scenes <- info$scenes
  if (isTRUE(sections) && is.data.frame(scenes) && nrow(scenes)) {
    scenes <- scenes[order(scenes$scene), , drop = FALSE]
    return(scenes[, c("scene", "x", "y", "width", "height"), drop = FALSE])
  }
  data.frame(
    scene = 0L,
    x = as.numeric(info$x %||% 0),
    y = as.numeric(info$y %||% 0),
    width = as.numeric(info$width),
    height = as.numeric(info$height),
    stringsAsFactors = FALSE
  )
}

wsi_czi_live_section_label <- function(scene_row, scene_meta, index = 1L) {
  scene_index <- as.integer(scene_row$scene[[1L]])
  name <- scene_meta$name[match(scene_index, scene_meta$scene)] %||% NA_character_
  if (is.na(name) || !nzchar(name)) {
    name <- sprintf("Scene %s", scene_index)
  }
  sprintf(
    "%s: %s x %s px",
    name,
    format(as.numeric(scene_row$width[[1L]]), trim = TRUE, scientific = FALSE),
    format(as.numeric(scene_row$height[[1L]]), trim = TRUE, scientific = FALSE)
  )
}

wsi_czi_live_match_preview <- function(previews, scene_index, index = 1L) {
  if (!length(previews)) {
    return(list())
  }
  scenes <- vapply(previews, function(x) as.integer(x$scene %||% NA_integer_), integer(1))
  hit <- which(scenes == scene_index)[1L]
  if (!is.na(hit)) {
    return(previews[[hit]])
  }
  if (index <= length(previews)) {
    return(previews[[index]])
  }
  list()
}

wsi_czi_live_apply_tile_metadata <- function(items, tile_metadata) {
  lapply(items, function(item) {
    sections <- item$sections %||% list()
    sections <- lapply(sections, function(section) {
      source_id <- section$tile_source_id %||% ""
      metadata <- tile_metadata[[source_id]]
      if (!is.null(metadata)) {
        for (field in c(
          "tile_size", "tile_format", "tile_url_base", "tile_url_template",
          "tile_url_style", "tile_overlap", "min_level", "max_level",
          "cache_key", "kind"
        )) {
          section[[field]] <- metadata[[field]]
        }
      }
      section
    })
    item$sections <- sections
    first <- sections[[1L]] %||% NULL
    if (!is.null(first)) {
      for (field in c(
        "tile_size", "tile_format", "tile_url_base", "tile_url_template",
        "tile_url_style", "tile_overlap", "min_level", "max_level"
      )) {
        item[[field]] <- first[[field]]
      }
    }
    item
  })
}

wsi_viewer_project_config <- function(slide, project_images = NULL, width = 768, height = NULL) {
  wsi_check_slide(slide)
  active <- wsi_viewer_project_item_from_slide(
    slide,
    width = width,
    height = height,
    include_preview = FALSE,
    active = TRUE
  )
  items <- list(active)
  if (!is.null(project_images) && length(project_images) > 0L) {
    items <- c(items, wsi_viewer_project_items(project_images, width = width, height = height))
  }
  list(items = items, active_index = 0L)
}

wsi_viewer_project_items <- function(images, width = 768, height = NULL, czi_sections = TRUE) {
  if (inherits(images, "wsi_slide")) {
    return(list(wsi_viewer_project_item_from_slide(images, width = width, height = height)))
  }
  if (is.data.frame(images)) {
    rows <- split(images, seq_len(nrow(images)))
    return(lapply(seq_along(rows), function(i) {
      wsi_viewer_project_item_from_record(as.list(rows[[i]]), index = i, width = width, height = height, czi_sections = czi_sections)
    }))
  }
  if (is.character(images)) {
    return(lapply(seq_along(images), function(i) {
      wsi_viewer_project_item_from_path(images[[i]], index = i, width = width, height = height, czi_sections = czi_sections)
    }))
  }
  if (is.list(images)) {
    return(lapply(seq_along(images), function(i) {
      item <- images[[i]]
      if (inherits(item, "wsi_slide")) {
        wsi_viewer_project_item_from_slide(item, width = width, height = height)
      } else if (is.character(item) && length(item) == 1L) {
        wsi_viewer_project_item_from_path(item, index = i, width = width, height = height, czi_sections = czi_sections)
      } else if (is.list(item)) {
        wsi_viewer_project_item_from_record(item, index = i, width = width, height = height, czi_sections = czi_sections)
      } else {
        wsi_abort("Unsupported `images` entry. Use file paths, `wsi_slide` objects, or project record lists.")
      }
    }))
  }
  wsi_abort("`images` must be a character vector, data frame, list, or `wsi_slide` object.")
}

wsi_viewer_project_item_from_record <- function(record, index = 1L, width = 768, height = NULL, czi_sections = TRUE) {
  record_path <- as.character(record$path %||% "")
  if (length(record_path) != 1L || is.na(record_path)) {
    record_path <- ""
  }
  if (nzchar(record_path)) {
    item <- wsi_viewer_project_item_from_path(record_path, index = index, width = width, height = height, czi_sections = czi_sections)
  } else {
    label <- as.character(record$label %||% record$name %||% sprintf("Image %d", index))
    item <- list(
      id = as.character(record$id %||% sprintf("project_image_%d", index)),
      label = label,
      path = as.character(record$path %||% ""),
      backend = as.character(record$backend %||% "record"),
      type = as.character(record$type %||% "image"),
      width = as.numeric(record$width %||% width),
      height = as.numeric(record$height %||% (height %||% max(1L, round(width * 0.7)))),
      status = as.character(record$status %||% "ready"),
      message = as.character(record$message %||% ""),
      image_data_uri = record$image_data_uri %||% NULL,
      navigator_image_data_uri = record$navigator_image_data_uri %||% record$image_data_uri %||% NULL,
      sections = record$sections %||% list(),
      active = isTRUE(record$active)
    )
  }

  for (field in intersect(names(record), names(item))) {
    if (!identical(field, "sections") && !identical(field, "image_data_uri") &&
        !identical(field, "navigator_image_data_uri") && !is.null(record[[field]])) {
      item[[field]] <- record[[field]]
    }
  }
  item
}

wsi_viewer_project_item_from_slide <- function(slide, width = 768, height = NULL,
                                               include_preview = TRUE, active = FALSE) {
  wsi_check_slide(slide)
  label <- basename(slide$path %||% "")
  if (!nzchar(label) || is.na(label)) {
    label <- sprintf("%s slide", slide$backend)
  }
  preview <- NULL
  if (isTRUE(include_preview)) {
    preview <- tryCatch(
      wsi_viewer_thumbnail_data_uri(slide, width = width, height = height),
      error = function(err) NULL
    )
  }
  list(
    id = paste0("project_slide_", wsi_project_id(label)),
    label = label,
    path = slide$path %||% "",
    backend = slide$backend,
    type = "slide",
    width = unname(slide$dimensions[["width"]]),
    height = unname(slide$dimensions[["height"]]),
    status = if (isTRUE(active)) "active" else if (is.null(preview)) "metadata only" else "ready",
    message = if (is.null(preview) && !isTRUE(active)) "Preview could not be generated with the available backends." else "",
    image_data_uri = preview,
    navigator_image_data_uri = preview,
    sections = wsi_viewer_project_sections_from_slide(slide),
    active = isTRUE(active)
  )
}

wsi_viewer_project_item_from_path <- function(path, index = 1L, width = 768, height = NULL, czi_sections = TRUE) {
  path <- wsi_validate_input_path(path)
  ext <- tolower(tools::file_ext(path))
  label <- basename(path)

  if (identical(ext, "czi")) {
    return(wsi_viewer_project_item_from_czi(path, index = index, width = width, height = height, czi_sections = czi_sections))
  }

  slide <- tryCatch(wsi_open(path), error = function(err) err)
  if (inherits(slide, "wsi_slide")) {
    on.exit(wsi_close(slide), add = TRUE)
    return(wsi_viewer_project_item_from_slide(slide, width = width, height = height))
  }

  message <- conditionMessage(slide)
  list(
    id = sprintf("project_image_%d", index),
    label = label,
    path = path,
    backend = "unavailable",
    type = ext %||% "image",
    width = width,
    height = height %||% max(1L, round(width * 0.7)),
    status = "preview unavailable",
    message = message,
    image_data_uri = wsi_viewer_placeholder_data_uri(label, message, width = width, height = height %||% max(1L, round(width * 0.7))),
    navigator_image_data_uri = NULL,
    sections = list(),
    active = FALSE
  )
}

wsi_viewer_project_item_from_czi <- function(path, index = 1L, width = 768, height = NULL, czi_sections = TRUE) {
  fast_slide <- if (isTRUE(czi_sections)) NULL else wsi_czi_fast_slide(path)
  if (inherits(fast_slide, "wsi_slide")) {
    on.exit(wsi_close(fast_slide), add = TRUE)
    item <- wsi_viewer_project_item_from_slide(fast_slide, width = width, height = height)
    item$id <- sprintf("project_czi_%d", index)
    item$type <- "czi"
    item$status <- "ready"
    item$message <- sprintf(
      "CZI opened with %s; using the fast tiled backend instead of CZI-specific preview fallbacks.",
      item$backend
    )
    return(item)
  }

  czi_preview <- wsi_czi_project_preview(path, width = width, height = height, sections = czi_sections)
  if (!is.null(czi_preview)) {
    first <- czi_preview$sections[[1L]]
    backend <- czi_preview$backend %||% "aicspylibczi"
    return(list(
      id = sprintf("project_czi_%d", index),
      label = basename(path),
      path = path,
      backend = backend,
      type = "czi",
      width = first$width,
      height = first$height,
      status = "ready",
      message = sprintf("CZI preview generated with optional %s; full-resolution pixels remain on disk.", backend),
      image_data_uri = first$image_data_uri,
      navigator_image_data_uri = first$image_data_uri,
      sections = czi_preview$sections,
      active = FALSE
    ))
  }

  bioformats_available <- wsi_has_bioformats()
  label <- basename(path)
  message <- if (bioformats_available) {
    paste(
      "CZI detected. Bio-Formats is available for metadata, but first visualization now requires a tile/region reader.",
      "Install ZEISS libCZI/libCZIAPI and set `WSITOOLS_LIBCZIAPI` if needed.",
      "Alternatively set `WSITOOLS_BIOFORMATS_JAR` to use the Bio-Formats Java ImageReader helper.",
      "wsiTools no longer runs `bfconvert` automatically for first visualization and will not use Python unless `WSITOOLS_CZI_ALLOW_PYTHON=true`.",
      sep = "\n"
    )
  } else {
    paste(
      "CZI detected. Install ZEISS libCZI/libCZIAPI for first visualization and set `WSITOOLS_LIBCZIAPI` if needed.",
      "Alternatively install Java plus `bioformats_package.jar` and set `WSITOOLS_BIOFORMATS_JAR`.",
      "Bio-Formats command-line tools (`showinf`/`bfconvert`) are still useful for metadata and conversion.",
      "wsiTools will not use Python unless `WSITOOLS_CZI_ALLOW_PYTHON=true`.",
      sep = "\n"
    )
  }
  status <- if (bioformats_available) "CZI preview backend required" else "CZI backend required"
  list(
    id = sprintf("project_czi_%d", index),
    label = label,
    path = path,
    backend = "bioformats",
    type = "czi",
    width = width,
    height = height %||% max(1L, round(width * 0.7)),
    status = status,
    message = message,
    image_data_uri = wsi_viewer_placeholder_data_uri(label, message, width = width, height = height %||% max(1L, round(width * 0.7))),
    navigator_image_data_uri = NULL,
    sections = list(list(
      id = "czi_series_pending",
      label = "CZI scenes/sections unavailable until a tile/region preview backend is configured",
      status = status,
      message = message
    )),
    active = FALSE
  )
}

wsi_czi_python_project_preview <- function(path, width = 768) {
  if (!wsi_has_czi_python()) {
    wsi_abort(
      "CZI preview generation requires Python with `aicspylibczi`, `numpy`, and `Pillow`. Set `WSITOOLS_CZI_PYTHON` to that Python executable.",
      class = "wsi_backend_unavailable"
    )
  }
  python <- wsi_czi_python_command()
  preview_width <- wsi_czi_initial_preview_width(width)
  output_dir <- tempfile("wsi_czi_preview_")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  metadata <- file.path(output_dir, "preview.json")
  script_file <- tempfile("wsi_czi_preview_", fileext = ".py")
  writeLines(wsi_czi_python_preview_script(), script_file, useBytes = TRUE)
  on.exit(unlink(script_file, force = TRUE), add = TRUE)
  args <- c(
    script_file,
    path,
    output_dir,
    as.character(as.integer(preview_width)),
    metadata
  )
  out <- tryCatch(
    suppressWarnings(system2(python, args = args, stdout = TRUE, stderr = TRUE)),
    error = function(err) {
      wsi_abort(
        sprintf("Python CZI preview generation failed before completion: %s", conditionMessage(err)),
        class = "wsi_backend_error"
      )
    }
  )
  status <- attr(out, "status", exact = TRUE) %||% 0L
  if (!identical(as.integer(status), 0L)) {
    wsi_abort(
      paste0("Python CZI preview generation failed.\n", paste(out, collapse = "\n")),
      class = "wsi_backend_error"
    )
  }
  if (!file.exists(metadata)) {
    wsi_abort("Python CZI preview generation did not produce metadata.", class = "wsi_backend_error")
  }
  info <- jsonlite::fromJSON(metadata, simplifyVector = FALSE)
  sections <- lapply(info$sections %||% list(), function(section) {
    image_path <- section$preview_path %||% ""
    if (!file.exists(image_path)) {
      wsi_abort(sprintf("CZI preview file was not created: %s", image_path), class = "wsi_backend_error")
    }
    list(
      id = section$id %||% sprintf("scene_%s", section$scene %||% 0),
      label = section$label %||% sprintf("Scene %s", section$scene %||% 0),
      scene = as.integer(section$scene %||% 0),
      width = as.numeric(section$width %||% width),
      height = as.numeric(section$height %||% width),
      preview_width = as.numeric(section$preview_width %||% width),
      preview_height = as.numeric(section$preview_height %||% width),
      x = as.numeric(section$x %||% 0),
      y = as.numeric(section$y %||% 0),
      status = "preview",
      message = "Scene preview from CZI mosaic",
      image_data_uri = wsi_image_data_uri(image_path, mime = "image/png"),
      navigator_image_data_uri = wsi_image_data_uri(image_path, mime = "image/png")
    )
  })
  if (!length(sections)) {
    wsi_abort("No CZI scenes could be previewed.", class = "wsi_backend_error")
  }
  list(path = path, sections = sections, output_dir = output_dir)
}

wsi_czi_python_preview_script <- function() {
  c(
    "import json, sys, os",
    "from aicspylibczi import CziFile",
    "import numpy as np",
    "from PIL import Image",
    "",
    "path, out_dir, target, metadata = sys.argv[1], sys.argv[2], int(sys.argv[3]), sys.argv[4]",
    "os.makedirs(out_dir, exist_ok=True)",
    "czi = CziFile(path)",
    "",
    "def bbox_tuple(b):",
    "    return (int(b.x), int(b.y), int(b.w), int(b.h))",
    "",
    "if czi.is_mosaic():",
    "    boxes = czi.get_all_mosaic_scene_bounding_boxes()",
    "else:",
    "    boxes = czi.get_all_scene_bounding_boxes()",
    "",
    "def as_uint8(arr):",
    "    arr = np.squeeze(arr)",
    "    if arr.ndim == 2:",
    "        arr = np.stack([arr, arr, arr], axis=-1)",
    "    if arr.ndim == 3 and arr.shape[0] in (3, 4):",
    "        arr = np.moveaxis(arr, 0, -1)",
    "    if arr.ndim == 3 and arr.shape[-1] > 3:",
    "        arr = arr[..., :3]",
    "    if arr.dtype != np.uint8:",
    "        high = np.percentile(arr, 99.5) if arr.size else 0",
    "        if high <= 0:",
    "            high = float(arr.max()) if arr.size else 1.0",
    "        arr = np.clip(arr.astype('float32') / max(high, 1.0) * 255.0, 0, 255).astype('uint8')",
    "    return arr",
    "",
    "sections = []",
    "for scene, bbox in sorted(boxes.items()):",
    "    x, y, w, h = bbox_tuple(bbox)",
    "    scale = min(1.0, float(target) / max(float(w), float(h), 1.0))",
    "    if czi.is_mosaic():",
    "        arr = czi.read_mosaic(C=0, region=(x, y, w, h), scale_factor=scale)",
    "    else:",
    "        arr = czi.read_image(S=int(scene), C=0)",
    "    arr = as_uint8(arr)",
    "    preview_path = os.path.join(out_dir, f'scene_{int(scene)}.png')",
    "    Image.fromarray(arr).save(preview_path)",
    "    sections.append({",
    "        'id': f'scene_{int(scene)}',",
    "        'scene': int(scene),",
    "        'label': f'Scene {int(scene)}: {w} x {h} px',",
    "        'x': x, 'y': y, 'width': w, 'height': h,",
    "        'preview_width': int(arr.shape[1]),",
    "        'preview_height': int(arr.shape[0]),",
    "        'preview_path': preview_path",
    "    })",
    "",
    "with open(metadata, 'w', encoding='utf-8') as handle:",
    "    json.dump({'path': path, 'sections': sections}, handle)"
  )
}

wsi_viewer_project_sections_from_slide <- function(slide) {
  levels <- wsi_levels(slide)
  lapply(seq_len(nrow(levels)), function(i) {
    row <- levels[i, , drop = FALSE]
    list(
      id = sprintf("level_%s", row$level[[1L]]),
      label = sprintf("Level %s: %s x %s px, %.3gx downsample",
                      row$level[[1L]], row$width[[1L]], row$height[[1L]], row$downsample[[1L]]),
      level = row$level[[1L]],
      width = row$width[[1L]],
      height = row$height[[1L]],
      downsample = row$downsample[[1L]],
      status = "pyramid level"
    )
  })
}

wsi_project_id <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", as.character(x)))
  x <- gsub("^_+|_+$", "", x)
  if (length(x) != 1L || is.na(x)) {
    return("image")
  }
  if (!nzchar(x)) "image" else x
}

wsi_viewer_placeholder_data_uri <- function(label, message = "", width = 1200, height = 800) {
  label <- wsi_html_escape(label)
  message <- wsi_html_escape(message)
  width <- as.integer(width)
  height <- as.integer(height)
  svg <- sprintf(
    paste0(
      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">",
      "<rect width=\"100%%\" height=\"100%%\" fill=\"#151515\"/>",
      "<rect x=\"30\" y=\"30\" width=\"%d\" height=\"%d\" rx=\"8\" fill=\"#202020\" stroke=\"#505050\"/>",
      "<text x=\"50%%\" y=\"44%%\" dominant-baseline=\"middle\" text-anchor=\"middle\" font-family=\"sans-serif\" font-size=\"34\" fill=\"#f2f2f2\">%s</text>",
      "<text x=\"50%%\" y=\"54%%\" dominant-baseline=\"middle\" text-anchor=\"middle\" font-family=\"sans-serif\" font-size=\"18\" fill=\"#b8b8b8\">%s</text>",
      "</svg>"
    ),
    width, height, width, height,
    max(1L, width - 60L), max(1L, height - 60L),
    label, message
  )
  paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(charToRaw(svg)))
}
