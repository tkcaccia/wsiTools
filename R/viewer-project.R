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
#' available. The older `aicspylibczi` bridge is used only if the user
#' explicitly sets `WSITOOLS_CZI_ALLOW_PYTHON=true`.
#' Bio-Formats is used for CZI metadata/conversion workflows, but `bfconvert`
#' is not used automatically for first visualization because it is too slow for
#' interactive opening.
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
                               roi_class_presets = wsi_roi_class_presets()) {
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
  items <- wsi_viewer_project_items(images, width = width, height = height)
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

wsi_viewer_project_items <- function(images, width = 768, height = NULL) {
  if (inherits(images, "wsi_slide")) {
    return(list(wsi_viewer_project_item_from_slide(images, width = width, height = height)))
  }
  if (is.data.frame(images)) {
    rows <- split(images, seq_len(nrow(images)))
    return(lapply(seq_along(rows), function(i) {
      wsi_viewer_project_item_from_record(as.list(rows[[i]]), index = i, width = width, height = height)
    }))
  }
  if (is.character(images)) {
    return(lapply(seq_along(images), function(i) {
      wsi_viewer_project_item_from_path(images[[i]], index = i, width = width, height = height)
    }))
  }
  if (is.list(images)) {
    return(lapply(seq_along(images), function(i) {
      item <- images[[i]]
      if (inherits(item, "wsi_slide")) {
        wsi_viewer_project_item_from_slide(item, width = width, height = height)
      } else if (is.character(item) && length(item) == 1L) {
        wsi_viewer_project_item_from_path(item, index = i, width = width, height = height)
      } else if (is.list(item)) {
        wsi_viewer_project_item_from_record(item, index = i, width = width, height = height)
      } else {
        wsi_abort("Unsupported `images` entry. Use file paths, `wsi_slide` objects, or project record lists.")
      }
    }))
  }
  wsi_abort("`images` must be a character vector, data frame, list, or `wsi_slide` object.")
}

wsi_viewer_project_item_from_record <- function(record, index = 1L, width = 768, height = NULL) {
  record_path <- as.character(record$path %||% "")
  if (length(record_path) != 1L || is.na(record_path)) {
    record_path <- ""
  }
  if (nzchar(record_path)) {
    item <- wsi_viewer_project_item_from_path(record_path, index = index, width = width, height = height)
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

wsi_viewer_project_item_from_path <- function(path, index = 1L, width = 768, height = NULL) {
  path <- wsi_validate_input_path(path)
  ext <- tolower(tools::file_ext(path))
  label <- basename(path)

  if (identical(ext, "czi")) {
    return(wsi_viewer_project_item_from_czi(path, index = index, width = width, height = height))
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

wsi_viewer_project_item_from_czi <- function(path, index = 1L, width = 768, height = NULL) {
  czi_preview <- wsi_czi_project_preview(path, width = width, height = height)
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
      "wsiTools no longer runs `bfconvert` automatically for first visualization and will not use Python unless `WSITOOLS_CZI_ALLOW_PYTHON=true`.",
      sep = "\n"
    )
  } else {
    paste(
      "CZI detected. Install ZEISS libCZI/libCZIAPI for first visualization and set `WSITOOLS_LIBCZIAPI` if needed.",
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
