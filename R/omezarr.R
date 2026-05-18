wsi_is_omezarr_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) || !dir.exists(path)) {
    return(FALSE)
  }
  file.exists(file.path(path, ".zattrs")) ||
    file.exists(file.path(path, ".zgroup")) ||
    file.exists(file.path(path, "zarr.json")) ||
    grepl("\\.ome\\.zarr$|\\.zarr$", basename(path), ignore.case = TRUE)
}

wsi_read_zarr_json <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

wsi_omezarr_dataset_shape <- function(root, dataset_path = "") {
  dataset_path <- dataset_path %||% ""
  zarray <- wsi_read_zarr_json(file.path(root, dataset_path, ".zarray"))
  if (!is.null(zarray$shape)) {
    return(as.numeric(unlist(zarray$shape, use.names = FALSE)))
  }
  zarr_json <- wsi_read_zarr_json(file.path(root, dataset_path, "zarr.json"))
  if (!is.null(zarr_json$shape)) {
    return(as.numeric(unlist(zarr_json$shape, use.names = FALSE)))
  }
  numeric()
}

wsi_omezarr_dataset_scale <- function(dataset) {
  transforms <- dataset$coordinateTransformations %||% list()
  for (transform in transforms) {
    if (identical(transform$type, "scale") && !is.null(transform$scale)) {
      values <- as.numeric(unlist(transform$scale, use.names = FALSE))
      if (length(values) >= 2L) {
        return(utils::tail(values, 2L))
      }
    }
  }
  c(NA_real_, NA_real_)
}

#' Read OME-Zarr metadata
#'
#' Reads OME-NGFF/Zarr metadata without loading image chunks into memory. The
#' first implementation extracts multiscale dataset paths, dimensions, pyramid
#' levels, and basic attributes from local OME-Zarr directories.
#'
#' @param path Path to an OME-Zarr directory.
#'
#' @return A list with attributes, multiscale metadata, and a levels data frame.
#' @export
omezarr_metadata <- function(path) {
  path <- wsi_validate_input_path(path)
  if (!wsi_is_omezarr_path(path)) {
    wsi_abort(sprintf("Input does not look like an OME-Zarr directory: %s", path))
  }

  attrs <- wsi_read_zarr_json(file.path(path, ".zattrs")) %||% list()
  multiscales <- attrs$multiscales %||% list()
  datasets <- if (length(multiscales) && !is.null(multiscales[[1L]]$datasets)) {
    multiscales[[1L]]$datasets
  } else {
    list(list(path = ""))
  }

  rows <- lapply(seq_along(datasets), function(i) {
    dataset_path <- datasets[[i]]$path %||% ""
    shape <- wsi_omezarr_dataset_shape(path, dataset_path)
    if (length(shape) < 2L) {
      width <- NA_real_
      height <- NA_real_
    } else {
      width <- utils::tail(shape, 1L)
      height <- utils::tail(shape, 2L)[[1L]]
    }
    scale <- wsi_omezarr_dataset_scale(datasets[[i]])
    data.frame(
      level = i - 1L,
      path = dataset_path,
      width = width,
      height = height,
      downsample = if (all(is.finite(scale))) max(scale) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  levels <- do.call(rbind, rows)
  if (nrow(levels) && is.na(levels$downsample[[1L]])) {
    levels$downsample <- levels$width[[1L]] / levels$width
  }
  levels$downsample[!is.finite(levels$downsample)] <- NA_real_
  levels$downsample[[1L]] <- levels$downsample[[1L]] %||% 1
  if (is.na(levels$downsample[[1L]])) {
    levels$downsample[[1L]] <- 1
  }

  list(
    path = path,
    ngff_version = (multiscales[[1L]]$version %||% attrs$ome$version %||% NA_character_),
    axes = multiscales[[1L]]$axes %||% list(),
    multiscales = multiscales,
    attributes = attrs,
    levels = levels
  )
}

#' @rdname omezarr_metadata
#' @export
wsi_omezarr_metadata <- omezarr_metadata

#' Open an OME-Zarr image as a lightweight slide handle
#'
#' The returned object exposes dimensions, pyramid levels, and metadata. Pixel
#' chunk decoding is intentionally not mandatory in this milestone.
#'
#' @param path Path to an OME-Zarr directory.
#'
#' @return A `wsi_slide` object with backend `"omezarr"`.
#' @export
open_omezarr <- function(path) {
  metadata <- omezarr_metadata(path)
  levels <- metadata$levels
  if (!nrow(levels) || is.na(levels$width[[1L]]) || is.na(levels$height[[1L]])) {
    wsi_abort("OME-Zarr dimensions could not be determined from metadata.")
  }
  wsi_make_slide(
    path = metadata$path,
    backend = "omezarr",
    dimensions = c(width = levels$width[[1L]], height = levels$height[[1L]]),
    levels = levels[, c("level", "width", "height", "downsample"), drop = FALSE],
    properties = metadata$attributes,
    metadata = list(vendor = "OME-Zarr", ngff_version = metadata$ngff_version, omezarr = metadata),
    associated_images = character()
  )
}

#' @rdname open_omezarr
#' @export
wsi_open_omezarr <- open_omezarr

wsi_omezarr_placeholder_data_uri <- function(slide, width = 1024) {
  height <- max(1L, as.integer(round(width * slide$dimensions[["height"]] / slide$dimensions[["width"]])))
  label <- sprintf("OME-Zarr metadata view\\n%s x %s px\\n%s levels",
                   format(slide$dimensions[["width"]], scientific = FALSE),
                   format(slide$dimensions[["height"]], scientific = FALSE),
                   nrow(slide$levels))
  svg <- sprintf(
    paste0(
      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">",
      "<rect width=\"100%%\" height=\"100%%\" fill=\"#f3f4f6\"/>",
      "<rect x=\"12\" y=\"12\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"#64748b\" stroke-width=\"3\"/>",
      "<text x=\"50%%\" y=\"46%%\" dominant-baseline=\"middle\" text-anchor=\"middle\" ",
      "font-family=\"sans-serif\" font-size=\"26\" fill=\"#334155\">%s</text>",
      "</svg>"
    ),
    width, height, width, height, width - 24L, height - 24L, wsi_html_escape(label)
  )
  paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(charToRaw(svg)))
}
