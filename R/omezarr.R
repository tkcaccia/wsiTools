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

wsi_omezarr_numeric_vector <- function(x) {
  if (is.null(x)) {
    return(numeric())
  }
  suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
}

wsi_omezarr_character_vector <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  as.character(unlist(x, use.names = FALSE))
}

wsi_omezarr_first_character <- function(x, default = NA_character_) {
  values <- wsi_omezarr_character_vector(x)
  if (length(values)) {
    return(values[[1L]])
  }
  default
}

wsi_omezarr_root_metadata <- function(path) {
  zattrs <- wsi_read_zarr_json(file.path(path, ".zattrs")) %||% list()
  zarr_json <- wsi_read_zarr_json(file.path(path, "zarr.json")) %||% list()
  attrs <- zattrs
  if (length(zarr_json$attributes %||% list())) {
    attrs <- utils::modifyList(attrs, zarr_json$attributes)
  }
  ome <- attrs$ome %||% list()
  multiscales <- attrs$multiscales %||% ome$multiscales %||% list()
  list(
    attrs = attrs,
    ome = ome,
    zarr_json = zarr_json,
    multiscales = multiscales
  )
}

wsi_omezarr_dataset_metadata <- function(root, dataset_path = "") {
  dataset_path <- dataset_path %||% ""
  zarray <- wsi_read_zarr_json(file.path(root, dataset_path, ".zarray"))
  zarr_json <- wsi_read_zarr_json(file.path(root, dataset_path, "zarr.json"))
  zattrs <- wsi_read_zarr_json(file.path(root, dataset_path, ".zattrs")) %||% list()

  metadata <- list(
    path = dataset_path,
    shape = numeric(),
    chunks = numeric(),
    dtype = NA_character_,
    dimension_names = character(),
    zarr_format = NA_integer_,
    compressor = NULL,
    codecs = list(),
    fill_value = NULL
  )

  if (!is.null(zarray)) {
    metadata$shape <- wsi_omezarr_numeric_vector(zarray$shape)
    metadata$chunks <- wsi_omezarr_numeric_vector(zarray$chunks)
    metadata$dtype <- wsi_omezarr_first_character(zarray$dtype %||% zarray$data_type)
    metadata$dimension_names <- wsi_omezarr_character_vector(zattrs$dimension_names %||% zattrs[["_ARRAY_DIMENSIONS"]])
    metadata$zarr_format <- as.integer(zarray$zarr_format %||% 2L)
    metadata$compressor <- zarray$compressor
    metadata$fill_value <- zarray$fill_value
  }

  if (!is.null(zarr_json)) {
    array_attrs <- zarr_json$attributes %||% list()
    shape <- wsi_omezarr_numeric_vector(zarr_json$shape)
    if (length(shape)) {
      metadata$shape <- shape
    }
    chunks <- wsi_omezarr_numeric_vector(
      zarr_json$chunk_grid$configuration$chunk_shape %||% zarr_json$chunks
    )
    if (length(chunks)) {
      metadata$chunks <- chunks
    }
    dtype <- wsi_omezarr_character_vector(zarr_json$data_type %||% zarr_json$dtype)
    if (length(dtype)) {
      metadata$dtype <- dtype[[1L]]
    }
    dimension_names <- wsi_omezarr_character_vector(
      zarr_json$dimension_names %||%
        array_attrs$dimension_names %||%
        array_attrs[["_ARRAY_DIMENSIONS"]]
    )
    if (length(dimension_names)) {
      metadata$dimension_names <- dimension_names
    }
    metadata$zarr_format <- as.integer(zarr_json$zarr_format %||% metadata$zarr_format)
    metadata$codecs <- zarr_json$codecs %||% metadata$codecs
    metadata$fill_value <- zarr_json$fill_value %||% metadata$fill_value
  }

  metadata
}

wsi_omezarr_dataset_shape <- function(root, dataset_path = "") {
  wsi_omezarr_dataset_metadata(root, dataset_path)$shape
}

wsi_omezarr_transform_values <- function(dataset, type = c("scale", "translation")) {
  type <- match.arg(type)
  transforms <- dataset$coordinateTransformations %||% list()
  if (!length(transforms)) {
    return(numeric())
  }
  if (!is.null(transforms$type)) {
    transforms <- list(transforms)
  }
  for (transform in transforms) {
    if (identical(transform$type, type) && !is.null(transform[[type]])) {
      return(wsi_omezarr_numeric_vector(transform[[type]]))
    }
  }
  numeric()
}

wsi_omezarr_axis_value <- function(values, index, fallback_position = 1L) {
  if (!length(values)) {
    return(NA_real_)
  }
  if (is.finite(index) && length(values) >= index) {
    return(values[[index]])
  }
  tail_values <- utils::tail(values, 2L)
  if (length(tail_values) >= fallback_position) {
    return(tail_values[[fallback_position]])
  }
  NA_real_
}

wsi_omezarr_infer_axis_names <- function(ndim) {
  if (ndim < 1L) {
    return(character())
  }
  names <- paste0("dim", seq_len(ndim))
  if (ndim >= 2L) {
    names[[ndim - 1L]] <- "y"
    names[[ndim]] <- "x"
  }
  if (ndim >= 3L) {
    names[[ndim - 2L]] <- "c"
  }
  if (ndim >= 4L) {
    names[[ndim - 3L]] <- "z"
  }
  if (ndim >= 5L) {
    names[[ndim - 4L]] <- "t"
  }
  names
}

wsi_omezarr_axis_table <- function(axes = list(), shape = numeric()) {
  if (is.data.frame(axes)) {
    axes <- lapply(seq_len(nrow(axes)), function(i) as.list(axes[i, , drop = FALSE]))
  }
  ndim <- max(length(axes), length(shape), 0L)
  if (ndim < 1L) {
    return(data.frame(
      axis_index = integer(),
      name = character(),
      type = character(),
      unit = character(),
      stringsAsFactors = FALSE
    ))
  }
  inferred <- wsi_omezarr_infer_axis_names(ndim)
  rows <- lapply(seq_len(ndim), function(i) {
    axis <- if (length(axes) >= i) axes[[i]] else list()
    axis <- axis %||% list()
    if (is.character(axis)) {
      axis <- list(name = axis[[1L]])
    }
    data.frame(
      axis_index = i,
      name = as.character(axis$name %||% inferred[[i]])[[1L]],
      type = as.character(axis$type %||% NA_character_)[[1L]],
      unit = as.character(axis$unit %||% NA_character_)[[1L]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

wsi_omezarr_spatial_axes <- function(axis_table, shape = numeric()) {
  ndim <- max(nrow(axis_table), length(shape), 0L)
  x_index <- match("x", tolower(axis_table$name))
  y_index <- match("y", tolower(axis_table$name))
  if (is.na(x_index) && ndim >= 1L) {
    x_index <- ndim
  }
  if (is.na(y_index) && ndim >= 2L) {
    y_index <- ndim - 1L
  }
  c(y = y_index, x = x_index)
}

wsi_omezarr_dataset_row <- function(root, dataset, level, axes) {
  dataset_path <- dataset$path %||% ""
  metadata <- wsi_omezarr_dataset_metadata(root, dataset_path)
  shape <- metadata$shape
  axis_table <- wsi_omezarr_axis_table(axes, shape)
  spatial <- wsi_omezarr_spatial_axes(axis_table, shape)
  x_index <- spatial[["x"]]
  y_index <- spatial[["y"]]
  width <- if (length(shape) >= x_index && is.finite(x_index)) shape[[x_index]] else NA_real_
  height <- if (length(shape) >= y_index && is.finite(y_index)) shape[[y_index]] else NA_real_
  scale <- wsi_omezarr_transform_values(dataset, "scale")
  translation <- wsi_omezarr_transform_values(dataset, "translation")
  scale_x <- wsi_omezarr_axis_value(scale, x_index, fallback_position = 2L)
  scale_y <- wsi_omezarr_axis_value(scale, y_index, fallback_position = 1L)
  translation_x <- wsi_omezarr_axis_value(translation, x_index, fallback_position = 2L)
  translation_y <- wsi_omezarr_axis_value(translation, y_index, fallback_position = 1L)
  unit_x <- if (nrow(axis_table) >= x_index && is.finite(x_index)) axis_table$unit[[x_index]] else NA_character_
  unit_y <- if (nrow(axis_table) >= y_index && is.finite(y_index)) axis_table$unit[[y_index]] else NA_character_
  dimension_names <- metadata$dimension_names
  if (!length(dimension_names) && nrow(axis_table)) {
    dimension_names <- axis_table$name
  }

  row <- data.frame(
    level = level,
    path = dataset_path,
    width = width,
    height = height,
    downsample = NA_real_,
    scale_x = scale_x,
    scale_y = scale_y,
    translation_x = translation_x,
    translation_y = translation_y,
    unit_x = unit_x,
    unit_y = unit_y,
    dtype = metadata$dtype,
    zarr_format = metadata$zarr_format,
    stringsAsFactors = FALSE
  )
  row$shape <- I(list(shape))
  row$chunks <- I(list(metadata$chunks))
  row$dimension_names <- I(list(dimension_names))
  row
}

wsi_omezarr_compute_downsample <- function(levels) {
  if (!nrow(levels)) {
    return(levels)
  }
  base_scale_x <- levels$scale_x[[1L]]
  base_scale_y <- levels$scale_y[[1L]]
  base_width <- levels$width[[1L]]
  base_height <- levels$height[[1L]]
  for (i in seq_len(nrow(levels))) {
    scale_downsample <- NA_real_
    if (is.finite(base_scale_x) && is.finite(base_scale_y) &&
        is.finite(levels$scale_x[[i]]) && is.finite(levels$scale_y[[i]]) &&
        base_scale_x != 0 && base_scale_y != 0) {
      scale_downsample <- max(
        abs(levels$scale_x[[i]] / base_scale_x),
        abs(levels$scale_y[[i]] / base_scale_y)
      )
    }
    size_downsample <- NA_real_
    if (is.finite(base_width) && is.finite(base_height) &&
        is.finite(levels$width[[i]]) && is.finite(levels$height[[i]]) &&
        levels$width[[i]] > 0 && levels$height[[i]] > 0) {
      size_downsample <- max(base_width / levels$width[[i]], base_height / levels$height[[i]])
    }
    levels$downsample[[i]] <- if (is.finite(scale_downsample)) scale_downsample else size_downsample
  }
  levels$downsample[!is.finite(levels$downsample)] <- NA_real_
  levels$downsample[[1L]] <- 1
  levels
}

#' Read OME-Zarr metadata
#'
#' Reads OME-NGFF/Zarr metadata without loading image chunks into memory. The
#' parser extracts multiscale dataset paths, axes, dimensions, chunks, dtype,
#' scale/translation transforms, and pyramid levels from local OME-Zarr
#' directories.
#'
#' @param path Path to an OME-Zarr directory.
#' @param multiscale Multiscale image index to inspect when a Zarr group stores
#'   more than one image.
#'
#' @return A list with attributes, multiscale metadata, and a levels data frame.
#' @export
omezarr_metadata <- function(path, multiscale = 1L) {
  path <- wsi_validate_input_path(path)
  if (!wsi_is_omezarr_path(path)) {
    wsi_abort(sprintf("Input does not look like an OME-Zarr directory: %s", path))
  }

  root <- wsi_omezarr_root_metadata(path)
  attrs <- root$attrs
  multiscales <- root$multiscales
  multiscale <- as.integer(wsi_check_scalar_number(multiscale, "multiscale", allow_zero = FALSE))
  selected <- if (length(multiscales)) {
    if (multiscale > length(multiscales)) {
      wsi_abort(sprintf("OME-Zarr multiscale index %d is not available.", multiscale))
    }
    multiscales[[multiscale]]
  } else {
    list(datasets = list(list(path = "")), axes = list())
  }
  datasets <- selected$datasets %||% list(list(path = ""))
  axes <- selected$axes %||% list()

  rows <- lapply(seq_along(datasets), function(i) {
    wsi_omezarr_dataset_row(path, datasets[[i]], level = i - 1L, axes = axes)
  })
  levels <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      level = integer(),
      path = character(),
      width = numeric(),
      height = numeric(),
      downsample = numeric(),
      stringsAsFactors = FALSE
    )
  }
  levels <- wsi_omezarr_compute_downsample(levels)
  base_shape <- if (nrow(levels)) levels$shape[[1L]] else numeric()
  axis_table <- wsi_omezarr_axis_table(axes, base_shape)

  list(
    path = path,
    ngff_version = (selected$version %||% root$ome$version %||% attrs$version %||% NA_character_),
    zarr_format = root$zarr_json$zarr_format %||% NA_integer_,
    multiscale_index = multiscale,
    image_name = selected$name %||% NA_character_,
    axes = axes,
    axis_table = axis_table,
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
#' @param multiscale Multiscale image index to open.
#'
#' @return A `wsi_slide` object with backend `"omezarr"`.
#' @export
open_omezarr <- function(path, multiscale = 1L) {
  metadata <- omezarr_metadata(path, multiscale = multiscale)
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
