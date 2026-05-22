wsi_mask_channel_matrix <- function(mask, channel = c("auto", "gray", "red", "green", "blue", "alpha", "rgb")) {
  channel <- match.arg(channel)
  if (inherits(mask, "wsi_tissue_mask")) {
    return(mask$mask)
  }
  if (inherits(mask, "magick-image")) {
    mask <- wsi_magick_to_array(mask)
  }
  if (inherits(mask, "raster")) {
    dims <- dim(mask)
    rgba <- grDevices::col2rgb(as.vector(mask), alpha = TRUE)
    arr <- array(aperm(array(rgba, dim = c(4L, dims[1L], dims[2L])), c(2L, 3L, 1L)),
                 dim = c(dims[1L], dims[2L], 4L))
    mask <- arr
  }
  if (is.matrix(mask)) {
    return(mask)
  }
  dims <- dim(mask)
  if (!is.array(mask) || length(dims) != 3L || dims[[3L]] < 3L) {
    wsi_abort("`mask` must be a matrix, raster, RGB/RGBA array, magick image, or `wsi_tissue_mask`.")
  }
  arr <- mask
  storage.mode(arr) <- "double"
  if (max(arr, na.rm = TRUE) <= 1) {
    arr <- arr * 255
  }
  arr <- pmin(pmax(round(arr), 0), 255)

  if (identical(channel, "auto")) {
    channel <- "gray"
  }
  if (identical(channel, "red")) {
    return(arr[, , 1L])
  }
  if (identical(channel, "green")) {
    return(arr[, , 2L])
  }
  if (identical(channel, "blue")) {
    return(arr[, , 3L])
  }
  if (identical(channel, "alpha")) {
    if (dims[[3L]] < 4L) {
      wsi_abort("`channel = \"alpha\"` requires an image with an alpha channel.")
    }
    return(arr[, , 4L])
  }
  if (identical(channel, "rgb")) {
    return(arr[, , 1L] * 65536 + arr[, , 2L] * 256 + arr[, , 3L])
  }
  round((arr[, , 1L] + arr[, , 2L] + arr[, , 3L]) / 3)
}

wsi_read_mask_matrix <- function(path, channel = c("auto", "gray", "red", "green", "blue", "alpha", "rgb")) {
  path <- wsi_validate_input_path(path)
  channel <- match.arg(channel)
  wsi_require_magick("read a mask image file")
  image <- magick::image_read(path)
  wsi_mask_channel_matrix(image, channel = channel)
}

wsi_mask_apply_threshold <- function(mat, threshold = NULL) {
  if (is.null(threshold)) {
    return(mat)
  }
  threshold <- wsi_check_scalar_number(threshold, "threshold")
  values <- mat
  storage.mode(values) <- "double"
  if (threshold <= 1 && max(values, na.rm = TRUE) > 1) {
    values <- values / 255
  }
  values > threshold
}

wsi_mask_label_values <- function(mat, label_values = NULL, background = 0) {
  if (!is.null(label_values)) {
    return(label_values)
  }
  if (is.logical(mat)) {
    return(TRUE)
  }
  values <- sort(unique(as.vector(mat)))
  values <- values[!is.na(values)]
  bg <- as.vector(background)
  if (length(bg)) {
    values <- values[!values %in% bg]
  }
  values
}

wsi_mask_origin <- function(origin) {
  if (!is.numeric(origin) || length(origin) < 2L || anyNA(origin[1:2]) || any(!is.finite(origin[1:2]))) {
    wsi_abort("`origin` must be a numeric x/y coordinate pair.")
  }
  c(x = as.numeric(origin[[1L]]), y = as.numeric(origin[[2L]]))
}

wsi_mask_scale <- function(scale) {
  if (!is.numeric(scale) || !length(scale) || anyNA(scale) || any(!is.finite(scale)) || any(scale <= 0)) {
    wsi_abort("`scale` must be one or two positive numeric values.")
  }
  if (length(scale) == 1L) {
    scale <- rep(scale, 2L)
  }
  c(x = as.numeric(scale[[1L]]), y = as.numeric(scale[[2L]]))
}

wsi_mask_lookup <- function(map, value, fallback = NA_character_) {
  if (is.null(map)) {
    return(fallback)
  }
  key <- as.character(value)
  if (!is.null(names(map)) && key %in% names(map)) {
    return(as.character(map[[key]]))
  }
  if (!is.null(names(map)) && paste0("label_", key) %in% names(map)) {
    return(as.character(map[[paste0("label_", key)]]))
  }
  fallback
}

wsi_mask_colour <- function(color_map, value, index) {
  colour <- wsi_mask_lookup(color_map, value, fallback = NA_character_)
  if (!is.na(colour) && nzchar(colour)) {
    return(wsi_colour_to_hex(colour, "color_map"))
  }
  wsi_stain_palette(index)[[index]]
}

wsi_mask_component_list_r <- function(binary, connectivity = c("4", "8"), min_area = 1L) {
  connectivity <- match.arg(as.character(connectivity), c("4", "8"))
  min_area <- as.integer(wsi_check_scalar_number(min_area, "min_area", allow_zero = FALSE))
  binary <- !is.na(binary) & binary
  nr <- nrow(binary)
  nc <- ncol(binary)
  visited <- matrix(FALSE, nr, nc)
  dirs <- if (identical(connectivity, "8")) {
    rbind(
      c(-1L, 0L), c(1L, 0L), c(0L, -1L), c(0L, 1L),
      c(-1L, -1L), c(-1L, 1L), c(1L, -1L), c(1L, 1L)
    )
  } else {
    rbind(c(-1L, 0L), c(1L, 0L), c(0L, -1L), c(0L, 1L))
  }

  starts <- which(binary, arr.ind = TRUE)
  components <- list()
  for (i in seq_len(nrow(starts))) {
    r0 <- starts[i, 1L]
    c0 <- starts[i, 2L]
    if (visited[r0, c0]) {
      next
    }
    rows <- integer(nr * nc)
    cols <- integer(nr * nc)
    rows[[1L]] <- r0
    cols[[1L]] <- c0
    visited[r0, c0] <- TRUE
    head <- 1L
    tail <- 1L
    while (head <= tail) {
      r <- rows[[head]]
      c <- cols[[head]]
      head <- head + 1L
      for (d in seq_len(nrow(dirs))) {
        rr <- r + dirs[d, 1L]
        cc <- c + dirs[d, 2L]
        if (rr < 1L || rr > nr || cc < 1L || cc > nc || visited[rr, cc] || !binary[rr, cc]) {
          next
        }
        tail <- tail + 1L
        rows[[tail]] <- rr
        cols[[tail]] <- cc
        visited[rr, cc] <- TRUE
      }
    }
    if (tail >= min_area) {
      components[[length(components) + 1L]] <- cbind(row = rows[seq_len(tail)], col = cols[seq_len(tail)])
    }
  }
  components
}

wsi_mask_component_list <- function(binary, connectivity = c("4", "8"), min_area = 1L) {
  connectivity <- match.arg(as.character(connectivity), c("4", "8"))
  min_area <- as.integer(wsi_check_scalar_number(min_area, "min_area", allow_zero = FALSE))
  binary <- !is.na(binary) & binary
  if (wsi_native_available("wsi_cpp_mask_components")) {
    return(.Call("wsi_cpp_mask_components", binary, connectivity, min_area, PACKAGE = "wsiTools"))
  }
  wsi_mask_component_list_r(binary, connectivity = connectivity, min_area = min_area)
}

wsi_mask_point_key <- function(x, y) {
  paste0(x, ",", y)
}

wsi_mask_simplify_ring <- function(ring) {
  if (nrow(ring) <= 5L) {
    return(ring)
  }
  closed <- identical(ring[1L, 1L], ring[nrow(ring), 1L]) &&
    identical(ring[1L, 2L], ring[nrow(ring), 2L])
  if (closed) {
    ring <- ring[-nrow(ring), , drop = FALSE]
  }
  keep <- rep(TRUE, nrow(ring))
  n <- nrow(ring)
  for (i in seq_len(n)) {
    prev <- if (i == 1L) n else i - 1L
    next_i <- if (i == n) 1L else i + 1L
    same_x <- ring[prev, 1L] == ring[i, 1L] && ring[i, 1L] == ring[next_i, 1L]
    same_y <- ring[prev, 2L] == ring[i, 2L] && ring[i, 2L] == ring[next_i, 2L]
    if (same_x || same_y) {
      keep[[i]] <- FALSE
    }
  }
  out <- ring[keep, , drop = FALSE]
  if (nrow(out) < 3L) {
    out <- ring
  }
  rbind(out, out[1L, , drop = FALSE])
}

wsi_mask_trace_rings <- function(starts, ends) {
  if (!nrow(starts)) {
    return(list())
  }
  used <- rep(FALSE, nrow(starts))
  start_keys <- wsi_mask_point_key(starts[, 1L], starts[, 2L])
  edge_map <- split(seq_len(nrow(starts)), start_keys)
  rings <- list()

  for (i in seq_len(nrow(starts))) {
    if (used[[i]]) {
      next
    }
    first <- starts[i, ]
    current <- ends[i, ]
    ring <- rbind(first, current)
    used[[i]] <- TRUE
    guard <- 0L
    repeat {
      guard <- guard + 1L
      if (guard > nrow(starts) + 1L) {
        break
      }
      if (identical(current[[1L]], first[[1L]]) && identical(current[[2L]], first[[2L]])) {
        break
      }
      candidates <- edge_map[[wsi_mask_point_key(current[[1L]], current[[2L]])]]
      candidates <- candidates[!used[candidates]]
      if (!length(candidates)) {
        break
      }
      next_edge <- candidates[[1L]]
      current <- ends[next_edge, ]
      ring <- rbind(ring, current)
      used[[next_edge]] <- TRUE
    }
    closed <- nrow(ring) >= 4L &&
      identical(ring[1L, 1L], ring[nrow(ring), 1L]) &&
      identical(ring[1L, 2L], ring[nrow(ring), 2L])
    if (closed) {
      rings[[length(rings) + 1L]] <- ring
    }
  }
  rings
}

wsi_mask_component_rings <- function(component, origin, scale, simplify = TRUE) {
  min_row <- min(component[, "row"])
  max_row <- max(component[, "row"])
  min_col <- min(component[, "col"])
  max_col <- max(component[, "col"])
  comp <- matrix(FALSE, nrow = max_row - min_row + 1L, ncol = max_col - min_col + 1L)
  comp[cbind(component[, "row"] - min_row + 1L, component[, "col"] - min_col + 1L)] <- TRUE

  starts <- matrix(integer(), ncol = 2L)
  ends <- matrix(integer(), ncol = 2L)
  add_edge <- function(x0, y0, x1, y1) {
    starts <<- rbind(starts, c(x0, y0))
    ends <<- rbind(ends, c(x1, y1))
  }

  cells <- which(comp, arr.ind = TRUE)
  for (i in seq_len(nrow(cells))) {
    r <- cells[i, 1L]
    c <- cells[i, 2L]
    x0 <- min_col + c - 2L
    x1 <- min_col + c - 1L
    y0 <- min_row + r - 2L
    y1 <- min_row + r - 1L
    if (r == 1L || !comp[r - 1L, c]) {
      add_edge(x0, y0, x1, y0)
    }
    if (c == ncol(comp) || !comp[r, c + 1L]) {
      add_edge(x1, y0, x1, y1)
    }
    if (r == nrow(comp) || !comp[r + 1L, c]) {
      add_edge(x1, y1, x0, y1)
    }
    if (c == 1L || !comp[r, c - 1L]) {
      add_edge(x0, y1, x0, y0)
    }
  }

  rings <- wsi_mask_trace_rings(starts, ends)
  if (isTRUE(simplify)) {
    rings <- lapply(rings, wsi_mask_simplify_ring)
  }
  rings <- lapply(rings, function(ring) {
    ring[, 1L] <- origin[["x"]] + ring[, 1L] * scale[["x"]]
    ring[, 2L] <- origin[["y"]] + ring[, 2L] * scale[["y"]]
    ring
  })
  areas <- vapply(rings, wsi_ring_area, numeric(1))
  rings[order(areas, decreasing = TRUE)]
}

wsi_mask_ring_to_geojson <- function(ring) {
  lapply(seq_len(nrow(ring)), function(i) unname(c(ring[i, 1L], ring[i, 2L])))
}

wsi_mask_roi_from_rows <- function(rows, geojson = list(type = "FeatureCollection")) {
  if (!length(rows)) {
    return(wsi_empty_roi(geojson))
  }
  roi <- do.call(rbind, lapply(rows, `[[`, "data"))
  roi$coordinates <- I(lapply(rows, `[[`, "coordinates"))
  roi$measurements <- I(lapply(rows, `[[`, "measurements"))
  roi$properties <- I(lapply(rows, `[[`, "properties"))
  roi$geometry <- I(lapply(rows, `[[`, "geometry"))
  roi$feature <- I(lapply(rows, `[[`, "feature"))
  class(roi) <- c("wsi_roi", class(roi))
  wsi_apply_geojson_attributes(roi, geojson)
}

wsi_mask_dimension <- function(value, name) {
  value <- wsi_check_scalar_number(value, name, allow_zero = FALSE)
  value <- as.integer(value)
  if (is.na(value) || value < 1L) {
    wsi_abort(sprintf("`%s` must be a positive integer.", name))
  }
  value
}

wsi_rois_to_mask_label_keys <- function(rois, label_by) {
  switch(
    label_by,
    index = as.character(seq_len(nrow(rois))),
    class = as.character(rois$class %||% rep(NA_character_, nrow(rois))),
    roi_id = as.character(rois$roi_id %||% rep(NA_character_, nrow(rois))),
    name = as.character(rois$name %||% rep(NA_character_, nrow(rois))),
    constant = rep("foreground", nrow(rois))
  )
}

wsi_rois_to_mask_values <- function(rois, label_by, values = NULL) {
  keys <- wsi_rois_to_mask_label_keys(rois, label_by)
  keys[is.na(keys) | !nzchar(keys)] <- "unlabelled"
  n <- nrow(rois)

  if (is.null(values)) {
    if (identical(label_by, "constant")) {
      labels <- rep(1L, n)
      label_table <- data.frame(value = 1L, key = "foreground", stringsAsFactors = FALSE)
    } else if (identical(label_by, "index")) {
      labels <- seq_len(n)
      label_table <- data.frame(value = labels, key = keys, stringsAsFactors = FALSE)
    } else {
      levels <- unique(keys)
      labels <- match(keys, levels)
      label_table <- data.frame(value = seq_along(levels), key = levels, stringsAsFactors = FALSE)
    }
    return(list(values = labels, table = label_table, keys = keys))
  }

  if (is.list(values) && !is.data.frame(values)) {
    values <- unlist(values, use.names = TRUE)
  }
  if (!is.atomic(values)) {
    wsi_abort("`values` must be NULL, an atomic vector, or a named list.")
  }

  if (!is.null(names(values)) && any(nzchar(names(values)))) {
    value_names <- as.character(names(values))
    labels <- values[match(keys, value_names)]
    missing <- unique(keys[is.na(labels)])
    if (length(missing)) {
      wsi_abort(sprintf(
        "`values` is missing labels for: %s",
        paste(utils::head(missing, 5L), collapse = ", ")
      ))
    }
  } else if (length(values) == 1L) {
    labels <- rep(values[[1L]], n)
  } else if (length(values) == n) {
    labels <- values
  } else {
    wsi_abort("`values` must have length 1, length `nrow(rois)`, or names matching the selected ROI labels.")
  }

  label_table <- data.frame(
    value = as.vector(labels),
    key = keys,
    stringsAsFactors = FALSE
  )
  list(values = as.vector(labels), table = label_table, keys = keys)
}

wsi_mask_storage_mode <- function(values, background) {
  all_values <- c(as.vector(values), background)
  if (is.logical(all_values)) {
    return("logical")
  }
  if (is.numeric(all_values) || is.integer(all_values)) {
    return("numeric")
  }
  "character"
}

wsi_rois_to_mask_write <- function(mask, output, overwrite = FALSE) {
  output <- wsi_validate_output_path(output, overwrite = overwrite)
  ext <- tolower(tools::file_ext(output))
  if (identical(ext, "rds")) {
    saveRDS(mask, output)
    return(invisible(output))
  }
  if (ext %in% c("csv", "txt")) {
    utils::write.csv(as.data.frame(unclass(mask)), output, row.names = FALSE)
    return(invisible(output))
  }
  if (!ext %in% c("png", "tif", "tiff", "jpg", "jpeg")) {
    wsi_abort("Mask output must use extension `.rds`, `.csv`, `.png`, `.tif`, `.tiff`, `.jpg`, or `.jpeg`.")
  }
  wsi_require_magick("write a mask image file")
  values <- unclass(mask)
  if (!is.numeric(values) && !is.integer(values) && !is.logical(values)) {
    wsi_abort("Image mask output requires numeric, integer, or logical mask values. Use `.rds` or `.csv` for character masks.")
  }
  values <- as.matrix(values)
  if (is.logical(values)) {
    values <- values * 255
  }
  max_value <- max(values, na.rm = TRUE)
  min_value <- min(values, na.rm = TRUE)
  if (!is.finite(max_value) || !is.finite(min_value)) {
    wsi_abort("Mask image contains no finite values.")
  }
  if (min_value < 0 || max_value > 255) {
    wsi_abort("Image mask output supports values from 0 to 255. Use `.rds` or `.csv` to preserve larger label values.")
  }
  gray <- sprintf("#%02X%02X%02X", as.integer(round(values)), as.integer(round(values)), as.integer(round(values)))
  dim(gray) <- dim(values)
  image <- magick::image_read(grDevices::as.raster(gray))
  magick::image_write(image, path = output)
  invisible(output)
}

#' Rasterise ROI polygons into a mask
#'
#' Converts polygon or multipolygon ROI annotations into a small labelled mask.
#' This is the reverse of [wsi_mask_to_rois()] and is intended for annotation,
#' segmentation, and machine-learning masks. It rasterises ROI coordinates into
#' an output matrix without reading any WSI pixels.
#'
#' Mask pixels are interpreted as pixel cells in slide coordinates. Pixel
#' `(row = 1, col = 1)` has centre `origin + 0.5 * scale`; the same `origin`
#' and `scale` convention is used by [wsi_mask_to_rois()].
#'
#' @param rois A `wsi_roi` object or a GeoJSON file path readable by
#'   [read_geojson()].
#' @param width,height Output mask width and height in pixels.
#' @param scale Numeric x/y size, in slide pixels, represented by one mask
#'   pixel. Use values greater than 1 to create a lower-resolution mask.
#' @param origin Numeric x/y slide coordinate represented by the top-left corner
#'   of the output mask.
#' @param label_by How ROI values are assigned: `"index"` gives one integer per
#'   ROI, `"class"` gives one integer per unique class, `"roi_id"`/`"name"` give
#'   one integer per unique ROI id/name, and `"constant"` burns all ROIs with the
#'   same foreground value.
#' @param values Optional mask values. Use a length-one value, one value per
#'   ROI, or a named vector/list keyed by the selected `label_by` values.
#' @param background Background mask value.
#' @param overlap How overlapping ROIs are handled. `"last"` lets later ROIs
#'   overwrite earlier ROIs, `"first"` keeps the first value, and `"error"`
#'   aborts when overlap is detected.
#' @param output Optional output file. `.rds` and `.csv` are written with base R;
#'   image formats `.png`, `.tif`, `.tiff`, `.jpg`, and `.jpeg` require the
#'   optional `magick` package and support 0-255 numeric labels.
#' @param overwrite Whether `output` may overwrite an existing file.
#'
#' @return A matrix with class `wsi_roi_mask`. Attributes include `origin`,
#'   `scale`, `labels`, `background`, and `output`.
#' @export
#'
#' @examples
#' seed_mask <- matrix(0, nrow = 6, ncol = 6)
#' seed_mask[2:4, 2:4] <- 1
#' rois <- mask_to_rois(seed_mask, class_map = c("1" = "tumour"))
#' mask <- rois_to_mask(rois, width = 6, height = 6, label_by = "class")
wsi_rois_to_mask <- function(rois,
                             width,
                             height,
                             scale = c(x = 1, y = 1),
                             origin = c(x = 0, y = 0),
                             label_by = c("index", "class", "roi_id", "name", "constant"),
                             values = NULL,
                             background = 0,
                             overlap = c("last", "first", "error"),
                             output = NULL,
                             overwrite = FALSE) {
  if (is.character(rois) && length(rois) == 1L) {
    rois <- read_geojson(rois)
  }
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object or a GeoJSON file path.")
  }
  width <- wsi_mask_dimension(width, "width")
  height <- wsi_mask_dimension(height, "height")
  scale <- wsi_mask_scale(scale)
  origin <- wsi_mask_origin(origin)
  label_by <- match.arg(label_by)
  overlap <- match.arg(overlap)
  if (length(background) != 1L || is.na(background)) {
    wsi_abort("`background` must be a single non-missing value.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    wsi_abort("`overwrite` must be `TRUE` or `FALSE`.")
  }

  label_info <- wsi_rois_to_mask_values(rois, label_by = label_by, values = values)
  storage_mode <- wsi_mask_storage_mode(label_info$values, background)
  mask <- matrix(background, nrow = height, ncol = width)
  storage.mode(mask) <- storage_mode
  filled <- matrix(FALSE, nrow = height, ncol = width)

  if (nrow(rois)) {
    for (i in seq_len(nrow(rois))) {
      xmin <- suppressWarnings(as.numeric(rois$xmin[[i]]))
      xmax <- suppressWarnings(as.numeric(rois$xmax[[i]]))
      ymin <- suppressWarnings(as.numeric(rois$ymin[[i]]))
      ymax <- suppressWarnings(as.numeric(rois$ymax[[i]]))
      if (any(!is.finite(c(xmin, xmax, ymin, ymax)))) {
        points <- wsi_collect_points(rois$coordinates[[i]])
        if (!nrow(points)) {
          next
        }
        xmin <- min(points[, 1L])
        xmax <- max(points[, 1L])
        ymin <- min(points[, 2L])
        ymax <- max(points[, 2L])
      }

      col_start <- max(1L, floor((xmin - origin[["x"]]) / scale[["x"]]) + 1L)
      col_end <- min(width, ceiling((xmax - origin[["x"]]) / scale[["x"]]))
      row_start <- max(1L, floor((ymin - origin[["y"]]) / scale[["y"]]) + 1L)
      row_end <- min(height, ceiling((ymax - origin[["y"]]) / scale[["y"]]))
      if (col_start > col_end || row_start > row_end) {
        next
      }
      cols <- seq.int(col_start, col_end)
      rows <- seq.int(row_start, row_end)

      xs <- origin[["x"]] + (cols - 0.5) * scale[["x"]]
      ys <- origin[["y"]] + (rows - 0.5) * scale[["y"]]
      inside <- wsi_points_in_roi(
        rois,
        i,
        x = rep(xs, each = length(rows)),
        y = rep(ys, times = length(cols))
      )
      inside <- matrix(inside, nrow = length(rows), ncol = length(cols))
      if (!any(inside)) {
        next
      }

      current_filled <- filled[rows, cols, drop = FALSE]
      if (identical(overlap, "error") && any(inside & current_filled)) {
        wsi_abort(sprintf("ROI `%s` overlaps an already-rasterised ROI.", rois$roi_id[[i]] %||% i))
      }
      write_cells <- if (identical(overlap, "first")) inside & !current_filled else inside
      if (!any(write_cells)) {
        next
      }
      submask <- mask[rows, cols, drop = FALSE]
      submask[write_cells] <- label_info$values[[i]]
      mask[rows, cols] <- submask
      current_filled[inside] <- TRUE
      filled[rows, cols] <- current_filled
    }
  }

  attr(mask, "origin") <- origin
  attr(mask, "scale") <- scale
  attr(mask, "labels") <- label_info$table
  attr(mask, "label_by") <- label_by
  attr(mask, "background") <- background
  class(mask) <- c("wsi_roi_mask", class(mask))

  if (!is.null(output)) {
    wsi_rois_to_mask_write(mask, output = output, overwrite = overwrite)
    attr(mask, "output") <- normalizePath(output, winslash = "/", mustWork = FALSE)
  }
  mask
}

#' @rdname wsi_rois_to_mask
#' @export
rois_to_mask <- wsi_rois_to_mask

#' Convert an annotation mask into ROI polygons
#'
#' Converts a binary or labelled annotation mask into a `wsi_roi` object. This
#' is intended for already-small annotation or segmentation masks, not for
#' loading a full whole-slide image into memory. File paths are read with the
#' optional `magick` package; matrix, raster, array, and `wsi_tissue_mask`
#' inputs do not require a WSI backend.
#'
#' Each connected non-background component becomes one polygon ROI. Mask pixels
#' are interpreted as pixel cells, so pixel `(row = 1, col = 1)` spans
#' `origin` to `origin + scale`.
#'
#' @param mask A matrix, logical/numeric labelled mask, raster, RGB/RGBA array,
#'   `magick` image, `wsi_tissue_mask`, or image file path.
#' @param channel Image channel used when `mask` is an image. `"gray"` uses the
#'   mean RGB value, `"rgb"` treats RGB colours as distinct integer labels, and
#'   `"alpha"` uses transparency.
#' @param threshold Optional threshold. When supplied, values greater than the
#'   threshold are treated as a single foreground label.
#' @param background Background label value(s) to ignore.
#' @param label_values Optional explicit mask label values to import.
#' @param class_map Optional named character vector or list mapping mask values
#'   to pathology classes.
#' @param color_map Optional named colour vector/list mapping mask values to
#'   annotation colours.
#' @param origin Numeric x/y origin of the mask in level-0 slide coordinates.
#' @param scale Numeric x/y pixel size, in slide pixels, for one mask pixel.
#' @param connectivity Pixel connectivity for connected components, `"4"` or
#'   `"8"`.
#' @param min_area Minimum component area in mask pixels.
#' @param simplify Remove collinear vertices from the pixel-boundary polygon.
#' @param prefix Prefix for generated ROI ids and names.
#'
#' @return A `wsi_roi` object that can be passed to [wsi_viewer()],
#'   `viewer$add_rois()`, [write_geojson()], [extract_tiles()], and measurement
#'   helpers.
#' @export
wsi_mask_to_rois <- function(mask,
                             channel = c("auto", "gray", "red", "green", "blue", "alpha", "rgb"),
                             threshold = NULL,
                             background = 0,
                             label_values = NULL,
                             class_map = NULL,
                             color_map = NULL,
                             origin = c(x = 0, y = 0),
                             scale = c(x = 1, y = 1),
                             connectivity = c("4", "8"),
                             min_area = 1,
                             simplify = TRUE,
                             prefix = "mask") {
  channel <- match.arg(channel)
  connectivity <- match.arg(as.character(connectivity), c("4", "8"))
  if (is.character(mask) && length(mask) == 1L) {
    mat <- wsi_read_mask_matrix(mask, channel = channel)
    source <- normalizePath(mask, mustWork = FALSE)
  } else {
    mat <- wsi_mask_channel_matrix(mask, channel = channel)
    source <- NA_character_
  }
  mat <- wsi_mask_apply_threshold(mat, threshold = threshold)
  origin <- wsi_mask_origin(origin)
  scale <- wsi_mask_scale(scale)
  if (!is.logical(simplify) || length(simplify) != 1L || is.na(simplify)) {
    wsi_abort("`simplify` must be `TRUE` or `FALSE`.")
  }
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix) || !nzchar(prefix)) {
    wsi_abort("`prefix` must be a single non-empty character value.")
  }

  labels <- wsi_mask_label_values(mat, label_values = label_values, background = background)
  if (!length(labels)) {
    return(wsi_empty_roi(list(type = "FeatureCollection", name = "mask annotations")))
  }

  rows <- list()
  palette_index <- 0L
  for (value in labels) {
    binary <- if (is.logical(mat)) mat else mat == value
    components <- wsi_mask_component_list(binary, connectivity = connectivity, min_area = min_area)
    if (!length(components)) {
      next
    }
    value_key <- as.character(value)
    class_name <- wsi_mask_lookup(
      class_map,
      value,
      fallback = if (isTRUE(value) || identical(value_key, "1")) "mask" else paste0("label_", value_key)
    )
    for (component_index in seq_along(components)) {
      rings <- wsi_mask_component_rings(
        components[[component_index]],
        origin = origin,
        scale = scale,
        simplify = simplify
      )
      if (!length(rings)) {
        next
      }
      palette_index <- palette_index + 1L
      coordinates <- lapply(rings, wsi_mask_ring_to_geojson)
      points <- wsi_collect_points(coordinates)
      roi_id <- sprintf("%s_%s_%d", wsi_safe_id(prefix), wsi_safe_id(value_key, "label"), component_index)
      name <- sprintf("%s %s %d", prefix, value_key, component_index)
      color <- wsi_mask_colour(color_map, value, palette_index)
      measurements <- list(list(name = "Mask area px", value = nrow(components[[component_index]])))
      properties <- list(
        objectType = "annotation",
        name = name,
        label = name,
        classification = list(name = class_name, color = color),
        class = class_name,
        source = "mask",
        source_file = if (!is.na(source)) source else NULL,
        mask_value = value,
        mask_area_px = nrow(components[[component_index]]),
        measurements = measurements
      )
      geometry <- list(type = "Polygon", coordinates = coordinates)
      feature <- list(type = "Feature", id = roi_id, properties = properties, geometry = geometry)
      rows[[length(rows) + 1L]] <- list(
        data = data.frame(
          roi_id = roi_id,
          name = name,
          class = class_name,
          object_type = "annotation",
          color = color,
          classification_color = color,
          is_locked = FALSE,
          geometry_type = "Polygon",
          xmin = min(points[, 1L]),
          ymin = min(points[, 2L]),
          xmax = max(points[, 1L]),
          ymax = max(points[, 2L]),
          crs = NA_character_,
          stringsAsFactors = FALSE
        ),
        coordinates = coordinates,
        measurements = measurements,
        properties = properties,
        geometry = geometry,
        feature = feature
      )
    }
  }
  wsi_mask_roi_from_rows(rows, geojson = list(type = "FeatureCollection", name = "mask annotations"))
}

#' @rdname wsi_mask_to_rois
#' @param path Image mask file path.
#' @param ... Additional arguments passed to [wsi_mask_to_rois()].
#' @export
wsi_read_mask_annotations <- function(path, ...) {
  wsi_mask_to_rois(path, ...)
}

#' @rdname wsi_mask_to_rois
#' @export
mask_to_rois <- wsi_mask_to_rois

#' @rdname wsi_mask_to_rois
#' @export
read_mask_annotations <- function(path, ...) {
  wsi_read_mask_annotations(path, ...)
}
