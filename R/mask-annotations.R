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

wsi_roi_mask_values_for_image <- function(mask) {
  values <- as.matrix(unclass(mask))
  if (!is.numeric(values) && !is.integer(values) && !is.logical(values)) {
    wsi_abort("TIFF mask output requires numeric, integer, or logical mask values.")
  }
  if (is.logical(values)) {
    values <- values * 255
  }
  max_value <- max(values, na.rm = TRUE)
  min_value <- min(values, na.rm = TRUE)
  if (!is.finite(max_value) || !is.finite(min_value)) {
    wsi_abort("Mask image contains no finite values.")
  }
  if (min_value < 0 || max_value > 255) {
    wsi_abort("TIFF mask output currently supports integer values from 0 to 255.")
  }
  out <- pmin(pmax(as.integer(round(values)), 0L), 255L)
  dim(out) <- dim(values)
  out
}

wsi_write_mask_pgm <- function(mask, output) {
  values <- wsi_roi_mask_values_for_image(mask)
  con <- file(output, open = "wb")
  on.exit(close(con), add = TRUE)
  header <- sprintf("P5\n%d %d\n255\n", ncol(values), nrow(values))
  writeBin(charToRaw(header), con)
  for (i in seq_len(nrow(values))) {
    writeBin(as.raw(values[i, ]), con, size = 1L)
  }
  invisible(output)
}

wsi_hex_to_rgb_int <- function(colour, fallback = "#FFFFFF") {
  colour <- wsi_colour_to_hex(colour %||% fallback, "colour")
  col <- grDevices::col2rgb(colour)
  as.integer(col[, 1L])
}

wsi_geojson_mask_label_colours <- function(rois, labels, label_by, colour_map = NULL) {
  if (is.null(labels) || !nrow(labels)) {
    return(labels)
  }
  if (!is.null(colour_map) && is.list(colour_map) && !is.data.frame(colour_map)) {
    colour_map <- unlist(colour_map, use.names = TRUE)
  }
  roi_keys <- wsi_rois_to_mask_label_keys(rois, label_by)
  roi_keys[is.na(roi_keys) | !nzchar(roi_keys)] <- "unlabelled"
  roi_colours <- rois$color %||% rois$colour %||% rep(NA_character_, nrow(rois))
  out <- character(nrow(labels))
  for (i in seq_len(nrow(labels))) {
    key <- as.character(labels$key[[i]])
    value <- as.character(labels$value[[i]])
    colour <- NA_character_
    if (!is.null(colour_map) && length(colour_map)) {
      if (!is.null(names(colour_map)) && any(nzchar(names(colour_map)))) {
        idx <- match(key, names(colour_map))
        if (is.na(idx)) {
          idx <- match(value, names(colour_map))
        }
        if (!is.na(idx)) {
          colour <- as.character(colour_map[[idx]])
        }
      } else if (length(colour_map) >= i) {
        colour <- as.character(colour_map[[i]])
      }
    }
    if (is.na(colour) || !nzchar(colour)) {
      idx <- match(key, roi_keys)
      if (!is.na(idx)) {
        colour <- as.character(roi_colours[[idx]])
      }
    }
    if (is.na(colour) || !nzchar(colour)) {
      colour <- wsi_stain_palette(nrow(labels))[[i]]
    }
    out[[i]] <- wsi_colour_to_hex(colour, "colour")
  }
  labels$colour <- out
  labels$color <- out
  labels
}

wsi_write_mask_ppm <- function(mask, output, labels = NULL, background_colour = "#000000") {
  values <- wsi_roi_mask_values_for_image(mask)
  labels <- labels %||% attr(mask, "labels", exact = TRUE)
  value_keys <- as.character(labels$value %||% character())
  colours <- as.character(labels$colour %||% labels$color %||% character())
  rgb <- lapply(seq_along(value_keys), function(i) wsi_hex_to_rgb_int(colours[[i]] %||% NA_character_))
  names(rgb) <- value_keys
  bg <- wsi_hex_to_rgb_int(background_colour, fallback = "#000000")
  con <- file(output, open = "wb")
  on.exit(close(con), add = TRUE)
  header <- sprintf("P6\n%d %d\n255\n", ncol(values), nrow(values))
  writeBin(charToRaw(header), con)
  for (i in seq_len(nrow(values))) {
    row <- values[i, ]
    raw_row <- raw(length(row) * 3L)
    for (j in seq_along(row)) {
      colour <- rgb[[as.character(row[[j]])]] %||% bg
      pos <- (j - 1L) * 3L + 1L
      raw_row[pos:(pos + 2L)] <- as.raw(colour)
    }
    writeBin(raw_row, con, size = 1L)
  }
  invisible(output)
}

wsi_geojson_mask_format <- function(output, format = c("auto", "tiff", "ome-tiff")) {
  format <- match.arg(format)
  if (!identical(format, "auto")) {
    return(format)
  }
  output_lower <- tolower(output)
  if (grepl("\\.ome\\.tiff?$", output_lower)) {
    return("ome-tiff")
  }
  "tiff"
}

wsi_geojson_mask_dimensions <- function(rois, slide, width, height, origin, downsample) {
  if (!is.null(slide)) {
    if (inherits(slide, "wsi_slide")) {
      width <- as.numeric(slide$dimensions[["width"]])
      height <- as.numeric(slide$dimensions[["height"]])
    } else if (is.character(slide) && length(slide) == 1L) {
      slide <- wsi_open(slide)
      width <- as.numeric(slide$dimensions[["width"]])
      height <- as.numeric(slide$dimensions[["height"]])
      wsi_close(slide)
    } else {
      wsi_abort("`slide` must be a `wsi_slide` object, an image path, or `NULL`.")
    }
  }

  if (is.null(width) || is.null(height)) {
    if (!nrow(rois)) {
      wsi_abort("`width` and `height` are required when the GeoJSON contains no ROIs.")
    }
    xmax <- suppressWarnings(max(as.numeric(rois$xmax), na.rm = TRUE))
    ymax <- suppressWarnings(max(as.numeric(rois$ymax), na.rm = TRUE))
    if (!is.finite(xmax) || !is.finite(ymax)) {
      points <- do.call(rbind, lapply(rois$coordinates, wsi_collect_points))
      if (!nrow(points)) {
        wsi_abort("Could not infer mask dimensions from the GeoJSON coordinates.")
      }
      xmax <- max(points[, 1L], na.rm = TRUE)
      ymax <- max(points[, 2L], na.rm = TRUE)
    }
    width <- xmax - origin[["x"]]
    height <- ymax - origin[["y"]]
    wsi_warn("`width`/`height` were not supplied; inferred mask extent from ROI bounds.")
  }

  width <- wsi_check_scalar_number(width, "width", allow_zero = FALSE)
  height <- wsi_check_scalar_number(height, "height", allow_zero = FALSE)
  mask_width <- max(1L, as.integer(ceiling(width / downsample[["x"]])))
  mask_height <- max(1L, as.integer(ceiling(height / downsample[["y"]])))
  list(slide_width = width, slide_height = height, mask_width = mask_width, mask_height = mask_height)
}

wsi_write_geojson_mask_legend <- function(mask, output, overwrite = FALSE) {
  if (is.null(output)) {
    return(invisible(NULL))
  }
  output <- wsi_validate_output_path(output, overwrite = overwrite)
  labels <- attr(mask, "labels", exact = TRUE)
  if (is.null(labels)) {
    labels <- data.frame(value = integer(), key = character(), stringsAsFactors = FALSE)
  }
  utils::write.csv(labels, output, row.names = FALSE)
  invisible(output)
}

wsi_smooth_mask_iterations <- function(iterations) {
  iterations <- wsi_check_scalar_number(iterations, "smooth_iterations")
  iterations <- as.integer(iterations)
  if (is.na(iterations) || iterations < 0L) {
    wsi_abort("`smooth_iterations` must be a non-negative integer.")
  }
  iterations
}

wsi_smooth_mask_max_vertices <- function(max_vertices) {
  max_vertices <- wsi_check_scalar_number(max_vertices, "smooth_max_vertices", allow_zero = FALSE)
  max_vertices <- as.integer(max_vertices)
  if (is.na(max_vertices) || max_vertices < 4L) {
    wsi_abort("`smooth_max_vertices` must be at least 4.")
  }
  max_vertices
}

wsi_ring_matrix_to_geojson <- function(ring) {
  lapply(seq_len(nrow(ring)), function(i) unname(c(ring[i, 1L], ring[i, 2L])))
}

wsi_chaikin_ring_once <- function(points) {
  n <- nrow(points)
  next_i <- c(seq_len(n)[-1L], 1L)
  p1 <- points
  p2 <- points[next_i, , drop = FALSE]
  q <- 0.75 * p1 + 0.25 * p2
  r <- 0.25 * p1 + 0.75 * p2
  out <- matrix(NA_real_, nrow = n * 2L, ncol = 2L)
  out[seq(1L, n * 2L, by = 2L), ] <- q
  out[seq(2L, n * 2L, by = 2L), ] <- r
  out
}

wsi_rescale_ring_area <- function(ring, target_area) {
  current_area <- wsi_ring_area(ring)
  if (!is.finite(target_area) || !is.finite(current_area) ||
      target_area <= 0 || current_area <= 0) {
    return(ring)
  }
  factor <- sqrt(target_area / current_area)
  if (!is.finite(factor) || factor <= 0 || factor < 0.25 || factor > 4) {
    return(ring)
  }
  centre <- colMeans(ring, na.rm = TRUE)
  sweep(sweep(ring, 2L, centre, "-") * factor, 2L, centre, "+")
}

wsi_smooth_ring_chaikin <- function(ring, iterations = 1L,
                                    preserve_area = TRUE,
                                    max_vertices = 2000L) {
  points <- wsi_ring_matrix(ring)
  if (nrow(points) < 4L || iterations < 1L) {
    return(ring)
  }
  closed <- identical(points[1L, 1L], points[nrow(points), 1L]) &&
    identical(points[1L, 2L], points[nrow(points), 2L])
  if (closed) {
    points <- points[-nrow(points), , drop = FALSE]
  }
  if (nrow(points) < 3L) {
    return(ring)
  }
  original_area <- wsi_ring_area(points)
  available_iterations <- iterations
  while (available_iterations > 0L &&
         nrow(points) * (2L ^ available_iterations) > max_vertices) {
    available_iterations <- available_iterations - 1L
  }
  if (available_iterations < 1L) {
    return(wsi_ring_matrix_to_geojson(rbind(points, points[1L, , drop = FALSE])))
  }
  out <- points
  for (i in seq_len(available_iterations)) {
    out <- wsi_chaikin_ring_once(out)
  }
  if (isTRUE(preserve_area)) {
    out <- wsi_rescale_ring_area(out, original_area)
  }
  out <- rbind(out, out[1L, , drop = FALSE])
  wsi_ring_matrix_to_geojson(out)
}

wsi_smooth_polygon_coordinates <- function(polygon, iterations = 1L,
                                           preserve_area = TRUE,
                                           max_vertices = 2000L) {
  lapply(polygon, wsi_smooth_ring_chaikin,
         iterations = iterations,
         preserve_area = preserve_area,
         max_vertices = max_vertices)
}

wsi_smooth_roi_coordinates <- function(coordinates, geometry_type,
                                       iterations = 1L,
                                       preserve_area = TRUE,
                                       max_vertices = 2000L) {
  geometry_type <- tolower(as.character(geometry_type %||% ""))
  if (identical(geometry_type, "polygon")) {
    return(wsi_smooth_polygon_coordinates(
      coordinates,
      iterations = iterations,
      preserve_area = preserve_area,
      max_vertices = max_vertices
    ))
  }
  if (identical(geometry_type, "multipolygon")) {
    return(lapply(
      coordinates,
      wsi_smooth_polygon_coordinates,
      iterations = iterations,
      preserve_area = preserve_area,
      max_vertices = max_vertices
    ))
  }
  coordinates
}

wsi_smooth_rois_for_mask <- function(rois, smooth = FALSE,
                                     smooth_iterations = 1L,
                                     smooth_preserve_area = TRUE,
                                     smooth_max_vertices = 2000L) {
  if (!is.logical(smooth) || length(smooth) != 1L || is.na(smooth)) {
    wsi_abort("`smooth` must be `TRUE` or `FALSE`.")
  }
  smooth_iterations <- wsi_smooth_mask_iterations(smooth_iterations)
  if (!is.logical(smooth_preserve_area) || length(smooth_preserve_area) != 1L ||
      is.na(smooth_preserve_area)) {
    wsi_abort("`smooth_preserve_area` must be `TRUE` or `FALSE`.")
  }
  smooth_max_vertices <- wsi_smooth_mask_max_vertices(smooth_max_vertices)
  if (!isTRUE(smooth) || smooth_iterations < 1L || !nrow(rois)) {
    return(rois)
  }

  out <- rois
  for (i in seq_len(nrow(out))) {
    coords <- wsi_smooth_roi_coordinates(
      out$coordinates[[i]],
      out$geometry_type[[i]],
      iterations = smooth_iterations,
      preserve_area = smooth_preserve_area,
      max_vertices = smooth_max_vertices
    )
    out$coordinates[i] <- list(coords)
    geometry <- out$geometry[[i]] %||% list()
    geometry$coordinates <- coords
    out$geometry[i] <- list(geometry)
    feature <- out$feature[[i]] %||% list()
    feature$geometry <- geometry
    out$feature[i] <- list(feature)

    points <- wsi_collect_points(coords)
    if (nrow(points)) {
      out$xmin[[i]] <- min(points[, 1L])
      out$xmax[[i]] <- max(points[, 1L])
      out$ymin[[i]] <- min(points[, 2L])
      out$ymax[[i]] <- max(points[, 2L])
    }
  }
  out
}

#' Convert GeoJSON annotations to a TIFF mask
#'
#' Reads QuPath-style GeoJSON polygon annotations and rasterises them into a
#' labelled TIFF mask. This function does not read whole-slide pixels; it only
#' burns annotation geometry into a mask grid. When `format = "ome-tiff"` or
#' `pyramid = TRUE`, libvips is used at runtime to write a tiled/pyramidal
#' TIFF suitable for use as a viewer layer.
#'
#' @param geojson GeoJSON annotation file path.
#' @param output Output TIFF path. Use `.ome.tif` or `.ome.tiff` for OME-TIFF.
#' @param slide Optional `wsi_slide` object or image path used to derive the
#'   full-resolution image width and height.
#' @param width,height Full-resolution slide width and height in pixels. Ignored
#'   when `slide` is supplied. If neither `slide` nor dimensions are supplied,
#'   the extent is inferred from the ROI bounds.
#' @param downsample One or two positive numbers giving the full-resolution
#'   pixel size represented by each mask pixel. For example, `downsample = 4`
#'   creates a mask at one quarter of the slide width and height.
#' @param origin Full-resolution x/y coordinate of the top-left mask corner.
#' @param transform Optional affine transform, such as one returned by
#'   [wsi_orientation_transform()] or [estimate_transform()], applied to ROI
#'   coordinates before rasterisation.
#' @param label_by How ROI values are assigned: `"constant"` burns all ROIs as
#'   foreground, `"class"` gives one value per class, `"index"` one per ROI, and
#'   `"roi_id"`/`"name"` one per ROI id/name.
#' @param values Optional mask values passed to [rois_to_mask()].
#' @param background Background mask value.
#' @param overlap How overlapping ROIs are handled.
#' @param smooth Whether to smooth polygon boundaries before rasterisation.
#'   This uses a lightweight Chaikin corner-cutting pass and is useful for
#'   turning jagged cell-segmentation GeoJSON into cleaner mask borders.
#' @param smooth_iterations Number of smoothing passes when `smooth = TRUE`.
#'   One pass is usually enough for cell masks; higher values create more
#'   rounded boundaries and more vertices.
#' @param smooth_preserve_area Whether to rescale each smoothed ring back toward
#'   its original area. This reduces shrinkage of small segmented cells.
#' @param smooth_max_vertices Maximum vertices allowed per ring after smoothing.
#'   The number of passes is reduced for very large rings to avoid memory blowup.
#' @param format `"auto"`, `"tiff"`, or `"ome-tiff"`. `"auto"` selects OME-TIFF
#'   when `output` ends in `.ome.tif` or `.ome.tiff`.
#' @param pyramid Whether to write a tiled pyramid. Enabled automatically for
#'   OME-TIFF output.
#' @param tile_size TIFF tile size for libvips output.
#' @param compression TIFF compression for libvips output.
#' @param bigtiff Whether libvips should write BigTIFF.
#' @param colour Whether to write an RGB mask coloured by the annotation label
#'   table instead of a single-band labelled mask.
#' @param colour_map Optional colour map keyed by class/name/ROI id, depending
#'   on `label_by`, or by mask value.
#' @param background_colour Background colour for RGB masks.
#' @param legend_output Optional CSV path for the value-to-label table.
#' @param overwrite Whether existing output files may be overwritten.
#' @param return_mask Whether to include the in-memory mask matrix in the return
#'   value. Leave this `FALSE` for large slides.
#'
#' @return A `wsi_geojson_mask_tiff` list with output path, dimensions,
#'   downsample, label table, and optional mask.
#' @export
#'
#' @examples
#' geojson <- system.file("extdata", "example-qupath-annotations.geojson", package = "wsiTools")
#' if (nzchar(geojson) && requireNamespace("magick", quietly = TRUE)) {
#'   out <- tempfile(fileext = ".tif")
#'   result <- wsi_geojson_to_mask_tiff(geojson, out, width = 1000, height = 1000)
#'   file.exists(result$output)
#' }
wsi_geojson_to_mask_tiff <- function(geojson,
                                     output,
                                     slide = NULL,
                                     width = NULL,
                                     height = NULL,
                                     downsample = 1,
                                     origin = c(x = 0, y = 0),
                                     transform = NULL,
                                     label_by = c("constant", "class", "index", "roi_id", "name"),
                                     values = NULL,
                                     background = 0,
                                     overlap = c("last", "first", "error"),
                                     smooth = FALSE,
                                     smooth_iterations = 1,
                                     smooth_preserve_area = TRUE,
                                     smooth_max_vertices = 2000,
                                     format = c("auto", "tiff", "ome-tiff"),
                                     pyramid = NULL,
                                     tile_size = 512,
                                     compression = c("lzw", "jpeg", "deflate", "zstd", "webp", "packbits", "none"),
                                     bigtiff = TRUE,
                                     colour = FALSE,
                                     colour_map = NULL,
                                     background_colour = "#000000",
                                     legend_output = NULL,
                                     overwrite = FALSE,
                                     return_mask = FALSE) {
  geojson <- wsi_validate_input_path(geojson)
  output <- wsi_validate_output_path(output, overwrite = overwrite)
  label_by <- match.arg(label_by)
  overlap <- match.arg(overlap)
  format <- wsi_geojson_mask_format(output, format)
  compression <- match.arg(compression)
  downsample <- wsi_mask_scale(downsample)
  origin <- wsi_mask_origin(origin)
  if (!is.null(pyramid) && (!is.logical(pyramid) || length(pyramid) != 1L || is.na(pyramid))) {
    wsi_abort("`pyramid` must be `TRUE`, `FALSE`, or `NULL`.")
  }
  if (is.null(pyramid)) {
    pyramid <- identical(format, "ome-tiff")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    wsi_abort("`overwrite` must be `TRUE` or `FALSE`.")
  }
  if (!is.logical(return_mask) || length(return_mask) != 1L || is.na(return_mask)) {
    wsi_abort("`return_mask` must be `TRUE` or `FALSE`.")
  }
  if (!is.logical(colour) || length(colour) != 1L || is.na(colour)) {
    wsi_abort("`colour` must be `TRUE` or `FALSE`.")
  }

  rois <- read_geojson(geojson)
  if (!is.null(transform)) {
    rois <- transform_rois(rois, transform)
  }
  rois <- wsi_smooth_rois_for_mask(
    rois,
    smooth = smooth,
    smooth_iterations = smooth_iterations,
    smooth_preserve_area = smooth_preserve_area,
    smooth_max_vertices = smooth_max_vertices
  )
  dims <- wsi_geojson_mask_dimensions(rois, slide, width, height, origin, downsample)
  mask <- rois_to_mask(
    rois,
    width = dims$mask_width,
    height = dims$mask_height,
    scale = downsample,
    origin = origin,
    label_by = label_by,
    values = values,
    background = background,
    overlap = overlap
  )

  labels <- wsi_geojson_mask_label_colours(
    rois,
    attr(mask, "labels", exact = TRUE),
    label_by = label_by,
    colour_map = colour_map
  )
  attr(mask, "labels") <- labels
  if (is.null(legend_output)) {
    stem <- sub("\\.ome\\.tiff?$", "", output, ignore.case = TRUE)
    stem <- sub("\\.tiff?$", "", stem, ignore.case = TRUE)
    legend_output <- paste0(stem, "_labels.csv")
  }

  if (identical(format, "ome-tiff") || isTRUE(pyramid) || wsi_has_vips()) {
    if (!wsi_has_vips()) {
      wsi_abort(
        wsi_backend_action_message(
          "libvips is required to write tiled or OME-TIFF mask output.",
          backend = "vips"
        ),
        class = "wsi_backend_unavailable"
      )
    }
    temp_pgm <- tempfile(fileext = if (isTRUE(colour)) ".ppm" else ".pgm")
    on.exit(unlink(temp_pgm), add = TRUE)
    if (isTRUE(colour)) {
      wsi_write_mask_ppm(mask, temp_pgm, labels = labels, background_colour = background_colour)
    } else {
      wsi_write_mask_pgm(mask, temp_pgm)
    }
    wsi_convert(
      input = temp_pgm,
      output = output,
      format = if (identical(format, "ome-tiff")) "ome-tiff" else "tiff",
      backend = "vips",
      tile_size = tile_size,
      compression = compression,
      pyramid = isTRUE(pyramid),
      bigtiff = bigtiff,
      overwrite = TRUE
    )
  } else {
    wsi_rois_to_mask_write(mask, output = output, overwrite = TRUE)
  }
  wsi_write_geojson_mask_legend(mask, legend_output, overwrite = overwrite)

  result <- list(
    output = normalizePath(output, winslash = "/", mustWork = FALSE),
    legend = normalizePath(legend_output, winslash = "/", mustWork = FALSE),
    geojson = normalizePath(geojson, winslash = "/", mustWork = TRUE),
    format = format,
    pyramid = isTRUE(pyramid),
    slide_width = dims$slide_width,
    slide_height = dims$slide_height,
    mask_width = dims$mask_width,
    mask_height = dims$mask_height,
    downsample = downsample,
    origin = origin,
    transform = transform,
    labels = labels
  )
  if (isTRUE(return_mask)) {
    result$mask <- mask
  }
  class(result) <- c("wsi_geojson_mask_tiff", "list")
  result
}

#' @rdname wsi_geojson_to_mask_tiff
#' @export
geojson_to_mask_tiff <- wsi_geojson_to_mask_tiff

#' Create a tiled mask overlay from dense GeoJSON annotations
#'
#' Dense cell-level GeoJSON files can contain thousands to millions of polygon
#' vertices, which is too expensive to draw interactively as vector annotations.
#' This helper converts the GeoJSON into a coloured, pyramidal OME-TIFF mask,
#' creates Deep Zoom tiles, and returns a [wsi_channel_source()] that the viewer
#' can display as a transparent image overlay with a class legend. Keep ordinary
#' editable tissue ROIs as GeoJSON; use this for dense cell annotations.
#'
#' @param geojson Dense cell annotation GeoJSON file.
#' @param slide A `wsi_slide`, image path, or slide-like object used to derive
#'   slide dimensions and mask extent.
#' @param output_dir Directory where the OME-TIFF mask, legend CSV, and tile
#'   pyramid are written.
#' @param name,id Display name and stable layer id for the mask overlay.
#' @param output_html Optional viewer HTML path. When supplied, tile URLs are
#'   made relative to that HTML file for portable static viewers.
#' @param downsample Full-resolution pixels represented by one mask pixel.
#' @param label_by Annotation field used to define mask classes.
#' @param visible,opacity Initial overlay visibility and opacity.
#' @param tile_size Deep Zoom tile size for the generated mask overlay. The
#'   default 254 is the standard Deep Zoom size and works reliably for small
#'   and large masks.
#' @param rebuild Rebuild existing mask/tiles.
#' @param overwrite Overwrite an existing mask file when rebuilding.
#' @param ... Additional arguments passed to [wsi_geojson_to_mask_tiff()].
#'
#' @return A list with `source`, `mask`, and `tiles`. The `source` element is a
#'   `wsi_channel_source` suitable for `channel_sources = list(source)` or
#'   `viewer$add_channel_source(source)`.
#' @export
wsi_geojson_mask_channel_source <- function(geojson,
                                            slide,
                                            output_dir,
                                            name = "Cell annotation mask",
                                            id = "cell_annotation_mask",
                                            output_html = NULL,
                                            downsample = 4,
                                            label_by = c("class", "name", "roi_id", "index", "constant"),
                                            visible = TRUE,
                                            opacity = 0.45,
                                            tile_size = 254,
                                            rebuild = FALSE,
                                            overwrite = FALSE,
                                            ...) {
  geojson <- wsi_validate_input_path(geojson)
  label_by <- match.arg(label_by)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_dir)) {
    wsi_abort(sprintf("Could not create output directory: %s", output_dir))
  }
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  slide_obj <- if (inherits(slide, "wsi_slide")) slide else wsi_open(slide)
  slide_width <- as.numeric(slide_obj$dimensions[["width"]] %||% NA_real_)
  slide_height <- as.numeric(slide_obj$dimensions[["height"]] %||% NA_real_)
  if (!is.finite(slide_width) || !is.finite(slide_height)) {
    wsi_abort("Could not determine slide dimensions for the mask overlay.")
  }
  downsample_xy <- wsi_mask_scale(downsample)
  mask_width_est <- max(1L, as.integer(ceiling(slide_width / downsample_xy[["x"]])))
  mask_height_est <- max(1L, as.integer(ceiling(slide_height / downsample_xy[["y"]])))
  ome_tile_size <- max(16L, min(512L, tile_size, mask_width_est, mask_height_est))

  id <- wsi_channel_source_id(id, name)
  stem <- wsi_safe_id(id)
  mask_output <- file.path(output_dir, paste0(stem, ".ome.tif"))
  tile_dir <- file.path(output_dir, paste0(stem, "_deepzoom"))
  mask_result <- NULL

  if (file.exists(mask_output) && !isTRUE(rebuild)) {
    legend_file <- sub("\\.ome\\.tiff?$", "_labels.csv", mask_output, ignore.case = TRUE)
    labels <- if (file.exists(legend_file)) utils::read.csv(legend_file, stringsAsFactors = FALSE) else data.frame()
    mask_result <- list(
      output = normalizePath(mask_output, winslash = "/", mustWork = FALSE),
      legend = normalizePath(legend_file, winslash = "/", mustWork = FALSE),
      geojson = normalizePath(geojson, winslash = "/", mustWork = TRUE),
      format = "ome-tiff",
      pyramid = TRUE,
      slide_width = slide_width,
      slide_height = slide_height,
      mask_width = mask_width_est,
      mask_height = mask_height_est,
      downsample = downsample_xy,
      origin = c(x = 0, y = 0),
      labels = labels
    )
    class(mask_result) <- c("wsi_geojson_mask_tiff", "list")
  } else {
    mask_result <- wsi_geojson_to_mask_tiff(
      geojson = geojson,
      output = mask_output,
      slide = slide_obj,
      downsample = downsample,
      label_by = label_by,
      colour = TRUE,
      background_colour = "#000000",
      format = "ome-tiff",
      pyramid = TRUE,
      tile_size = ome_tile_size,
      overwrite = isTRUE(overwrite) || isTRUE(rebuild),
      return_mask = FALSE,
      ...
    )
  }

  mask_slide <- wsi_open(mask_result$output)
  tiles <- wsi_create_deepzoom_tiles(
    slide = mask_slide,
    tile_dir = tile_dir,
    tile_size = tile_size,
    tile_overlap = 1,
    tile_format = "png",
    quality = 90,
    rebuild = isTRUE(rebuild)
  )
  tile_url_base <- if (!is.null(output_html)) {
    wsi_tile_base_url(tile_dir, output_html)
  } else {
    wsi_file_url(tiles$tiles)
  }
  legend <- wsi_mask_channel_legend(mask_result$labels)
  slide_path <- if (!is.null(slide_obj$path) && nzchar(slide_obj$path)) {
    normalizePath(slide_obj$path, winslash = "/", mustWork = FALSE)
  } else {
    NULL
  }
  source <- wsi_channel_source(
    name = name,
    id = id,
    type = "deepzoom",
    tile_url_base = tile_url_base,
    width = mask_result$mask_width,
    height = mask_result$mask_height,
    tile_size = tile_size,
    tile_format = "png",
    max_level = wsi_dz_max_level(mask_result$mask_width, mask_result$mask_height),
    tile_overlap = as.integer(tiles$overlap %||% 1L),
    visible = visible,
    opacity = opacity,
    colour = "#ffffff",
    metadata = list(
      kind = "mask",
      transparent_background = TRUE,
      legend = legend,
      selected_values = vapply(legend, function(x) as.character(x$value), character(1)),
      extent = list(x = 0, y = 0, width = slide_width, height = slide_height),
      mask_downsample = unname(mask_result$downsample),
      source_geojson = normalizePath(geojson, winslash = "/", mustWork = TRUE),
      source_mask = mask_result$output,
      legend_csv = mask_result$legend,
      target_path = slide_path,
      base_slide_path = slide_path,
      project_image_id = "active_project_image"
    )
  )
  list(source = source, mask = mask_result, tiles = tiles)
}

#' @rdname wsi_geojson_mask_channel_source
#' @export
wsi_add_geojson_mask_overlay <- function(viewer, geojson, slide, output_dir, ..., service = TRUE) {
  result <- wsi_geojson_mask_channel_source(
    geojson = geojson,
    slide = slide,
    output_dir = output_dir,
    ...
  )
  wsi_add_channel_source(viewer, result$source, service = service)
  invisible(result)
}

wsi_mask_channel_legend <- function(labels) {
  if (is.null(labels) || !nrow(labels)) {
    return(list())
  }
  lapply(seq_len(nrow(labels)), function(i) {
    key <- as.character(labels$key[[i]] %||% labels$name[[i]] %||% labels$class[[i]] %||% labels$value[[i]])
    value <- as.character(labels$value[[i]] %||% i)
    colour <- as.character(labels$colour[[i]] %||% labels$color[[i]] %||% wsi_stain_palette(nrow(labels))[[i]])
    list(
      value = value,
      label = key,
      class = key,
      colour = wsi_colour_to_hex(colour, "colour")
    )
  })
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
