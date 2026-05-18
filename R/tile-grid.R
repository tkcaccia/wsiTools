wsi_tissue_fraction_for_tile <- function(mask, x, y, width, height, downsample) {
  if (is.null(mask)) {
    return(NA_real_)
  }
  if (!inherits(mask, "wsi_tissue_mask")) {
    wsi_abort("`tissue_mask` must be a `wsi_tissue_mask` object returned by `wsi_tissue_mask()`.")
  }
  mat <- mask$mask
  coverage_width <- width * downsample
  coverage_height <- height * downsample

  x1 <- max(1L, floor(x / mask$scale_x) + 1L)
  x2 <- min(ncol(mat), ceiling((x + coverage_width) / mask$scale_x))
  y1 <- max(1L, floor(y / mask$scale_y) + 1L)
  y2 <- min(nrow(mat), ceiling((y + coverage_height) / mask$scale_y))
  if (x2 < x1 || y2 < y1) {
    return(NA_real_)
  }
  mean(mat[y1:y2, x1:x2, drop = FALSE], na.rm = TRUE)
}

wsi_coords_to_data_frame <- function(coords) {
  if (is.data.frame(coords)) {
    out <- as.data.frame(coords, stringsAsFactors = FALSE)
  } else if (is.matrix(coords)) {
    coords <- coords[, seq_len(min(ncol(coords), 5L)), drop = FALSE]
    out <- as.data.frame(coords, stringsAsFactors = FALSE)
    if (is.null(colnames(coords))) {
      names(out) <- c("x", "y", "width", "height", "level")[seq_len(ncol(out))]
    }
  } else if (is.list(coords) && !is.null(coords$x) && !is.null(coords$y)) {
    out <- as.data.frame(coords, stringsAsFactors = FALSE)
  } else if (is.list(coords)) {
    rows <- lapply(seq_along(coords), function(i) {
      item <- coords[[i]]
      if (is.data.frame(item)) {
        return(as.data.frame(item[1L, , drop = FALSE], stringsAsFactors = FALSE))
      }
      if (is.list(item) && !is.null(item$x) && !is.null(item$y)) {
        return(as.data.frame(item, stringsAsFactors = FALSE))
      }
      if (is.numeric(item) && length(item) >= 2L) {
        item <- item[seq_len(min(length(item), 5L))]
        values <- as.list(rep(NA_real_, 5L))
        names(values) <- c("x", "y", "width", "height", "level")
        values[seq_along(item)] <- as.list(item)
        return(as.data.frame(values, stringsAsFactors = FALSE))
      }
      wsi_abort(sprintf("Coordinate entry %s must provide at least `x` and `y`.", i))
    })
    all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
    rows <- lapply(rows, function(row) {
      missing <- setdiff(all_names, names(row))
      for (name in missing) {
        row[[name]] <- NA
      }
      row[all_names]
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
  } else {
    wsi_abort("`coords` must be a data frame, matrix, or list of coordinate pairs.")
  }

  names(out) <- tolower(names(out))
  if (!all(c("x", "y") %in% names(out))) {
    wsi_abort("`coords` must include `x` and `y` coordinates.")
  }
  out
}

#' Generate a tile grid from explicit coordinates
#'
#' Turns an arbitrary coordinate list into a standard wsiTools tile grid without
#' reading image pixels. Coordinates are level-0 slide pixels. By default, `x`
#' and `y` are interpreted as the top-left corner of each requested tile; use
#' `anchor = "center"` when the coordinates mark tile centers.
#'
#' @param slide A `wsi_slide` object.
#' @param coords Data frame, matrix, or list with at least `x` and `y`. Optional
#'   columns include `width`, `height`, `level`, `tile_id`, `output_file`, and
#'   `roi_id`.
#' @param tile_size Default tile width and height in pixels at `level` when
#'   `coords$width` or `coords$height` are absent.
#' @param level Default pyramid level when `coords$level` is absent.
#' @param anchor Whether coordinates mark the tile `"top_left"` or `"center"`.
#' @param bounds How to handle tiles outside slide bounds: `"error"` aborts,
#'   `"trim"` clips partial edge tiles, and `"drop"` removes out-of-bounds
#'   tiles.
#' @param tissue_mask Optional `wsi_tissue_mask` object.
#'
#' @return A data frame of tile coordinates and metadata.
#' @export
#' @examples
#' slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800)
#' coords <- data.frame(x = c(0, 512), y = c(0, 256))
#' grid <- wsi_tile_grid_from_coords(slide, coords, tile_size = 256)
wsi_tile_grid_from_coords <- function(slide, coords, tile_size = 512, level = 0,
                                      anchor = c("top_left", "center"),
                                      bounds = c("error", "trim", "drop"),
                                      tissue_mask = NULL) {
  wsi_check_slide(slide)
  anchor <- match.arg(anchor)
  bounds <- match.arg(bounds)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  coords <- wsi_coords_to_data_frame(coords)
  n <- nrow(coords)

  x_values <- suppressWarnings(as.numeric(coords$x))
  y_values <- suppressWarnings(as.numeric(coords$y))
  if (anyNA(x_values) || anyNA(y_values) || any(!is.finite(x_values)) || any(!is.finite(y_values))) {
    wsi_abort("`coords$x` and `coords$y` must be finite numeric values.")
  }

  widths <- if ("width" %in% names(coords)) suppressWarnings(as.numeric(coords$width)) else rep(tile_size, n)
  heights <- if ("height" %in% names(coords)) suppressWarnings(as.numeric(coords$height)) else rep(tile_size, n)
  widths[is.na(widths)] <- tile_size
  heights[is.na(heights)] <- tile_size
  if (any(!is.finite(widths)) || any(widths <= 0) || any(!is.finite(heights)) || any(heights <= 0)) {
    wsi_abort("Coordinate tile `width` and `height` values must be finite and greater than zero.")
  }

  levels <- if ("level" %in% names(coords)) suppressWarnings(as.integer(coords$level)) else rep(as.integer(level), n)
  levels[is.na(levels)] <- as.integer(level)
  if (anyNA(levels)) {
    wsi_abort("Coordinate `level` values must be integer pyramid levels.")
  }

  tile_ids <- if ("tile_id" %in% names(coords)) {
    as.character(coords$tile_id)
  } else {
    sprintf("coord_%05d", seq_len(n))
  }
  tile_ids[is.na(tile_ids) | !nzchar(tile_ids)] <- sprintf("coord_%05d", which(is.na(tile_ids) | !nzchar(tile_ids)))

  output_files <- if ("output_file" %in% names(coords)) {
    as.character(coords$output_file)
  } else {
    sprintf("%s.png", tile_ids)
  }
  output_files[is.na(output_files) | !nzchar(output_files)] <- sprintf("%s.png", tile_ids[is.na(output_files) | !nzchar(output_files)])

  rows <- vector("list", n)
  kept <- 0L
  dropped <- character()
  for (i in seq_len(n)) {
    level_info <- wsi_level_row(slide, levels[[i]])
    downsample <- level_info$downsample[[1L]]
    width <- as.integer(round(widths[[i]]))
    height <- as.integer(round(heights[[i]]))
    x <- x_values[[i]]
    y <- y_values[[i]]
    if (identical(anchor, "center")) {
      x <- x - (width * downsample) / 2
      y <- y - (height * downsample) / 2
    }

    left <- x
    top <- y
    right <- x + width * downsample
    bottom <- y + height * downsample
    outside <- left < 0 || top < 0 ||
      right > slide$dimensions[["width"]] + 1e-6 ||
      bottom > slide$dimensions[["height"]] + 1e-6

    if (isTRUE(outside)) {
      if (identical(bounds, "error")) {
        wsi_abort(sprintf(
          "Coordinate row %s creates a tile outside slide bounds. Use `bounds = \"trim\"` or `bounds = \"drop\"` to handle edge tiles.",
          i
        ), class = "wsi_region_out_of_bounds")
      }
      if (identical(bounds, "drop")) {
        dropped <- c(dropped, tile_ids[[i]])
        next
      }

      left <- max(0, left)
      top <- max(0, top)
      right <- min(slide$dimensions[["width"]], right)
      bottom <- min(slide$dimensions[["height"]], bottom)
      if (right <= left || bottom <= top) {
        dropped <- c(dropped, tile_ids[[i]])
        next
      }
      width <- as.integer(ceiling((right - left) / downsample))
      height <- as.integer(ceiling((bottom - top) / downsample))
    }

    kept <- kept + 1L
    row <- if ("row" %in% names(coords)) as.integer(coords$row[[i]]) else i
    col <- if ("col" %in% names(coords)) as.integer(coords$col[[i]]) else 1L
    rows[[kept]] <- data.frame(
      tile_id = tile_ids[[i]],
      x = as.integer(round(left)),
      y = as.integer(round(top)),
      width = as.integer(width),
      height = as.integer(height),
      level = as.integer(levels[[i]]),
      row = row,
      col = col,
      downsample = downsample,
      tissue_fraction = wsi_tissue_fraction_for_tile(tissue_mask, left, top, width, height, downsample),
      output_file = output_files[[i]],
      roi_id = if ("roi_id" %in% names(coords)) as.character(coords$roi_id[[i]]) else NA_character_,
      stringsAsFactors = FALSE
    )
  }

  if (length(dropped)) {
    wsi_warn(sprintf("Dropped %s coordinate tile%s outside slide bounds.", length(dropped), if (length(dropped) == 1L) "" else "s"))
  }

  if (!kept) {
    return(data.frame(
      tile_id = character(),
      x = numeric(),
      y = numeric(),
      width = integer(),
      height = integer(),
      level = integer(),
      row = integer(),
      col = integer(),
      downsample = numeric(),
      tissue_fraction = numeric(),
      output_file = character(),
      roi_id = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows[seq_len(kept)])
}

#' Generate a tile grid
#'
#' Generates coordinates only. No image pixels are read.
#'
#' @param slide A `wsi_slide` object.
#' @param tile_size Tile width and height in pixels at `level`.
#' @param overlap Overlap in pixels at `level`.
#' @param level Pyramid level.
#' @param region Optional level-0 region (`x`, `y`, `width`, `height`).
#' @param tissue_mask Optional `wsi_tissue_mask` object.
#' @param include_partial Include edge tiles smaller than `tile_size`.
#'
#' @return A data frame of tile coordinates and metadata.
#' @export
wsi_tile_grid <- function(slide, tile_size = 512, overlap = 0, level = 0,
                          region = NULL, tissue_mask = NULL,
                          include_partial = FALSE) {
  wsi_check_slide(slide)
  level_info <- wsi_level_row(slide, level)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  overlap <- as.integer(wsi_check_scalar_number(overlap, "overlap"))
  if (overlap >= tile_size) {
    wsi_abort("`overlap` must be smaller than `tile_size`.")
  }

  region <- wsi_normalize_region(region, slide)
  downsample <- level_info$downsample[[1L]]
  tile_coverage <- tile_size * downsample
  step_coverage <- (tile_size - overlap) * downsample
  x_end <- region[["x"]] + region[["width"]]
  y_end <- region[["y"]] + region[["height"]]

  if (isTRUE(include_partial)) {
    x_starts <- seq(region[["x"]], x_end - 1e-6, by = step_coverage)
    y_starts <- seq(region[["y"]], y_end - 1e-6, by = step_coverage)
  } else {
    last_x <- x_end - tile_coverage
    last_y <- y_end - tile_coverage
    x_starts <- if (last_x >= region[["x"]]) seq(region[["x"]], last_x, by = step_coverage) else numeric()
    y_starts <- if (last_y >= region[["y"]]) seq(region[["y"]], last_y, by = step_coverage) else numeric()
  }

  if (!length(x_starts) || !length(y_starts)) {
    out <- data.frame(
      tile_id = character(),
      x = numeric(),
      y = numeric(),
      width = integer(),
      height = integer(),
      level = integer(),
      row = integer(),
      col = integer(),
      downsample = numeric(),
      tissue_fraction = numeric(),
      output_file = character(),
      stringsAsFactors = FALSE
    )
    return(out)
  }

  rows <- vector("list", length(x_starts) * length(y_starts))
  k <- 0L
  for (row in seq_along(y_starts)) {
    for (col in seq_along(x_starts)) {
      x <- as.integer(round(x_starts[[col]]))
      y <- as.integer(round(y_starts[[row]]))
      width_level <- if (isTRUE(include_partial)) min(tile_size, ceiling((x_end - x) / downsample)) else tile_size
      height_level <- if (isTRUE(include_partial)) min(tile_size, ceiling((y_end - y) / downsample)) else tile_size
      if (width_level <= 0L || height_level <= 0L) {
        next
      }
      k <- k + 1L
      tile_id <- sprintf("L%d_R%05d_C%05d", level, row, col)
      rows[[k]] <- data.frame(
        tile_id = tile_id,
        x = x,
        y = y,
        width = as.integer(width_level),
        height = as.integer(height_level),
        level = as.integer(level),
        row = as.integer(row),
        col = as.integer(col),
        downsample = downsample,
        tissue_fraction = wsi_tissue_fraction_for_tile(tissue_mask, x, y, width_level, height_level, downsample),
        output_file = sprintf("%s.png", tile_id),
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows[seq_len(k)])
}

#' Extract tiles to disk
#'
#' Generates a tile grid, optionally filters by tissue, writes tiles to disk, and
#' returns a manifest.
#'
#' @param slide A `wsi_slide` object.
#' @param output_dir Output directory.
#' @param tile_size,overlap,level,region Arguments passed to [wsi_tile_grid()].
#' @param roi Optional ROI object; use [wsi_tile_roi()] for ROI-specific tiling.
#' @param tissue_mask Whether to estimate and filter by tissue mask.
#' @param tissue_threshold Minimum tissue fraction when `tissue_mask = TRUE`.
#' @param format Tile image format.
#' @param prefix Optional filename prefix.
#' @param overwrite Whether to overwrite existing tiles.
#' @param workers Reserved for future parallel processing.
#'
#' @return A tile manifest data frame.
#' @export
wsi_tile <- function(slide, output_dir, tile_size = 512, overlap = 0, level = 0,
                     region = NULL, roi = NULL, tissue_mask = FALSE,
                     tissue_threshold = 0.1, format = c("png", "jpeg", "tiff"),
                     prefix = NULL, overwrite = FALSE, workers = 1) {
  wsi_check_slide(slide)
  format <- match.arg(format)
  if (!identical(as.integer(workers), 1L)) {
    wsi_warn("Parallel tile extraction is planned but not implemented yet; using `workers = 1`.")
  }

  if (!is.null(roi)) {
    wsi_warn("Use `wsi_tile_roi()` for ROI-aware tiling. This call will use the ROI bounding box in the first milestone.")
    region <- c(x = roi$xmin[[1L]], y = roi$ymin[[1L]], width = roi$xmax[[1L]] - roi$xmin[[1L]], height = roi$ymax[[1L]] - roi$ymin[[1L]])
  }

  mask <- NULL
  if (isTRUE(tissue_mask)) {
    mask <- wsi_tissue_mask(slide)
  }

  grid <- wsi_tile_grid(
    slide,
    tile_size = tile_size,
    overlap = overlap,
    level = level,
    region = region,
    tissue_mask = mask,
    include_partial = FALSE
  )

  if (isTRUE(tissue_mask)) {
    grid <- grid[is.na(grid$tissue_fraction) | grid$tissue_fraction >= tissue_threshold, , drop = FALSE]
  }

  if (!is.null(prefix) && nrow(grid)) {
    grid$output_file <- sprintf("%s_%s.%s", prefix, tools::file_path_sans_ext(grid$tile_id), wsi_format_extension(format))
  }

  manifest <- wsi_export_tiles(slide, grid, output_dir = output_dir, format = format, overwrite = overwrite)
  class(manifest) <- c("wsi_tile_manifest", setdiff(class(manifest), "wsi_tile_manifest"))
  manifest
}

#' Extract tiles from explicit coordinates
#'
#' Converts a coordinate list to a tile grid with [wsi_tile_grid_from_coords()],
#' writes each requested region to disk, and returns a tile manifest. This is
#' useful when coordinates come from a model, CSV file, tissue detector, or an
#' external annotation workflow.
#'
#' @inheritParams wsi_tile_grid_from_coords
#' @param output_dir Output directory.
#' @param format Tile image format.
#' @param prefix Optional filename prefix.
#' @param overwrite Whether to overwrite existing tiles.
#'
#' @return A tile manifest data frame.
#' @export
#' @examples
#' \dontrun{
#' slide <- wsi_open("sample.svs")
#' coords <- data.frame(x = c(10000, 12000), y = c(20000, 22000))
#' manifest <- wsi_tile_from_coords(slide, coords, output_dir = "tiles")
#' wsi_close(slide)
#' }
wsi_tile_from_coords <- function(slide, coords, output_dir, tile_size = 512,
                                 level = 0, anchor = c("top_left", "center"),
                                 bounds = c("error", "trim", "drop"),
                                 format = c("png", "jpeg", "tiff"),
                                 prefix = NULL, overwrite = FALSE) {
  wsi_check_slide(slide)
  format <- match.arg(format)
  anchor <- match.arg(anchor)
  bounds <- match.arg(bounds)

  grid <- wsi_tile_grid_from_coords(
    slide = slide,
    coords = coords,
    tile_size = tile_size,
    level = level,
    anchor = anchor,
    bounds = bounds
  )

  if (!is.null(prefix) && nrow(grid)) {
    grid$output_file <- sprintf(
      "%s_%s.%s",
      prefix,
      tools::file_path_sans_ext(basename(grid$output_file)),
      wsi_format_extension(format)
    )
  }

  manifest <- wsi_export_tiles(slide, grid, output_dir = output_dir, format = format, overwrite = overwrite)
  class(manifest) <- c("wsi_tile_manifest", setdiff(class(manifest), "wsi_tile_manifest"))
  manifest
}
