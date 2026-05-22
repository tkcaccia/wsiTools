wsi_whitespace_metric_names <- function() {
  c(
    "whitespace_tile_width",
    "whitespace_tile_height",
    "whitespace_brightness_mean",
    "whitespace_saturation_mean",
    "whitespace_fraction",
    "whitespace_tissue_like_fraction",
    "whitespace_flag",
    "whitespace_read_error",
    "background_fraction",
    "background_flag"
  )
}

wsi_empty_whitespace_metrics <- function(n = 0L) {
  data.frame(
    whitespace_tile_width = integer(n),
    whitespace_tile_height = integer(n),
    whitespace_brightness_mean = numeric(n),
    whitespace_saturation_mean = numeric(n),
    whitespace_fraction = numeric(n),
    whitespace_tissue_like_fraction = numeric(n),
    whitespace_flag = logical(n),
    whitespace_read_error = logical(n),
    background_fraction = numeric(n),
    background_flag = logical(n),
    stringsAsFactors = FALSE
  )
}

#' Configure whitespace/background detection thresholds
#'
#' Creates threshold settings for optional tile-level whitespace/background
#' labelling. The detector is intentionally simple and dependency-free: it
#' flags pixels that are bright and low-saturation, then summarises the fraction
#' of such pixels per tile. It is useful for ML manifests and background
#' filtering, but it is not a tissue segmentation model.
#'
#' @param brightness_threshold Pixel brightness threshold for whitespace.
#' @param saturation_threshold Pixel saturation threshold for whitespace.
#' @param whitespace_fraction_threshold Tile fraction that triggers
#'   `whitespace_flag`.
#' @param brightness_mean_threshold Mean tile brightness threshold that can also
#'   trigger a whitespace flag when the tile is low saturation.
#' @param saturation_mean_threshold Mean tile saturation threshold paired with
#'   `brightness_mean_threshold`.
#'
#' @return A `wsi_whitespace_options` list.
#' @export
wsi_whitespace_options <- function(brightness_threshold = 0.86,
                                   saturation_threshold = 0.12,
                                   whitespace_fraction_threshold = 0.75,
                                   brightness_mean_threshold = 0.90,
                                   saturation_mean_threshold = 0.10) {
  out <- list(
    brightness_threshold = brightness_threshold,
    saturation_threshold = saturation_threshold,
    whitespace_fraction_threshold = whitespace_fraction_threshold,
    brightness_mean_threshold = brightness_mean_threshold,
    saturation_mean_threshold = saturation_mean_threshold
  )
  for (name in names(out)) {
    out[[name]] <- wsi_check_scalar_number(out[[name]], name)
    if (out[[name]] > 1) {
      wsi_abort(sprintf("`%s` must be less than or equal to 1.", name))
    }
  }
  class(out) <- c("wsi_whitespace_options", "list")
  out
}

wsi_normalize_whitespace_options <- function(options = NULL) {
  defaults <- wsi_whitespace_options()
  if (is.null(options)) {
    return(defaults)
  }
  if (!is.list(options)) {
    wsi_abort("`whitespace_options` must be a list created by `wsi_whitespace_options()`.")
  }
  unknown <- setdiff(names(options), names(defaults))
  if (length(unknown)) {
    wsi_abort(sprintf(
      "`whitespace_options` contains unknown setting%s: %s",
      if (length(unknown) == 1L) "" else "s",
      paste(unknown, collapse = ", ")
    ))
  }
  out <- defaults
  out[names(options)] <- options
  for (name in names(out)) {
    out[[name]] <- wsi_check_scalar_number(out[[name]], name)
    if (out[[name]] > 1) {
      wsi_abort(sprintf("`%s` must be less than or equal to 1.", name))
    }
  }
  class(out) <- c("wsi_whitespace_options", "list")
  out
}

wsi_whitespace_metric_error_row <- function() {
  out <- wsi_empty_whitespace_metrics(1L)
  out$whitespace_tile_width <- NA_integer_
  out$whitespace_tile_height <- NA_integer_
  numeric_cols <- vapply(out, is.numeric, logical(1))
  out[numeric_cols] <- lapply(out[numeric_cols], function(x) NA_real_)
  out$whitespace_flag <- TRUE
  out$background_flag <- TRUE
  out$whitespace_read_error <- TRUE
  out
}

#' Detect whitespace/background in a tile
#'
#' Computes simple bright-low-saturation background metrics for one image tile.
#' The function accepts an array, raster, or magick image and returns a one-row
#' data frame suitable for appending to tile manifests.
#'
#' @param tile An image tile as an array, raster, or magick image.
#' @param options Thresholds from [wsi_whitespace_options()].
#'
#' @return A one-row data frame with whitespace/background metrics.
#' @export
#' @examples
#' tile <- array(1, dim = c(32, 32, 3))
#' wsi_detect_whitespace(tile)
wsi_detect_whitespace <- function(tile, options = wsi_whitespace_options()) {
  options <- wsi_normalize_whitespace_options(options)
  arr <- wsi_artifact_array(tile)
  r <- arr[, , 1L]
  g <- arr[, , 2L]
  b <- arr[, , 3L]
  brightness <- (r + g + b) / 3
  max_rgb <- pmax(r, g, b)
  min_rgb <- pmin(r, g, b)
  saturation <- ifelse(max_rgb > 0, (max_rgb - min_rgb) / max_rgb, 0)
  brightness_mean <- mean(brightness, na.rm = TRUE)
  saturation_mean <- mean(saturation, na.rm = TRUE)
  whitespace_pixels <- brightness >= options$brightness_threshold &
    saturation <= options$saturation_threshold
  whitespace_fraction <- mean(whitespace_pixels, na.rm = TRUE)
  whitespace_flag <- whitespace_fraction >= options$whitespace_fraction_threshold ||
    (brightness_mean >= options$brightness_mean_threshold &&
       saturation_mean <= options$saturation_mean_threshold)

  data.frame(
    whitespace_tile_width = as.integer(dim(arr)[[2L]]),
    whitespace_tile_height = as.integer(dim(arr)[[1L]]),
    whitespace_brightness_mean = brightness_mean,
    whitespace_saturation_mean = saturation_mean,
    whitespace_fraction = whitespace_fraction,
    whitespace_tissue_like_fraction = 1 - whitespace_fraction,
    whitespace_flag = isTRUE(whitespace_flag),
    whitespace_read_error = FALSE,
    background_fraction = whitespace_fraction,
    background_flag = isTRUE(whitespace_flag),
    stringsAsFactors = FALSE
  )
}

#' Flag or filter whitespace/background tiles in a grid
#'
#' Reads only the regions listed in `grid`, computes whitespace/background
#' metrics for each tile, appends manifest columns, and optionally drops tiles
#' where `whitespace_flag` is `TRUE`. This is region-based and never reads a
#' whole-slide image into memory.
#'
#' @param slide A `wsi_slide` object.
#' @param grid Tile grid from [wsi_tile_grid()] or [extract_tiles()].
#' @param options Thresholds from [wsi_whitespace_options()].
#' @param action `"flag"` keeps all rows and appends columns; `"drop"` removes
#'   rows where `whitespace_flag` is `TRUE`.
#'
#' @return A tile grid or manifest with whitespace/background metrics.
#' @export
wsi_flag_whitespace <- function(slide, grid, options = wsi_whitespace_options(),
                                action = c("flag", "drop")) {
  wsi_check_slide(slide)
  action <- match.arg(action)
  options <- wsi_normalize_whitespace_options(options)
  needed <- c("x", "y", "width", "height")
  if (!is.data.frame(grid) || !all(needed %in% names(grid))) {
    wsi_abort("`grid` must be a data frame with `x`, `y`, `width`, and `height` columns.")
  }
  if (!"level" %in% names(grid)) {
    grid$level <- 0L
  }

  metrics <- if (!nrow(grid)) {
    wsi_empty_whitespace_metrics(0L)
  } else {
    rows <- vector("list", nrow(grid))
    read_errors <- character()
    for (i in seq_len(nrow(grid))) {
      rows[[i]] <- tryCatch(
        {
          tile <- wsi_read_region(
            slide,
            x = grid$x[[i]],
            y = grid$y[[i]],
            width = grid$width[[i]],
            height = grid$height[[i]],
            level = grid$level[[i]],
            format = "array"
          )
          wsi_detect_whitespace(tile, options = options)
        },
        error = function(err) {
          tile_label <- if ("tile_id" %in% names(grid)) grid$tile_id[[i]] else i
          read_errors <<- c(read_errors, sprintf("%s: %s", tile_label %||% i, conditionMessage(err)))
          wsi_whitespace_metric_error_row()
        }
      )
    }
    if (length(read_errors)) {
      wsi_warn(sprintf(
        "Whitespace detection could not read %s tile%s; those rows were flagged as background.",
        length(read_errors),
        if (length(read_errors) == 1L) "" else "s"
      ))
    }
    do.call(rbind, rows)
  }

  whitespace_cols <- wsi_whitespace_metric_names()
  grid <- grid[, setdiff(names(grid), whitespace_cols), drop = FALSE]
  out <- cbind(grid, metrics)
  if (identical(action, "drop") && nrow(out)) {
    out <- out[is.na(out$whitespace_flag) | !out$whitespace_flag, , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

#' @rdname wsi_flag_whitespace
#' @export
wsi_filter_whitespace_tiles <- function(slide, grid, options = wsi_whitespace_options(),
                                        action = c("drop", "flag")) {
  action <- match.arg(action)
  wsi_flag_whitespace(slide, grid, options = options, action = action)
}

wsi_apply_whitespace_filter <- function(slide, grid, whitespace_filter = FALSE,
                                        whitespace_action = c("flag", "drop"),
                                        whitespace_options = wsi_whitespace_options()) {
  whitespace_action <- match.arg(whitespace_action)
  if (!isTRUE(whitespace_filter)) {
    return(grid)
  }
  wsi_flag_whitespace(slide, grid, options = whitespace_options, action = whitespace_action)
}
