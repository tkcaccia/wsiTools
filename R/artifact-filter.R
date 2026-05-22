wsi_artifact_metric_names <- function() {
  c(
    "artifact_tile_width",
    "artifact_tile_height",
    "artifact_brightness_mean",
    "artifact_brightness_sd",
    "artifact_saturation_mean",
    "artifact_dark_fraction",
    "artifact_bright_fraction",
    "artifact_pen_fraction",
    "artifact_fold_fraction",
    "artifact_bubble_fraction",
    "artifact_edge_strength",
    "artifact_blur_score",
    "artifact_too_dark",
    "artifact_too_bright",
    "artifact_blur",
    "artifact_out_of_focus",
    "artifact_pen",
    "artifact_fold",
    "artifact_bubble",
    "artifact_score",
    "artifact_flag",
    "artifact_read_error"
  )
}

wsi_empty_artifact_metrics <- function(n = 0L) {
  data.frame(
    artifact_tile_width = integer(n),
    artifact_tile_height = integer(n),
    artifact_brightness_mean = numeric(n),
    artifact_brightness_sd = numeric(n),
    artifact_saturation_mean = numeric(n),
    artifact_dark_fraction = numeric(n),
    artifact_bright_fraction = numeric(n),
    artifact_pen_fraction = numeric(n),
    artifact_fold_fraction = numeric(n),
    artifact_bubble_fraction = numeric(n),
    artifact_edge_strength = numeric(n),
    artifact_blur_score = numeric(n),
    artifact_too_dark = logical(n),
    artifact_too_bright = logical(n),
    artifact_blur = logical(n),
    artifact_out_of_focus = logical(n),
    artifact_pen = logical(n),
    artifact_fold = logical(n),
    artifact_bubble = logical(n),
    artifact_score = numeric(n),
    artifact_flag = logical(n),
    artifact_read_error = logical(n),
    stringsAsFactors = FALSE
  )
}

#' Configure tile artifact detection thresholds
#'
#' Creates a threshold list for optional tile artifact detection. These
#' heuristics are intentionally lightweight and CRAN-safe: they inspect only the
#' requested tile or region array and do not require OpenSlide, libvips, Python,
#' or machine-learning models. They are best used to flag suspicious tiles for
#' review or to remove obvious low-quality patches before machine-learning
#' export.
#'
#' @param blur_threshold Minimum grayscale edge strength. Lower values are
#'   flagged as blur.
#' @param out_of_focus_threshold,out_of_focus_sd_threshold Stricter edge and
#'   texture thresholds used to flag out-of-focus regions.
#' @param dark_mean_threshold,bright_mean_threshold Mean brightness thresholds
#'   for very dark or very bright tiles.
#' @param dark_pixel_threshold,bright_pixel_threshold Pixel-level brightness
#'   thresholds used to compute dark and bright fractions.
#' @param dark_fraction_threshold,bright_fraction_threshold Tile fractions that
#'   trigger very dark or very bright flags.
#' @param pen_saturation_threshold Minimum saturation for pen-mark candidates.
#' @param pen_dominance Channel dominance ratio for red, green, or blue pen-like
#'   pixels.
#' @param pen_fraction_threshold Tile fraction that triggers the pen-mark flag.
#' @param fold_dark_threshold,fold_saturation_threshold Pixel thresholds for
#'   fold-like dark saturated regions.
#' @param fold_fraction_threshold Tile fraction that triggers the fold flag.
#' @param bubble_brightness_threshold,bubble_saturation_threshold Pixel
#'   thresholds for bubble-like bright low-saturation regions.
#' @param bubble_fraction_threshold Tile fraction that triggers the bubble flag.
#'
#' @return A `wsi_artifact_options` list.
#' @export
wsi_artifact_options <- function(blur_threshold = 0.012,
                                 out_of_focus_threshold = 0.008,
                                 out_of_focus_sd_threshold = 0.025,
                                 dark_mean_threshold = 0.12,
                                 bright_mean_threshold = 0.92,
                                 dark_pixel_threshold = 0.18,
                                 bright_pixel_threshold = 0.92,
                                 dark_fraction_threshold = 0.80,
                                 bright_fraction_threshold = 0.80,
                                 pen_saturation_threshold = 0.45,
                                 pen_dominance = 1.25,
                                 pen_fraction_threshold = 0.01,
                                 fold_dark_threshold = 0.28,
                                 fold_saturation_threshold = 0.25,
                                 fold_fraction_threshold = 0.08,
                                 bubble_brightness_threshold = 0.88,
                                 bubble_saturation_threshold = 0.12,
                                 bubble_fraction_threshold = 0.20) {
  out <- list(
    blur_threshold = blur_threshold,
    out_of_focus_threshold = out_of_focus_threshold,
    out_of_focus_sd_threshold = out_of_focus_sd_threshold,
    dark_mean_threshold = dark_mean_threshold,
    bright_mean_threshold = bright_mean_threshold,
    dark_pixel_threshold = dark_pixel_threshold,
    bright_pixel_threshold = bright_pixel_threshold,
    dark_fraction_threshold = dark_fraction_threshold,
    bright_fraction_threshold = bright_fraction_threshold,
    pen_saturation_threshold = pen_saturation_threshold,
    pen_dominance = pen_dominance,
    pen_fraction_threshold = pen_fraction_threshold,
    fold_dark_threshold = fold_dark_threshold,
    fold_saturation_threshold = fold_saturation_threshold,
    fold_fraction_threshold = fold_fraction_threshold,
    bubble_brightness_threshold = bubble_brightness_threshold,
    bubble_saturation_threshold = bubble_saturation_threshold,
    bubble_fraction_threshold = bubble_fraction_threshold
  )
  for (name in names(out)) {
    out[[name]] <- wsi_check_scalar_number(out[[name]], name)
  }
  class(out) <- c("wsi_artifact_options", "list")
  out
}

wsi_normalize_artifact_options <- function(options = NULL) {
  defaults <- wsi_artifact_options()
  if (is.null(options)) {
    return(defaults)
  }
  if (!is.list(options)) {
    wsi_abort("`artifact_options` must be a list created by `wsi_artifact_options()`.")
  }
  unknown <- setdiff(names(options), names(defaults))
  if (length(unknown)) {
    wsi_abort(sprintf(
      "`artifact_options` contains unknown setting%s: %s",
      if (length(unknown) == 1L) "" else "s",
      paste(unknown, collapse = ", ")
    ))
  }
  out <- defaults
  out[names(options)] <- options
  for (name in names(out)) {
    out[[name]] <- wsi_check_scalar_number(out[[name]], name)
  }
  class(out) <- c("wsi_artifact_options", "list")
  out
}

wsi_artifact_array <- function(tile) {
  if (inherits(tile, "magick-image")) {
    tile <- wsi_magick_to_array(tile)
  }

  if (inherits(tile, "raster")) {
    dims <- dim(tile)
    if (length(dims) != 2L) {
      wsi_abort("Raster artifact detection requires a two-dimensional raster image.")
    }
    rgba <- t(grDevices::col2rgb(as.vector(tile), alpha = TRUE) / 255)
    tile <- array(rgba, dim = c(dims[[1L]], dims[[2L]], 4L))
  }

  if (is.matrix(tile) && is.numeric(tile)) {
    tile <- array(as.numeric(tile), dim = c(nrow(tile), ncol(tile), 1L))
  }

  dims <- dim(tile)
  if (!is.array(tile) || length(dims) != 3L || dims[[1L]] < 1L || dims[[2L]] < 1L) {
    wsi_abort("`tile` must be an image array, raster, or magick image.")
  }

  arr <- as.numeric(tile)
  dim(arr) <- dims
  if (dims[[3L]] == 1L) {
    arr <- array(rep(arr[, , 1L], 3L), dim = c(dims[[1L]], dims[[2L]], 3L))
  }
  if (dim(arr)[[3L]] < 3L) {
    wsi_abort("Artifact detection requires at least one grayscale channel or RGB channels.")
  }
  max_value <- suppressWarnings(max(arr, na.rm = TRUE))
  if (is.finite(max_value) && max_value > 1) {
    arr <- arr / 255
  }
  arr[!is.finite(arr)] <- 0
  arr[arr < 0] <- 0
  arr[arr > 1] <- 1
  arr[, , seq_len(3L), drop = FALSE]
}

wsi_artifact_edge_strength <- function(gray) {
  if (nrow(gray) < 2L || ncol(gray) < 2L) {
    return(0)
  }
  dx <- abs(gray[, -1L, drop = FALSE] - gray[, -ncol(gray), drop = FALSE])
  dy <- abs(gray[-1L, , drop = FALSE] - gray[-nrow(gray), , drop = FALSE])
  mean(c(dx, dy), na.rm = TRUE)
}

wsi_pen_threshold_01 <- function(x, name) {
  x <- wsi_check_scalar_number(x, name)
  if (x > 1) {
    x <- x / 255
  }
  if (x > 1) {
    wsi_abort(sprintf("`%s` must be in 0..1 or 0..255 units.", name))
  }
  x
}

wsi_pen_edge_magnitude <- function(gray) {
  nr <- nrow(gray)
  nc <- ncol(gray)
  edge <- matrix(0, nr, nc)
  if (nr < 2L && nc < 2L) {
    return(edge)
  }
  if (nc >= 2L) {
    dx <- abs(gray[, -1L, drop = FALSE] - gray[, -nc, drop = FALSE])
    edge[, -1L] <- pmax(edge[, -1L], dx)
    edge[, -nc] <- pmax(edge[, -nc], dx)
  }
  if (nr >= 2L) {
    dy <- abs(gray[-1L, , drop = FALSE] - gray[-nr, , drop = FALSE])
    edge[-1L, ] <- pmax(edge[-1L, ], dy)
    edge[-nr, ] <- pmax(edge[-nr, ], dy)
  }
  edge
}

wsi_pen_mask_from_components <- function(mask, min_area = 1L) {
  components <- wsi_mask_component_list(mask, connectivity = "8", min_area = max(1L, min_area))
  filtered <- matrix(FALSE, nrow = nrow(mask), ncol = ncol(mask))
  for (component in components) {
    filtered[cbind(component[, "row"], component[, "col"])] <- TRUE
  }
  list(mask = filtered, components = components)
}

wsi_pen_tissue_matrix <- function(tissue_mask, image, estimate_tissue = TRUE) {
  if (is.null(tissue_mask)) {
    if (!isTRUE(estimate_tissue)) {
      return(NULL)
    }
    return(wsi_detect_tissue(image, min_area = 1L)$mask)
  }
  if (inherits(tissue_mask, "wsi_tissue_mask")) {
    tissue_mask <- tissue_mask$mask
  } else {
    tissue_mask <- wsi_mask_channel_matrix(tissue_mask)
  }
  tissue_mask <- !is.na(tissue_mask) & tissue_mask != 0
  dims <- dim(image)
  if (!identical(dim(tissue_mask), dims[1:2])) {
    wsi_abort("`tissue_mask` must have the same height and width as `image`.")
  }
  tissue_mask
}

wsi_pen_fraction <- function(mask, denominator = NULL) {
  if (is.null(denominator)) {
    return(mean(mask, na.rm = TRUE))
  }
  denom <- sum(denominator, na.rm = TRUE)
  if (!denom) {
    return(NA_real_)
  }
  sum(mask & denominator, na.rm = TRUE) / denom
}

wsi_artifact_metric_error_row <- function() {
  out <- wsi_empty_artifact_metrics(1L)
  out$artifact_tile_width <- NA_integer_
  out$artifact_tile_height <- NA_integer_
  numeric_cols <- vapply(out, is.numeric, logical(1))
  out[numeric_cols] <- lapply(out[numeric_cols], function(x) NA_real_)
  out$artifact_read_error <- TRUE
  out$artifact_flag <- TRUE
  out
}

#' Detect pen marks and ink-like artifacts in a small image
#'
#' Detects strongly colour-dominant blue, green, and red pen/marker pixels,
#' plus black ink-like dark components with visible edge content. The function
#' operates on the supplied image only, so it is suitable for slide thumbnails,
#' viewport captures, regions, or tiles without loading a whole-slide image into
#' memory.
#'
#' @param image RGB/RGBA array, raster, or magick image.
#' @param tissue_mask Optional logical/numeric matrix or `wsi_tissue_mask` with
#'   the same height and width as `image`. When supplied, tissue affected
#'   percentages are calculated against tissue pixels only.
#' @param estimate_tissue If `TRUE` and `tissue_mask` is `NULL`, estimate a
#'   simple tissue mask from `image` using [wsi_detect_tissue()].
#' @param channel_threshold Minimum dominant channel intensity for coloured ink.
#'   Values may be supplied in `0..1` or `0..255` units.
#' @param blue_red_ratio,blue_green_ratio Dominance ratios for blue pen pixels.
#' @param green_red_ratio,green_blue_ratio Dominance ratios for green marker
#'   pixels.
#' @param red_green_ratio,red_blue_ratio Dominance ratios for red pen pixels.
#' @param black_brightness_threshold Maximum grayscale brightness for black ink
#'   candidates, in `0..1` or `0..255` units.
#' @param black_edge_threshold Minimum local grayscale edge magnitude used to
#'   decide whether a dark component is ink-like.
#' @param black_edge_fraction_threshold Minimum fraction of edge-rich pixels
#'   inside a dark component before the whole dark component is classified as
#'   black ink.
#' @param min_area Minimum connected pen/ink component size in pixels.
#'
#' @return A `wsi_pen_mark_mask` object containing the combined `mask`, colour
#'   masks, total and tissue-aware affected percentages, and connected-component
#'   bounding boxes.
#' @export
#' @examples
#' img <- array(1, dim = c(32, 32, 3))
#' img[, 12:15, 1] <- 0.05
#' img[, 12:15, 2] <- 0.10
#' img[, 12:15, 3] <- 0.90
#' wsi_detect_pen_marks(img, estimate_tissue = FALSE, min_area = 1)
wsi_detect_pen_marks <- function(image,
                                 tissue_mask = NULL,
                                 estimate_tissue = TRUE,
                                 channel_threshold = 120,
                                 blue_red_ratio = 1.4,
                                 blue_green_ratio = 1.2,
                                 green_red_ratio = 1.3,
                                 green_blue_ratio = 1.3,
                                 red_green_ratio = 1.3,
                                 red_blue_ratio = 1.3,
                                 black_brightness_threshold = 45,
                                 black_edge_threshold = 35,
                                 black_edge_fraction_threshold = 0.05,
                                 min_area = 5) {
  arr <- wsi_artifact_array(image)
  r <- arr[, , 1L]
  g <- arr[, , 2L]
  b <- arr[, , 3L]
  channel_threshold <- wsi_pen_threshold_01(channel_threshold, "channel_threshold")
  black_brightness_threshold <- wsi_pen_threshold_01(black_brightness_threshold, "black_brightness_threshold")
  black_edge_threshold <- wsi_pen_threshold_01(black_edge_threshold, "black_edge_threshold")
  min_area <- as.integer(wsi_check_scalar_number(min_area, "min_area", allow_zero = FALSE))
  blue_red_ratio <- wsi_check_scalar_number(blue_red_ratio, "blue_red_ratio", allow_zero = FALSE)
  blue_green_ratio <- wsi_check_scalar_number(blue_green_ratio, "blue_green_ratio", allow_zero = FALSE)
  green_red_ratio <- wsi_check_scalar_number(green_red_ratio, "green_red_ratio", allow_zero = FALSE)
  green_blue_ratio <- wsi_check_scalar_number(green_blue_ratio, "green_blue_ratio", allow_zero = FALSE)
  red_green_ratio <- wsi_check_scalar_number(red_green_ratio, "red_green_ratio", allow_zero = FALSE)
  red_blue_ratio <- wsi_check_scalar_number(red_blue_ratio, "red_blue_ratio", allow_zero = FALSE)
  black_edge_fraction_threshold <- wsi_check_scalar_number(
    black_edge_fraction_threshold,
    "black_edge_fraction_threshold"
  )
  if (black_edge_fraction_threshold > 1) {
    wsi_abort("`black_edge_fraction_threshold` must be less than or equal to 1.")
  }

  blue <- b >= channel_threshold & b >= blue_red_ratio * r & b >= blue_green_ratio * g
  green <- g >= channel_threshold & g >= green_red_ratio * r & g >= green_blue_ratio * b
  red <- r >= channel_threshold & r >= red_green_ratio * g & r >= red_blue_ratio * b

  gray <- 0.299 * r + 0.587 * g + 0.114 * b
  dark <- gray <= black_brightness_threshold
  edge <- wsi_pen_edge_magnitude(gray)
  dark_components <- wsi_mask_component_list(dark, connectivity = "8", min_area = min_area)
  black <- matrix(FALSE, nrow = nrow(gray), ncol = ncol(gray))
  for (component in dark_components) {
    idx <- cbind(component[, "row"], component[, "col"])
    if (mean(edge[idx] >= black_edge_threshold, na.rm = TRUE) >= black_edge_fraction_threshold) {
      black[idx] <- TRUE
    }
  }

  combined_raw <- blue | green | red | black
  filtered <- wsi_pen_mask_from_components(combined_raw, min_area = min_area)
  mask <- filtered$mask
  tissue <- wsi_pen_tissue_matrix(tissue_mask, arr, estimate_tissue = estimate_tissue)

  blue <- blue & mask
  green <- green & mask
  red <- red & mask
  black <- black & mask

  pen_pixels <- sum(mask, na.rm = TRUE)
  tissue_pixels <- if (is.null(tissue)) NA_integer_ else as.integer(sum(tissue, na.rm = TRUE))
  tissue_pen_pixels <- if (is.null(tissue)) NA_integer_ else as.integer(sum(mask & tissue, na.rm = TRUE))
  pen_fraction <- mean(mask, na.rm = TRUE)
  tissue_affected_fraction <- if (is.null(tissue)) NA_real_ else wsi_pen_fraction(mask, tissue)
  component_bboxes <- wsi_tissue_component_bboxes(
    filtered$components,
    scale = c(x = 1, y = 1),
    origin = c(x = 0, y = 0)
  )

  structure(
    list(
      mask = mask,
      blue_mask = blue,
      green_mask = green,
      red_mask = red,
      black_ink_mask = black,
      tissue_mask = tissue,
      pen_pixel_count = as.integer(pen_pixels),
      blue_pixel_count = as.integer(sum(blue, na.rm = TRUE)),
      green_pixel_count = as.integer(sum(green, na.rm = TRUE)),
      red_pixel_count = as.integer(sum(red, na.rm = TRUE)),
      black_ink_pixel_count = as.integer(sum(black, na.rm = TRUE)),
      total_pixel_count = as.integer(length(mask)),
      tissue_pixel_count = tissue_pixels,
      tissue_pen_pixel_count = tissue_pen_pixels,
      pen_fraction = pen_fraction,
      pen_percentage = pen_fraction * 100,
      tissue_affected_fraction = tissue_affected_fraction,
      tissue_affected_percentage = tissue_affected_fraction * 100,
      blue_fraction = mean(blue, na.rm = TRUE),
      green_fraction = mean(green, na.rm = TRUE),
      red_fraction = mean(red, na.rm = TRUE),
      black_ink_fraction = mean(black, na.rm = TRUE),
      component_bboxes = component_bboxes,
      parameters = list(
        channel_threshold = channel_threshold,
        blue_red_ratio = blue_red_ratio,
        blue_green_ratio = blue_green_ratio,
        green_red_ratio = green_red_ratio,
        green_blue_ratio = green_blue_ratio,
        red_green_ratio = red_green_ratio,
        red_blue_ratio = red_blue_ratio,
        black_brightness_threshold = black_brightness_threshold,
        black_edge_threshold = black_edge_threshold,
        black_edge_fraction_threshold = black_edge_fraction_threshold,
        min_area = min_area,
        estimate_tissue = isTRUE(estimate_tissue)
      )
    ),
    class = "wsi_pen_mark_mask"
  )
}

#' Detect common tile artifacts from a small image region
#'
#' Computes simple, transparent artifact metrics for one tile or region. The
#' function can flag blur, out-of-focus regions, pen marks, fold-like dark
#' saturated regions, bubble-like bright low-saturation regions, and very
#' bright or dark tiles. It operates on the supplied tile only and is intended
#' for optional quality control, not diagnostic artifact classification.
#'
#' @param tile An image tile as an array, raster, or magick image. Arrays should
#'   be `height x width x channels` with grayscale, RGB, or RGBA values in
#'   `0..1` or `0..255`.
#' @param options Thresholds from [wsi_artifact_options()].
#'
#' @return A one-row data frame with artifact metrics and flags.
#' @export
#' @examples
#' tile <- array(1, dim = c(32, 32, 3))
#' wsi_detect_artifacts(tile)
wsi_detect_artifacts <- function(tile, options = wsi_artifact_options()) {
  options <- wsi_normalize_artifact_options(options)
  arr <- wsi_artifact_array(tile)
  r <- arr[, , 1L]
  g <- arr[, , 2L]
  b <- arr[, , 3L]
  brightness <- (r + g + b) / 3
  gray <- 0.299 * r + 0.587 * g + 0.114 * b
  max_rgb <- pmax(r, g, b)
  min_rgb <- pmin(r, g, b)
  saturation <- ifelse(max_rgb > 0, (max_rgb - min_rgb) / max_rgb, 0)

  brightness_mean <- mean(brightness, na.rm = TRUE)
  brightness_sd <- stats::sd(as.vector(brightness), na.rm = TRUE)
  if (!is.finite(brightness_sd)) {
    brightness_sd <- 0
  }
  saturation_mean <- mean(saturation, na.rm = TRUE)
  dark_fraction <- mean(brightness <= options$dark_pixel_threshold, na.rm = TRUE)
  bright_fraction <- mean(brightness >= options$bright_pixel_threshold, na.rm = TRUE)

  dominant_blue <- b >= options$pen_dominance * pmax(r, g) &
    saturation >= options$pen_saturation_threshold
  dominant_green <- g >= options$pen_dominance * pmax(r, b) &
    saturation >= options$pen_saturation_threshold
  dominant_red <- r >= options$pen_dominance * pmax(g, b) &
    saturation >= options$pen_saturation_threshold
  pen_fraction <- mean(dominant_blue | dominant_green | dominant_red, na.rm = TRUE)

  fold_fraction <- mean(
    brightness <= options$fold_dark_threshold &
      saturation >= options$fold_saturation_threshold,
    na.rm = TRUE
  )
  bubble_fraction <- mean(
    brightness >= options$bubble_brightness_threshold &
      saturation <= options$bubble_saturation_threshold,
    na.rm = TRUE
  )
  edge_strength <- wsi_artifact_edge_strength(gray)

  too_dark <- brightness_mean <= options$dark_mean_threshold ||
    dark_fraction >= options$dark_fraction_threshold
  too_bright <- brightness_mean >= options$bright_mean_threshold ||
    bright_fraction >= options$bright_fraction_threshold
  blur <- edge_strength <= options$blur_threshold
  out_of_focus <- edge_strength <= options$out_of_focus_threshold &&
    brightness_sd <= options$out_of_focus_sd_threshold
  pen <- pen_fraction >= options$pen_fraction_threshold
  fold <- fold_fraction >= options$fold_fraction_threshold
  bubble <- bubble_fraction >= options$bubble_fraction_threshold
  flags <- c(too_dark, too_bright, blur, out_of_focus, pen, fold, bubble)

  data.frame(
    artifact_tile_width = as.integer(dim(arr)[[2L]]),
    artifact_tile_height = as.integer(dim(arr)[[1L]]),
    artifact_brightness_mean = brightness_mean,
    artifact_brightness_sd = brightness_sd,
    artifact_saturation_mean = saturation_mean,
    artifact_dark_fraction = dark_fraction,
    artifact_bright_fraction = bright_fraction,
    artifact_pen_fraction = pen_fraction,
    artifact_fold_fraction = fold_fraction,
    artifact_bubble_fraction = bubble_fraction,
    artifact_edge_strength = edge_strength,
    artifact_blur_score = edge_strength,
    artifact_too_dark = isTRUE(too_dark),
    artifact_too_bright = isTRUE(too_bright),
    artifact_blur = isTRUE(blur),
    artifact_out_of_focus = isTRUE(out_of_focus),
    artifact_pen = isTRUE(pen),
    artifact_fold = isTRUE(fold),
    artifact_bubble = isTRUE(bubble),
    artifact_score = mean(flags),
    artifact_flag = any(flags),
    artifact_read_error = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Flag or filter artifact tiles in a grid
#'
#' Reads only the regions listed in `grid`, computes artifact metrics for each
#' tile, appends artifact columns, and optionally drops flagged tiles. This is a
#' tile-by-tile quality-control step; it never loads an entire whole-slide image
#' into R memory.
#'
#' @param slide A `wsi_slide` object.
#' @param grid Tile grid from [wsi_tile_grid()] or [extract_tiles()].
#' @param options Thresholds from [wsi_artifact_options()].
#' @param action `"flag"` keeps all rows with artifact columns; `"drop"` removes
#'   rows where `artifact_flag` is `TRUE`.
#'
#' @return A tile grid or manifest with artifact metrics.
#' @export
wsi_flag_artifacts <- function(slide, grid, options = wsi_artifact_options(),
                               action = c("flag", "drop")) {
  wsi_check_slide(slide)
  action <- match.arg(action)
  options <- wsi_normalize_artifact_options(options)
  needed <- c("x", "y", "width", "height")
  if (!is.data.frame(grid) || !all(needed %in% names(grid))) {
    wsi_abort("`grid` must be a data frame with `x`, `y`, `width`, and `height` columns.")
  }
  if (!"level" %in% names(grid)) {
    grid$level <- 0L
  }

  metrics <- if (!nrow(grid)) {
    wsi_empty_artifact_metrics(0L)
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
          wsi_detect_artifacts(tile, options = options)
        },
        error = function(err) {
          tile_label <- if ("tile_id" %in% names(grid)) grid$tile_id[[i]] else i
          read_errors <<- c(read_errors, sprintf("%s: %s", tile_label %||% i, conditionMessage(err)))
          wsi_artifact_metric_error_row()
        }
      )
    }
    if (length(read_errors)) {
      wsi_warn(sprintf(
        "Artifact detection could not read %s tile%s; those rows were flagged as artifacts.",
        length(read_errors),
        if (length(read_errors) == 1L) "" else "s"
      ))
    }
    do.call(rbind, rows)
  }

  artifact_cols <- wsi_artifact_metric_names()
  grid <- grid[, setdiff(names(grid), artifact_cols), drop = FALSE]
  out <- cbind(grid, metrics)
  if (identical(action, "drop") && nrow(out)) {
    out <- out[is.na(out$artifact_flag) | !out$artifact_flag, , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

#' @rdname wsi_flag_artifacts
#' @export
wsi_filter_artifact_tiles <- function(slide, grid, options = wsi_artifact_options(),
                                      action = c("drop", "flag")) {
  action <- match.arg(action)
  wsi_flag_artifacts(slide, grid, options = options, action = action)
}

wsi_apply_artifact_filter <- function(slide, grid, artifact_filter = FALSE,
                                      artifact_action = c("flag", "drop"),
                                      artifact_options = wsi_artifact_options()) {
  artifact_action <- match.arg(artifact_action)
  if (!isTRUE(artifact_filter)) {
    return(grid)
  }
  wsi_flag_artifacts(slide, grid, options = artifact_options, action = artifact_action)
}
