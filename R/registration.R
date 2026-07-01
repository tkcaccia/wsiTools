wsi_transform_matrix <- function(transform) {
  if (inherits(transform, "wsi_affine_transform")) {
    return(transform$matrix)
  }
  if (is.matrix(transform) && all(dim(transform) == c(3L, 3L))) {
    return(transform)
  }
  wsi_abort("`transform` must be a `wsi_affine_transform` object or a 3 x 3 affine matrix.")
}

#' Create a translation transform
#'
#' @param dx,dy Translation in slide pixels.
#'
#' @return A `wsi_affine_transform` object.
#' @export
wsi_translation_transform <- function(dx = 0, dy = 0) {
  if (!is.numeric(dx) || length(dx) != 1L || is.na(dx) || !is.finite(dx)) {
    wsi_abort("`dx` must be a single finite number.")
  }
  if (!is.numeric(dy) || length(dy) != 1L || is.na(dy) || !is.finite(dy)) {
    wsi_abort("`dy` must be a single finite number.")
  }
  dx <- as.numeric(dx)
  dy <- as.numeric(dy)
  structure(
    list(
      matrix = matrix(c(1, 0, dx, 0, 1, dy, 0, 0, 1), nrow = 3, byrow = TRUE),
      dx = dx,
      dy = dy
    ),
    class = "wsi_affine_transform"
  )
}

#' Estimate a simple annotation-to-tissue translation
#'
#' Estimates a global x/y offset that moves centroid coordinates onto tissue in a
#' low-resolution thumbnail. This is useful for repairing small global
#' registration offsets in cached cell masks or centroid tables without loading
#' the whole-slide image into memory. It intentionally estimates translation only;
#' use [estimate_transform()] when landmark-based rotation, scaling, or shearing
#' is required.
#'
#' @param thumbnail Low-resolution tissue thumbnail as a logical tissue mask, a
#'   numeric matrix, a 3-channel RGB array, a `magick-image`, or an image path.
#' @param points Data frame or matrix with `x` and `y` centroid coordinates in
#'   level-0 slide pixels.
#' @param slide_width,slide_height Full-resolution slide dimensions in pixels.
#' @param sample_n Maximum number of points sampled for scoring.
#' @param max_shift Maximum translation searched in thumbnail pixels. Values
#'   between 0 and 1 are interpreted as a fraction of thumbnail width/height.
#' @param coarse_step,refine_radius,refine_step Search parameters in thumbnail
#'   pixels.
#' @param saturation_threshold,brightness_threshold Thresholds used to derive a
#'   tissue mask from RGB thumbnails.
#' @param seed Optional sampling seed.
#'
#' @return A `wsi_annotation_translation` object containing `dx`, `dy`, `score`,
#'   thumbnail-space offsets, and a `transform` usable by
#'   [wsi_geojson_to_mask_tiff()].
#' @export
wsi_estimate_tissue_translation <- function(thumbnail,
                                            points,
                                            slide_width,
                                            slide_height,
                                            sample_n = 20000L,
                                            max_shift = 0.25,
                                            coarse_step = 20,
                                            refine_radius = 25,
                                            refine_step = 1,
                                            saturation_threshold = 0.035,
                                            brightness_threshold = 0.985,
                                            seed = 1L) {
  tissue <- wsi_registration_tissue_mask(
    thumbnail,
    saturation_threshold = saturation_threshold,
    brightness_threshold = brightness_threshold
  )
  pts <- wsi_points_matrix(points, "points")
  slide_width <- as.numeric(wsi_check_scalar_number(slide_width, "slide_width", allow_zero = FALSE))
  slide_height <- as.numeric(wsi_check_scalar_number(slide_height, "slide_height", allow_zero = FALSE))
  sample_n <- as.integer(wsi_check_scalar_number(sample_n, "sample_n", allow_zero = FALSE))
  coarse_step <- as.numeric(wsi_check_scalar_number(coarse_step, "coarse_step", allow_zero = FALSE))
  refine_radius <- as.numeric(wsi_check_scalar_number(refine_radius, "refine_radius", allow_zero = TRUE))
  refine_step <- as.numeric(wsi_check_scalar_number(refine_step, "refine_step", allow_zero = FALSE))
  max_shift <- as.numeric(wsi_check_scalar_number(max_shift, "max_shift", allow_zero = FALSE))
  if (!is.null(seed)) {
    seed <- as.integer(wsi_check_scalar_number(seed, "seed", allow_zero = TRUE))
  }

  pts <- pts[is.finite(pts[, 1L]) & is.finite(pts[, 2L]), , drop = FALSE]
  if (!nrow(pts)) {
    wsi_abort("`points` does not contain any finite x/y coordinates.")
  }
  if (nrow(pts) > sample_n) {
    if (!is.null(seed)) {
      old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        get(".Random.seed", envir = .GlobalEnv)
      } else {
        NULL
      }
      on.exit({
        if (is.null(old_seed)) {
          rm(".Random.seed", envir = .GlobalEnv)
        } else {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        }
      }, add = TRUE)
      set.seed(seed)
    }
    pts <- pts[sample.int(nrow(pts), sample_n), , drop = FALSE]
  }

  thumb_height <- nrow(tissue)
  thumb_width <- ncol(tissue)
  px <- as.integer(round(pts[, 1L] * thumb_width / slide_width))
  py <- as.integer(round(pts[, 2L] * thumb_height / slide_height))
  max_x <- if (max_shift <= 1) ceiling(thumb_width * max_shift) else ceiling(max_shift)
  max_y <- if (max_shift <= 1) ceiling(thumb_height * max_shift) else ceiling(max_shift)

  score_offset <- function(dx, dy) {
    x <- px + as.integer(dx)
    y <- py + as.integer(dy)
    ok <- is.finite(x) & is.finite(y) & x >= 1L & x <= thumb_width & y >= 1L & y <= thumb_height
    if (!any(ok)) {
      return(0)
    }
    mean(tissue[cbind(y[ok], x[ok])]) * mean(ok)
  }

  best <- c(score = -Inf, dx = 0, dy = 0)
  for (dx in seq(-max_x, max_x, by = coarse_step)) {
    for (dy in seq(-max_y, max_y, by = coarse_step)) {
      score <- score_offset(dx, dy)
      if (score > best[["score"]]) {
        best <- c(score = score, dx = dx, dy = dy)
      }
    }
  }
  if (refine_radius > 0) {
    for (dx in seq(best[["dx"]] - refine_radius, best[["dx"]] + refine_radius, by = refine_step)) {
      for (dy in seq(best[["dy"]] - refine_radius, best[["dy"]] + refine_radius, by = refine_step)) {
        score <- score_offset(dx, dy)
        if (score > best[["score"]]) {
          best <- c(score = score, dx = dx, dy = dy)
        }
      }
    }
  }

  slide_dx <- best[["dx"]] * slide_width / thumb_width
  slide_dy <- best[["dy"]] * slide_height / thumb_height
  transform <- wsi_translation_transform(slide_dx, slide_dy)
  structure(
    list(
      dx = slide_dx,
      dy = slide_dy,
      score = best[["score"]],
      thumbnail_dx = best[["dx"]],
      thumbnail_dy = best[["dy"]],
      thumbnail_width = thumb_width,
      thumbnail_height = thumb_height,
      slide_width = slide_width,
      slide_height = slide_height,
      transform = transform,
      sampled_points = nrow(pts)
    ),
    class = c("wsi_annotation_translation", "list")
  )
}

wsi_registration_tissue_mask <- function(thumbnail,
                                         saturation_threshold = 0.035,
                                         brightness_threshold = 0.985) {
  if (is.matrix(thumbnail)) {
    return(thumbnail > 0)
  }
  if (is.character(thumbnail) && length(thumbnail) == 1L) {
    thumbnail <- wsi_validate_input_path(thumbnail)
    if (!requireNamespace("magick", quietly = TRUE)) {
      wsi_abort("Reading image thumbnails requires the optional `magick` package.")
    }
    thumbnail <- magick::image_read(thumbnail)
  }
  if (inherits(thumbnail, "magick-image")) {
    raw <- magick::image_data(thumbnail, channels = "rgb")
    thumbnail <- array(as.integer(raw), dim = dim(raw)) / 255
  }
  if (!is.array(thumbnail) || length(dim(thumbnail)) != 3L) {
    wsi_abort("`thumbnail` must be a logical/numeric matrix, RGB array, magick image, or image path.")
  }
  dims <- dim(thumbnail)
  if (dims[[1L]] == 3L || dims[[1L]] == 4L) {
    red <- t(thumbnail[1L, , ])
    green <- t(thumbnail[2L, , ])
    blue <- t(thumbnail[3L, , ])
  } else if (dims[[3L]] >= 3L) {
    red <- thumbnail[, , 1L]
    green <- thumbnail[, , 2L]
    blue <- thumbnail[, , 3L]
  } else {
    wsi_abort("`thumbnail` RGB arrays must contain at least three channels.")
  }
  max_channel <- pmax(red, green, blue)
  min_channel <- pmin(red, green, blue)
  saturation <- ifelse(max_channel == 0, 0, (max_channel - min_channel) / max_channel)
  saturation > saturation_threshold & max_channel < brightness_threshold
}

#' @export
print.wsi_annotation_translation <- function(x, ...) {
  cat("<wsi_annotation_translation>\n")
  cat("  dx:    ", signif(x$dx, 6), " px\n", sep = "")
  cat("  dy:    ", signif(x$dy, 6), " px\n", sep = "")
  cat("  score: ", signif(x$score, 5), "\n", sep = "")
  invisible(x)
}

#' Estimate an affine transform from landmark points
#'
#' @param landmarks1,landmarks2 Matching landmark coordinates as data frames or
#'   matrices with x/y columns.
#'
#' @return A `wsi_affine_transform` object.
#' @export
estimate_transform <- function(landmarks1, landmarks2) {
  from <- wsi_points_matrix(landmarks1, "landmarks1")
  to <- wsi_points_matrix(landmarks2, "landmarks2")
  if (nrow(from) != nrow(to)) {
    wsi_abort("`landmarks1` and `landmarks2` must contain the same number of points.")
  }
  if (nrow(from) < 3L) {
    wsi_abort("At least three landmark pairs are required to estimate an affine transform.")
  }

  design <- cbind(from[, 1L], from[, 2L], 1)
  coef_x <- stats::lm.fit(design, to[, 1L])$coefficients
  coef_y <- stats::lm.fit(design, to[, 2L])$coefficients
  matrix <- rbind(
    c(coef_x[[1L]], coef_x[[2L]], coef_x[[3L]]),
    c(coef_y[[1L]], coef_y[[2L]], coef_y[[3L]]),
    c(0, 0, 1)
  )
  structure(
    list(
      matrix = matrix,
      landmarks1 = from,
      landmarks2 = to
    ),
    class = "wsi_affine_transform"
  )
}

#' @rdname estimate_transform
#' @export
wsi_estimate_transform <- estimate_transform

wsi_orientation_matrix <- function(width, height, flip = c("none", "horizontal", "vertical", "both"),
                                   rotation = c(0, 90, 180, 270)) {
  flip <- match.arg(flip)
  rotation <- as.integer(rotation[[1L]])
  if (!rotation %in% c(0L, 90L, 180L, 270L)) {
    wsi_abort("`rotation` must be one of 0, 90, 180, or 270.")
  }
  width <- as.numeric(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  height <- as.numeric(wsi_check_scalar_number(height, "height", allow_zero = FALSE))

  flip_mat <- diag(3)
  if (flip %in% c("horizontal", "both")) {
    flip_mat <- matrix(c(-1, 0, width, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE) %*% flip_mat
  }
  if (flip %in% c("vertical", "both")) {
    flip_mat <- matrix(c(1, 0, 0, 0, -1, height, 0, 0, 1), nrow = 3, byrow = TRUE) %*% flip_mat
  }

  rot_mat <- switch(
    as.character(rotation),
    "0" = diag(3),
    "90" = matrix(c(0, -1, height, 1, 0, 0, 0, 0, 1), nrow = 3, byrow = TRUE),
    "180" = matrix(c(-1, 0, width, 0, -1, height, 0, 0, 1), nrow = 3, byrow = TRUE),
    "270" = matrix(c(0, 1, 0, -1, 0, width, 0, 0, 1), nrow = 3, byrow = TRUE)
  )
  rot_mat %*% flip_mat
}

#' Create a slide-orientation affine transform
#'
#' Creates an affine transform for common image-coordinate orientation fixes,
#' such as mirroring annotations into the level-0 slide coordinate system before
#' converting them to a mask or overlaying them in the viewer.
#'
#' @param width,height Full-resolution slide width and height in pixels.
#' @param flip One of `"none"`, `"horizontal"`, `"vertical"`, or `"both"`.
#' @param rotation Clockwise rotation in degrees: `0`, `90`, `180`, or `270`.
#'
#' @return A `wsi_affine_transform` object.
#' @export
wsi_orientation_transform <- function(width, height,
                                      flip = c("none", "horizontal", "vertical", "both"),
                                      rotation = c(0, 90, 180, 270)) {
  flip <- match.arg(flip)
  matrix <- wsi_orientation_matrix(width = width, height = height, flip = flip, rotation = rotation)
  structure(
    list(
      matrix = matrix,
      width = width,
      height = height,
      flip = flip,
      rotation = as.integer(rotation[[1L]])
    ),
    class = "wsi_affine_transform"
  )
}

#' Transform ROI coordinates with an affine transform
#'
#' @param rois A `wsi_roi` object.
#' @param transform A transform from [estimate_transform()].
#'
#' @return A transformed `wsi_roi` object.
#' @export
transform_rois <- function(rois, transform) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  out <- rois
  out$coordinates <- I(lapply(out$coordinates, wsi_transform_coordinates, transform = transform))
  for (i in seq_len(nrow(out))) {
    points <- wsi_collect_points(out$coordinates[[i]])
    out$xmin[[i]] <- min(points[, 1L])
    out$ymin[[i]] <- min(points[, 2L])
    out$xmax[[i]] <- max(points[, 1L])
    out$ymax[[i]] <- max(points[, 2L])
  }
  class(out) <- class(rois)
  out
}

#' @rdname transform_rois
#' @export
wsi_transform_rois <- transform_rois

#' @export
print.wsi_affine_transform <- function(x, ...) {
  cat("<wsi_affine_transform>\n")
  print(x$matrix)
  invisible(x)
}
