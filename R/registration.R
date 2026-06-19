wsi_transform_matrix <- function(transform) {
  if (inherits(transform, "wsi_affine_transform")) {
    return(transform$matrix)
  }
  if (is.matrix(transform) && all(dim(transform) == c(3L, 3L))) {
    return(transform)
  }
  wsi_abort("`transform` must be a `wsi_affine_transform` object or a 3 x 3 affine matrix.")
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
