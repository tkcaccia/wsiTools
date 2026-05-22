wsi_points_matrix <- function(x, name = "points") {
  if (is.data.frame(x)) {
    if (!all(c("x", "y") %in% names(x))) {
      wsi_abort(sprintf("`%s` must contain `x` and `y` columns.", name))
    }
    mat <- as.matrix(x[, c("x", "y"), drop = FALSE])
  } else if (is.matrix(x)) {
    if (ncol(x) < 2L) {
      wsi_abort(sprintf("`%s` must have at least two columns.", name))
    }
    mat <- x[, seq_len(2L), drop = FALSE]
  } else if (is.numeric(x) && length(x) >= 2L) {
    mat <- matrix(as.numeric(x[seq_len(2L)]), ncol = 2L)
  } else {
    wsi_abort(sprintf("`%s` must be a data frame, matrix, or numeric coordinate pair.", name))
  }
  storage.mode(mat) <- "double"
  if (anyNA(mat) || any(!is.finite(mat))) {
    wsi_abort(sprintf("`%s` coordinates must be finite numeric values.", name))
  }
  colnames(mat) <- c("x", "y")
  mat
}

wsi_geojson_ring_matrix <- function(ring) {
  pts <- lapply(ring, function(point) as.numeric(point[seq_len(2L)]))
  mat <- do.call(rbind, pts)
  colnames(mat) <- c("x", "y")
  mat
}

wsi_roi_polygons <- function(roi, index = 1L) {
  if (!inherits(roi, "wsi_roi")) {
    wsi_abort("`roi` must be a `wsi_roi` object.")
  }
  geometry_type <- roi$geometry_type[[index]]
  coords <- roi$coordinates[[index]]
  if (identical(geometry_type, "Polygon")) {
    return(list(lapply(coords, wsi_geojson_ring_matrix)))
  }
  if (identical(geometry_type, "MultiPolygon")) {
    return(lapply(coords, function(polygon) {
      lapply(polygon, wsi_geojson_ring_matrix)
    }))
  }
  wsi_abort(sprintf("Geometry type `%s` is not polygonal.", geometry_type %||% NA_character_))
}

wsi_roi_rings <- function(roi, index = 1L) {
  unlist(wsi_roi_polygons(roi, index), recursive = FALSE)
}

wsi_ring_area <- function(ring) {
  if (nrow(ring) < 3L) {
    return(0)
  }
  x <- ring[, 1L]
  y <- ring[, 2L]
  j <- c(seq_len(nrow(ring))[-1L], 1L)
  abs(sum(x * y[j] - x[j] * y) / 2)
}

wsi_roi_area_px <- function(roi, index = 1L) {
  polygons <- wsi_roi_polygons(roi, index)
  if (!length(polygons)) {
    return(0)
  }
  areas <- vapply(polygons, function(rings) {
    if (!length(rings)) {
      return(0)
    }
    outer <- wsi_ring_area(rings[[1L]])
    holes <- if (length(rings) > 1L) sum(vapply(rings[-1L], wsi_ring_area, numeric(1))) else 0
    max(0, outer - holes)
  }, numeric(1))
  sum(areas)
}

wsi_point_in_ring <- function(point, ring) {
  x <- point[[1L]]
  y <- point[[2L]]
  inside <- FALSE
  n <- nrow(ring)
  j <- n
  for (i in seq_len(n)) {
    xi <- ring[i, 1L]
    yi <- ring[i, 2L]
    xj <- ring[j, 1L]
    yj <- ring[j, 2L]
    hit <- ((yi > y) != (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
    if (hit) {
      inside <- !inside
    }
    j <- i
  }
  inside
}

wsi_point_in_roi <- function(point, roi, index = 1L) {
  polygons <- wsi_roi_polygons(roi, index)
  if (!length(polygons)) {
    return(FALSE)
  }
  any(vapply(polygons, function(rings) {
    if (!length(rings) || !wsi_point_in_ring(point, rings[[1L]])) {
      return(FALSE)
    }
    if (length(rings) == 1L) {
      return(TRUE)
    }
    !any(vapply(rings[-1L], function(ring) wsi_point_in_ring(point, ring), logical(1)))
  }, logical(1)))
}

wsi_point_segment_distance <- function(point, a, b) {
  ab <- b - a
  denom <- sum(ab^2)
  if (denom <= .Machine$double.eps) {
    return(sqrt(sum((point - a)^2)))
  }
  t <- max(0, min(1, sum((point - a) * ab) / denom))
  projection <- a + t * ab
  sqrt(sum((point - projection)^2))
}

wsi_point_ring_distance <- function(point, ring) {
  n <- nrow(ring)
  if (n < 2L) {
    return(NA_real_)
  }
  distances <- vapply(seq_len(n - 1L), function(i) {
    wsi_point_segment_distance(point, ring[i, ], ring[i + 1L, ])
  }, numeric(1))
  min(distances, na.rm = TRUE)
}

wsi_point_roi_boundary_distance <- function(point, roi, index = 1L) {
  rings <- wsi_roi_rings(roi, index)
  distances <- vapply(rings, function(ring) wsi_point_ring_distance(point, ring), numeric(1))
  min(distances, na.rm = TRUE)
}

wsi_transform_point <- function(point, transform) {
  mat <- wsi_transform_matrix(transform)
  out <- mat %*% c(point[[1L]], point[[2L]], 1)
  c(out[[1L]], out[[2L]])
}

wsi_transform_coordinates <- function(coords, transform) {
  if (is.numeric(coords) && length(coords) >= 2L) {
    point <- wsi_transform_point(coords, transform)
    coords[[1L]] <- point[[1L]]
    coords[[2L]] <- point[[2L]]
    return(coords)
  }
  if (is.list(coords) && length(coords) >= 2L && all(vapply(coords[1:2], is.numeric, logical(1)))) {
    point <- wsi_transform_point(as.numeric(unlist(coords[1:2], use.names = FALSE)), transform)
    coords[[1L]] <- point[[1L]]
    coords[[2L]] <- point[[2L]]
    return(coords)
  }
  if (is.list(coords)) {
    return(lapply(coords, wsi_transform_coordinates, transform = transform))
  }
  coords
}
