wsi_stride_to_overlap <- function(tile_size, stride) {
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  stride <- as.integer(wsi_check_scalar_number(stride, "stride", allow_zero = FALSE))
  if (stride > tile_size) {
    wsi_abort("`stride` greater than `tile_size` is not implemented in this milestone because it creates gaps between tiles.")
  }
  tile_size - stride
}

wsi_safe_id <- function(x, fallback = "item") {
  x <- as.character(x %||% fallback)
  x[is.na(x) | !nzchar(x)] <- fallback
  gsub("[^A-Za-z0-9_.-]+", "_", x)
}

wsi_slide_id <- function(slide, slide_id = NULL) {
  if (!is.null(slide_id)) {
    if (!is.character(slide_id) || length(slide_id) != 1L || is.na(slide_id) || !nzchar(slide_id)) {
      wsi_abort("`slide_id` must be a single non-empty character value.")
    }
    return(slide_id)
  }
  path <- slide$path
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(slide$backend %||% "slide")
  }
  id <- tools::file_path_sans_ext(basename(path))
  if (!nzchar(id)) "slide" else id
}

wsi_resolve_tile_mask <- function(image, tissue_mask = FALSE, skip_background = FALSE) {
  if (inherits(tissue_mask, "wsi_tissue_mask")) {
    return(tissue_mask)
  }
  if (isTRUE(tissue_mask) || isTRUE(skip_background)) {
    return(wsi_tissue_mask(image))
  }
  if (is.null(tissue_mask) || identical(tissue_mask, FALSE)) {
    return(NULL)
  }
  wsi_abort("`tissue_mask` must be `TRUE`, `FALSE`, `NULL`, or a `wsi_tissue_mask` object.")
}

wsi_ring_matrix <- function(ring) {
  if (is.null(ring)) {
    return(matrix(numeric(), ncol = 2))
  }
  if (is.matrix(ring) && ncol(ring) >= 2L) {
    return(matrix(as.numeric(ring[, 1:2, drop = FALSE]), ncol = 2))
  }
  if (is.data.frame(ring) && all(c("x", "y") %in% names(ring))) {
    return(cbind(as.numeric(ring$x), as.numeric(ring$y)))
  }
  if (is.list(ring)) {
    rows <- lapply(ring, function(point) {
      if (is.numeric(point) && length(point) >= 2L) {
        return(as.numeric(point[1:2]))
      }
      if (is.list(point) && length(point) >= 2L) {
        return(as.numeric(unlist(point[1:2], use.names = FALSE)))
      }
      c(NA_real_, NA_real_)
    })
    out <- do.call(rbind, rows)
    out <- out[!is.na(out[, 1L]) & !is.na(out[, 2L]), , drop = FALSE]
    return(out)
  }
  matrix(numeric(), ncol = 2)
}

wsi_points_in_ring <- function(x, y, ring) {
  pts <- wsi_ring_matrix(ring)
  n <- nrow(pts)
  if (n < 3L) {
    return(rep(FALSE, length(x)))
  }
  if (identical(pts[1L, 1L], pts[n, 1L]) && identical(pts[1L, 2L], pts[n, 2L])) {
    pts <- pts[-n, , drop = FALSE]
    n <- nrow(pts)
  }
  inside <- rep(FALSE, length(x))
  j <- n
  for (i in seq_len(n)) {
    xi <- pts[i, 1L]
    yi <- pts[i, 2L]
    xj <- pts[j, 1L]
    yj <- pts[j, 2L]
    crosses <- ((yi > y) != (yj > y)) &
      (x < (xj - xi) * (y - yi) / ((yj - yi) + .Machine$double.eps) + xi)
    inside <- xor(inside, crosses)
    j <- i
  }
  inside
}

wsi_points_in_polygon <- function(x, y, polygon) {
  if (!length(polygon)) {
    return(rep(FALSE, length(x)))
  }
  inside <- wsi_points_in_ring(x, y, polygon[[1L]])
  if (length(polygon) > 1L) {
    for (hole in polygon[-1L]) {
      inside <- inside & !wsi_points_in_ring(x, y, hole)
    }
  }
  inside
}

wsi_points_in_roi <- function(roi, index, x, y) {
  geometry_type <- tolower(as.character(roi$geometry_type[[index]] %||% ""))
  coords <- roi$coordinates[[index]]
  if (identical(geometry_type, "polygon")) {
    return(wsi_points_in_polygon(x, y, coords))
  }
  if (identical(geometry_type, "multipolygon")) {
    inside <- rep(FALSE, length(x))
    for (polygon in coords) {
      inside <- inside | wsi_points_in_polygon(x, y, polygon)
    }
    return(inside)
  }
  x >= roi$xmin[[index]] & x <= roi$xmax[[index]] &
    y >= roi$ymin[[index]] & y <= roi$ymax[[index]]
}

wsi_tile_centers <- function(grid) {
  data.frame(
    x = grid$x + (grid$width * grid$downsample) / 2,
    y = grid$y + (grid$height * grid$downsample) / 2
  )
}

wsi_tile_grid_for_rois <- function(image, roi, tile_size, overlap, level,
                                   mask = NULL, include_partial = FALSE,
                                   roi_only = TRUE, format = "png",
                                   prefix = NULL) {
  if (!inherits(roi, "wsi_roi")) {
    wsi_abort("`roi` must be a `wsi_roi` object returned by `read_geojson()`.")
  }
  if (!nrow(roi)) {
    return(wsi_tile_grid(image, tile_size = tile_size, overlap = overlap, level = level)[0, , drop = FALSE])
  }

  grids <- vector("list", nrow(roi))
  for (i in seq_len(nrow(roi))) {
    region <- c(
      x = roi$xmin[[i]],
      y = roi$ymin[[i]],
      width = roi$xmax[[i]] - roi$xmin[[i]],
      height = roi$ymax[[i]] - roi$ymin[[i]]
    )
    grid <- wsi_tile_grid(
      image,
      tile_size = tile_size,
      overlap = overlap,
      level = level,
      region = region,
      tissue_mask = mask,
      include_partial = include_partial
    )
    if (!nrow(grid)) {
      grids[[i]] <- grid
      next
    }
    centers <- wsi_tile_centers(grid)
    if (isTRUE(roi_only)) {
      keep <- wsi_points_in_roi(roi, i, centers$x, centers$y)
      grid <- grid[keep, , drop = FALSE]
    }
    if (!nrow(grid)) {
      grids[[i]] <- grid
      next
    }

    roi_id <- as.character(roi$roi_id[[i]] %||% sprintf("roi_%d", i))
    roi_name <- as.character(roi$name[[i]] %||% roi_id)
    roi_class <- as.character(roi$class[[i]] %||% NA_character_)
    if (is.na(roi_class) || !nzchar(roi_class)) {
      roi_class <- "unlabelled"
    }
    safe_prefix <- paste(c(prefix, wsi_safe_id(roi_class), wsi_safe_id(roi_id)), collapse = "_")
    safe_prefix <- sub("^_+", "", safe_prefix)
    grid$tile_id <- sprintf("%s_%s", wsi_safe_id(roi_id), grid$tile_id)
    grid$output_file <- sprintf(
      "%s_%s.%s",
      safe_prefix,
      tools::file_path_sans_ext(basename(grid$output_file)),
      wsi_format_extension(format)
    )
    grid$roi_id <- roi_id
    grid$roi_name <- roi_name
    grid$class <- roi_class
    grid$source <- "roi"
    grids[[i]] <- grid
  }

  out <- do.call(rbind, grids)
  rownames(out) <- NULL
  out
}

wsi_tile_grid_add_provenance <- function(grid, slide, slide_id, sampling, seed) {
  if (!nrow(grid)) {
    grid$slide_id <- character()
    grid$slide_path <- character()
    grid$sampling <- character()
    grid$seed <- integer()
    return(grid)
  }
  if (!"source" %in% names(grid)) {
    grid$source <- "whole_slide"
  }
  if (!"roi_id" %in% names(grid)) {
    grid$roi_id <- NA_character_
  }
  if (!"roi_name" %in% names(grid)) {
    grid$roi_name <- NA_character_
  }
  if (!"class" %in% names(grid)) {
    grid$class <- NA_character_
  }
  grid$slide_id <- slide_id
  grid$slide_path <- slide$path %||% NA_character_
  grid$sampling <- sampling
  grid$seed <- if (is.null(seed)) NA_integer_ else as.integer(seed)
  grid
}

wsi_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(force(expr))
  }
  seed <- as.integer(wsi_check_scalar_number(seed, "seed"))
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

wsi_sample_rows <- function(grid, max_tiles = NULL, seed = NULL) {
  if (is.null(max_tiles) || !nrow(grid)) {
    return(grid)
  }
  max_tiles <- as.integer(wsi_check_scalar_number(max_tiles, "max_tiles", allow_zero = FALSE))
  if (nrow(grid) <= max_tiles) {
    return(grid)
  }
  idx <- wsi_with_seed(seed, sample.int(nrow(grid), max_tiles))
  grid[sort(idx), , drop = FALSE]
}

wsi_balance_tile_grid <- function(grid, tiles_per_class = NULL, max_tiles = NULL, seed = NULL) {
  if (!nrow(grid)) {
    return(grid)
  }
  if (!"class" %in% names(grid)) {
    wsi_abort("Balanced tile sampling requires ROI classes.")
  }
  labels <- as.character(grid$class)
  labels[is.na(labels) | !nzchar(labels)] <- "unlabelled"
  grid$class <- labels
  counts <- table(labels)
  if (length(counts) < 1L) {
    return(grid[0, , drop = FALSE])
  }
  target <- if (is.null(tiles_per_class)) {
    min(as.integer(counts))
  } else {
    as.integer(wsi_check_scalar_number(tiles_per_class, "tiles_per_class", allow_zero = FALSE))
  }
  if (!is.null(max_tiles) && is.null(tiles_per_class)) {
    max_tiles <- as.integer(wsi_check_scalar_number(max_tiles, "max_tiles", allow_zero = FALSE))
    target <- min(target, max(1L, floor(max_tiles / length(counts))))
  }
  if (target < 1L) {
    wsi_abort("Balanced sampling would select zero tiles per class.")
  }

  indices <- wsi_with_seed(seed, {
    unlist(lapply(names(counts), function(label) {
      idx <- which(labels == label)
      if (length(idx) <= target) idx else sample(idx, target)
    }), use.names = FALSE)
  })
  out <- grid[sort(indices), , drop = FALSE]
  rownames(out) <- NULL
  out
}

wsi_split_spec <- function(split, train_fraction = 0.8) {
  if (is.character(split)) {
    split <- match.arg(split, c("none", "train_validation"))
    if (identical(split, "none")) {
      return(NULL)
    }
    train_fraction <- wsi_check_scalar_number(train_fraction, "train_fraction")
    if (train_fraction <= 0 || train_fraction >= 1) {
      wsi_abort("`train_fraction` must be greater than 0 and less than 1.")
    }
    return(c(train = train_fraction, validation = 1 - train_fraction))
  }

  if (!is.numeric(split) || is.null(split)) {
    wsi_abort("`split` must be \"none\", \"train_validation\", or a named numeric vector such as `c(train = 0.7, valid = 0.3)`.")
  }
  labels <- names(split)
  split <- as.numeric(split)
  if (!length(split) || any(!is.finite(split)) || any(split < 0) || sum(split) <= 0) {
    wsi_abort("Numeric `split` values must be finite, non-negative, and sum to a positive value.")
  }
  if (is.null(labels) || length(labels) != length(split) || any(is.na(labels)) || any(!nzchar(labels))) {
    labels <- if (length(split) == 2L) c("train", "validation") else paste0("split_", seq_along(split))
  }
  labels <- make.unique(as.character(labels), sep = "_")
  names(split) <- labels
  split / sum(split)
}

wsi_split_counts <- function(n, proportions) {
  n <- as.integer(n)
  counts <- rep(0L, length(proportions))
  if (n <= 0L) {
    return(counts)
  }
  positive <- which(proportions > 0)
  if (!length(positive)) {
    return(counts)
  }
  if (n >= length(positive)) {
    counts[positive] <- 1L
    remaining <- n - length(positive)
    if (remaining <= 0L) {
      return(counts)
    }
    raw <- remaining * proportions[positive] / sum(proportions[positive])
    extra <- floor(raw)
    leftover <- remaining - sum(extra)
    if (leftover > 0L) {
      order_idx <- order(raw - extra, proportions[positive], decreasing = TRUE)
      extra[order_idx[seq_len(leftover)]] <- extra[order_idx[seq_len(leftover)]] + 1L
    }
    counts[positive] <- counts[positive] + as.integer(extra)
    return(counts)
  }

  chosen <- positive[order(proportions[positive], decreasing = TRUE)][seq_len(n)]
  counts[chosen] <- 1L
  counts
}

wsi_add_train_validation_split <- function(grid, split, train_fraction = 0.8,
                                           stratify = TRUE, seed = NULL) {
  proportions <- wsi_split_spec(split, train_fraction = train_fraction)
  if (is.null(proportions) || !nrow(grid)) {
    grid$split <- NA_character_
    return(grid)
  }

  groups <- if (isTRUE(stratify) && "class" %in% names(grid)) {
    labels <- as.character(grid$class)
    labels[is.na(labels) | !nzchar(labels)] <- "unlabelled"
    base::split(seq_len(nrow(grid)), labels)
  } else {
    list(all = seq_len(nrow(grid)))
  }
  grid$split <- wsi_with_seed(seed, {
    assigned <- rep(NA_character_, nrow(grid))
    split_labels <- names(proportions)
    for (idx in groups) {
      idx <- if (length(idx) > 1L) sample(idx) else idx
      counts <- wsi_split_counts(length(idx), proportions)
      start <- 1L
      for (j in seq_along(counts)) {
        count <- counts[[j]]
        if (count <= 0L) {
          next
        }
        end <- start + count - 1L
        assigned[idx[start:end]] <- split_labels[[j]]
        start <- end + 1L
      }
    }
    assigned
  })
  grid
}

wsi_filter_tissue_tiles <- function(grid, tissue_threshold = 0.1) {
  if (!nrow(grid) || !"tissue_fraction" %in% names(grid)) {
    return(grid)
  }
  tissue_threshold <- wsi_check_scalar_number(tissue_threshold, "tissue_threshold")
  grid[is.na(grid$tissue_fraction) | grid$tissue_fraction >= tissue_threshold, , drop = FALSE]
}

wsi_write_tile_manifest_file <- function(manifest, file, overwrite = FALSE) {
  if (is.null(file)) {
    return(invisible(manifest))
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    wsi_abort("`manifest_file` must be a single non-empty file path.")
  }
  if (file.exists(file) && !isTRUE(overwrite)) {
    wsi_abort(
      sprintf("Output file already exists and `overwrite = FALSE`: %s", file),
      class = "wsi_output_exists"
    )
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(file))) {
    wsi_abort(sprintf("Could not create output directory: %s", dirname(file)))
  }
  utils::write.csv(as.data.frame(manifest), file, row.names = FALSE)
  invisible(manifest)
}

#' Extract tiles with fixed tile size and stride
#'
#' Convenience wrapper around [wsi_tile_grid()] and [wsi_tile()]. When
#' `save_images = FALSE`, only tile coordinates are returned and no pixels are
#' read. When `save_images = TRUE`, tiles are exported through the existing
#' region-based tile pipeline. ML-oriented options can restrict extraction to
#' ROIs, balance tiles by annotation class, skip background using a tissue mask,
#' assign train/validation splits, and record provenance.
#' Optional whitespace/background labelling reads candidate tiles one at a time
#' and appends `whitespace_*` manifest columns.
#' Optional artifact filtering reads candidate tiles one at a time and appends
#' artifact quality columns for blur, pen marks, folds, bubbles,
#' out-of-focus appearance, and very bright or dark tiles.
#'
#' @param image A `wsi_slide` object.
#' @param roi Optional ROI object. When supplied, tiles are generated per ROI
#'   and, by default, tile centers must fall inside polygon ROIs.
#' @param tile_size Tile width and height in pixels at `level`.
#' @param stride Step size in pixels at `level`.
#' @param output_dir Optional output directory.
#' @param save_images Whether to save tile image files. Defaults to `TRUE` when
#'   `output_dir` is supplied.
#' @param level Pyramid level.
#' @param sampling Sampling mode: `"all"` for the whole slide or ROI grid,
#'   `"roi"` for ROI-only tiles, `"balanced"` for equal tiles per ROI class, or
#'   `"auto"` to use ROI mode when `roi` is supplied.
#' @param roi_only Whether ROI tiles should require the tile center to fall
#'   inside the ROI polygon. When `FALSE`, ROI bounding boxes are tiled.
#' @param tissue_mask Whether to estimate and filter by tissue mask, or a
#'   precomputed `wsi_tissue_mask` object.
#' @param skip_background Alias for enabling tissue-mask filtering.
#' @param tissue_threshold Minimum tissue fraction when `tissue_mask = TRUE`.
#' @param whitespace_filter Whether to compute simple whitespace/background
#'   metrics for candidate tiles. This is optional because it reads each
#'   candidate tile region.
#' @param whitespace_action `"flag"` keeps all tiles and appends whitespace
#'   columns; `"drop"` removes rows where `whitespace_flag` is `TRUE`.
#' @param whitespace_options Thresholds from [wsi_whitespace_options()].
#' @param artifact_filter Whether to compute artifact metrics for candidate
#'   tiles. This is optional because it reads each candidate tile region.
#' @param artifact_action `"flag"` keeps all tiles and appends artifact columns;
#'   `"drop"` removes rows where `artifact_flag` is `TRUE`.
#' @param artifact_options Thresholds from [wsi_artifact_options()].
#' @param max_tiles Optional maximum number of tiles after filtering.
#' @param tiles_per_class Optional number of tiles per class for balanced
#'   sampling. When omitted, the smallest class count is used.
#' @param split Split mode. Use `"train_validation"` to add train/validation
#'   labels, or pass a named numeric vector such as
#'   `c(train = 0.7, valid = 0.3)` for reproducible custom splits.
#' @param train_fraction Fraction assigned to the training split when
#'   `split = "train_validation"`.
#' @param stratify_split Whether train/validation split should be stratified by
#'   class when class labels are available.
#' @param seed Optional random seed for reproducible sampling and splitting.
#' @param slide_id Optional slide identifier stored in the manifest.
#' @param manifest_file Optional CSV file for saving the returned manifest/grid.
#' @param format Tile image format.
#' @param prefix Optional filename prefix.
#' @param overwrite Whether to overwrite existing tile files.
#' @param include_partial Include partial edge tiles when returning coordinates.
#' @param ... Reserved for future extensions.
#'
#' @return A tile grid or tile manifest data frame.
#' @export
extract_tiles <- function(image, roi = NULL, tile_size = 512, stride = 512,
                          output_dir = NULL, save_images = !is.null(output_dir),
                          level = 0, tissue_mask = FALSE,
                          tissue_threshold = 0.1,
                          whitespace_filter = FALSE,
                          whitespace_action = c("flag", "drop"),
                          whitespace_options = wsi_whitespace_options(),
                          artifact_filter = FALSE,
                          artifact_action = c("flag", "drop"),
                          artifact_options = wsi_artifact_options(),
                          sampling = c("auto", "all", "roi", "balanced"),
                          roi_only = TRUE,
                          skip_background = FALSE,
                          max_tiles = NULL,
                          tiles_per_class = NULL,
                          split = c("none", "train_validation"),
                          train_fraction = 0.8,
                          stratify_split = TRUE,
                          seed = NULL,
                          slide_id = NULL,
                          manifest_file = NULL,
                          format = c("png", "jpeg", "tiff"),
                          prefix = NULL, overwrite = FALSE,
                          include_partial = FALSE, ...) {
  wsi_check_slide(image)
  format <- match.arg(format)
  whitespace_action <- match.arg(whitespace_action)
  artifact_action <- match.arg(artifact_action)
  sampling <- match.arg(sampling)
  if (identical(sampling, "auto")) {
    sampling <- if (is.null(roi)) "all" else "roi"
  }
  if (identical(sampling, "balanced") && is.null(roi)) {
    wsi_abort("`sampling = \"balanced\"` requires ROI annotations with class labels.")
  }
  overlap <- wsi_stride_to_overlap(tile_size, stride)
  mask <- wsi_resolve_tile_mask(image, tissue_mask = tissue_mask, skip_background = skip_background)
  slide_id <- wsi_slide_id(image, slide_id)

  if (!is.null(roi)) {
    grid <- wsi_tile_grid_for_rois(
      image,
      roi = roi,
      tile_size = tile_size,
      overlap = overlap,
      level = level,
      mask = mask,
      include_partial = include_partial,
      roi_only = isTRUE(roi_only) || identical(sampling, "roi") || identical(sampling, "balanced"),
      format = format,
      prefix = prefix
    )
  } else {
    grid <- wsi_tile_grid(
      image,
      tile_size = tile_size,
      overlap = overlap,
      level = level,
      region = NULL,
      tissue_mask = mask,
      include_partial = include_partial
    )
    if (!is.null(prefix) && nrow(grid)) {
      grid$output_file <- sprintf(
        "%s_%s.%s",
        prefix,
        tools::file_path_sans_ext(basename(grid$output_file)),
        wsi_format_extension(format)
      )
    }
  }

  if (!is.null(mask) || isTRUE(skip_background)) {
    grid <- wsi_filter_tissue_tiles(grid, tissue_threshold = tissue_threshold)
  }
  grid <- wsi_apply_whitespace_filter(
    image,
    grid,
    whitespace_filter = whitespace_filter,
    whitespace_action = whitespace_action,
    whitespace_options = whitespace_options
  )
  grid <- wsi_apply_artifact_filter(
    image,
    grid,
    artifact_filter = artifact_filter,
    artifact_action = artifact_action,
    artifact_options = artifact_options
  )
  grid <- wsi_tile_grid_add_provenance(grid, image, slide_id, sampling, seed)

  if (identical(sampling, "balanced")) {
    grid <- wsi_balance_tile_grid(grid, tiles_per_class = tiles_per_class, max_tiles = max_tiles, seed = seed)
  } else {
    grid <- wsi_sample_rows(grid, max_tiles = max_tiles, seed = seed)
  }
  grid <- wsi_add_train_validation_split(
    grid,
    split = split,
    train_fraction = train_fraction,
    stratify = stratify_split,
    seed = if (is.null(seed)) NULL else as.integer(seed) + 1L
  )

  if (!isTRUE(save_images)) {
    class(grid) <- c("wsi_tile_manifest", setdiff(class(grid), "wsi_tile_manifest"))
    wsi_write_tile_manifest_file(grid, manifest_file, overwrite = overwrite)
    return(grid)
  }

  if (is.null(output_dir)) {
    wsi_abort("`output_dir` is required when `save_images = TRUE`.")
  }

  manifest <- wsi_export_tiles(
    image,
    grid,
    output_dir = output_dir,
    format = format,
    overwrite = overwrite
  )
  class(manifest) <- c("wsi_tile_manifest", setdiff(class(manifest), "wsi_tile_manifest"))
  wsi_write_tile_manifest_file(manifest, manifest_file, overwrite = overwrite)
  manifest
}

#' @rdname extract_tiles
#' @export
wsi_extract_tiles <- extract_tiles
