wsi_empty_trajectory_profile <- function() {
  out <- data.frame(
    trajectory_id = character(),
    trajectory_name = character(),
    source_id = character(),
    source_name = character(),
    feature = character(),
    feature_type = character(),
    category = character(),
    bin = integer(),
    bin_start_px = numeric(),
    bin_end_px = numeric(),
    distance_px = numeric(),
    distance_fraction = numeric(),
    width_px = numeric(),
    total_length_px = numeric(),
    count = integer(),
    mean = numeric(),
    median = numeric(),
    min = numeric(),
    max = numeric(),
    sd = numeric(),
    dominant = character(),
    dominant_count = integer(),
    fraction = numeric(),
    project_image = character(),
    project_section = character(),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_trajectory_profile", class(out))
  out
}

wsi_empty_trajectory_correlations <- function() {
  out <- data.frame(
    rank = integer(),
    feature = character(),
    method = character(),
    correlation = numeric(),
    p_value = numeric(),
    adjusted_p_value = numeric(),
    n_points = integer(),
    feature_source = character(),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_trajectory_correlations", class(out))
  out
}

wsi_trajectory_profile_points_from_payload <- function(x) {
  if (is.null(x) || !length(x)) {
    return(data.frame(id = character(), distance_fraction = numeric()))
  }
  if (is.data.frame(x)) {
    out <- x
  } else {
    rows <- lapply(x, function(row) if (is.list(row)) row else list())
    out <- data.frame(
      id = vapply(rows, function(row) as.character(row$id %||% ""), character(1)),
      distance_fraction = vapply(rows, function(row) {
        suppressWarnings(as.numeric(row$distance_fraction %||% NA_real_))
      }, numeric(1)),
      stringsAsFactors = FALSE
    )
  }
  if (!"id" %in% names(out)) out$id <- ""
  if (!"distance_fraction" %in% names(out)) out$distance_fraction <- NA_real_
  out$id <- as.character(out$id)
  out$distance_fraction <- suppressWarnings(as.numeric(out$distance_fraction))
  out <- out[nzchar(out$id) & is.finite(out$distance_fraction), c("id", "distance_fraction"), drop = FALSE]
  out[!duplicated(out$id), , drop = FALSE]
}

wsi_trajectory_match_points <- function(points, profile_points) {
  fields <- intersect(c("id", "feature_id", "barcode", "spot_id", "cell_id", "label"), names(points))
  if (!length(fields)) {
    wsi_abort("The live spatial/cell table has no identifiers for trajectory correlation analysis.")
  }
  index <- rep(NA_integer_, nrow(profile_points))
  for (field in fields) {
    missing <- which(is.na(index))
    if (!length(missing)) break
    index[missing] <- match(profile_points$id[missing], as.character(points[[field]]))
  }
  keep <- !is.na(index)
  if (sum(keep) < 3L) {
    wsi_abort("At least three trajectory spots/cells must match the live spatial object.")
  }
  list(
    ids = profile_points$id[keep],
    distance = profile_points$distance_fraction[keep],
    points = points[index[keep], , drop = FALSE]
  )
}

wsi_trajectory_correlation_table <- function(context, payload) {
  profile_points <- wsi_trajectory_profile_points_from_payload(payload$profile_points %||% NULL)
  if (nrow(profile_points) < 3L) {
    wsi_abort("At least three spots/cells along the trajectory are needed for gene correlation analysis.")
  }
  requested_source <- as.character(payload$point_source %||% "spatial:points")
  point_source <- if (startsWith(requested_source, "cellphenotyper:")) {
    "cellphenotyper:cells"
  } else {
    "spatial:points"
  }
  points <- wsi_prediction_points(context, point_source)
  points <- wsi_prediction_filter_project_scope(points, wsi_proximity_scope(payload))
  matched <- wsi_trajectory_match_points(points, profile_points)
  feature_source <- wsi_proximity_feature_source(
    point_source,
    payload$feature_source %||% NULL
  )
  max_features <- suppressWarnings(as.integer(payload$max_features %||% 5000L))
  if (!is.finite(max_features) || max_features < 1L) max_features <- 5000L
  x <- wsi_prediction_feature_matrix(
    context,
    feature_source,
    matched$ids,
    points = matched$points,
    max_features = max_features
  )
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  keep <- colSums(is.finite(x)) >= 3L
  x <- x[, keep, drop = FALSE]
  if (!ncol(x)) {
    wsi_abort("No genes with at least three finite values were found for the trajectory spots/cells.")
  }
  variable <- apply(x, 2L, function(value) {
    value <- value[is.finite(value)]
    length(value) >= 3L && is.finite(stats::var(value)) && stats::var(value) > 0
  })
  x <- x[, variable, drop = FALSE]
  if (!ncol(x)) {
    wsi_abort("Gene expression has no variable features for the trajectory spots/cells.")
  }
  method <- match.arg(
    tolower(as.character(payload$method %||% "spearman")),
    c("spearman", "pearson")
  )
  table <- wsi_proximity_cor_table(x, matched$distance, method = method)
  table$adjusted_p_value <- stats::p.adjust(table$p_value, method = "BH")
  table$rank <- seq_len(nrow(table))
  table$n_points <- vapply(seq_len(ncol(x)), function(j) {
    sum(is.finite(x[, j]) & is.finite(matched$distance))
  }, integer(1))[match(table$feature, colnames(x))]
  table$feature_source <- feature_source
  table <- table[, c(
    "rank", "feature", "method", "correlation", "p_value",
    "adjusted_p_value", "n_points", "feature_source"
  ), drop = FALSE]
  class(table) <- c("wsi_trajectory_correlations", class(table))
  table
}

wsi_trajectory_profile_from_payload <- function(x) {
  template <- wsi_empty_trajectory_profile()
  cols <- names(template)
  if (is.null(x)) {
    return(template)
  }
  if (is.data.frame(x)) {
    out <- x
  } else if (is.list(x)) {
    if (!length(x)) {
      return(template)
    }
    rows <- lapply(x, function(row) {
      if (!is.list(row)) {
        row <- list()
      }
      row
    })
    out <- data.frame(
      lapply(cols, function(col) {
        vapply(rows, function(row) {
          value <- row[[col]]
          if (length(value) == 0L || is.null(value)) {
            return(NA_character_)
          }
          as.character(value[[1L]])
        }, character(1))
      }),
      stringsAsFactors = FALSE
    )
    names(out) <- cols
  } else {
    return(template)
  }

  for (col in setdiff(cols, names(out))) {
    out[[col]] <- template[[col]]
  }
  out <- out[, cols, drop = FALSE]

  integer_cols <- c("bin", "count", "dominant_count")
  numeric_cols <- c(
    "bin_start_px", "bin_end_px", "distance_px", "distance_fraction",
    "width_px", "total_length_px", "mean", "median", "min", "max",
    "sd", "fraction"
  )
  for (col in integer_cols) {
    out[[col]] <- suppressWarnings(as.integer(out[[col]]))
  }
  for (col in numeric_cols) {
    out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
  }
  character_cols <- setdiff(cols, c(integer_cols, numeric_cols))
  for (col in character_cols) {
    out[[col]] <- as.character(out[[col]])
    out[[col]][is.na(out[[col]])] <- ""
  }

  class(out) <- c("wsi_trajectory_profile", setdiff(class(out), "wsi_trajectory_profile"))
  out
}
