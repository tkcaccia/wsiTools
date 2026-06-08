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
