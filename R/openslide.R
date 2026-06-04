wsi_openslide_properties <- function(path) {
  out <- wsi_run_command(
    "openslide-show-properties",
    args = path,
    error_message = sprintf("OpenSlide could not read metadata from `%s`.", path)
  )
  wsi_parse_key_value(out)
}

wsi_properties_to_levels <- function(properties) {
  names(properties) <- wsi_clean_text(names(properties))
  count <- suppressWarnings(as.integer(properties[["openslide.level-count"]] %||% NA_integer_))

  if (is.na(count)) {
    keys <- names(properties)
    matches <- regmatches(keys, regexec("^openslide\\.level\\[([0-9]+)\\]\\.width$", keys))
    ids <- suppressWarnings(as.integer(vapply(matches, function(x) if (length(x) == 2L) x[[2L]] else NA_character_, character(1))))
    ids <- ids[!is.na(ids)]
    count <- if (length(ids)) max(ids) + 1L else 1L
  }

  levels <- lapply(seq_len(count) - 1L, function(level) {
    width <- suppressWarnings(as.numeric(properties[[sprintf("openslide.level[%d].width", level)]] %||% NA_real_))
    height <- suppressWarnings(as.numeric(properties[[sprintf("openslide.level[%d].height", level)]] %||% NA_real_))
    downsample <- suppressWarnings(as.numeric(properties[[sprintf("openslide.level[%d].downsample", level)]] %||% NA_real_))
    data.frame(
      level = level,
      width = width,
      height = height,
      downsample = downsample,
      stringsAsFactors = FALSE
    )
  })

  levels <- do.call(rbind, levels)
  if (is.na(levels$downsample[[1L]])) {
    levels$downsample[[1L]] <- 1
  }
  missing_downsample <- is.na(levels$downsample) & !is.na(levels$width) & !is.na(levels$width[[1L]])
  levels$downsample[missing_downsample] <- levels$width[[1L]] / levels$width[missing_downsample]
  levels
}

wsi_openslide_associated_images <- function(properties) {
  names(properties) <- wsi_clean_text(names(properties))
  keys <- names(properties)
  matches <- regmatches(keys, regexec("^openslide\\.associated\\.([^\\.]+)\\.width$", keys))
  names <- vapply(matches, function(x) if (length(x) == 2L) x[[2L]] else NA_character_, character(1))
  sort(unique(names[!is.na(names)]))
}

wsi_openslide_open <- function(path) {
  if (!wsi_has_openslide()) {
    wsi_abort(
      wsi_backend_action_message(
        "OpenSlide could not open the slide because the OpenSlide backend is not installed.",
        backend = "openslide"
      ),
      class = "wsi_backend_unavailable"
    )
  }

  properties <- wsi_openslide_properties(path)
  levels <- wsi_properties_to_levels(properties)
  if (nrow(levels) == 0L || is.na(levels$width[[1L]]) || is.na(levels$height[[1L]])) {
    wsi_abort("OpenSlide did not report level-0 dimensions for this file.")
  }

  metadata <- list(
    vendor = properties[["openslide.vendor"]] %||% NA_character_,
    backend_version = wsi_command_version("openslide-show-properties")
  )

  wsi_make_slide(
    path = path,
    backend = "openslide",
    dimensions = c(width = levels$width[[1L]], height = levels$height[[1L]]),
    levels = levels,
    properties = properties,
    metadata = metadata,
    associated_images = wsi_openslide_associated_images(properties)
  )
}

wsi_openslide_read_region_file <- function(slide, region, output) {
  if (!wsi_command_exists("openslide-write-png")) {
    wsi_abort(
      wsi_backend_action_message(
        "OpenSlide region reading requires `openslide-write-png` on PATH for this milestone.",
        backend = "openslide"
      ),
      class = "wsi_backend_unavailable"
    )
  }
  args <- c(
    slide$path,
    output,
    as.character(region$x),
    as.character(region$y),
    as.character(region$level),
    as.character(region$width),
    as.character(region$height)
  )
  wsi_run_command(
    "openslide-write-png",
    args = args,
    error_message = "OpenSlide failed to read the requested region."
  )
  invisible(output)
}
