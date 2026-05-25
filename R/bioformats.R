wsi_is_czi_path <- function(path) {
  identical(tolower(tools::file_ext(path)), "czi")
}

wsi_czi_backend_message <- function(path = NULL) {
  paste(
    "CZI files are not readable through the ImageMagick fallback backend.",
    "Install one CZI-capable backend, then restart R/RStudio:",
    "1. Bio-Formats command-line tools: `conda install -c ome bftools`, or download `bftools.zip` and add `showinf`/`bfconvert` to PATH.",
    "2. Optional Python CZI preview bridge: install `aicspylibczi`, `numpy`, and `Pillow`, then set `WSITOOLS_CZI_PYTHON`.",
    "Check with `Sys.which(c(\"showinf\", \"bfconvert\"))` and `wsi_backends()`.",
    "For CZI project viewing, use `wsi_viewer_project(\"file.czi\")`.",
    sep = "\n"
  )
}

wsi_bioformats_showinf <- function(path, omexml = TRUE) {
  showinf <- wsi_bioformats_command("showinf")
  if (!wsi_command_exists(showinf)) {
    wsi_abort(
      "Bio-Formats metadata reading requires `showinf` on PATH. Install OME bftools, then retry.",
      class = "wsi_backend_unavailable"
    )
  }
  args <- if (isTRUE(omexml)) c("-nopix", "-omexml", path) else c("-nopix", path)
  wsi_run_command(
    showinf,
    args = args,
    error_message = sprintf("Bio-Formats could not read metadata from `%s`.", path)
  )
}

wsi_bioformats_xml_attrs <- function(tag) {
  matches <- gregexpr("([A-Za-z_:][-A-Za-z0-9_:.]*)\\s*=\\s*\"([^\"]*)\"", tag, perl = TRUE)
  pieces <- regmatches(tag, matches)[[1L]]
  if (!length(pieces)) {
    return(list())
  }
  out <- lapply(pieces, function(piece) {
    key <- sub("\\s*=.*$", "", piece)
    value <- sub("^[^=]+\\s*=\\s*\"", "", piece)
    value <- sub("\"$", "", value)
    stats::setNames(list(value), key)
  })
  do.call(c, out)
}

wsi_bioformats_parse_omexml <- function(lines) {
  text <- paste(lines, collapse = "\n")
  matches <- gregexpr("<Pixels\\b[^>]*>", text, perl = TRUE)
  tags <- regmatches(text, matches)[[1L]]
  if (!length(tags)) {
    return(NULL)
  }

  rows <- lapply(seq_along(tags), function(i) {
    attrs <- wsi_bioformats_xml_attrs(tags[[i]])
    width <- suppressWarnings(as.numeric(attrs$SizeX %||% NA_real_))
    height <- suppressWarnings(as.numeric(attrs$SizeY %||% NA_real_))
    data.frame(
      series = i - 1L,
      width = width,
      height = height,
      size_z = suppressWarnings(as.integer(attrs$SizeZ %||% NA_integer_)),
      size_c = suppressWarnings(as.integer(attrs$SizeC %||% NA_integer_)),
      size_t = suppressWarnings(as.integer(attrs$SizeT %||% NA_integer_)),
      pixel_type = as.character(attrs$Type %||% NA_character_),
      dimension_order = as.character(attrs$DimensionOrder %||% NA_character_),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[!is.na(out$width) & !is.na(out$height), , drop = FALSE]
  if (!nrow(out)) NULL else out
}

wsi_bioformats_parse_plain <- function(lines) {
  current <- 0L
  series <- list()
  for (line in lines) {
    series_match <- regexec("^\\s*Series\\s+#?([0-9]+)", line, perl = TRUE)
    series_parts <- regmatches(line, series_match)[[1L]]
    if (length(series_parts) == 2L) {
      current <- as.integer(series_parts[[2L]])
      if (is.null(series[[as.character(current)]])) {
        series[[as.character(current)]] <- list(series = current)
      }
      next
    }
    key_match <- regexec("^\\s*([A-Za-z][A-Za-z0-9 _.-]*)\\s*=\\s*(.+?)\\s*$", line, perl = TRUE)
    key_parts <- regmatches(line, key_match)[[1L]]
    if (length(key_parts) == 3L) {
      key <- tolower(gsub("[^A-Za-z0-9]+", "_", trimws(key_parts[[2L]])))
      value <- trimws(key_parts[[3L]])
      id <- as.character(current)
      series[[id]] <- series[[id]] %||% list(series = current)
      series[[id]][[key]] <- value
    }
  }
  if (!length(series)) {
    return(NULL)
  }
  rows <- lapply(series, function(x) {
    width <- suppressWarnings(as.numeric(x$width %||% x$sizex %||% x$size_x %||% NA_real_))
    height <- suppressWarnings(as.numeric(x$height %||% x$sizey %||% x$size_y %||% NA_real_))
    data.frame(
      series = as.integer(x$series %||% 0L),
      width = width,
      height = height,
      size_z = suppressWarnings(as.integer(x$sizez %||% x$size_z %||% NA_integer_)),
      size_c = suppressWarnings(as.integer(x$sizec %||% x$size_c %||% NA_integer_)),
      size_t = suppressWarnings(as.integer(x$sizet %||% x$size_t %||% NA_integer_)),
      pixel_type = as.character(x$pixel_type %||% NA_character_),
      dimension_order = as.character(x$dimension_order %||% NA_character_),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[!is.na(out$width) & !is.na(out$height), , drop = FALSE]
  if (!nrow(out)) NULL else out
}

wsi_bioformats_series_metadata <- function(path) {
  xml <- tryCatch(
    wsi_bioformats_showinf(path, omexml = TRUE),
    error = function(err) NULL
  )
  series <- if (!is.null(xml)) wsi_bioformats_parse_omexml(xml) else NULL
  if (!is.null(series)) {
    return(list(series = series, raw = xml, source = "omexml"))
  }

  plain <- wsi_bioformats_showinf(path, omexml = FALSE)
  series <- wsi_bioformats_parse_plain(plain)
  if (is.null(series)) {
    wsi_abort("Bio-Formats did not report image dimensions for this file.")
  }
  list(series = series, raw = plain, source = "showinf")
}

wsi_bioformats_properties <- function(info) {
  series <- info$series
  props <- list(
    "bioformats.version" = wsi_bioformats_version(),
    "bioformats.metadata-source" = info$source %||% NA_character_,
    "bioformats.series-count" = nrow(series)
  )
  for (i in seq_len(nrow(series))) {
    s <- series[i, , drop = FALSE]
    prefix <- sprintf("bioformats.series[%d].", s$series[[1L]])
    props[[paste0(prefix, "width")]] <- as.character(s$width[[1L]])
    props[[paste0(prefix, "height")]] <- as.character(s$height[[1L]])
    props[[paste0(prefix, "size-z")]] <- as.character(s$size_z[[1L]])
    props[[paste0(prefix, "size-c")]] <- as.character(s$size_c[[1L]])
    props[[paste0(prefix, "size-t")]] <- as.character(s$size_t[[1L]])
    props[[paste0(prefix, "pixel-type")]] <- as.character(s$pixel_type[[1L]])
    props[[paste0(prefix, "dimension-order")]] <- as.character(s$dimension_order[[1L]])
  }
  props
}

wsi_bioformats_open <- function(path) {
  if (!wsi_has_bioformats()) {
    wsi_abort(
      paste(
        "Bio-Formats backend is not installed. Install OME bftools so `showinf` and `bfconvert` are on PATH, then retry.",
        "From conda: `conda install -c ome bftools`.",
        sep = "\n"
      ),
      class = "wsi_backend_unavailable"
    )
  }

  info <- wsi_bioformats_series_metadata(path)
  series <- info$series
  first <- series$series[[1L]]
  first_row <- series[series$series == first, , drop = FALSE]
  width <- first_row$width[[1L]]
  height <- first_row$height[[1L]]
  if (is.na(width) || is.na(height) || width <= 0 || height <= 0) {
    wsi_abort("Bio-Formats did not report level-0 dimensions for this file.")
  }

  levels <- data.frame(
    level = 0L,
    width = width,
    height = height,
    downsample = 1,
    stringsAsFactors = FALSE
  )
  properties <- wsi_bioformats_properties(info)
  properties[["bioformats.metadata-only"]] <- "TRUE"
  metadata <- list(
    vendor = "bioformats",
    backend_version = wsi_bioformats_version(),
    series = series
  )

  wsi_make_slide(
    path = path,
    backend = "bioformats",
    dimensions = c(width = width, height = height),
    levels = levels,
    properties = properties,
    metadata = metadata,
    associated_images = character()
  )
}

wsi_bioformats_read_region_file <- function(slide, region, output) {
  bfconvert <- wsi_bioformats_command("bfconvert")
  if (!wsi_command_exists(bfconvert)) {
    wsi_abort(
      "Bio-Formats region export requires `bfconvert` on PATH. Install OME bftools, then retry.",
      class = "wsi_backend_unavailable"
    )
  }
  if (region$level != 0L) {
    wsi_abort("The Bio-Formats backend currently exposes level 0 only for region export.")
  }
  args <- c(
    "-overwrite",
    "-crop",
    sprintf("%d,%d,%d,%d", region$x, region$y, region$width, region$height),
    slide$path,
    output
  )
  wsi_run_command(
    bfconvert,
    args = args,
    error_message = "Bio-Formats failed to export the requested region."
  )
  invisible(output)
}
