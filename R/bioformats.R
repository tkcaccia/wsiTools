wsi_is_czi_path <- function(path) {
  identical(tolower(tools::file_ext(path)), "czi")
}

wsi_czi_backend_message <- function(path = NULL) {
  paste(
    "CZI files are not readable through the ImageMagick fallback backend.",
    "Install one CZI-capable backend, then restart R/RStudio:",
    "1. If OpenSlide or libvips can read this CZI variant, wsiTools will use them first because they are fast and tile-friendly.",
    "2. Otherwise, for first visualization: install ZEISS libCZI/libCZIAPI and set `WSITOOLS_LIBCZIAPI` if the shared library is not already discoverable.",
    "3. Alternative first visualization: configure the Bio-Formats Java helper with Java plus `bioformats_package.jar`, then set `WSITOOLS_BIOFORMATS_JAR` if needed.",
    "4. For metadata/conversion: install Bio-Formats with `conda install --override-channels -c ome -c conda-forge bftools`, or download `bftools.zip` and add `showinf`/`bfconvert` to PATH.",
    "5. Legacy fallback only if you explicitly opt in: set `WSITOOLS_CZI_ALLOW_PYTHON=true` and configure `WSITOOLS_CZI_PYTHON` for `aicspylibczi`.",
    "Check with `wsi_has_native_czi()`, `wsi_has_czi_python()`, and `wsi_backends()`.",
    "For CZI project viewing, use `wsi_viewer_project(\"file.czi\")`.",
    sep = "\n"
  )
}

wsi_bioformats_java_source <- function() {
  template <- system.file("java", "WsiToolsBioFormatsHelper.java.txt", package = "wsiTools", mustWork = FALSE)
  if (!nzchar(template) || !file.exists(template)) {
    template <- file.path(getwd(), "inst", "java", "WsiToolsBioFormatsHelper.java.txt")
  }
  if (!file.exists(template)) {
    return("")
  }
  source <- file.path(wsi_bioformats_java_class_dir(), "WsiToolsBioFormatsHelper.java")
  if (!file.exists(source) || file.info(source)$mtime < file.info(template)$mtime) {
    file.copy(template, source, overwrite = TRUE)
  }
  source
}

wsi_bioformats_java_class_dir <- function() {
  dir <- file.path(tempdir(), "wsiTools_bioformats_java")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

wsi_bioformats_java_compile <- function(jar = NULL, class_dir = NULL) {
  jar <- wsi_bioformats_java_jar(jar)
  if (!nzchar(jar) || !file.exists(jar)) {
    wsi_abort(
      wsi_backend_action_message(
        "Bio-Formats Java helper requires `bioformats_package.jar`.",
        backend = "bioformats_java",
        is_czi = TRUE,
        details = "Set `WSITOOLS_BIOFORMATS_JAR` to the full jar path, or set `WSITOOLS_BIOFORMATS_HOME` to the Bio-Formats folder."
      ),
      class = "wsi_backend_unavailable"
    )
  }
  source <- wsi_bioformats_java_source()
  if (!file.exists(source)) {
    wsi_abort("The Bio-Formats Java helper source file is missing from this wsiTools installation.")
  }
  class_dir <- class_dir %||% wsi_bioformats_java_class_dir()
  class_file <- file.path(class_dir, "WsiToolsBioFormatsHelper.class")
  if (file.exists(class_file) && file.info(class_file)$mtime >= file.info(source)$mtime) {
    return(class_dir)
  }
  javac <- wsi_bioformats_javac_command()
  if (!wsi_command_exists(javac)) {
    wsi_abort(
      wsi_backend_action_message(
        "Compiling the Bio-Formats Java helper requires `javac`.",
        backend = "bioformats_java",
        is_czi = TRUE,
        details = "Install a JDK, not only a JRE, then retry."
      ),
      class = "wsi_backend_unavailable"
    )
  }
  dir.create(class_dir, recursive = TRUE, showWarnings = FALSE)
  wsi_run_command(
    javac,
    args = c("-cp", jar, "-d", class_dir, source),
    error_message = "Bio-Formats Java helper compilation failed."
  )
  class_dir
}

wsi_bioformats_java_run <- function(args, jar = NULL, java = NULL, class_dir = NULL) {
  jar <- wsi_bioformats_java_jar(jar)
  java <- wsi_bioformats_java_command(java)
  if (!wsi_command_exists(java)) {
    wsi_abort(
      wsi_backend_action_message(
        "Bio-Formats Java helper requires `java` on PATH or `WSITOOLS_JAVA`.",
        backend = "bioformats_java",
        is_czi = TRUE
      ),
      class = "wsi_backend_unavailable"
    )
  }
  class_dir <- wsi_bioformats_java_compile(jar = jar, class_dir = class_dir)
  classpath <- paste(c(class_dir, jar), collapse = .Platform$path.sep)
  wsi_run_command(
    java,
    args = c("-cp", classpath, "WsiToolsBioFormatsHelper", args),
    error_message = "Bio-Formats Java helper failed."
  )
}

wsi_bioformats_java_metadata <- function(path, jar = NULL) {
  path <- wsi_validate_input_path(path)
  out <- wsi_bioformats_java_run(c("metadata", path), jar = jar)
  parsed <- tryCatch(
    jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE),
    error = function(err) {
      wsi_abort(sprintf("Bio-Formats Java helper returned invalid metadata JSON: %s", conditionMessage(err)))
    }
  )
  rows <- parsed$series %||% list()
  if (!length(rows)) {
    wsi_abort("Bio-Formats Java helper did not report image dimensions for this file.")
  }
  series <- do.call(rbind, lapply(rows, function(row) {
    data.frame(
      series = as.integer(row$series %||% 0L),
      resolution = as.integer(row$resolution %||% 0L),
      width = as.numeric(row$width %||% NA_real_),
      height = as.numeric(row$height %||% NA_real_),
      size_z = as.integer(row$size_z %||% NA_integer_),
      size_c = as.integer(row$size_c %||% NA_integer_),
      size_t = as.integer(row$size_t %||% NA_integer_),
      pixel_type = as.character(row$pixel_type %||% NA_character_),
      dimension_order = as.character(row$dimension_order %||% NA_character_),
      rgb = isTRUE(row$rgb),
      stringsAsFactors = FALSE
    )
  }))
  series <- series[!is.na(series$width) & !is.na(series$height), , drop = FALSE]
  if (!nrow(series)) {
    wsi_abort("Bio-Formats Java helper did not report usable image dimensions for this file.")
  }
  list(series = series, raw = out, source = "bioformats-java", json = parsed)
}

wsi_bioformats_java_read_region_file <- function(path, output, series = 0L,
                                                 resolution = 0L, x = 0L, y = 0L,
                                                 width, height, z = 0L, channel = 0L,
                                                 time = 0L, format = "png",
                                                 jar = NULL) {
  path <- wsi_validate_input_path(path)
  output <- wsi_validate_output_path(output, overwrite = TRUE)
  args <- c(
    "region",
    path,
    as.character(as.integer(series)),
    as.character(as.integer(resolution)),
    as.character(as.integer(z)),
    as.character(as.integer(channel)),
    as.character(as.integer(time)),
    as.character(as.integer(x)),
    as.character(as.integer(y)),
    as.character(as.integer(width)),
    as.character(as.integer(height)),
    output,
    format
  )
  wsi_bioformats_java_run(args, jar = jar)
  invisible(output)
}

wsi_bioformats_showinf <- function(path, omexml = TRUE) {
  showinf <- wsi_bioformats_command("showinf")
  if (!wsi_command_exists(showinf)) {
    wsi_abort(
      wsi_backend_action_message(
        "Bio-Formats metadata reading requires `showinf` on PATH.",
        backend = "bioformats"
      ),
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
  tag <- wsi_clean_text(tag)
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
  lines <- wsi_clean_text(lines)
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
  lines <- wsi_clean_text(lines)
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
  if (wsi_has_bioformats_java()) {
    java <- tryCatch(
      wsi_bioformats_java_metadata(path),
      error = function(err) NULL
    )
    if (!is.null(java)) {
      return(java)
    }
  }

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
      wsi_backend_action_message(
        "Bio-Formats could not open the image because OME bftools are not installed.",
        backend = "bioformats"
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

wsi_bioformats_preview_resize <- function(input, output, width, height = NULL) {
  width <- as.integer(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  if (!is.null(height)) {
    height <- as.integer(wsi_check_scalar_number(height, "height", allow_zero = FALSE))
  }

  if (wsi_has_vips()) {
    args <- c("thumbnail", input, output, as.character(width))
    if (!is.null(height)) {
      args <- c(args, "--height", as.character(height))
    }
    wsi_run_command("vips", args = args, error_message = "libvips failed to resize the Bio-Formats preview.")
    return(invisible(output))
  }

  geometry <- if (is.null(height)) sprintf("%dx", width) else sprintf("%dx%d>", width, height)
  if (wsi_imagemagick_cli()) {
    wsi_run_command(
      "magick",
      args = c(wsi_imagemagick_input(input), "-auto-orient", "-thumbnail", geometry, output),
      error_message = "ImageMagick failed to resize the Bio-Formats preview."
    )
    return(invisible(output))
  }

  wsi_require_magick("resize a Bio-Formats preview for the browser")
  image <- magick::image_read(input)
  image <- magick::image_resize(image, geometry)
  magick::image_write(image, path = output, format = "png")
  invisible(output)
}

wsi_bioformats_preview_series <- function(path, series, output, width, height = NULL) {
  bfconvert <- wsi_bioformats_command("bfconvert")
  if (!wsi_command_exists(bfconvert)) {
    wsi_abort(
      wsi_backend_action_message(
        "Bio-Formats CZI preview generation requires `bfconvert` on PATH.",
        backend = "bioformats",
        is_czi = TRUE
      ),
      class = "wsi_backend_unavailable"
    )
  }

  tmp <- tempfile("wsi_bioformats_series_", fileext = ".tif")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  args <- c(
    "-overwrite",
    "-series", as.character(as.integer(series)),
    path,
    tmp
  )
  wsi_run_command(
    bfconvert,
    args = args,
    error_message = sprintf("Bio-Formats could not export series %d for preview.", as.integer(series))
  )
  wsi_bioformats_preview_resize(tmp, output, width = width, height = height)
  invisible(output)
}

wsi_bioformats_preview_rows <- function(series, width, max_series = 8L, max_input_pixels = 8e7) {
  series <- series[!is.na(series$width) & !is.na(series$height), , drop = FALSE]
  if (!nrow(series)) {
    return(series)
  }
  series$preview_pixels <- series$width * series$height
  small_enough <- series[series$preview_pixels <= max_input_pixels, , drop = FALSE]
  if (nrow(small_enough)) {
    small_enough$preview_score <- abs(small_enough$width - width)
    small_enough <- small_enough[order(small_enough$preview_score, small_enough$preview_pixels), , drop = FALSE]
    return(utils::head(small_enough, max_series))
  }

  # Last resort: expose the smallest available series. This still avoids R
  # loading the source pixels, but bfconvert may be slow for very large images.
  series <- series[order(series$preview_pixels), , drop = FALSE]
  utils::head(series, 1L)
}

wsi_bioformats_java_project_preview <- function(path, width = 768, height = NULL,
                                                max_series = 8L,
                                                max_input_pixels = 2e7) {
  if (!wsi_has_bioformats_java()) {
    wsi_abort(
      wsi_backend_action_message(
        "Bio-Formats Java preview requires Java and `bioformats_package.jar`.",
        backend = "bioformats_java",
        is_czi = TRUE
      ),
      class = "wsi_backend_unavailable"
    )
  }
  info <- wsi_bioformats_java_metadata(path)
  rows <- wsi_bioformats_preview_rows(
    info$series,
    width = width,
    max_series = max_series,
    max_input_pixels = max_input_pixels
  )
  rows <- rows[rows$width * rows$height <= max_input_pixels, , drop = FALSE]
  if (!nrow(rows)) {
    wsi_abort(
      "Bio-Formats Java helper did not report a low-resolution series suitable for first visualization.",
      class = "wsi_backend_unavailable"
    )
  }

  output_dir <- tempfile("wsi_bioformats_java_preview_")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sections <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    raw_png <- file.path(output_dir, sprintf("series_%d_res_%d_raw.png", row$series[[1L]], row$resolution[[1L]] %||% 0L))
    png <- file.path(output_dir, sprintf("series_%d_res_%d.png", row$series[[1L]], row$resolution[[1L]] %||% 0L))
    wsi_bioformats_java_read_region_file(
      path,
      output = raw_png,
      series = row$series[[1L]],
      resolution = row$resolution[[1L]] %||% 0L,
      x = 0L,
      y = 0L,
      width = row$width[[1L]],
      height = row$height[[1L]],
      format = "png"
    )
    wsi_bioformats_preview_resize(raw_png, png, width = width, height = height)
    list(
      id = sprintf("bioformats_series_%d_resolution_%d", row$series[[1L]], row$resolution[[1L]] %||% 0L),
      label = sprintf(
        "Series %d, resolution %d (%sx%s)",
        row$series[[1L]],
        row$resolution[[1L]] %||% 0L,
        format(row$width[[1L]], scientific = FALSE, trim = TRUE),
        format(row$height[[1L]], scientific = FALSE, trim = TRUE)
      ),
      series = as.integer(row$series[[1L]]),
      resolution = as.integer(row$resolution[[1L]] %||% 0L),
      width = as.numeric(row$width[[1L]]),
      height = as.numeric(row$height[[1L]]),
      preview_width = width,
      preview_height = height %||% NA_real_,
      status = "low-resolution preview",
      message = "Preview generated with Bio-Formats ImageReader region reads; full-resolution pixels remain on disk.",
      image_data_uri = wsi_image_data_uri(png, mime = "image/png"),
      navigator_image_data_uri = wsi_image_data_uri(png, mime = "image/png")
    )
  })

  list(
    backend = "bioformats_java",
    sections = sections,
    metadata = info
  )
}

wsi_bioformats_project_preview <- function(path, width = 768, height = NULL,
                                           max_series = 8L,
                                           max_input_pixels = 8e7) {
  if (wsi_has_bioformats_java()) {
    return(wsi_bioformats_java_project_preview(
      path,
      width = width,
      height = height,
      max_series = max_series,
      max_input_pixels = min(max_input_pixels, 2e7)
    ))
  }
  if (!wsi_has_bioformats()) {
    wsi_abort(
      wsi_backend_action_message(
        "Bio-Formats CZI preview generation requires `showinf` and `bfconvert` on PATH.",
        backend = "bioformats",
        is_czi = TRUE
      ),
      class = "wsi_backend_unavailable"
    )
  }
  if (!wsi_command_exists(wsi_bioformats_command("bfconvert"))) {
    wsi_abort(
      wsi_backend_action_message(
        "Bio-Formats metadata is available, but CZI preview generation also requires `bfconvert` on PATH.",
        backend = "bioformats",
        is_czi = TRUE
      ),
      class = "wsi_backend_unavailable"
    )
  }

  info <- wsi_bioformats_series_metadata(path)
  rows <- wsi_bioformats_preview_rows(
    info$series,
    width = width,
    max_series = max_series,
    max_input_pixels = max_input_pixels
  )
  if (!nrow(rows)) {
    wsi_abort("Bio-Formats did not report any previewable CZI series.")
  }

  output_dir <- tempfile("wsi_bioformats_preview_")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  sections <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    png <- file.path(output_dir, sprintf("series_%d.png", row$series[[1L]]))
    wsi_bioformats_preview_series(
      path,
      series = row$series[[1L]],
      output = png,
      width = width,
      height = height
    )
    list(
      id = sprintf("bioformats_series_%d", row$series[[1L]]),
      label = sprintf(
        "Series %d (%sx%s)",
        row$series[[1L]],
        format(row$width[[1L]], scientific = FALSE, trim = TRUE),
        format(row$height[[1L]], scientific = FALSE, trim = TRUE)
      ),
      series = as.integer(row$series[[1L]]),
      width = as.numeric(row$width[[1L]]),
      height = as.numeric(row$height[[1L]]),
      preview_width = width,
      preview_height = height %||% NA_real_,
      status = "preview",
      message = "Preview generated with Bio-Formats bfconvert; full-resolution pixels remain on disk.",
      image_data_uri = wsi_image_data_uri(png, mime = "image/png"),
      navigator_image_data_uri = wsi_image_data_uri(png, mime = "image/png")
    )
  })

  list(
    sections = sections,
    metadata = info
  )
}

wsi_bioformats_read_region_file <- function(slide, region, output) {
  bfconvert <- wsi_bioformats_command("bfconvert")
  if (!wsi_command_exists(bfconvert)) {
    wsi_abort(
      wsi_backend_action_message(
        "Bio-Formats region export requires `bfconvert` on PATH.",
        backend = "bioformats"
      ),
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
