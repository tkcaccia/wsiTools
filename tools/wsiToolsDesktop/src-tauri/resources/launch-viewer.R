args <- commandArgs(trailingOnly = TRUE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

desktop_emit <- function(key, value) {
  cat(sprintf("WSITOOLS_%s=%s\n", key, value %||% ""))
  flush(stdout())
}

desktop_file_url <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  encoded <- utils::URLencode(path, reserved = FALSE)
  if (startsWith(encoded, "/")) {
    paste0("file://", encoded)
  } else {
    paste0("file:///", encoded)
  }
}

desktop_log <- function(..., log_file = NULL) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", paste0(..., collapse = ""))
  message(line)
  if (!is.null(log_file) && nzchar(log_file)) {
    cat(line, "\n", file = log_file, append = TRUE)
  }
}

desktop_error_log_file <- function() {
  Sys.getenv("WSITOOLS_DESKTOP_LOG_FILE", unset = "")
}

desktop_prepare_backend_path <- function() {
  windows_conda_candidates <- function() {
    roots <- character()
    for (key in c("CONDA_PREFIX", "MAMBA_ROOT_PREFIX")) {
      value <- Sys.getenv(key, unset = "")
      if (nzchar(value)) {
        roots <- c(roots, value)
      }
    }
    userprofile <- Sys.getenv("USERPROFILE", unset = "")
    if (nzchar(userprofile)) {
      roots <- c(
        roots,
        file.path(userprofile, "miniconda3"),
        file.path(userprofile, "anaconda3"),
        file.path(userprofile, "miniforge3"),
        file.path(userprofile, "mambaforge"),
        file.path(userprofile, "micromamba")
      )
    }
    unique(unlist(lapply(roots, function(root) {
      file.path(root, c("Library/bin", "Scripts", "condabin"))
    }), use.names = FALSE))
  }
  candidates <- switch(
    Sys.info()[["sysname"]],
    Darwin = c(
      "/opt/homebrew/bin", "/opt/homebrew/sbin",
      "/usr/local/bin", "/usr/local/sbin",
      "/opt/local/bin", "/usr/bin", "/bin", "/usr/sbin", "/sbin"
    ),
    Linux = c(
      "/usr/local/bin", "/usr/bin", "/bin",
      "/usr/local/sbin", "/usr/sbin", "/sbin",
      "/opt/conda/bin", "/opt/homebrew/bin"
    ),
    Windows = c(
      "C:/Program Files/libvips/bin",
      "C:/Program Files/openslide-win64/bin",
      "C:/Program Files/Git/cmd",
      "C:/rtools44/x86_64-w64-mingw32.static.posix/bin",
      "C:/rtools44/usr/bin",
      windows_conda_candidates()
    ),
    character()
  )
  candidates <- candidates[dir.exists(candidates)]
  current <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1L]]
  Sys.setenv(PATH = paste(unique(c(candidates, current)), collapse = .Platform$path.sep))
  invisible(Sys.getenv("PATH"))
}

desktop_prepare_backend_path()

desktop_content_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    html = "text/html; charset=utf-8",
    htm = "text/html; charset=utf-8",
    js = "application/javascript; charset=utf-8",
    css = "text/css; charset=utf-8",
    json = "application/json; charset=utf-8",
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    svg = "image/svg+xml",
    "application/octet-stream"
  )
}

desktop_is_czi_path <- function(path) {
  identical(tolower(tools::file_ext(path)), "czi")
}

desktop_open_czi_project <- function(image_paths, output, log_file, title = "wsiTools desktop CZI viewer") {
  image_paths <- normalizePath(image_paths, winslash = "/", mustWork = TRUE)
  desktop_log(
    "Opening CZI project through the live native CZI tile server with ",
    length(image_paths),
    " file(s).",
    log_file = log_file
  )
  viewer <- wsiTools::wsi_viewer_czi_project_live(
    image_paths,
    output = output,
    open = FALSE,
    wait = FALSE,
    overwrite = TRUE,
    title = title,
    czi_preview = "lazy",
    sections = TRUE,
    transport = "auto"
  )
  if (!inherits(viewer, "wsi_viewer_session")) {
    stop(
      "The desktop app requires a live CZI viewer session. ",
      "Install the optional R package httpuv and native CZI backend, then try again.",
      call. = FALSE
    )
  }
  viewer
}

desktop_http_response <- function(status, body = raw(), content_type = "text/plain; charset=utf-8") {
  list(
    status = as.integer(status),
    headers = c(
      "Content-Type" = content_type,
      "Cache-Control" = "no-store"
    ),
    body = body
  )
}

desktop_start_html_server <- function(html, host = "127.0.0.1", port = 8900L, max_tries = 100L) {
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    stop("The desktop HTML bridge requires the optional R package `httpuv`.", call. = FALSE)
  }
  html <- normalizePath(html, winslash = "/", mustWork = TRUE)
  root <- normalizePath(dirname(html), winslash = "/", mustWork = TRUE)
  index <- basename(html)
  app <- list(
    call = function(req) {
      request_path <- req$PATH_INFO %||% "/"
      request_path <- utils::URLdecode(request_path)
      if (identical(request_path, "/") || !nzchar(request_path)) {
        request_path <- paste0("/", index)
      }
      if (identical(request_path, "/favicon.ico")) {
        return(desktop_http_response(204L))
      }
      relative <- sub("^/+", "", request_path)
      candidate <- normalizePath(file.path(root, relative), winslash = "/", mustWork = FALSE)
      if (!startsWith(candidate, paste0(root, "/")) && !identical(candidate, root)) {
        return(desktop_http_response(403L, charToRaw("Forbidden")))
      }
      if (!file.exists(candidate) || dir.exists(candidate)) {
        return(desktop_http_response(404L, charToRaw("Not found")))
      }
      desktop_http_response(
        200L,
        readBin(candidate, what = "raw", n = file.info(candidate)$size),
        desktop_content_type(candidate)
      )
    }
  )
  for (candidate in seq.int(as.integer(port), as.integer(port) + as.integer(max_tries))) {
    server <- try(httpuv::startServer(host, candidate, app), silent = TRUE)
    if (!inherits(server, "try-error")) {
      return(list(
        server = server,
        url = sprintf("http://%s:%d/", host, candidate)
      ))
    }
  }
  stop("Could not start the desktop HTML server on localhost.", call. = FALSE)
}

find_source_tree <- function() {
  source_dir <- Sys.getenv("WSITOOLS_DESKTOP_SOURCE_DIR", unset = "")
  raw_candidates <- c(getwd())
  if (nzchar(source_dir)) {
    raw_candidates <- c(
      source_dir,
      file.path(source_dir, ".."),
      file.path(source_dir, "..", ".."),
      file.path(source_dir, "..", "..", ".."),
      raw_candidates
    )
  }
  candidates <- unique(normalizePath(raw_candidates, winslash = "/", mustWork = FALSE))
  keep <- vapply(candidates, function(candidate) {
    description <- file.path(candidate, "DESCRIPTION")
    if (!file.exists(description)) {
      return(FALSE)
    }
    text <- paste(readLines(description, warn = FALSE, n = 8L), collapse = "\n")
    grepl("Package:\\s*wsiTools", text)
  }, logical(1))
  candidates[keep]
}

load_wsitools <- function() {
  source_tree <- find_source_tree()
  if (length(source_tree) && requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(source_tree[[1L]], quiet = TRUE)
    return(invisible(TRUE))
  }

  if (requireNamespace("wsiTools", quietly = TRUE)) {
    return(invisible(TRUE))
  }

  stop(
    "The R package wsiTools is not installed. Install it with: ",
    "remotes::install_github(\"tkcaccia/wsiTools\", upgrade = \"never\")",
    call. = FALSE
  )
}

desktop_log_runtime_diagnostics <- function(log_file = NULL) {
  desktop_log(
    "R runtime: ",
    paste(R.version$major, R.version$minor, sep = "."),
    " (",
    R.version$platform,
    ")",
    log_file = log_file
  )
  wsitools_version <- tryCatch(
    as.character(utils::packageVersion("wsiTools")),
    error = function(err) {
      desc <- utils::packageDescription("wsiTools", fields = "Version")
      if (is.na(desc) || !nzchar(desc)) "unknown" else desc
    }
  )
  desktop_log("wsiTools version: ", wsitools_version, log_file = log_file)
  commands <- c(
    "Rscript",
    "vips",
    "vipsheader",
    "openslide-show-properties",
    "openslide-write-png",
    "magick",
    "java",
    "showinf",
    "bfconvert"
  )
  paths <- Sys.which(commands)
  desktop_log("Executable paths:", log_file = log_file)
  for (name in names(paths)) {
    desktop_log("  ", name, ": ", if (nzchar(paths[[name]])) paths[[name]] else "<not found>", log_file = log_file)
  }
  backend_text <- tryCatch(
    paste(capture.output(print(wsiTools::wsi_backends())), collapse = "\n"),
    error = function(err) paste("Could not run wsi_backends():", conditionMessage(err))
  )
  desktop_log("Backend status:\n", backend_text, log_file = log_file)
  invisible(TRUE)
}

desktop_parse_args <- function(args) {
  mode <- "image"
  path <- character()
  items <- list()
  current <- NULL
  if (length(args) >= 3L && identical(args[[1L]], "--mode")) {
    mode <- args[[2L]]
    rest <- args[-(1:2)]
    if (identical(mode, "new-project")) {
      i <- 1L
      while (i <= length(rest)) {
        key <- rest[[i]]
        value <- if (i < length(rest)) rest[[i + 1L]] else ""
        if (identical(key, "--image")) {
          if (!is.null(current)) {
            items[[length(items) + 1L]] <- current
          }
          current <- list(
            image = value,
            cell_annotation = NULL,
            tissue_annotation = NULL,
            spatial_data = NULL,
            spatial_sample_id = NULL
          )
          i <- i + 2L
        } else if (identical(key, "--cell")) {
          if (is.null(current)) {
            stop("Cell annotation was supplied before an image path.", call. = FALSE)
          }
          current$cell_annotation <- value
          i <- i + 2L
        } else if (identical(key, "--tissue")) {
          if (is.null(current)) {
            stop("Tissue annotation was supplied before an image path.", call. = FALSE)
          }
          current$tissue_annotation <- value
          i <- i + 2L
        } else if (identical(key, "--spatial")) {
          if (is.null(current)) {
            stop("Spatial transcriptomics data was supplied before an image path.", call. = FALSE)
          }
          current$spatial_data <- value
          i <- i + 2L
        } else if (identical(key, "--sample")) {
          if (is.null(current)) {
            stop("Spatial tissue/sample ID was supplied before an image path.", call. = FALSE)
          }
          current$spatial_sample_id <- value
          i <- i + 2L
        } else {
          path <- value
          i <- i + 1L
        }
      }
      if (!is.null(current)) {
        items[[length(items) + 1L]] <- current
      }
    } else {
      path <- rest[[1L]]
    }
  } else if (length(args) >= 2L && args[[1L]] %in% c("--image", "--project")) {
    mode <- sub("^--", "", args[[1L]])
    path <- args[[2L]]
  } else if (length(args) >= 1L) {
    path <- args[[1L]]
  }
  if (!mode %in% c("image", "project", "new-project")) {
    stop("Unsupported desktop launcher mode: ", mode, call. = FALSE)
  }
  if (identical(mode, "new-project")) {
    items <- Filter(function(item) {
      is.list(item) && is.character(item$image) && length(item$image) == 1L && nzchar(item$image)
    }, items)
    if (!length(items)) {
      stop("No image paths were supplied by the desktop app.", call. = FALSE)
    }
    return(list(
      mode = mode,
      items = items
    ))
  }
  if (!length(path) || !nzchar(path[[1L]])) {
    stop("No image or project path was supplied by the desktop app.", call. = FALSE)
  }
  list(mode = mode, path = path[[1L]])
}

desktop_project_slide_path <- function(project) {
  slide_path <- project$slide_path %||%
    project$manifest$slide$path %||%
    project$manifest$slide$absolute_path %||%
    NA_character_
  if (is.na(slide_path) || !nzchar(slide_path)) {
    stop("The selected project does not record a slide path.", call. = FALSE)
  }
  if (!file.exists(slide_path)) {
    absolute <- project$manifest$slide$absolute_path %||% NA_character_
    if (!is.na(absolute) && nzchar(absolute) && file.exists(absolute)) {
      return(normalizePath(absolute, winslash = "/", mustWork = TRUE))
    }
    stop(
      "The slide recorded in this project could not be found: ",
      slide_path,
      call. = FALSE
    )
  }
  normalizePath(slide_path, winslash = "/", mustWork = TRUE)
}

desktop_load_spatial_object <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return(NULL)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "rds")) {
    return(readRDS(path))
  }
  env <- new.env(parent = emptyenv())
  loaded <- load(path, envir = env)
  if (!length(loaded)) {
    stop("No R object was found in spatial transcriptomics file: ", path, call. = FALSE)
  }
  env[[loaded[[1L]]]]
}

desktop_mark_dense_cell_rois <- function(rois) {
  if (!is.data.frame(rois) || !nrow(rois)) {
    return(rois)
  }
  if (!"properties" %in% names(rois)) {
    rois$properties <- I(rep(list(list()), nrow(rois)))
  }
  rois$properties <- I(lapply(rois$properties, function(properties) {
    if (!is.list(properties)) {
      properties <- list()
    }
    properties$wsiTools_dense_geometry <- TRUE
    wsi_meta <- properties$wsiTools
    if (!is.list(wsi_meta)) {
      wsi_meta <- list()
    }
    wsi_meta$dense_geometry <- TRUE
    properties$wsiTools <- wsi_meta
    properties
  }))
  if ("object_type" %in% names(rois)) {
    missing_object_type <- is.na(rois$object_type) | !nzchar(as.character(rois$object_type))
    rois$object_type[missing_object_type] <- "detection"
  }
  rois
}

desktop_geojson_geometry_summary <- function(rois) {
  if (!is.data.frame(rois) || !nrow(rois) || !"geometry_type" %in% names(rois)) {
    return("0 region(s)")
  }
  types <- table(as.character(rois$geometry_type), useNA = "ifany")
  paste(
    sprintf("%s %s", as.integer(types), names(types)),
    collapse = ", "
  )
}

desktop_initial_tissue_rois <- function(items, log_file = NULL) {
  if (!length(items)) {
    return(NULL)
  }
  tissue_annotation <- items[[1L]]$tissue_annotation %||% ""
  if (!nzchar(tissue_annotation) || !file.exists(tissue_annotation)) {
    return(NULL)
  }
  ext <- tolower(tools::file_ext(tissue_annotation))
  if (!ext %in% c("geojson", "json")) {
    return(NULL)
  }
  max_embed <- suppressWarnings(as.numeric(Sys.getenv("WSITOOLS_DESKTOP_EMBED_TISSUE_GEOJSON_MB", "4")))
  if (!is.finite(max_embed) || max_embed < 0) {
    max_embed <- 4
  }
  size <- suppressWarnings(file.info(tissue_annotation)$size)
  if (is.finite(size) && size > max_embed * 1024^2) {
    desktop_log(
      "Tissue annotation is ",
      round(size / 1024^2, 1),
      " MB, so it will be streamed after the viewer opens instead of embedded in the initial HTML: ",
      tissue_annotation,
      log_file = log_file
    )
    return(NULL)
  }
  rois <- wsiTools::read_geojson(tissue_annotation)
  desktop_log(
    "Embedding initial tissue annotation in viewer HTML: ",
    tissue_annotation,
    " (",
    desktop_geojson_geometry_summary(rois),
    "). Non-polygon geometries are listed but excluded from area/proximity summaries.",
    log_file = log_file
  )
  rois
}

desktop_geojson_defer_size <- function() {
  value <- suppressWarnings(as.numeric(Sys.getenv("WSITOOLS_DESKTOP_DEFER_GEOJSON_MB", "4")))
  if (!is.finite(value) || value < 0) {
    value <- 4
  }
  value * 1024^2
}

desktop_geojson_chunk_size <- function() {
  value <- suppressWarnings(as.integer(Sys.getenv("WSITOOLS_DESKTOP_GEOJSON_CHUNK", "250")))
  if (!is.finite(value) || value < 50L) {
    value <- 250L
  }
  value
}

desktop_spatial_max_points <- function() {
  raw <- Sys.getenv("WSITOOLS_DESKTOP_SPATIAL_MAX_POINTS", "all")
  if (tolower(raw) %in% c("", "all", "none", "full", "false", "off")) {
    return(.Machine$integer.max)
  }
  value <- suppressWarnings(as.integer(raw))
  if (!is.finite(value) || value < 1000L) {
    return(.Machine$integer.max)
  }
  value
}

desktop_auto_send_large_geojson <- function() {
  tolower(Sys.getenv("WSITOOLS_DESKTOP_AUTO_SEND_LARGE_GEOJSON", "false")) %in%
    c("1", "true", "yes", "on")
}

desktop_should_defer_geojson <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(FALSE)
  }
  size <- suppressWarnings(file.info(path)$size)
  is.finite(size) && size >= desktop_geojson_defer_size()
}

desktop_dense_geojson_id <- function(path, kind) {
  base <- tools::file_path_sans_ext(basename(path %||% kind %||% "geojson"))
  base <- gsub("[^A-Za-z0-9_]+", "_", base)
  base <- gsub("^_+|_+$", "", base)
  paste0("dense_", kind %||% "geojson", "_", base %||% "annotation")
}

desktop_geojson_cache_root <- function() {
  explicit <- Sys.getenv("WSITOOLS_DESKTOP_GEOJSON_CACHE_DIR", "")
  if (nzchar(explicit)) {
    return(explicit)
  }
  if (.Platform$OS.type == "windows") {
    local <- Sys.getenv("LOCALAPPDATA", "")
    if (nzchar(local)) {
      return(file.path(local, "wsiTools", "geojson_cache"))
    }
  }
  xdg <- Sys.getenv("XDG_CACHE_HOME", "")
  if (nzchar(xdg)) {
    return(file.path(xdg, "wsiTools", "geojson_cache"))
  }
  file.path(path.expand("~"), ".cache", "wsiTools", "geojson_cache")
}

desktop_simple_hash <- function(value) {
  ints <- utf8ToInt(enc2utf8(as.character(value %||% "")))
  if (!length(ints)) {
    return("00000000")
  }
  hash <- sum((seq_along(ints) * ints) %% 2147483647) %% 2147483647
  sprintf("%08x", as.integer(hash))
}

desktop_geojson_persistent_cache_file <- function(path, kind) {
  info <- suppressWarnings(file.info(path))
  key <- paste(
    normalizePath(path, winslash = "/", mustWork = FALSE),
    kind,
    suppressWarnings(as.numeric(info$size[[1L]])),
    suppressWarnings(as.numeric(info$mtime[[1L]])),
    sep = "|"
  )
  root <- desktop_geojson_cache_root()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  file.path(root, paste0(kind %||% "geojson", "_", desktop_simple_hash(key), ".rds"))
}

desktop_start_geojson_import_job <- function(path, output, name, kind, log_file = NULL) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    desktop_log(
      "Large GeoJSON import needs the optional callr package to keep live sync responsive. ",
      "Falling back to foreground import for: ",
      path,
      log_file = log_file
    )
    return(NULL)
  }
  cache_dir <- file.path(dirname(output), "deferred_geojson")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(
    cache_dir,
    paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", tools::file_path_sans_ext(basename(path)), ".rds")
  )
  persistent_cache_file <- desktop_geojson_persistent_cache_file(path, kind)
  desktop_log(
    "Deferring large ",
    kind,
    " GeoJSON import so the tiled viewer and R sync start first: ",
    path,
    log_file = log_file
  )
  job <- callr::r_bg(
    func = function(path, cache_file, persistent_cache_file, kind, lib_paths) {
      .libPaths(lib_paths)
      suppressPackageStartupMessages(library(wsiTools))
      info <- suppressWarnings(file.info(path))
      source_key <- list(
        path = normalizePath(path, winslash = "/", mustWork = FALSE),
        kind = kind,
        size = suppressWarnings(as.numeric(info$size[[1L]])),
        mtime = suppressWarnings(as.numeric(info$mtime[[1L]])),
        cache_version = 2L
      )
      if (file.exists(persistent_cache_file)) {
        cached <- tryCatch(readRDS(persistent_cache_file), error = function(err) NULL)
        cached_key <- if (is.list(cached)) cached$source_key else NULL
        if (is.list(cached_key) &&
            identical(cached_key$path, source_key$path) &&
            identical(cached_key$kind, source_key$kind) &&
            isTRUE(all.equal(cached_key$size, source_key$size, tolerance = 0)) &&
            isTRUE(all.equal(cached_key$mtime, source_key$mtime, tolerance = 0)) &&
            identical(cached_key$cache_version, source_key$cache_version) &&
            inherits(cached$rois, "wsi_roi")) {
          cached$cache_hit <- TRUE
          saveRDS(cached, cache_file)
          return(cache_file)
        }
      }
      rois <- wsiTools::read_geojson(path)
      if (identical(kind, "cell")) {
        if (!"properties" %in% names(rois)) {
          rois$properties <- I(rep(list(list()), nrow(rois)))
        }
        rois$properties <- I(lapply(rois$properties, function(properties) {
          if (!is.list(properties)) {
            properties <- list()
          }
          properties$wsiTools_dense_geometry <- TRUE
          wsi_meta <- properties$wsiTools
          if (!is.list(wsi_meta)) {
            wsi_meta <- list()
          }
          wsi_meta$dense_geometry <- TRUE
          properties$wsiTools <- wsi_meta
          properties
        }))
        if ("object_type" %in% names(rois)) {
          missing_object_type <- is.na(rois$object_type) | !nzchar(as.character(rois$object_type))
          rois$object_type[missing_object_type] <- "detection"
        }
      }
      result <- list(
        rois = rois,
        path = path,
        kind = kind,
        source_key = source_key,
        cache_hit = FALSE
      )
      try(saveRDS(result, persistent_cache_file), silent = TRUE)
      saveRDS(result, cache_file)
      cache_file
    },
    args = list(
      path = normalizePath(path, winslash = "/", mustWork = TRUE),
      cache_file = normalizePath(cache_file, winslash = "/", mustWork = FALSE),
      persistent_cache_file = normalizePath(persistent_cache_file, winslash = "/", mustWork = FALSE),
      kind = kind,
      lib_paths = .libPaths()
    ),
    supervise = FALSE
  )
  list(
    type = "geojson_rois",
    id = desktop_dense_geojson_id(path, kind),
    name = name,
    kind = kind,
    path = path,
    cache_file = cache_file,
    job = job,
    loaded = FALSE,
    done = FALSE,
    rois = NULL,
    offset = 0L
  )
}

desktop_tissue_preview_points <- function() {
  value <- suppressWarnings(as.integer(Sys.getenv("WSITOOLS_DESKTOP_TISSUE_PREVIEW_POINTS", "3000")))
  if (!is.finite(value) || value < 128L) {
    value <- 3000L
  }
  value
}

desktop_geojson_class_palette <- function() {
  tryCatch(
    getFromNamespace("wsi_viewer_roi_class_palette", "wsiTools")(),
    error = function(err) c(
      "#00BFC4", "#F8766D", "#7CAE00", "#C77CFF", "#E69F00", "#56B4E9",
      "#CC79A7", "#D55E00", "#009E73", "#0072B2", "#F0E442", "#A6761D",
      "#E7298A", "#66A61E", "#E6AB02", "#7570B3", "#1B9E77", "#D95F02"
    )
  )
}

desktop_first_valid_colour <- function(values, fallback = NA_character_) {
  tryCatch(
    getFromNamespace("wsi_viewer_first_valid_colour", "wsiTools")(values, fallback = fallback),
    error = function(err) {
      values <- trimws(as.character(values %||% character()))
      values <- values[!is.na(values) & nzchar(values)]
      valid <- vapply(values, function(value) {
        tryCatch(
          {
            grDevices::col2rgb(value)
            TRUE
          },
          error = function(err) FALSE
        )
      }, logical(1))
      values <- values[valid]
      if (length(values)) values[[1L]] else fallback
    }
  )
}

desktop_apply_geojson_class_colours <- function(rois) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    return(rois)
  }
  classes <- trimws(as.character(rois$class %||% rep("annotation", nrow(rois))))
  classes[is.na(classes) | !nzchar(classes)] <- "annotation"
  keys <- tolower(classes)
  unique_keys <- unique(keys)
  palette <- desktop_geojson_class_palette()
  colour_map <- stats::setNames(character(length(unique_keys)), unique_keys)
  for (j in seq_along(unique_keys)) {
    key <- unique_keys[[j]]
    explicit <- if ("color" %in% names(rois)) {
      desktop_first_valid_colour(rois$color[keys == key], fallback = NA_character_)
    } else {
      NA_character_
    }
    colour_map[[key]] <- if (!is.na(explicit) && nzchar(explicit)) {
      explicit
    } else {
      palette[((j - 1L) %% length(palette)) + 1L]
    }
  }
  rois$color <- unname(colour_map[keys])
  if ("classification_color" %in% names(rois)) {
    rois$classification_color <- rois$color
  }
  if ("properties" %in% names(rois)) {
    rois$properties <- I(lapply(seq_len(nrow(rois)), function(i) {
      properties <- rois$properties[[i]]
      if (!is.list(properties)) {
        properties <- list()
      }
      classification <- properties$classification
      if (!is.list(classification)) {
        classification <- list()
      }
      classification$name <- classes[[i]]
      classification$color <- rois$color[[i]]
      classification$colour <- NULL
      classification$colorRGB <- NULL
      properties$classification <- classification
      properties$class <- classes[[i]]
      properties$name <- as.character(rois$name[[i]] %||% rois$roi_id[[i]] %||% classes[[i]])
      properties
    }))
  }
  rois
}

desktop_ring_to_geojson <- function(ring) {
  lapply(ring, function(point) {
    unname(c(
      suppressWarnings(as.numeric(point$x %||% point[[1L]] %||% NA_real_)),
      suppressWarnings(as.numeric(point$y %||% point[[2L]] %||% NA_real_))
    ))
  })
}

desktop_ring_groups_to_geometry <- function(ring_groups, geometry_type) {
  geometry_type <- tolower(as.character(geometry_type %||% "Polygon"))
  if (!length(ring_groups)) {
    return(NULL)
  }
  if (identical(geometry_type, "multipolygon")) {
    return(list(
      type = "MultiPolygon",
      coordinates = lapply(ring_groups, function(group) {
        lapply(group, desktop_ring_to_geojson)
      })
    ))
  }
  list(
    type = "Polygon",
    coordinates = lapply(ring_groups[[1L]], desktop_ring_to_geojson)
  )
}

desktop_tissue_preview_geometry <- function() {
  tolower(Sys.getenv("WSITOOLS_DESKTOP_TISSUE_PREVIEW_GEOMETRY", "false")) %in%
    c("1", "true", "yes", "on")
}

desktop_bbox_geometry <- function(xmin, ymin, xmax, ymax) {
  xmin <- suppressWarnings(as.numeric(xmin))
  ymin <- suppressWarnings(as.numeric(ymin))
  xmax <- suppressWarnings(as.numeric(xmax))
  ymax <- suppressWarnings(as.numeric(ymax))
  if (!all(is.finite(c(xmin, ymin, xmax, ymax))) || xmax <= xmin || ymax <= ymin) {
    return(NULL)
  }
  list(
    type = "Polygon",
    coordinates = list(list(
      c(xmin, ymin),
      c(xmax, ymin),
      c(xmax, ymax),
      c(xmin, ymax),
      c(xmin, ymin)
    ))
  )
}

desktop_tissue_list_rois <- function(rois) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    return(rois)
  }
  rois <- desktop_apply_geojson_class_colours(rois)
  features <- vector("list", nrow(rois))
  for (i in seq_len(nrow(rois))) {
    roi_class <- as.character(rois$class[[i]] %||% "annotation")
    if (is.na(roi_class) || !nzchar(trimws(roi_class))) {
      roi_class <- "annotation"
    }
    colour <- as.character(rois$color[[i]] %||% "#00BFC4")
    properties <- if ("properties" %in% names(rois)) rois$properties[[i]] else list()
    if (!is.list(properties)) {
      properties <- list()
    }
    classification <- properties$classification
    if (!is.list(classification)) {
      classification <- list()
    }
    classification$name <- roi_class
    classification$color <- colour
    classification$colour <- NULL
    classification$colorRGB <- NULL
    wsi_meta <- properties$wsiTools
    if (!is.list(wsi_meta)) {
      wsi_meta <- list()
    }
    wsi_meta$list_only <- TRUE
    wsi_meta$full_geometry_source <- TRUE
    wsi_meta$coordinate_space <- "level0_slide_pixels"
    properties$wsiTools <- wsi_meta
    properties$wsiTools_list_only <- TRUE
    properties$list_only <- TRUE
    properties$classification <- classification
    properties$class <- roi_class
    properties$name <- as.character(rois$name[[i]] %||% rois$roi_id[[i]] %||% roi_class)
    geometry <- desktop_bbox_geometry(rois$xmin[[i]], rois$ymin[[i]], rois$xmax[[i]], rois$ymax[[i]])
    if (is.null(geometry)) {
      geometry <- list(type = "Point", coordinates = c(0, 0))
    }
    features[[i]] <- list(
      type = "Feature",
      id = as.character(rois$roi_id[[i]] %||% i),
      properties = properties,
      geometry = geometry
    )
    bbox <- suppressWarnings(as.numeric(c(rois$xmin[[i]], rois$ymin[[i]], rois$xmax[[i]], rois$ymax[[i]])))
    if (length(bbox) == 4L && all(is.finite(bbox))) {
      features[[i]]$bbox <- unname(bbox)
    }
  }
  out <- getFromNamespace("wsi_roi_from_geojson", "wsiTools")(list(
    type = "FeatureCollection",
    features = features
  ))
  desktop_apply_geojson_class_colours(out)
}

desktop_decimate_tissue_rois <- function(rois, max_points_per_roi = desktop_tissue_preview_points()) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    return(rois)
  }
  if (!desktop_tissue_preview_geometry()) {
    return(desktop_tissue_list_rois(rois))
  }
  rois <- desktop_apply_geojson_class_colours(rois)
  max_points_per_roi <- suppressWarnings(as.integer(max_points_per_roi %||% 3000L))
  if (!is.finite(max_points_per_roi) || max_points_per_roi < 128L) {
    max_points_per_roi <- 3000L
  }
  features <- vector("list", nrow(rois))
  for (i in seq_len(nrow(rois))) {
    geometry_type <- as.character(rois$geometry_type[[i]] %||% "Polygon")
    ring_groups <- tryCatch(
      getFromNamespace("wsi_viewer_roi_ring_groups", "wsiTools")(geometry_type, rois$coordinates[[i]]),
      error = function(err) list()
    )
    if (length(ring_groups)) {
      ring_groups <- getFromNamespace("wsi_viewer_decimate_dense_ring_groups", "wsiTools")(
        ring_groups,
        max_points_per_roi = max_points_per_roi
      )
      geometry <- desktop_ring_groups_to_geometry(ring_groups, geometry_type)
    } else {
      geometry <- if ("geometry" %in% names(rois) && is.list(rois$geometry[[i]]) && length(rois$geometry[[i]])) {
        rois$geometry[[i]]
      } else {
        list(type = geometry_type, coordinates = rois$coordinates[[i]])
      }
    }
    properties <- if ("properties" %in% names(rois)) rois$properties[[i]] else list()
    if (!is.list(properties)) {
      properties <- list()
    }
    roi_class <- as.character(rois$class[[i]] %||% "annotation")
    if (is.na(roi_class) || !nzchar(trimws(roi_class))) {
      roi_class <- "annotation"
    }
    colour <- as.character(rois$color[[i]] %||% "#00BFC4")
    classification <- properties$classification
    if (!is.list(classification)) {
      classification <- list()
    }
    classification$name <- roi_class
    classification$color <- colour
    classification$colour <- NULL
    classification$colorRGB <- NULL
    wsi_meta <- properties$wsiTools
    if (!is.list(wsi_meta)) {
      wsi_meta <- list()
    }
    wsi_meta$preview_geometry <- TRUE
    wsi_meta$coordinate_space <- "level0_slide_pixels"
    properties$wsiTools <- wsi_meta
    properties$classification <- classification
    properties$class <- roi_class
    properties$name <- as.character(rois$name[[i]] %||% rois$roi_id[[i]] %||% roi_class)
    features[[i]] <- list(
      type = "Feature",
      id = as.character(rois$roi_id[[i]] %||% i),
      properties = properties,
      geometry = geometry
    )
    bbox <- suppressWarnings(as.numeric(c(rois$xmin[[i]], rois$ymin[[i]], rois$xmax[[i]], rois$ymax[[i]])))
    if (length(bbox) == 4L && all(is.finite(bbox))) {
      features[[i]]$bbox <- unname(bbox)
    }
  }
  out <- getFromNamespace("wsi_roi_from_geojson", "wsiTools")(list(
    type = "FeatureCollection",
    features = features
  ))
  desktop_apply_geojson_class_colours(out)
}

desktop_register_dense_geojson_source <- function(viewer, item, log_file = NULL) {
  if (!inherits(viewer, "wsi_viewer_session") ||
      !is.environment(viewer$dense_geojson_context) ||
      !inherits(item$rois, "wsi_roi") ||
      !nrow(item$rois)) {
    return(FALSE)
  }
  sources <- viewer$dense_geojson_context$sources %||% list()
  id <- item$id %||% desktop_dense_geojson_id(item$path, item$kind)
  kind <- item$kind %||% "geojson"
  is_tissue <- identical(kind, "tissue")
  sources[[id]] <- list(
    id = id,
    name = item$name %||% if (is_tissue) "Tissue annotation" else "Cell annotation",
    kind = kind,
    source_type = if (is_tissue) "annotation" else "cell_segmentation",
    path = item$path,
    rois = item$rois,
    total_count = nrow(item$rois),
    visible = TRUE,
    opacity = if (is_tissue) 0.86 else 0.92,
    colour = if (is_tissue) "#22C55E" else "#F97316",
    fill_alpha = if (is_tissue) 0.16 else 0.22,
    line_width = if (is_tissue) 2.2 else 1.8,
    max_points_per_roi = if (is_tissue) Inf else 700L,
    full_resolution_zoom = if (is_tissue) 3 else Inf
  )
  viewer$dense_geojson_context$sources <- sources
  desktop_log(
    "Registered ",
    item$kind,
    " GeoJSON for viewport-limited rendering: ",
    nrow(item$rois),
    " region(s).",
    log_file = log_file
  )
  TRUE
}

desktop_poll_pending_imports <- function(viewer, pending, log_file = NULL) {
  if (!length(pending) || !inherits(viewer, "wsi_viewer_session")) {
    return(pending)
  }
  for (i in seq_along(pending)) {
    item <- pending[[i]]
    chunk_size <- desktop_geojson_chunk_size()
    if (isTRUE(item$done)) {
      next
    }
    if (!isTRUE(item$loaded)) {
      job <- item$job
      if (!is.null(job) && isTRUE(job$is_alive())) {
        next
      }
      if (!is.null(job)) {
        status <- job$get_exit_status()
        if (!identical(status, 0L)) {
          err <- tryCatch(job$read_error(), error = function(e) "")
          desktop_log(
            "Deferred GeoJSON import failed for ",
            item$path,
            if (nzchar(err)) paste0(": ", err) else "",
            log_file = log_file
          )
          item$done <- TRUE
          pending[[i]] <- item
          next
        }
        tryCatch(job$get_result(timeout = 0), error = function(e) NULL)
      }
      if (!file.exists(item$cache_file)) {
        next
      }
      imported <- tryCatch(readRDS(item$cache_file), error = function(err) err)
      if (inherits(imported, "error")) {
        desktop_log(
          "Could not read deferred GeoJSON cache for ",
          item$path,
          ": ",
          conditionMessage(imported),
          log_file = log_file
        )
        item$done <- TRUE
        pending[[i]] <- item
        next
      }
      item$rois <- desktop_apply_geojson_class_colours(imported$rois)
      item$loaded <- TRUE
      item$offset <- 0L
      desktop_log(
        "Deferred ",
        item$kind,
        if (isTRUE(imported$cache_hit)) " GeoJSON loaded from cache: " else " GeoJSON parsed: ",
        nrow(item$rois),
        " region(s).",
        log_file = log_file
      )
      if (identical(item$kind, "tissue") && !desktop_auto_send_large_geojson()) {
        full_rois <- item$rois
        registered <- desktop_register_dense_geojson_source(viewer, item, log_file = log_file)
        item$rois <- tryCatch(
          desktop_decimate_tissue_rois(full_rois, max_points_per_roi = desktop_tissue_preview_points()),
          error = function(err) {
            desktop_log(
              "Could not prepare tissue annotation preview; using viewport-only rendering: ",
              conditionMessage(err),
              log_file = log_file
            )
            NULL
          }
        )
        if (inherits(item$rois, "wsi_roi") && nrow(item$rois)) {
          item$compact_list_only <- !desktop_tissue_preview_geometry()
          desktop_log(
            "Large tissue GeoJSON will be listed in the Annotations panel as class-coloured compact entries; ",
            "full outlines are rendered by viewport at 3x zoom and above.",
            log_file = log_file
          )
        } else {
          desktop_log(
            "Large tissue GeoJSON was registered for viewport rendering but no preview ROIs could be queued. ",
            "Set WSITOOLS_DESKTOP_AUTO_SEND_LARGE_GEOJSON=true if you explicitly want to stream all full-resolution regions as editable ROIs.",
            log_file = log_file
          )
          item$done <- TRUE
          pending[[i]] <- item
          next
        }
      } else if (identical(item$kind, "cell") && !desktop_auto_send_large_geojson()) {
        registered <- desktop_register_dense_geojson_source(viewer, item, log_file = log_file)
        desktop_log(
          "Large ",
          item$kind,
          if (isTRUE(registered)) {
            " GeoJSON will be streamed to the browser by viewport instead of queued as one large sync payload. "
          } else {
            " GeoJSON was parsed but not auto-sent to the browser to keep the tissue viewer and R sync responsive. "
          },
          "Set WSITOOLS_DESKTOP_AUTO_SEND_LARGE_GEOJSON=true before launching if you explicitly want to stream all regions as editable vector ROIs.",
          log_file = log_file
        )
        item$done <- TRUE
        item$rois <- NULL
        pending[[i]] <- item
        next
      }
    }
    if (!is.data.frame(item$rois) || !nrow(item$rois)) {
      item$done <- TRUE
      pending[[i]] <- item
      next
    }
    if (isTRUE(item$compact_list_only)) {
      chunk_size <- max(1L, nrow(item$rois))
    } else if (identical(item$kind, "tissue")) {
      chunk_size <- min(desktop_geojson_chunk_size(), 12L)
    }
    start <- item$offset + 1L
    end <- min(nrow(item$rois), item$offset + chunk_size)
    if (start <= end) {
      chunk <- item$rois[start:end, , drop = FALSE]
      tryCatch(
        viewer$add_rois(chunk, name = item$name, service = FALSE),
        error = function(err) {
          desktop_log(
            "Could not send deferred GeoJSON chunk for ",
            item$path,
            ": ",
            conditionMessage(err),
            log_file = log_file
          )
        }
      )
      item$offset <- end
      if (end >= nrow(item$rois)) {
        item$done <- TRUE
        desktop_log(
          "Finished sending deferred ",
          item$kind,
          " GeoJSON to viewer: ",
          item$path,
          log_file = log_file
        )
      }
    } else {
      item$done <- TRUE
    }
    pending[[i]] <- item
  }
  pending
}

desktop_add_annotation_files <- function(viewer, cell_annotation = NULL,
                                         tissue_annotation = NULL,
                                         image = NULL,
                                         output = NULL,
                                         log_file = NULL) {
  if (!inherits(viewer, "wsi_viewer_session")) {
    return(invisible(list()))
  }
  pending <- list()
  if (!is.null(tissue_annotation) && nzchar(tissue_annotation) && file.exists(tissue_annotation)) {
    ext <- tolower(tools::file_ext(tissue_annotation))
    if (ext %in% c("geojson", "json")) {
      deferred <- if (desktop_should_defer_geojson(tissue_annotation)) {
        desktop_start_geojson_import_job(
          tissue_annotation,
          output = output,
          name = "Tissue annotation",
          kind = "tissue",
          log_file = log_file
        )
      } else {
        NULL
      }
      if (is.null(deferred)) {
        rois <- wsiTools::read_geojson(tissue_annotation)
        viewer$add_rois(rois, name = "Tissue annotation", service = FALSE)
        desktop_log(
          "Added tissue annotation: ",
          tissue_annotation,
          " (",
          desktop_geojson_geometry_summary(rois),
          "). Non-polygon geometries are listed but excluded from area/proximity summaries.",
          log_file = log_file
        )
      } else {
        pending[[length(pending) + 1L]] <- deferred
      }
    } else {
      desktop_log("Skipped tissue annotation with unsupported extension: ", tissue_annotation, log_file = log_file)
    }
  }
  if (!is.null(cell_annotation) && nzchar(cell_annotation) && file.exists(cell_annotation)) {
    ext <- tolower(tools::file_ext(cell_annotation))
    if (ext %in% c("geojson", "json")) {
      deferred <- if (desktop_should_defer_geojson(cell_annotation)) {
        desktop_start_geojson_import_job(
          cell_annotation,
          output = output,
          name = "Cell annotation",
          kind = "cell",
          log_file = log_file
        )
      } else {
        NULL
      }
      if (is.null(deferred)) {
        rois <- wsiTools::read_geojson(cell_annotation)
        rois <- desktop_mark_dense_cell_rois(rois)
        viewer$add_rois(rois, name = "Cell annotation", service = FALSE)
        desktop_log("Added cell annotation as vector geometry: ", cell_annotation, log_file = log_file)
      } else {
        pending[[length(pending) + 1L]] <- deferred
      }
    } else if (ext %in% c("csv", "tsv")) {
      cells <- wsiTools::import_segmentation(cell_annotation)
      viewer$add_segmentation(cells, name = "Cell annotation", service = FALSE)
      desktop_log("Added cell annotation: ", cell_annotation, log_file = log_file)
    } else if (ext %in% c("tif", "tiff", "png", "jpg", "jpeg")) {
      viewer$add_layer(
        "Cell annotation mask",
        cell_annotation,
        type = "image",
        opacity = 0.65,
        service = FALSE
      )
      desktop_log("Added cell annotation mask layer: ", cell_annotation, log_file = log_file)
    } else {
      desktop_log("Skipped cell annotation with unsupported extension: ", cell_annotation, log_file = log_file)
    }
  }
  invisible(pending)
}

desktop_open_spatial_target <- function(object, image_paths, output, log_file,
                                        sample_ids = NULL,
                                        initial_rois = NULL) {
  if (!is.null(sample_ids)) {
    sample_ids <- as.character(sample_ids)
    sample_ids[is.na(sample_ids)] <- ""
    if (length(sample_ids) != length(image_paths) || any(!nzchar(sample_ids))) {
      desktop_log(
        "Ignoring incomplete spatial tissue/sample mapping from the desktop app.",
        log_file = log_file
      )
      sample_ids <- NULL
    } else {
      names(image_paths) <- sample_ids
      desktop_log(
        "Using explicit spatial tissue/sample mapping: ",
        paste(sprintf("%s -> %s", basename(image_paths), sample_ids), collapse = "; "),
        log_file = log_file
      )
    }
  }
  if (length(image_paths) == 1L) {
    tile_dir <- file.path(dirname(output), desktop_project_source_id(image_paths[[1L]], 1L))
    slide_for_tiles <- tryCatch(wsiTools::wsi_open(image_paths[[1L]]), error = function(err) NULL)
    use_prebuilt_tiles <- !is.null(slide_for_tiles) &&
      desktop_deepzoom_cache_valid(slide_for_tiles, tile_dir)
    if (!is.null(slide_for_tiles)) {
      try(wsiTools::wsi_close(slide_for_tiles), silent = TRUE)
    }
    if (!use_prebuilt_tiles && desktop_build_missing_tiles()) {
      use_prebuilt_tiles <- TRUE
    }
    single_args <- list(
      object,
      image_paths[[1L]],
      live = TRUE,
      dynamic_tiles = !use_prebuilt_tiles,
      mode = "tiles",
      tile_format = "jpg",
      tile_overlap = 1,
      quality = 90,
      rebuild = FALSE,
      max_points = desktop_spatial_max_points(),
      open = FALSE,
      wait = FALSE,
      output = output,
      overwrite = TRUE
    )
    if (use_prebuilt_tiles) {
      single_args$tile_dir <- tile_dir
    } else {
      single_args$progressive_preview <- FALSE
      desktop_log(
        "No valid prebuilt Deep Zoom tile cache was found for ",
        basename(image_paths[[1L]]),
        ". Opening spatial viewer immediately with live dynamic tiles.",
        log_file = log_file
      )
    }
    if (!is.null(initial_rois)) {
      single_args$roi <- initial_rois
    }
    if (!is.null(sample_ids) && length(sample_ids) == 1L) {
      single_args$image_name <- sample_ids[[1L]]
      if (inherits(object, "SpatialExperiment") ||
          inherits(object, "SingleCellExperiment") ||
          inherits(object, "SummarizedExperiment")) {
        single_args$sample_id <- sample_ids[[1L]]
      }
    }
    return(do.call(wsiTools::wsi_viewer_spatial, single_args))
  }
  if (inherits(object, "Seurat")) {
    return(wsiTools::wsi_viewer_seurat_project(
      seurat = object,
      images = image_paths,
      image_names = sample_ids,
	      labels = sample_ids,
	      live = TRUE,
	      dynamic_tiles = FALSE,
	      max_points = desktop_spatial_max_points(),
	      open = FALSE,
	      wait = FALSE,
	      output = output,
      overwrite = TRUE
    ))
  }
  if (inherits(object, "SpatialExperiment") ||
      inherits(object, "SingleCellExperiment") ||
      inherits(object, "SummarizedExperiment")) {
    return(wsiTools::wsi_viewer_spatialexperiment_project(
      spe = object,
      images = image_paths,
      sample_ids = sample_ids,
      image_names = sample_ids,
	      labels = sample_ids,
	      live = TRUE,
	      dynamic_tiles = FALSE,
	      max_points = desktop_spatial_max_points(),
	      open = FALSE,
	      wait = FALSE,
	      output = output,
      overwrite = TRUE
    ))
  }
  desktop_log(
    "Spatial object is not a recognised multi-image object; opening image project without spatial overlay.",
    log_file = log_file
  )
  desktop_open_live_image_project(
    image_paths,
    output = output,
    log_file = log_file,
    initial_rois = initial_rois
  )
}

desktop_spatial_fallback_message <- function(err, items) {
  msg <- conditionMessage(err)
  lower <- tolower(msg)
  has_cell_annotation <- any(vapply(items, function(item) {
    nzchar(item$cell_annotation %||% "")
  }, logical(1)))
  if (grepl("could not extract spatial coordinates|spatial coordinates", lower)) {
    extra <- paste(
      "This usually means the selected spatial object does not contain slide x/y coordinates.",
      "For cell-level Seurat/VisiumHD projects, associate a cell annotation GeoJSON,",
      "cell-centroid CSV, or cell mask generated from the segmentation so wsiTools can",
      "place cells on the image."
    )
    if (!has_cell_annotation) {
      extra <- paste(
        extra,
        "No cell annotation file was selected in this desktop project, so the viewer",
        "will open the microscopy image without a spatial/cell overlay."
      )
    }
    return(paste(msg, extra))
  }
  paste(
    msg,
    "The desktop app will open the microscopy image project without the spatial overlay."
  )
}

desktop_project_source_id <- function(path, index) {
  base <- tools::file_path_sans_ext(basename(path))
  base <- gsub("[^A-Za-z0-9_]+", "_", base)
  base <- gsub("^_+|_+$", "", base)
  path_key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  values <- utf8ToInt(path_key)
  hash <- if (length(values)) {
    sum((seq_along(values) * values) %% 100000000) %% 100000000
  } else {
    0
  }
  paste0("desktop_project_", as.integer(index), "_", base %||% "image", "_", sprintf("%08d", as.integer(hash)))
}

desktop_tile_base_url <- function(tile_dir, output) {
  root <- normalizePath(dirname(output), winslash = "/", mustWork = FALSE)
  tile_files <- normalizePath(file.path(tile_dir, "slide_files"), winslash = "/", mustWork = FALSE)
  if (startsWith(tile_files, paste0(root, "/"))) {
    return(utils::URLencode(sub(paste0("^", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", root), "/"), "", tile_files), reserved = FALSE))
  }
  paste0("file://", utils::URLencode(tile_files, reserved = FALSE))
}

desktop_build_missing_tiles <- function() {
  tolower(Sys.getenv("WSITOOLS_DESKTOP_BUILD_MISSING_TILES", "false")) %in%
    c("1", "true", "yes", "on")
}

desktop_deepzoom_cache_valid <- function(slide, tile_dir,
                                         tile_size = 512,
                                         tile_overlap = 1,
                                         tile_format = "jpg",
                                         quality = 90) {
  status <- tryCatch(
    getFromNamespace("wsi_deepzoom_cache_status", "wsiTools")(
      slide = slide,
      tile_dir = tile_dir,
      dzi_file = file.path(tile_dir, "slide.dzi"),
      tile_files = file.path(tile_dir, "slide_files"),
      tile_size = tile_size,
      tile_overlap = tile_overlap,
      tile_format = tile_format,
      quality = quality
    ),
    error = function(err) list(valid = FALSE, reason = conditionMessage(err))
  )
  isTRUE(status$valid)
}

desktop_create_deepzoom_project_item <- function(slide, index, output, log_file,
                                                 active = FALSE) {
  source_id <- desktop_project_source_id(slide$path %||% sprintf("image_%d", index), index)
  tile_dir <- file.path(dirname(output), source_id)
  create_tiles <- getFromNamespace("wsi_create_deepzoom_tiles", "wsiTools")
  dz_max_level <- getFromNamespace("wsi_dz_max_level", "wsiTools")
  tiles <- create_tiles(
    slide = slide,
    tile_dir = tile_dir,
    tile_size = 512,
    tile_overlap = 1,
    tile_format = "jpg",
    quality = 90,
    rebuild = FALSE
  )
  desktop_log(
    "Using prebuilt Deep Zoom tiles for ",
    basename(slide$path %||% source_id),
    ": ",
    tile_dir,
    log_file = log_file
  )
  list(
    id = source_id,
    label = basename(slide$path %||% source_id),
    path = slide$path %||% "",
    backend = slide$backend %||% "deepzoom",
    type = "slide",
    status = if (isTRUE(active)) "active" else "fast prebuilt tiles",
    message = if (isTRUE(active)) "" else "Using fast prebuilt Deep Zoom tiles.",
    width = unname(as.numeric(slide$dimensions[["width"]])),
    height = unname(as.numeric(slide$dimensions[["height"]])),
    tile_url_base = desktop_tile_base_url(tile_dir, output),
    tile_url_style = "deepzoom",
    tile_format = "jpg",
    tile_size = 512,
    tile_overlap = tiles$overlap %||% 1L,
    min_level = 0L,
    max_level = dz_max_level(slide$dimensions[["width"]], slide$dimensions[["height"]]),
    image_data_uri = NULL,
    navigator_image_data_uri = tryCatch(
      getFromNamespace("wsi_viewer_navigator_data_uri", "wsiTools")(slide, width = 512),
      error = function(err) NULL
    ),
    sections = list(),
    active = isTRUE(active)
  )
}

desktop_create_dynamic_project_source <- function(slide, index, log_file,
                                                  active = FALSE,
                                                  tile_format = "jpg") {
  source_id <- desktop_project_source_id(slide$path %||% sprintf("image_%d", index), index)
  source <- wsiTools::wsi_dynamic_tile_source(
    slide,
    slide_id = source_id,
    tile_size = 512,
    tile_overlap = 1,
    format = tile_format
  )
  label <- basename(slide$path %||% source_id)
  source$name <- label
  source$metadata <- list(
    project_item_id = source_id,
    id = source_id,
    label = label,
    path = slide$path %||% "",
    backend = slide$backend %||% "dynamic",
    type = "slide",
    status = if (isTRUE(active)) "active" else "live dynamic tiles",
    message = "Full-resolution tiles are served on demand by the live R session.",
    active = isTRUE(active)
  )
  desktop_log(
    "Using live dynamic tiles for ",
    label,
    ".",
    log_file = log_file
  )
  source
}

desktop_open_live_slide_prebuilt <- function(slide, output, log_file,
                                             project_images = list(),
                                             project_tile_sources = list(),
                                             initial_rois = NULL,
                                             title = "wsiTools desktop viewer") {
  base_id <- desktop_project_source_id(slide$path %||% "image", 1L)
  base_tile_dir <- file.path(dirname(output), base_id)
  use_prebuilt_tiles <- desktop_deepzoom_cache_valid(slide, base_tile_dir) ||
    desktop_build_missing_tiles()
  if (!use_prebuilt_tiles) {
    desktop_log(
      "No valid prebuilt Deep Zoom tile cache was found for ",
      basename(slide$path %||% base_id),
      ". Starting immediately with live dynamic tiles to avoid desktop launch timeout. ",
      "Set WSITOOLS_DESKTOP_BUILD_MISSING_TILES=true to build prebuilt tiles before opening.",
      log_file = log_file
    )
    return(wsiTools::wsi_viewer_live(
      slide,
      mode = "tiles",
      dynamic_tiles = TRUE,
      dynamic_tile_format = "jpg",
      progressive_preview = FALSE,
      project_images = project_images,
      project_tile_sources = project_tile_sources,
      roi = initial_rois,
      open = FALSE,
      wait = FALSE,
      output = output,
      overwrite = TRUE,
      title = title
    ))
  }
  tryCatch(
    {
      desktop_log(
        "Opening with fast prebuilt Deep Zoom tiles and live R synchronization: ",
        basename(slide$path %||% base_id),
        log_file = log_file
      )
      wsiTools::wsi_viewer_live(
        slide,
        mode = "tiles",
        dynamic_tiles = FALSE,
        tile_dir = base_tile_dir,
        tile_format = "jpg",
        tile_overlap = 1,
        rebuild = FALSE,
        project_images = project_images,
        project_tile_sources = project_tile_sources,
        roi = initial_rois,
        open = FALSE,
        wait = FALSE,
        output = output,
        overwrite = TRUE,
        title = title
      )
    },
    error = function(err) {
      desktop_log(
        "Could not start live prebuilt tiled viewer for ",
        basename(slide$path %||% base_id),
        ": ",
        conditionMessage(err),
        log_file = log_file
      )
      stop(err)
    }
  )
}

desktop_open_live_image_project <- function(image_paths, output, log_file,
                                            initial_rois = NULL) {
  if (!length(image_paths)) {
    stop("No image paths were supplied for the desktop project.", call. = FALSE)
  }
  image_paths <- normalizePath(image_paths, winslash = "/", mustWork = TRUE)
  czi_paths <- vapply(image_paths, desktop_is_czi_path, logical(1))
  if (all(czi_paths)) {
    return(desktop_open_czi_project(
      image_paths,
      output = output,
      log_file = log_file,
      title = "wsiTools desktop CZI project viewer"
    ))
  }
  if (any(czi_paths)) {
    stop(
      "Mixed CZI and non-CZI desktop image projects are not opened through one route yet. ",
      "Open the CZI files as a CZI project, or convert them to pyramidal OME-TIFF first.",
      call. = FALSE
    )
  }
  desktop_log(
    "Opening live tiled image project with ",
    length(image_paths),
    " image(s).",
    log_file = log_file
  )
  slides <- lapply(image_paths, function(path) {
    wsiTools::wsi_open(path)
  })
  project_items <- list()
  project_tile_sources <- list()
  if (length(slides) > 1L) {
    for (i in seq.int(2L, length(slides))) {
      slide <- slides[[i]]
      tile_dir <- file.path(dirname(output), desktop_project_source_id(slide$path %||% image_paths[[i]], i))
      if (!desktop_deepzoom_cache_valid(slide, tile_dir) && !desktop_build_missing_tiles()) {
        source <- desktop_create_dynamic_project_source(
          slide,
          index = i,
          log_file = log_file,
          active = FALSE,
          tile_format = "jpg"
        )
        project_tile_sources[[length(project_tile_sources) + 1L]] <- source
        item <- source$metadata
      } else {
        item <- tryCatch(
          desktop_create_deepzoom_project_item(
            slide,
            index = i,
            output = output,
            log_file = log_file,
            active = FALSE
          ),
          error = function(err) {
            desktop_log(
              "Could not prepare fast prebuilt tiles for ",
              basename(slide$path %||% image_paths[[i]]),
              ": ",
              conditionMessage(err),
              log_file = log_file
            )
            source <- desktop_create_dynamic_project_source(
              slide,
              index = i,
              log_file = log_file,
              active = FALSE,
              tile_format = "jpg"
            )
            project_tile_sources[[length(project_tile_sources) + 1L]] <<- source
            source$metadata
          }
        )
      }
      project_items[[length(project_items) + 1L]] <- item
    }
  }

  desktop_open_live_slide_prebuilt(
    slides[[1L]],
    output = output,
    log_file = log_file,
    project_images = project_items,
    project_tile_sources = project_tile_sources,
    initial_rois = initial_rois,
    title = "wsiTools desktop project viewer"
  )
}

desktop_apply_associated_data <- function(viewer, items, output, log_file) {
  skip_tissue_indices <- attr(viewer, "desktop_initial_tissue_index", exact = TRUE)
  skip_tissue_indices <- as.integer(skip_tissue_indices %||% integer())
  pending <- list()
  if (inherits(viewer, "wsi_viewer_session")) {
    for (i in seq_along(items)) {
      item <- items[[i]]
      desktop_log("Applying associated data for image ", i, ": ", basename(item$image), log_file = log_file)
      tissue_annotation <- item$tissue_annotation
      if (i %in% skip_tissue_indices) {
        tissue_annotation <- NULL
      }
      jobs <- desktop_add_annotation_files(
        viewer,
        cell_annotation = item$cell_annotation,
        tissue_annotation = tissue_annotation,
        image = item$image,
        output = output,
        log_file = log_file
      )
      if (length(jobs)) {
        pending <- c(pending, jobs)
      }
    }
  } else {
    any_annotations <- any(vapply(items, function(item) {
      nzchar(item$cell_annotation %||% "") || nzchar(item$tissue_annotation %||% "")
    }, logical(1)))
    if (any_annotations) {
      desktop_log(
        "Annotation files were associated, but the selected image route returned a static project viewer. ",
        "Open a live single-image or spatial project route to inject annotations automatically.",
        log_file = log_file
      )
    }
  }
  invisible(pending)
}

desktop_open_new_project <- function(items, output, log_file) {
  image_paths <- vapply(items, function(item) item$image, character(1))
  image_paths <- normalizePath(image_paths, winslash = "/", mustWork = TRUE)
  sample_ids <- vapply(items, function(item) item$spatial_sample_id %||% "", character(1))
  sample_ids <- if (any(nzchar(sample_ids))) sample_ids else NULL
  spatial_paths <- unique(vapply(items, function(item) item$spatial_data %||% "", character(1)))
  spatial_paths <- spatial_paths[nzchar(spatial_paths)]
  initial_rois <- desktop_initial_tissue_rois(items, log_file = log_file)
  desktop_log("Opening new project with ", length(image_paths), " image(s).", log_file = log_file)
  if (length(spatial_paths) == 1L) {
    desktop_log("Loading spatial transcriptomics object: ", spatial_paths[[1L]], log_file = log_file)
    spatial_object <- desktop_load_spatial_object(spatial_paths[[1L]])
    viewer <- tryCatch(
      desktop_open_spatial_target(
        spatial_object,
        image_paths,
        output,
        log_file,
        sample_ids = sample_ids,
        initial_rois = initial_rois
      ),
      error = function(err) {
        desktop_log(
          "Spatial overlay could not be created: ",
          desktop_spatial_fallback_message(err, items),
          log_file = log_file
        )
        desktop_log(
          "Opening the image project without the spatial overlay so the viewer can still start.",
          log_file = log_file
        )
        desktop_open_live_image_project(
          image_paths,
          output = output,
          log_file = log_file,
          initial_rois = initial_rois
        )
      }
    )
  } else if (length(spatial_paths) > 1L) {
    desktop_log(
      "Multiple different spatial transcriptomics files were associated. ",
      "Opening the images first; per-image multi-object spatial loading will be handled by a later desktop workflow.",
      log_file = log_file
    )
    viewer <- desktop_open_live_image_project(
      image_paths,
      output = output,
      log_file = log_file,
      initial_rois = initial_rois
    )
  } else {
    viewer <- desktop_open_live_image_project(
      image_paths,
      output = output,
      log_file = log_file,
      initial_rois = initial_rois
    )
  }

  if (!is.null(initial_rois)) {
    attr(viewer, "desktop_initial_tissue_index") <- 1L
  }
  viewer
}

desktop_open_target <- function(target_path, mode, output, log_file) {
  project <- NULL
  if (identical(mode, "project")) {
    target_path <- normalizePath(target_path, winslash = "/", mustWork = TRUE)
    desktop_log("Opening project: ", target_path, log_file = log_file)
    project <- wsiTools::wsi_read_project(target_path, open_slide = FALSE)
    image_path <- desktop_project_slide_path(project)
  } else {
    image_path <- normalizePath(target_path, winslash = "/", mustWork = TRUE)
    desktop_log("Opening image: ", image_path, log_file = log_file)
  }

  if (desktop_is_czi_path(image_path)) {
    viewer <- desktop_open_czi_project(
      image_path,
      output = output,
      log_file = log_file,
      title = "wsiTools desktop CZI viewer"
    )
    if (inherits(project, "wsi_project")) {
      wsiTools::restore_project_state(viewer, project, service = FALSE)
      desktop_log("Restored project state.", log_file = log_file)
    }
    return(viewer)
  }

  slide <- wsiTools::wsi_open(image_path)
  viewer <- desktop_open_live_slide_prebuilt(
    slide,
    output = output,
    log_file = log_file,
    title = "wsiTools desktop viewer"
  )

  if (!inherits(viewer, "wsi_viewer_session")) {
    stop(
      "The desktop app requires a live wsiTools viewer session. ",
      "Install the optional R package httpuv and try again.",
      call. = FALSE
    )
  }

  if (inherits(project, "wsi_project")) {
    wsiTools::restore_project_state(viewer, project, service = FALSE)
    desktop_log("Restored project state.", log_file = log_file)
  }

  viewer
}

main <- function() {
  parsed <- desktop_parse_args(args)
  mode <- parsed$mode
  session_dir <- Sys.getenv("WSITOOLS_DESKTOP_SESSION_DIR", unset = tempdir())
  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(
    session_dir,
    paste0("wsiTools-desktop-r-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".log")
  )
  Sys.setenv(WSITOOLS_DESKTOP_LOG_FILE = log_file)
  desktop_emit("LOG_FILE", normalizePath(log_file, winslash = "/", mustWork = FALSE))

  load_wsitools()
  desktop_log_runtime_diagnostics(log_file = log_file)

  output <- file.path(session_dir, "wsiTools_desktop_live_viewer.html")
  viewer <- if (identical(mode, "new-project")) {
    desktop_open_new_project(
      parsed$items,
      output = output,
      log_file = log_file
    )
  } else {
    desktop_open_target(parsed$path, mode, output, log_file)
  }

  is_live <- inherits(viewer, "wsi_viewer_session")
  html <- if (is_live) {
    normalizePath(viewer$html, winslash = "/", mustWork = FALSE)
  } else if (is.character(viewer) && length(viewer) == 1L) {
    normalizePath(viewer, winslash = "/", mustWork = FALSE)
  } else {
    stop("The desktop R launcher did not receive a valid viewer object.", call. = FALSE)
  }
  if (is_live && identical(Sys.getenv("WSITOOLS_DESKTOP_PREWARM_TILES", unset = "false"), "true")) {
    warmed <- tryCatch(
      getFromNamespace("wsi_dynamic_prewarm_tiles", "wsiTools")(
        viewer$dynamic_tile_sources %||% list(),
        timeout_warning = FALSE
      ),
      error = function(err) {
        desktop_log("Dynamic tile prewarm skipped: ", conditionMessage(err), log_file = log_file)
        0L
      }
    )
    desktop_log("Prewarmed ", as.integer(warmed), " dynamic tile cache entries.", log_file = log_file)
  }
  html_server <- desktop_start_html_server(html)
  session_token <- paste(Sys.getpid(), format(Sys.time(), "%Y%m%d%H%M%OS3"), sep = "-")
  viewer_url <- paste0(
    html_server$url,
    if (grepl("\\?", html_server$url)) "&" else "?",
    "session=",
    utils::URLencode(session_token, reserved = TRUE)
  )
  desktop_emit("VIEWER_FILE", html)
  desktop_emit("SYNC_URL", if (is_live) viewer$url %||% "" else "")
  desktop_emit("VIEWER_HTTP_URL", html_server$url)
  desktop_emit("VIEWER_URL", viewer_url)
  desktop_log("Live viewer ready: ", desktop_file_url(html), log_file = log_file)
  desktop_log("Desktop viewer served at: ", viewer_url, log_file = log_file)
  desktop_log("Sync endpoint: ", if (is_live) viewer$url %||% "not available" else "not available for static viewer", log_file = log_file)

  on.exit({
    try(httpuv::stopServer(html_server$server), silent = TRUE)
    if (is_live) {
      try(wsiTools::wsi_viewer_stop(viewer), silent = TRUE)
    }
    desktop_log("Stopped wsiTools live viewer.", log_file = log_file)
  }, add = TRUE)

  if (is_live) {
    for (i in seq_len(25L)) {
      try(wsiTools::wsi_viewer_service(viewer, timeout = 20L), silent = TRUE)
      try(httpuv::service(20L), silent = TRUE)
      Sys.sleep(0.01)
    }
  }

  pending_imports <- list()
  if (identical(mode, "new-project")) {
    pending_imports <- tryCatch(
      desktop_apply_associated_data(viewer, parsed$items, output = output, log_file = log_file),
      error = function(err) {
        desktop_log("Associated data could not be applied after viewer startup: ", conditionMessage(err), log_file = log_file)
        list()
      }
    )
  }

  repeat {
    if (is_live) {
      pending_imports <- desktop_poll_pending_imports(viewer, pending_imports, log_file = log_file)
      wsiTools::wsi_viewer_service(viewer, timeout = 100L)
      Sys.sleep(0.02)
    } else {
      Sys.sleep(0.5)
    }
  }
}

tryCatch(
  main(),
  error = function(err) {
    msg <- conditionMessage(err)
    desktop_log("WSITOOLS_DESKTOP_ERROR=", msg, log_file = desktop_error_log_file())
    message("WSITOOLS_DESKTOP_ERROR=", msg)
    quit(status = 1L)
  }
)
