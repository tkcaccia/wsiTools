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
            spatial_data = NULL
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

desktop_add_annotation_files <- function(viewer, cell_annotation = NULL,
                                         tissue_annotation = NULL,
                                         log_file = NULL) {
  if (!inherits(viewer, "wsi_viewer_session")) {
    return(invisible(viewer))
  }
  if (!is.null(tissue_annotation) && nzchar(tissue_annotation) && file.exists(tissue_annotation)) {
    ext <- tolower(tools::file_ext(tissue_annotation))
    if (ext %in% c("geojson", "json")) {
      rois <- wsiTools::read_geojson(tissue_annotation)
      viewer$add_rois(rois, name = "Tissue annotation", service = FALSE)
      desktop_log("Added tissue annotation: ", tissue_annotation, log_file = log_file)
    } else {
      desktop_log("Skipped tissue annotation with unsupported extension: ", tissue_annotation, log_file = log_file)
    }
  }
  if (!is.null(cell_annotation) && nzchar(cell_annotation) && file.exists(cell_annotation)) {
    ext <- tolower(tools::file_ext(cell_annotation))
    if (ext %in% c("geojson", "json", "csv", "tsv")) {
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
  invisible(viewer)
}

desktop_open_spatial_target <- function(object, image_paths, output, log_file) {
  if (length(image_paths) == 1L) {
    return(wsiTools::wsi_viewer_spatial(
      object,
      image_paths[[1L]],
      live = TRUE,
      dynamic_tiles = FALSE,
      mode = "tiles",
      tile_dir = file.path(dirname(output), desktop_project_source_id(image_paths[[1L]], 1L)),
      tile_format = "jpg",
      tile_overlap = 1,
      quality = 90,
      rebuild = FALSE,
      open = FALSE,
      wait = FALSE,
      output = output,
      overwrite = TRUE
    ))
  }
  if (inherits(object, "Seurat")) {
    return(wsiTools::wsi_viewer_seurat_project(
      seurat = object,
      images = image_paths,
      live = TRUE,
      dynamic_tiles = FALSE,
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
      live = TRUE,
      dynamic_tiles = FALSE,
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
  desktop_open_live_image_project(image_paths, output = output, log_file = log_file)
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

desktop_open_live_slide_prebuilt <- function(slide, output, log_file,
                                             project_images = list(),
                                             title = "wsiTools desktop viewer") {
  base_id <- desktop_project_source_id(slide$path %||% "image", 1L)
  base_tile_dir <- file.path(dirname(output), base_id)
  tryCatch(
    {
      desktop_log(
        "Opening with fast prebuilt Deep Zoom tiles: ",
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
        quality = 90,
        rebuild = FALSE,
        project_images = project_images,
        open = FALSE,
        wait = FALSE,
        output = output,
        overwrite = TRUE,
        title = title
      )
    },
    error = function(err) {
      desktop_log(
        "Could not create prebuilt tiles for ",
        basename(slide$path %||% base_id),
        "; falling back to live on-demand tiles: ",
        conditionMessage(err),
        log_file = log_file
      )
      wsiTools::wsi_viewer_live(
        slide,
        mode = "tiles",
        dynamic_tiles = TRUE,
        dynamic_tile_format = "jpg",
        project_images = project_images,
        open = FALSE,
        wait = FALSE,
        output = output,
        overwrite = TRUE,
        title = title
      )
    }
  )
}

desktop_open_live_image_project <- function(image_paths, output, log_file) {
  if (!length(image_paths)) {
    stop("No image paths were supplied for the desktop project.", call. = FALSE)
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
  if (length(slides) > 1L) {
    for (i in seq.int(2L, length(slides))) {
      slide <- slides[[i]]
      item <- tryCatch(
        desktop_create_deepzoom_project_item(
          slide,
          index = i,
          output = output,
          log_file = log_file
        ),
        error = function(err) {
          desktop_log(
            "Could not prebuild tiles for ",
            basename(slide$path %||% image_paths[[i]]),
            ": ",
            conditionMessage(err),
            log_file = log_file
          )
          list(
            id = desktop_project_source_id(slide$path %||% image_paths[[i]], i),
            label = basename(slide$path %||% image_paths[[i]]),
            path = slide$path %||% image_paths[[i]],
            backend = slide$backend %||% "unavailable",
            type = "slide",
            status = "tile build failed",
            message = paste("Could not create fast Deep Zoom tiles:", conditionMessage(err)),
            width = unname(as.numeric(slide$dimensions[["width"]])),
            height = unname(as.numeric(slide$dimensions[["height"]])),
            sections = list(),
            active = FALSE
          )
        }
      )
      project_items[[length(project_items) + 1L]] <- item
    }
  }

  desktop_open_live_slide_prebuilt(
    slides[[1L]],
    output = output,
    log_file = log_file,
    project_images = project_items,
    title = "wsiTools desktop project viewer"
  )
}

desktop_open_new_project <- function(items, output, log_file) {
  image_paths <- vapply(items, function(item) item$image, character(1))
  image_paths <- normalizePath(image_paths, winslash = "/", mustWork = TRUE)
  spatial_paths <- unique(vapply(items, function(item) item$spatial_data %||% "", character(1)))
  spatial_paths <- spatial_paths[nzchar(spatial_paths)]
  desktop_log("Opening new project with ", length(image_paths), " image(s).", log_file = log_file)
  if (length(spatial_paths) == 1L) {
    desktop_log("Loading spatial transcriptomics object: ", spatial_paths[[1L]], log_file = log_file)
    spatial_object <- desktop_load_spatial_object(spatial_paths[[1L]])
    viewer <- desktop_open_spatial_target(spatial_object, image_paths, output, log_file)
  } else if (length(spatial_paths) > 1L) {
    desktop_log(
      "Multiple different spatial transcriptomics files were associated. ",
      "Opening the images first; per-image multi-object spatial loading will be handled by a later desktop workflow.",
      log_file = log_file
    )
    viewer <- desktop_open_live_image_project(image_paths, output = output, log_file = log_file)
  } else {
    viewer <- desktop_open_live_image_project(image_paths, output = output, log_file = log_file)
  }

  if (inherits(viewer, "wsi_viewer_session")) {
    for (i in seq_along(items)) {
      item <- items[[i]]
      desktop_log("Applying associated data for image ", i, ": ", basename(item$image), log_file = log_file)
      desktop_add_annotation_files(
        viewer,
        cell_annotation = item$cell_annotation,
        tissue_annotation = item$tissue_annotation,
        log_file = log_file
      )
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

  repeat {
    if (is_live) {
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
