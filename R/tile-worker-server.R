wsi_dynamic_tile_worker_count <- function(value = NULL) {
  value <- value %||% Sys.getenv("WSITOOLS_DYNAMIC_TILE_WORKERS", unset = "2")
  value <- suppressWarnings(as.integer(value))
  if (length(value) != 1L || is.na(value) || value < 0L) {
    value <- 2L
  }
  min(value, 4L)
}

wsi_dynamic_tile_worker_sources <- function(sources) {
  lapply(sources, function(source) {
    if (inherits(source, "wsi_dynamic_czi_section_tile_source")) {
      source$czi_handle <- NULL
    }
    source
  })
}

wsi_dynamic_tile_worker_start <- function(sources, host = "127.0.0.1",
                                          workers = NULL,
                                          startup_timeout = 15) {
  workers <- wsi_dynamic_tile_worker_count(workers)
  if (!length(sources) || workers < 1L || !wsi_has_callr() ||
      !requireNamespace("httpuv", quietly = TRUE)) {
    return(NULL)
  }
  sources <- wsi_dynamic_tile_worker_sources(sources)
  worker_dir <- tempfile("wsi_tile_workers_")
  dir.create(worker_dir, recursive = TRUE, showWarnings = FALSE)
  processes <- list()
  urls <- character()

  stop_started <- function() {
    for (process in processes) {
      if (isTRUE(tryCatch(process$is_alive(), error = function(err) FALSE))) {
        try(process$kill(), silent = TRUE)
      }
    }
    unlink(worker_dir, recursive = TRUE, force = TRUE)
  }

  for (index in seq_len(workers)) {
    ready_file <- file.path(worker_dir, sprintf("worker_%d.rds", index))
    requested_port <- httpuv::randomPort()
    process <- callr::r_bg(
      func = function(sources, host, requested_port, ready_file) {
        suppressPackageStartupMessages(library(wsiTools))
        tile_route <- wsiTools:::wsi_dynamic_tile_route("/tiles")
        for (i in seq_along(sources)) {
          source <- sources[[i]]
          if (inherits(source, "wsi_dynamic_czi_section_tile_source") &&
              is.null(source$czi_handle) &&
              wsiTools:::wsi_native_available("wsi_native_czi_open_handle")) {
            source$czi_handle <- tryCatch(
              wsiTools:::wsi_native_czi_open_handle(source$path),
              error = function(err) NULL
            )
            sources[[i]] <- source
          }
        }
        names(sources) <- vapply(sources, function(source) {
          as.character(source$id)
        }, character(1))
        app <- list(call = function(req) {
          method <- req$REQUEST_METHOD
          if (identical(method, "OPTIONS")) {
            return(list(
              status = 204L,
              headers = list(
                "Access-Control-Allow-Origin" = "*",
                "Access-Control-Allow-Methods" = "GET, OPTIONS",
                "Access-Control-Allow-Headers" = "Content-Type, If-None-Match"
              ),
              body = NULL
            ))
          }
          request <- wsiTools:::wsi_dynamic_tile_parse(req$PATH_INFO, route = tile_route)
          if (!identical(method, "GET") || is.null(request)) {
            return(wsiTools:::wsi_http_json_response(
              status = if (identical(method, "GET")) 404L else 405L,
              body = list(error = "Dynamic tile worker accepts GET tile requests only.")
            ))
          }
          source <- sources[[request$slide_id]]
          if (is.null(source)) {
            return(wsiTools:::wsi_http_json_response(
              status = 404L,
              body = list(error = "Unknown slide tile source.")
            ))
          }
          tryCatch(
            wsiTools:::wsi_dynamic_tile_response(
              source,
              level = request$level,
              col = request$x,
              row = request$y,
              format = request$format,
              settings = wsiTools:::wsi_dynamic_tile_query_settings(req$QUERY_STRING),
              request_etag = req$HTTP_IF_NONE_MATCH
            ),
            error = function(err) wsiTools:::wsi_http_json_response(
              status = if (inherits(err, "wsi_region_out_of_bounds")) 404L else 500L,
              body = list(error = conditionMessage(err))
            )
          )
        })
        server <- httpuv::startServer(host, requested_port, app, quiet = TRUE)
        on.exit(try(httpuv::stopServer(server), silent = TRUE), add = TRUE)
        # Publish only a complete descriptor; the parent polls for this path.
        pending_file <- paste0(ready_file, ".pending")
        saveRDS(list(port = requested_port, pid = Sys.getpid()), pending_file)
        if (!file.rename(pending_file, ready_file)) {
          stop("Could not publish the tile worker readiness descriptor.")
        }
        repeat {
          httpuv::service(100L)
          Sys.sleep(0.002)
        }
      },
      args = list(
        sources = sources,
        host = host,
        requested_port = requested_port,
        ready_file = ready_file
      ),
      libpath = .libPaths(),
      supervise = FALSE,
      stdout = file.path(worker_dir, sprintf("worker_%d.stdout.log", index)),
      stderr = file.path(worker_dir, sprintf("worker_%d.stderr.log", index))
    )
    processes[[index]] <- process
    deadline <- Sys.time() + startup_timeout
    while (!file.exists(ready_file) && Sys.time() < deadline &&
           isTRUE(tryCatch(process$is_alive(), error = function(err) FALSE))) {
      Sys.sleep(0.04)
    }
    if (!file.exists(ready_file)) {
      stop_started()
      return(NULL)
    }
    ready <- readRDS(ready_file)
    urls[[index]] <- sprintf("http://%s:%d", host, as.integer(ready$port))
  }

  structure(
    list(processes = processes, urls = urls, directory = worker_dir),
    class = "wsi_dynamic_tile_worker_pool"
  )
}

wsi_dynamic_tile_worker_stop <- function(pool) {
  if (is.null(pool)) {
    return(invisible(FALSE))
  }
  for (process in pool$processes %||% list()) {
    if (isTRUE(tryCatch(process$is_alive(), error = function(err) FALSE))) {
      try(process$kill(), silent = TRUE)
    }
  }
  if (nzchar(pool$directory %||% "")) {
    unlink(pool$directory, recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}
