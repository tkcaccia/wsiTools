wsi_new_viewer_state <- function(name = "wsi_viewer_live_state", envir = parent.frame(),
                                 max_events = 1000L) {
  state <- new.env(parent = emptyenv())
  state$rois <- wsi_empty_roi()
  state$measurements <- wsi_empty_measurements()
  state$segmentation <- wsi_empty_roi()
  state$selected_roi <- NULL
  state$view <- list()
  state$stain <- NULL
  state$events <- list()
  state$last_event <- NULL
  state$last_payload <- NULL
  state$last_sync <- NULL
  state$export_name <- name
  state$export_envir <- envir
  state$max_events <- as.integer(max_events)
  class(state) <- c("wsi_viewer_state", "environment")
  wsi_assign_viewer_state(state)
  state
}

wsi_empty_measurements <- function() {
  data.frame(
    id = character(),
    start_x = numeric(),
    start_y = numeric(),
    end_x = numeric(),
    end_y = numeric(),
    distance_px = numeric(),
    distance_um = numeric(),
    stringsAsFactors = FALSE
  )
}

wsi_measurements_from_payload <- function(measures) {
  if (is.null(measures) || !length(measures)) {
    return(wsi_empty_measurements())
  }
  rows <- lapply(seq_along(measures), function(i) {
    m <- measures[[i]]
    start <- m$start %||% list()
    end <- m$end %||% list()
    data.frame(
      id = as.character(m$id %||% sprintf("measure_%d", i)),
      start_x = as.numeric(start$x %||% NA_real_),
      start_y = as.numeric(start$y %||% NA_real_),
      end_x = as.numeric(end$x %||% NA_real_),
      end_y = as.numeric(end$y %||% NA_real_),
      distance_px = as.numeric(m$distance_px %||% NA_real_),
      distance_um = as.numeric(m$distance_um %||% NA_real_),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

wsi_rois_from_payload <- function(geojson) {
  if (is.null(geojson)) {
    return(wsi_empty_roi())
  }
  wsi_roi_from_geojson(geojson)
}

wsi_selected_roi_from_payload <- function(feature) {
  if (is.null(feature)) {
    return(NULL)
  }
  wsi_roi_from_geojson(list(type = "FeatureCollection", features = list(feature)))
}

wsi_assign_viewer_state <- function(state) {
  envir <- state$export_envir
  name <- state$export_name
  if (!is.environment(envir) || is.null(name) || !nzchar(name)) {
    return(invisible(state))
  }
  assign(name, state, envir = envir)
  assign(paste0(name, "_rois"), state$rois, envir = envir)
  assign(paste0(name, "_measurements"), state$measurements, envir = envir)
  assign(paste0(name, "_segmentation"), state$segmentation, envir = envir)
  assign(paste0(name, "_selected_roi"), state$selected_roi, envir = envir)
  assign(paste0(name, "_last_event"), state$last_payload, envir = envir)
  invisible(state)
}

wsi_viewer_state_apply <- function(state, payload) {
  if (!inherits(state, "wsi_viewer_state")) {
    wsi_abort("`state` must be a `wsi_viewer_state` object.")
  }
  if (!is.list(payload)) {
    wsi_abort("Viewer state payload must be a JSON object.")
  }

  state$rois <- wsi_rois_from_payload(payload$rois)
  state$measurements <- wsi_measurements_from_payload(payload$measurements)
  state$segmentation <- wsi_rois_from_payload(payload$segmentation)
  state$selected_roi <- wsi_selected_roi_from_payload(payload$selected_roi)
  state$view <- payload$view %||% list()
  state$stain <- payload$stain %||% NULL
  state$last_event <- as.character(payload$event %||% "viewer_state")
  state$last_payload <- payload
  state$last_sync <- Sys.time()

  event <- list(
    event = state$last_event,
    time = as.character(payload$time %||% format(state$last_sync, "%Y-%m-%dT%H:%M:%OS%z")),
    roi_count = nrow(state$rois),
    measurement_count = nrow(state$measurements),
    segmentation_count = nrow(state$segmentation)
  )
  state$events[[length(state$events) + 1L]] <- event
  max_events <- state$max_events %||% 1000L
  if (length(state$events) > max_events) {
    state$events <- utils::tail(state$events, max_events)
  }

  wsi_assign_viewer_state(state)
  invisible(state)
}

wsi_viewer_state_response <- function(state) {
  list(
    ok = TRUE,
    event = state$last_event,
    roi_count = nrow(state$rois),
    measurement_count = nrow(state$measurements),
    segmentation_count = nrow(state$segmentation),
    last_sync = as.character(state$last_sync)
  )
}

wsi_start_viewer_state_server <- function(state, host = "127.0.0.1", port = 8788,
                                          path = "/viewer-state", max_tries = 20L) {
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    wsi_abort(
      "Live viewer state sync requires the optional package `httpuv`.",
      class = "wsi_missing_dependency"
    )
  }
  port <- as.integer(wsi_check_scalar_number(port, "port", allow_zero = FALSE))
  max_tries <- as.integer(wsi_check_scalar_number(max_tries, "max_tries", allow_zero = TRUE))
  if (!startsWith(path, "/")) {
    path <- paste0("/", path)
  }

  app <- list(
    call = function(req) {
      method <- req$REQUEST_METHOD %||% "GET"
      request_path <- req$PATH_INFO %||% "/"
      if (identical(method, "OPTIONS")) {
        return(wsi_http_json_response(status = 204L, body = ""))
      }
      if (!identical(request_path, path)) {
        return(wsi_http_json_response(status = 404L, body = list(error = "Not found.")))
      }
      if (identical(method, "GET")) {
        return(wsi_http_json_response(body = wsi_viewer_state_response(state)))
      }
      if (!identical(method, "POST")) {
        return(wsi_http_json_response(status = 405L, body = list(error = "Use POST with viewer state JSON.")))
      }

      tryCatch({
        body <- wsi_http_request_body(req)
        if (!nzchar(body)) {
          wsi_abort("Viewer state request body was empty.")
        }
        payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)
        wsi_viewer_state_apply(state, payload)
        wsi_http_json_response(body = wsi_viewer_state_response(state))
      }, error = function(err) {
        wsi_http_json_response(status = 500L, body = list(error = conditionMessage(err)))
      })
    }
  )

  last_error <- NULL
  for (candidate in seq.int(port, port + max_tries)) {
    server <- try(httpuv::startServer(host, candidate, app), silent = TRUE)
    if (!inherits(server, "try-error")) {
      url <- sprintf("http://%s:%d%s", host, candidate, path)
      return(list(server = server, host = host, port = candidate, path = path, url = url))
    }
    last_error <- conditionMessage(attr(server, "condition"))
  }
  wsi_abort(sprintf("Could not start live viewer state server near port %d: %s", port, last_error %||% "unknown error"))
}

#' Start a live viewer session that syncs back to R
#'
#' Opens an interactive viewer with a local HTTP bridge. Browser-side
#' annotations, imported GeoJSON, edited ROI labels/classes, distance
#' measurements, and segmentation overlays are posted back to the current R
#' session. The state object is an environment, so it is updated in place as new
#' events arrive.
#'
#' This live bridge is optional and requires the suggested `httpuv` package.
#' The ordinary [wsi_viewer()] remains a static HTML viewer for file-only use.
#'
#' @param slide A `wsi_slide` object.
#' @param ... Additional arguments passed to [wsi_viewer()].
#' @param name Name assigned in `envir` for the live state object. Companion
#'   objects named `<name>_rois`, `<name>_measurements`,
#'   `<name>_segmentation`, `<name>_selected_roi`, and `<name>_last_event` are
#'   refreshed after every browser sync.
#' @param envir Environment where live state objects are assigned.
#' @param host,port,path Local HTTP address used for browser-to-R sync.
#' @param max_tries Number of subsequent ports to try if `port` is busy.
#' @param wait If `TRUE`, service the HTTP bridge until interrupted. This is
#'   the most reliable mode for plain R sessions. Press Esc or Ctrl+C to return
#'   to the console; synced objects remain in `envir`.
#' @param open Whether to open the viewer with [utils::browseURL()].
#'
#' @return A `wsi_viewer_session` object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' slide <- wsi_open("sample.svs")
#' session <- wsi_viewer_live(slide, mode = "tiles")
#'
#' # After drawing in the browser and stopping the live loop:
#' wsi_viewer_live_state_rois
#' wsi_viewer_live_state_measurements
#' wsi_viewer_state(session)$rois
#' }
wsi_viewer_session <- function(slide, ..., name = "wsi_viewer_live_state",
                               envir = parent.frame(), host = "127.0.0.1",
                               port = 8788, path = "/viewer-state",
                               max_tries = 20L, wait = interactive(),
                               open = interactive()) {
  state <- wsi_new_viewer_state(name = name, envir = envir)
  bridge <- wsi_start_viewer_state_server(
    state = state,
    host = host,
    port = port,
    path = path,
    max_tries = max_tries
  )

  dots <- list(...)
  dots$viewer_state_url <- bridge$url
  dots$open <- open
  dots$slide <- slide
  html <- do.call(wsi_viewer, dots)

  session <- structure(
    c(
      bridge,
      list(
        state = state,
        html = html,
        name = name,
        envir = envir
      )
    ),
    class = "wsi_viewer_session"
  )

  message("wsiTools live viewer sync listening at ", bridge$url)
  message("Browser edits update `", name, "` and companion objects in the chosen R environment.")

  if (isTRUE(wait)) {
    message("Press Ctrl+C or Esc to stop the live sync loop and return to R.")
    on.exit(httpuv::stopServer(bridge$server), add = TRUE)
    tryCatch(
      repeat httpuv::service(100),
      interrupt = function(e) NULL
    )
  }

  invisible(session)
}

#' @rdname wsi_viewer_session
#' @export
wsi_viewer_live <- wsi_viewer_session

#' Read live viewer state
#'
#' @param x A `wsi_viewer_session` or `wsi_viewer_state` object.
#'
#' @return A list containing ROIs, measurements, segmentation overlays, selected
#'   ROI, view/stain settings, and event history.
#' @export
wsi_viewer_state <- function(x) {
  state <- if (inherits(x, "wsi_viewer_session")) {
    x$state
  } else {
    x
  }
  if (!inherits(state, "wsi_viewer_state")) {
    wsi_abort("`x` must be a `wsi_viewer_session` or `wsi_viewer_state` object.")
  }
  list(
    rois = state$rois,
    measurements = state$measurements,
    segmentation = state$segmentation,
    selected_roi = state$selected_roi,
    view = state$view,
    stain = state$stain,
    events = state$events,
    last_event = state$last_event,
    last_payload = state$last_payload,
    last_sync = state$last_sync
  )
}

#' @rdname wsi_viewer_state
#' @export
viewer_state <- wsi_viewer_state

#' Service or stop a live viewer session
#'
#' @param session A `wsi_viewer_session` object.
#' @param timeout Milliseconds to service pending HTTP events.
#'
#' @return `session`, invisibly.
#' @export
wsi_viewer_service <- function(session, timeout = 100L) {
  if (!inherits(session, "wsi_viewer_session")) {
    wsi_abort("`session` must be a `wsi_viewer_session` object.")
  }
  timeout <- as.integer(wsi_check_scalar_number(timeout, "timeout", allow_zero = TRUE))
  httpuv::service(timeout)
  invisible(session)
}

#' @rdname wsi_viewer_service
#' @export
wsi_viewer_stop <- function(session) {
  if (!inherits(session, "wsi_viewer_session")) {
    wsi_abort("`session` must be a `wsi_viewer_session` object.")
  }
  httpuv::stopServer(session$server)
  invisible(session)
}

#' @export
print.wsi_viewer_state <- function(x, ...) {
  cat("<wsi_viewer_state>\n")
  cat(sprintf("  ROIs: %d\n", nrow(x$rois)))
  cat(sprintf("  measurements: %d\n", nrow(x$measurements)))
  cat(sprintf("  segmentation overlays: %d\n", nrow(x$segmentation)))
  cat(sprintf("  last event: %s\n", x$last_event %||% "none"))
  invisible(x)
}

#' @export
print.wsi_viewer_session <- function(x, ...) {
  cat("<wsi_viewer_session>\n")
  cat(sprintf("  url: %s\n", x$url))
  cat(sprintf("  html: %s\n", x$html))
  cat(sprintf("  state: %s\n", x$name))
  cat("  stop with: wsi_viewer_stop(x)\n")
  invisible(x)
}
