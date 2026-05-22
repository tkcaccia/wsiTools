#' Check whether background jobs are available
#'
#' Background jobs are optional and use the suggested `callr` package. This
#' keeps the wsiTools core installable without process-management dependencies,
#' while allowing long-running work to run in a separate R process when
#' `callr` is installed.
#'
#' @return `TRUE` when `callr` is installed, otherwise `FALSE`.
#' @export
wsi_has_callr <- function() {
  requireNamespace("callr", quietly = TRUE)
}

wsi_job_callable <- function(fun) {
  if (is.function(fun)) {
    return(fun)
  }
  if (!is.character(fun) || length(fun) != 1L || is.na(fun) || !nzchar(fun)) {
    stop("`fun` must be a function or a single function name.", call. = FALSE)
  }
  if (grepl("::", fun, fixed = TRUE)) {
    parts <- strsplit(fun, "::", fixed = TRUE)[[1L]]
    if (length(parts) != 2L || !nzchar(parts[[1L]]) || !nzchar(parts[[2L]])) {
      stop("Namespaced function names must have the form `package::function`.", call. = FALSE)
    }
    return(getExportedValue(parts[[1L]], parts[[2L]]))
  }
  get(fun, mode = "function", envir = .GlobalEnv)
}

wsi_job_call <- function(fun, args, packages) {
  tryCatch(
    {
      if (is.null(packages) || !length(packages)) {
        packages <- character()
      }
      packages <- as.character(packages)
      for (package in packages) {
        if (!requireNamespace(package, quietly = TRUE)) {
          stop(sprintf("Required package `%s` is not available in the background R process.", package), call. = FALSE)
        }
      }
      if (!is.function(fun)) {
        if (!is.character(fun) || length(fun) != 1L || is.na(fun) || !nzchar(fun)) {
          stop("`fun` must be a function or a single function name.", call. = FALSE)
        }
        if (grepl("::", fun, fixed = TRUE)) {
          parts <- strsplit(fun, "::", fixed = TRUE)[[1L]]
          if (length(parts) != 2L || !nzchar(parts[[1L]]) || !nzchar(parts[[2L]])) {
            stop("Namespaced function names must have the form `package::function`.", call. = FALSE)
          }
          fun <- getExportedValue(parts[[1L]], parts[[2L]])
        } else {
          fun <- get(fun, mode = "function", envir = .GlobalEnv)
        }
      }
      if (is.null(args)) {
        args <- list()
      }
      list(ok = TRUE, value = do.call(fun, args))
    },
    error = function(err) {
      list(
        ok = FALSE,
        message = conditionMessage(err),
        class = class(err)
      )
    }
  )
}

wsi_job_error_condition <- function(message, class = character()) {
  structure(
    list(message = message, call = NULL),
    class = unique(c("wsi_job_error", class, "wsi_error", "error", "condition"))
  )
}

wsi_job_child_error <- function(payload) {
  message <- payload$message %||% "Background job failed."
  class <- payload$class %||% character()
  wsi_job_error_condition(message, class = c("wsi_job_child_error", class))
}

wsi_job_callback_args <- function(callback, value, job) {
  formals <- tryCatch(formals(callback), error = function(err) NULL)
  if (is.null(formals)) {
    return(list(value))
  }
  if (!length(formals)) {
    return(list())
  }
  if ("..." %in% names(formals)) {
    return(list(value, job = job))
  }
  args <- list(value, job)
  unname(args[seq_len(min(length(formals), length(args)))])
}

wsi_job_run_callbacks <- function(callbacks, value, job, kind = "result") {
  if (!length(callbacks)) {
    return(invisible(NULL))
  }
  for (callback in callbacks) {
    tryCatch(
      do.call(callback, wsi_job_callback_args(callback, value, job)),
      error = function(err) {
        wsi_warn(sprintf(
          "Background job %s callback failed: %s",
          kind,
          conditionMessage(err)
        ))
      }
    )
  }
  invisible(NULL)
}

wsi_job_output <- function(process, stream = c("output", "error")) {
  stream <- match.arg(stream)
  method <- if (identical(stream, "output")) "read_all_output_lines" else "read_all_error_lines"
  tryCatch(process[[method]](), error = function(err) character())
}

wsi_job_refresh_output <- function(private) {
  private$stdout <- c(private$stdout, wsi_job_output(private$process, "output"))
  private$stderr <- c(private$stderr, wsi_job_output(private$process, "error"))
  invisible(private)
}

wsi_job_display_status <- function(status) {
  status <- as.character(status %||% "unknown")[[1L]]
  switch(
    status,
    finished = "completed",
    complete = "completed",
    success = "completed",
    status
  )
}

wsi_job_parse_progress <- function(lines) {
  lines <- as.character(lines %||% character())
  lines <- lines[nzchar(lines)]
  if (!length(lines)) {
    return(NA_real_)
  }
  patterns <- c(
    "(?i)WSITOOLS_PROGRESS\\s*[:= ]\\s*([0-9]{1,3}(?:\\.[0-9]+)?)",
    "(?i)(?:progress|percent|complete|completed)\\D{0,24}([0-9]{1,3}(?:\\.[0-9]+)?)\\s*%"
  )
  values <- numeric()
  for (pattern in patterns) {
    hits <- regexec(pattern, lines, perl = TRUE)
    matches <- regmatches(lines, hits)
    found <- suppressWarnings(as.numeric(vapply(matches, function(x) {
      if (length(x) >= 2L) x[[2L]] else NA_character_
    }, character(1))))
    values <- c(values, found[is.finite(found)])
  }
  if (!length(values)) {
    return(NA_real_)
  }
  max(0, min(100, utils::tail(values, 1L)))
}

wsi_job_log <- function(private, n = 40L) {
  n <- as.integer(wsi_check_scalar_number(n, "n", allow_zero = TRUE))
  wsi_job_refresh_output(private)
  lines <- c(
    if (length(private$stdout)) paste0("[stdout] ", private$stdout) else character(),
    if (length(private$stderr)) paste0("[stderr] ", private$stderr) else character()
  )
  utils::tail(lines, n)
}

wsi_job_progress <- function(private, job) {
  status <- wsi_job_display_status(job$status())
  log <- wsi_job_log(private, n = 80L)
  percent <- wsi_job_parse_progress(log)
  if (identical(status, "completed")) {
    percent <- 100
  } else if (identical(status, "queued")) {
    percent <- 0
  }
  message <- utils::tail(log[nzchar(log)], 1L) %||% ""
  list(
    status = status,
    percent = if (is.finite(percent)) percent else NA_real_,
    message = message,
    available = is.finite(percent)
  )
}

wsi_job_collect <- function(private, job, wait = FALSE, timeout_ms = NULL) {
  if (isTRUE(private$collected) || isTRUE(private$cancelled)) {
    return(isTRUE(private$collected))
  }

  process <- private$process
  if (isTRUE(process$is_alive())) {
    if (!isTRUE(wait)) {
      return(FALSE)
    }
    if (is.null(timeout_ms)) {
      process$wait()
    } else {
      timeout_ms <- as.integer(wsi_check_scalar_number(timeout_ms, "timeout_ms", allow_zero = TRUE))
      process$wait(timeout_ms)
    }
    if (isTRUE(process$is_alive())) {
      return(FALSE)
    }
  }

  payload <- tryCatch(process$get_result(), error = function(err) err)
  wsi_job_refresh_output(private)
  private$finished <- Sys.time()
  private$collected <- TRUE

  if (inherits(payload, "error")) {
    private$error <- wsi_job_error_condition(
      sprintf("Background job `%s` failed before returning a result: %s", private$name, conditionMessage(payload))
    )
  } else if (!is.list(payload) || !isTRUE(payload$ok)) {
    private$error <- wsi_job_child_error(payload %||% list())
  } else {
    private$value <- payload$value
  }

  if (!is.null(private$error)) {
    if (!isTRUE(private$error_callbacks_ran)) {
      private$error_callbacks_ran <- TRUE
      wsi_job_run_callbacks(private$error_callbacks, private$error, job, kind = "error")
    }
  } else if (!isTRUE(private$result_callbacks_ran)) {
    private$result_callbacks_ran <- TRUE
    wsi_job_run_callbacks(private$result_callbacks, private$value, job, kind = "result")
  }

  TRUE
}

wsi_job_status <- function(private, job) {
  if (isTRUE(private$cancelled)) {
    return("cancelled")
  }
  wsi_job_collect(private, job, wait = FALSE)
  if (isTRUE(private$collected)) {
    if (!is.null(private$error)) {
      return("failed")
    }
    return("finished")
  }
  if (isTRUE(private$process$is_alive())) {
    return("running")
  }
  "finished"
}

#' Run an R task in a background process
#'
#' Starts a non-blocking R process for long-running tasks such as selected-ROI
#' segmentation, tile extraction, OME-TIFF conversion, and pyramid generation.
#' The returned object is an environment with methods including `status()`,
#' `result()`, `wait()`, and `cancel()`.
#'
#' @param fun A function, or a single function name such as
#'   `"wsiTools::wsi_convert"`.
#' @param args Named list of arguments passed to `fun`.
#' @param name Human-readable job name.
#' @param packages Optional packages to require in the background R process.
#' @param stdout,stderr Output capture passed to `callr::r_bg()`.
#' @param supervise Whether `callr` should supervise the child process.
#'
#' @return A `wsi_job` object.
#' @export
wsi_job <- function(fun, args = list(), name = NULL, packages = character(),
                    stdout = "|", stderr = "|", supervise = FALSE) {
  if (!wsi_has_callr()) {
    wsi_abort(
      "Background jobs require the optional package `callr`. Install it with install.packages(\"callr\") and retry.",
      class = "wsi_missing_dependency"
    )
  }
  if (!is.list(args)) {
    wsi_abort("`args` must be a list.")
  }
  name <- as.character(name %||% "wsiTools background job")
  if (length(name) != 1L || is.na(name) || !nzchar(name)) {
    wsi_abort("`name` must be a single non-empty character value.")
  }
  packages <- as.character(packages %||% character())

  process <- callr::r_bg(
    func = wsi_job_call,
    args = list(fun = fun, args = args, packages = packages),
    stdout = stdout,
    stderr = stderr,
    supervise = supervise,
    package = FALSE
  )

  private <- new.env(parent = emptyenv())
  private$process <- process
  private$id <- sprintf("job_%s_%d", format(Sys.time(), "%Y%m%d%H%M%OS3"), process$get_pid())
  private$name <- name
  private$started <- Sys.time()
  private$finished <- NULL
  private$value <- NULL
  private$error <- NULL
  private$stdout <- character()
  private$stderr <- character()
  private$collected <- FALSE
  private$cancelled <- FALSE
  private$result_callbacks <- list()
  private$error_callbacks <- list()
  private$result_callbacks_ran <- FALSE
  private$error_callbacks_ran <- FALSE

  job <- new.env(parent = emptyenv())
  job$id <- private$id
  job$name <- private$name
  job$started <- private$started
  job$pid <- function() private$process$get_pid()
  job$is_alive <- function() private$process$is_alive()
  job$status <- function() wsi_job_status(private, job)
  job$stdout <- function() {
    wsi_job_refresh_output(private)
    private$stdout
  }
  job$stderr <- function() {
    wsi_job_refresh_output(private)
    private$stderr
  }
  job$log <- function(n = 40L) wsi_job_log(private, n = n)
  job$progress <- function() wsi_job_progress(private, job)
  job$error <- function() {
    job$status()
    private$error
  }
  job$wait <- function(timeout_ms = NULL) {
    wsi_job_collect(private, job, wait = TRUE, timeout_ms = timeout_ms)
    invisible(job$status())
  }
  job$result <- function(wait = TRUE, timeout_ms = NULL) {
    wsi_job_collect(private, job, wait = wait, timeout_ms = timeout_ms)
    if (!isTRUE(private$collected)) {
      return(NULL)
    }
    if (!is.null(private$error)) {
      stop(private$error)
    }
    private$value
  }
  job$cancel <- function() {
    if (!isTRUE(private$collected) && isTRUE(private$process$is_alive())) {
      private$process$kill()
    }
    private$cancelled <- TRUE
    private$finished <- Sys.time()
    invisible(TRUE)
  }
  job$then <- function(callback) {
    if (!is.function(callback)) {
      wsi_abort("`callback` must be a function.")
    }
    if (isTRUE(private$collected) && is.null(private$error)) {
      wsi_job_run_callbacks(list(callback), private$value, job, kind = "result")
    } else {
      private$result_callbacks[[length(private$result_callbacks) + 1L]] <- callback
    }
    invisible(job)
  }
  job$catch <- function(callback) {
    if (!is.function(callback)) {
      wsi_abort("`callback` must be a function.")
    }
    if (isTRUE(private$collected) && !is.null(private$error)) {
      wsi_job_run_callbacks(list(callback), private$error, job, kind = "error")
    } else {
      private$error_callbacks[[length(private$error_callbacks) + 1L]] <- callback
    }
    invisible(job)
  }
  job$metadata <- function() {
    status <- job$status()
    progress <- job$progress()
    list(
      id = private$id,
      name = private$name,
      pid = private$process$get_pid(),
      status = status,
      display_status = wsi_job_display_status(status),
      progress = progress$percent,
      progress_available = isTRUE(progress$available),
      message = if (!is.null(private$error)) conditionMessage(private$error) else progress$message,
      started = private$started,
      finished = private$finished,
      log = job$log(n = 40L)
    )
  }
  class(job) <- c("wsi_job", "environment")
  job
}

#' @rdname wsi_job
#' @export
wsi_run_async <- function(fun, args = list(), name = NULL, packages = character(),
                          stdout = "|", stderr = "|", supervise = FALSE) {
  wsi_job(
    fun = fun,
    args = args,
    name = name,
    packages = packages,
    stdout = stdout,
    stderr = stderr,
    supervise = supervise
  )
}

#' @export
print.wsi_job <- function(x, ...) {
  cat("<wsi_job>\n")
  cat(sprintf("  id:     %s\n", x$id))
  cat(sprintf("  name:   %s\n", x$name))
  cat(sprintf("  pid:    %s\n", x$pid()))
  cat(sprintf("  status: %s\n", x$status()))
  invisible(x)
}

wsi_async_job_args <- function(...) {
  list(...)
}

#' Run StarDist ROI segmentation asynchronously
#'
#' @inheritParams stardist_segment_roi
#' @param ... Additional arguments forwarded to [stardist_segment_roi()].
#'
#' @return A `wsi_job` object. Call `job$status()` to poll and `job$result()` to
#'   collect the `wsi_stardist_result`.
#' @export
wsi_stardist_segment_roi_async <- function(image, roi, output_dir, ...) {
  wsi_run_async(
    "wsiTools::stardist_segment_roi",
    args = c(list(image = image, roi = roi, output_dir = output_dir), wsi_async_job_args(...)),
    name = "StarDist ROI segmentation",
    packages = "wsiTools"
  )
}

#' Run tile extraction asynchronously
#'
#' @inheritParams extract_tiles
#' @param ... Additional arguments forwarded to [extract_tiles()].
#'
#' @return A `wsi_job` object. Call `job$result()` to collect the tile manifest.
#' @export
wsi_extract_tiles_async <- function(image, ...) {
  wsi_run_async(
    "wsiTools::extract_tiles",
    args = c(list(image = image), wsi_async_job_args(...)),
    name = "WSI tile extraction",
    packages = "wsiTools"
  )
}

#' Run image conversion asynchronously
#'
#' @inheritParams wsi_convert
#' @param ... Additional arguments forwarded to [wsi_convert()].
#'
#' @return A `wsi_job` object. Call `job$result()` to collect the output path.
#' @export
wsi_convert_async <- function(input, output, ...) {
  wsi_run_async(
    "wsiTools::wsi_convert",
    args = c(list(input = input, output = output), wsi_async_job_args(...)),
    name = "WSI conversion",
    packages = "wsiTools"
  )
}

#' Run pyramid generation asynchronously
#'
#' @inheritParams wsi_pyramid
#' @param ... Additional arguments forwarded to [wsi_pyramid()].
#'
#' @return A `wsi_job` object. Call `job$result()` to collect the output path.
#' @export
wsi_pyramid_async <- function(input, output, ...) {
  wsi_run_async(
    "wsiTools::wsi_pyramid",
    args = c(list(input = input, output = output), wsi_async_job_args(...)),
    name = "WSI pyramid generation",
    packages = "wsiTools"
  )
}
