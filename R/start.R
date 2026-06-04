#' Start here: check wsiTools and get the next command
#'
#' `wsi_start()` is a short first-run helper for new users. It checks that the
#' package is loaded, summarizes installed runtime backends, optionally tests
#' whether a live `httpuv` viewer service can start, and prints a practical next
#' command. It is read-only: it does not install packages, modify system tools,
#' open a browser, or load any image pixels.
#'
#' @param live_test Whether to try starting a temporary local `httpuv` service.
#' @param sync_test Whether to also run the local browser/R sync self-test.
#'   This requires both optional packages `httpuv` and `callr`. The default is
#'   `FALSE` so the first-run check stays fast.
#' @param host,port,max_tries Host, starting port, and retry count for the
#'   temporary live-viewer check.
#' @param timeout Timeout in seconds for the optional sync self-test.
#'
#' @return Invisibly returns a `wsi_start` list with package, backend,
#'   live-viewer, and suggested-command fields.
#' @export
#'
#' @examples
#' wsi_start(live_test = FALSE)
wsi_start <- function(live_test = TRUE,
                      sync_test = FALSE,
                      host = "127.0.0.1",
                      port = 8794L,
                      max_tries = 20L,
                      timeout = 5) {
  version <- tryCatch(
    as.character(utils::packageVersion("wsiTools")),
    error = function(err) NA_character_
  )
  backends <- wsi_backends()
  live_viewer <- if (isTRUE(live_test)) {
    wsi_diagnose_live_bridge(
      host = host,
      port = port,
      max_tries = max_tries,
      sync_test = sync_test,
      timeout = timeout
    )
  } else {
    data.frame(
      check = c("httpuv_installed", "live_viewer_can_start", "browser_sync_self_test"),
      value = c(requireNamespace("httpuv", quietly = TRUE), NA, NA),
      details = c(
        if (requireNamespace("httpuv", quietly = TRUE)) "httpuv is installed." else "Install optional package `httpuv` for live viewer sessions.",
        "Skipped because `live_test = FALSE`.",
        "Skipped because `live_test = FALSE`."
      ),
      stringsAsFactors = FALSE
    )
  }

  installed <- if (nrow(backends)) {
    backends$backend[backends$installed %in% TRUE]
  } else {
    character()
  }
  core_backends <- c("openslide", "libvips", "native_czi", "bioformats", "bioformats_java", "imagemagick")
  missing_core <- setdiff(core_backends, backends$backend[backends$installed %in% TRUE])
  next_command <- wsi_start_next_command(backends, live_viewer)

  out <- list(
    package_version = version,
    backends = backends,
    installed_backends = installed,
    missing_core_backends = missing_core,
    live_viewer = live_viewer,
    suggested_next_command = next_command
  )
  class(out) <- "wsi_start"
  print(out)
  invisible(out)
}

wsi_start_next_command <- function(backends, live_viewer) {
  installed <- backends$backend[backends$installed %in% TRUE]
  has_image_backend <- any(installed %in% c("openslide", "libvips", "native_czi", "bioformats", "bioformats_java", "imagemagick"))
  live_can_start <- any(live_viewer$check == "live_viewer_can_start" & live_viewer$value %in% TRUE)

  if (!has_image_backend) {
    return(c(
      "wsi_demo_viewer(open = TRUE)",
      "wsi_install_backends(install = FALSE)"
    ))
  }

  if (isTRUE(live_can_start)) {
    return(c(
      "viewer <- wsi_open_viewer(\"/path/to/slide.svs\")"
    ))
  }

  c(
    "html <- wsi_open_viewer(\"/path/to/slide.svs\", live = \"no\")"
  )
}

#' @export
print.wsi_start <- function(x, ...) {
  cat("<wsi_start>\n")

  cat("\n1. Checking package\n")
  version <- x$package_version %||% NA_character_
  if (!is.na(version) && nzchar(version)) {
    cat(sprintf("   OK: wsiTools %s is installed and can be loaded.\n", version))
  } else {
    cat("   OK: wsiTools is loaded, but packageVersion(\"wsiTools\") was not available.\n")
  }

  cat("\n2. Checking backends\n")
  installed <- x$installed_backends
  if (length(installed)) {
    cat(sprintf("   Installed: %s\n", paste(installed, collapse = ", ")))
  } else {
    cat("   Installed: none detected yet.\n")
  }
  important <- intersect(c("openslide", "libvips", "native_czi", "bioformats", "bioformats_java", "imagemagick"), x$missing_core_backends)
  if (length(important)) {
    cat(sprintf("   Missing or unavailable: %s\n", paste(important, collapse = ", ")))
  }
  cat("   Details: wsi_backends()\n")

  cat("\n3. Checking live viewer\n")
  live <- x$live_viewer
  if (nrow(live)) {
    for (i in seq_len(nrow(live))) {
      value <- live$value[[i]]
      status <- if (isTRUE(value)) {
        "OK"
      } else if (isFALSE(value)) {
        "Needs attention"
      } else {
        "Skipped"
      }
      cat(sprintf("   %s: %s - %s\n", status, live$check[[i]], live$details[[i]]))
    }
  } else {
    cat("   Skipped: no live-viewer checks were returned.\n")
  }

  cat("\n4. Suggested next command\n")
  for (line in x$suggested_next_command) {
    cat(sprintf("   %s\n", line))
  }
  cat("\nFor a full support report, run: wsi_diagnose(live_test = FALSE)\n")

  invisible(x)
}
