wsi_cli_usage <- function() {
  paste(
    "wsiTools command line",
    "",
    "Usage:",
    "  wsitools help",
    "  wsitools backends",
    "  wsitools translate-rois --input crop.geojson --output slide.geojson --dx 100 --dy 200 [--overwrite]",
    "",
    "Examples:",
    "  wsitools backends",
    "  wsitools translate-rois --input crop.geojson --output slide.geojson --dx 100 --dy 200",
    "",
    "Cell segmentation is expected to be run outside wsiTools, for example with CellPhenotyper.",
    sep = "\n"
  )
}

wsi_cli_parse <- function(args) {
  opts <- list(.positionals = character(), arg = character())
  flags <- c("help", "overwrite", "plan", "no-run", "no-translate")
  i <- 1L
  while (i <= length(args)) {
    token <- args[[i]]
    if (identical(token, "--")) {
      if (i < length(args)) {
        opts$.positionals <- c(opts$.positionals, args[(i + 1L):length(args)])
      }
      break
    }
    if (!startsWith(token, "--")) {
      opts$.positionals <- c(opts$.positionals, token)
      i <- i + 1L
      next
    }

    item <- substring(token, 3L)
    if (grepl("=", item, fixed = TRUE)) {
      key <- sub("=.*$", "", item)
      value <- sub("^[^=]*=", "", item)
    } else {
      key <- item
      if (key %in% flags) {
        value <- TRUE
      } else {
        if (i == length(args)) {
          wsi_abort(sprintf("Command-line option `--%s` requires a value.", key))
        }
        if (!key %in% c("arg", "args") && startsWith(args[[i + 1L]], "--")) {
          wsi_abort(sprintf("Command-line option `--%s` requires a value.", key))
        }
        value <- args[[i + 1L]]
        i <- i + 1L
      }
    }

    key <- gsub("-", "_", key, fixed = TRUE)
    if (identical(key, "arg")) {
      opts$arg <- c(opts$arg, value)
    } else if (identical(key, "args")) {
      opts$arg <- c(opts$arg, strsplit(as.character(value), "[[:space:]]+")[[1L]])
      opts$arg <- opts$arg[nzchar(opts$arg)]
    } else {
      opts[[key]] <- value
    }
    i <- i + 1L
  }
  opts
}

wsi_cli_required <- function(opts, name) {
  value <- opts[[name]]
  if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
    wsi_abort(sprintf("Missing required command-line option `--%s`.", gsub("_", "-", name, fixed = TRUE)))
  }
  as.character(value)
}

wsi_cli_optional <- function(opts, name, default = NULL) {
  value <- opts[[name]]
  if (is.null(value)) {
    return(default)
  }
  as.character(value)
}

wsi_cli_flag <- function(opts, name) {
  isTRUE(opts[[name]])
}

wsi_cli_number <- function(opts, name, default = NULL) {
  value <- opts[[name]]
  if (is.null(value)) {
    return(default)
  }
  out <- suppressWarnings(as.numeric(value))
  if (length(out) != 1L || is.na(out) || !is.finite(out)) {
    wsi_abort(sprintf("Command-line option `--%s` must be numeric.", gsub("_", "-", name, fixed = TRUE)))
  }
  out
}

wsi_cli_integer <- function(opts, name, default = NULL) {
  value <- wsi_cli_number(opts, name, default = default)
  if (is.null(value)) {
    return(NULL)
  }
  as.integer(value)
}

wsi_cli_stardist_args <- function(opts) {
  if (length(opts$arg)) {
    return(as.character(opts$arg))
  }
  NULL
}

wsi_cli_print_result <- function(result) {
  print(result)
  if (!is.null(result$args) && length(result$args)) {
    cat("  command line:\n")
    cat("    ", paste(c(result$command, wsi_system2_args(result$args)), collapse = " "), "\n", sep = "")
  }
  invisible(result)
}

wsi_cli_backends <- function(opts) {
  print(wsi_backends())
  0L
}

wsi_cli_stardist_image <- function(opts) {
  result <- stardist_segment_image(
    input = wsi_cli_required(opts, "input"),
    output = wsi_cli_required(opts, "output"),
    model = wsi_cli_optional(opts, "model", "2D_versatile_he"),
    command = wsi_cli_optional(opts, "command", NULL),
    args = wsi_cli_stardist_args(opts),
    output_type = wsi_cli_optional(opts, "output_type", "auto"),
    prob_thresh = wsi_cli_number(opts, "prob_thresh"),
    nms_thresh = wsi_cli_number(opts, "nms_thresh"),
    overwrite = wsi_cli_flag(opts, "overwrite"),
    run = !(wsi_cli_flag(opts, "plan") || wsi_cli_flag(opts, "no_run"))
  )
  wsi_cli_print_result(result)
  0L
}

wsi_cli_stardist_roi <- function(opts) {
  roi <- read_geojson(wsi_cli_required(opts, "roi"))
  result <- stardist_segment_roi(
    image = wsi_cli_required(opts, "image"),
    roi = roi,
    output_dir = wsi_cli_required(opts, "output_dir"),
    roi_id = wsi_cli_optional(opts, "roi_id", NULL),
    level = wsi_cli_integer(opts, "level", 0L),
    crop_format = wsi_cli_optional(opts, "crop_format", "png"),
    model = wsi_cli_optional(opts, "model", "2D_versatile_he"),
    command = wsi_cli_optional(opts, "command", NULL),
    args = wsi_cli_stardist_args(opts),
    output = wsi_cli_optional(opts, "output", NULL),
    output_type = wsi_cli_optional(opts, "output_type", "auto"),
    prob_thresh = wsi_cli_number(opts, "prob_thresh"),
    nms_thresh = wsi_cli_number(opts, "nms_thresh"),
    translate_geojson = !wsi_cli_flag(opts, "no_translate"),
    overwrite = wsi_cli_flag(opts, "overwrite"),
    run = !(wsi_cli_flag(opts, "plan") || wsi_cli_flag(opts, "no_run")),
    backend = wsi_cli_optional(opts, "backend", "auto")
  )
  wsi_cli_print_result(result)
  0L
}

wsi_cli_translate_rois <- function(opts) {
  rois <- read_geojson(wsi_cli_required(opts, "input"))
  shifted <- translate_rois(
    rois,
    dx = wsi_cli_number(opts, "dx", 0),
    dy = wsi_cli_number(opts, "dy", 0)
  )
  output <- wsi_cli_required(opts, "output")
  write_geojson(shifted, output, overwrite = wsi_cli_flag(opts, "overwrite"))
  cat(sprintf("Wrote translated GeoJSON: %s\n", output))
  0L
}

#' Run the wsiTools command-line interface
#'
#' Provides a small dependency-free command-line interface around backend
#' checks and GeoJSON coordinate translation. It is used by the installed
#' `exec/wsitools` script and can also
#' be called directly with `Rscript -e 'wsiTools::wsi_cli()' ...`.
#'
#' @param args Character vector of command-line arguments. Defaults to
#'   [commandArgs()] trailing arguments.
#' @param quit Whether to terminate the R process with the command status.
#'
#' @return Integer process status, invisibly.
#' @export
#'
#' @examples
#' wsi_cli(c("help"))
wsi_cli <- function(args = commandArgs(trailingOnly = TRUE), quit = FALSE) {
  status <- tryCatch({
    if (!length(args) || args[[1L]] %in% c("help", "--help", "-h")) {
      cat(wsi_cli_usage(), "\n")
      0L
    } else {
      command <- args[[1L]]
      opts <- wsi_cli_parse(args[-1L])
      if (wsi_cli_flag(opts, "help")) {
        cat(wsi_cli_usage(), "\n")
        0L
      } else {
        switch(
          command,
          backends = wsi_cli_backends(opts),
          "translate-rois" = wsi_cli_translate_rois(opts),
          {
            cat(wsi_cli_usage(), "\n", file = stderr())
            wsi_abort(sprintf("Unknown wsiTools command: %s", command))
          }
        )
      }
    }
  }, error = function(err) {
    cat(sprintf("wsiTools command failed: %s\n", conditionMessage(err)), file = stderr())
    1L
  })

  if (isTRUE(quit)) {
    quit(save = "no", status = status, runLast = FALSE)
  }
  invisible(status)
}
