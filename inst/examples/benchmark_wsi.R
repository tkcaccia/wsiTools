#!/usr/bin/env Rscript

# Reproducible region-based WSI smoke benchmark.
# Usage:
#   Rscript benchmark_wsi.R image.tif benchmark-output

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript benchmark_wsi.R <image> [output-directory]", call. = FALSE)
}

input <- normalizePath(args[[1L]], mustWork = TRUE)
output_dir <- if (length(args) >= 2L) args[[2L]] else file.path(getwd(), "wsiTools-benchmark")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

library(wsiTools)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

measure <- function(label, expression) {
  started <- proc.time()[["elapsed"]]
  value <- force(expression)
  elapsed <- proc.time()[["elapsed"]] - started
  list(label = label, seconds = unname(elapsed), value = value)
}

records <- list()
add_record <- function(label, result, status = "ok", detail = "") {
  records[[length(records) + 1L]] <<- data.frame(
    operation = label,
    seconds = result$seconds,
    status = status,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

opened <- measure("open_metadata", tryCatch(wsi_open(input), error = identity))
if (inherits(opened$value, "error")) {
  add_record("open_metadata", opened, "failed", conditionMessage(opened$value))
  write.csv(do.call(rbind, records), file.path(output_dir, "timings.csv"), row.names = FALSE)
  writeLines(capture.output(wsi_backends()), file.path(output_dir, "backends.txt"))
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
  stop(opened$value)
}

slide <- opened$value
on.exit(wsi_close(slide), add = TRUE)
add_record("open_metadata", opened, detail = slide$backend %||% "unknown")

info <- wsi_info(slide)
writeLines(capture.output(str(info)), file.path(output_dir, "image-info.txt"))
writeLines(capture.output(wsi_backends()), file.path(output_dir, "backends.txt"))
writeLines(capture.output(wsi_diagnose(live_test = FALSE)), file.path(output_dir, "diagnose.txt"))
writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))

thumbnail <- measure("thumbnail", tryCatch(wsi_thumbnail(slide, width = 1024), error = identity))
if (inherits(thumbnail$value, "error")) {
  add_record("thumbnail", thumbnail, "failed", conditionMessage(thumbnail$value))
} else {
  add_record("thumbnail", thumbnail)
}

dimensions <- info$dimensions %||% c(width = NA_real_, height = NA_real_)
width <- as.numeric(info$width %||% info$level0_width %||% dimensions[["width"]] %||% NA_real_)
height <- as.numeric(info$height %||% info$level0_height %||% dimensions[["height"]] %||% NA_real_)
if (is.finite(width) && is.finite(height)) {
  region_width <- min(1024, width)
  region_height <- min(1024, height)
  region_x <- max(0, floor((width - region_width) / 2))
  region_y <- max(0, floor((height - region_height) / 2))
  region <- measure(
    "center_region_1024",
    tryCatch(
      wsi_read_region(
        slide,
        x = region_x,
        y = region_y,
        width = region_width,
        height = region_height,
        level = 0,
        format = "array"
      ),
      error = identity
    )
  )
  if (inherits(region$value, "error")) {
    add_record("center_region_1024", region, "failed", conditionMessage(region$value))
  } else {
    add_record("center_region_1024", region, detail = sprintf("%sx%s", region_width, region_height))
  }
}

timings <- do.call(rbind, records)
write.csv(timings, file.path(output_dir, "timings.csv"), row.names = FALSE)
message("Benchmark written to: ", normalizePath(output_dir, mustWork = FALSE))
print(timings, row.names = FALSE)
