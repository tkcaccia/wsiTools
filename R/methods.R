#' @export
print.wsi_slide <- function(x, ...) {
  cat("<wsi_slide>\n")
  cat("  path:    ", x$path %||% NA_character_, "\n", sep = "")
  cat("  backend: ", x$backend, "\n", sep = "")
  cat("  size:    ", x$dimensions[["width"]], " x ", x$dimensions[["height"]], " px\n", sep = "")
  cat("  levels:  ", nrow(x$levels), "\n", sep = "")
  invisible(x)
}

#' @export
summary.wsi_slide <- function(object, ...) {
  info <- wsi_info(object)
  cat("Whole-slide image\n")
  cat("  Backend: ", info$backend, "\n", sep = "")
  cat("  Vendor:  ", info$vendor %||% NA_character_, "\n", sep = "")
  cat("  Size:    ", info$dimensions[["width"]], " x ", info$dimensions[["height"]], " px\n", sep = "")
  cat("  Levels:  ", info$level_count, "\n", sep = "")
  print(info$levels, row.names = FALSE)
  invisible(info)
}

#' @export
plot.wsi_slide <- function(x, ...) {
  thumb <- tryCatch(wsi_thumbnail(x, width = 1024, format = "raster"), error = function(err) err)
  graphics::plot.new()
  if (inherits(thumb, "error")) {
    graphics::text(0.5, 0.5, labels = paste("Thumbnail unavailable:", conditionMessage(thumb)))
    return(invisible(x))
  }
  graphics::rasterImage(thumb, 0, 0, 1, 1)
  invisible(x)
}

#' @export
print.wsi_tile_manifest <- function(x, ...) {
  cat("<wsi_tile_manifest>\n")
  cat("  tiles: ", nrow(x), "\n", sep = "")
  if (nrow(x)) {
    cat("  output: ", dirname(x$file[[1L]]), "\n", sep = "")
  }
  utils::str(utils::head(as.data.frame(x), 5L), vec.len = 2L)
  invisible(x)
}

#' @export
summary.wsi_tile_manifest <- function(object, ...) {
  cat("Tile manifest\n")
  cat("  Tiles: ", nrow(object), "\n", sep = "")
  if ("tissue_fraction" %in% names(object) && any(!is.na(object$tissue_fraction))) {
    cat("  Mean tissue fraction: ", round(mean(object$tissue_fraction, na.rm = TRUE), 3), "\n", sep = "")
  }
  if ("class" %in% names(object) && any(!is.na(object$class) & nzchar(object$class))) {
    cat("  Classes:\n")
    print(table(object$class, useNA = "ifany"))
  }
  if ("split" %in% names(object) && any(!is.na(object$split) & nzchar(object$split))) {
    cat("  Splits:\n")
    print(table(object$split, useNA = "ifany"))
  }
  invisible(object)
}

#' @export
print.wsi_info <- function(x, ...) {
  cat("<wsi_info>\n")
  cat("  backend: ", x$backend, "\n", sep = "")
  cat("  vendor:  ", x$vendor %||% NA_character_, "\n", sep = "")
  cat("  size:    ", x$dimensions[["width"]], " x ", x$dimensions[["height"]], " px\n", sep = "")
  cat("  levels:  ", x$level_count, "\n", sep = "")
  invisible(x)
}
