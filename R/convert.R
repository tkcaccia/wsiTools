#' Convert WSI or large image files
#'
#' Uses libvips command-line tools through `system2()` and does not silently
#' overwrite existing files.
#'
#' @param input Input file.
#' @param output Output file.
#' @param format Output format.
#' @param backend Conversion backend.
#' @param tile_size TIFF tile size.
#' @param compression TIFF compression.
#' @param pyramid Write a pyramidal TIFF when supported.
#' @param bigtiff Use BigTIFF for TIFF outputs.
#' @param overwrite Whether to overwrite `output`.
#'
#' @return The output path, invisibly.
#' @export
wsi_convert <- function(input, output,
                        format = c("ome-tiff", "tiff", "pyramidal-tiff", "png", "jpeg"),
                        backend = c("auto", "vips"),
                        tile_size = 512,
                        compression = c("lzw", "jpeg", "deflate", "none"),
                        pyramid = TRUE,
                        bigtiff = TRUE,
                        overwrite = FALSE) {
  input <- wsi_validate_input_path(input)
  output <- wsi_validate_output_path(output, overwrite = overwrite)
  format <- match.arg(format)
  backend <- match.arg(backend)
  compression <- match.arg(compression)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))

  if (backend == "auto") {
    backend <- "vips"
  }
  if (backend != "vips") {
    wsi_abort("Only the libvips conversion backend is implemented in the first milestone.")
  }
  if (!wsi_has_vips()) {
    wsi_abort("libvips backend is not installed. Install `vips` and `vipsheader`, then retry.", class = "wsi_backend_unavailable")
  }

  target <- output
  if (format %in% c("ome-tiff", "tiff", "pyramidal-tiff")) {
    target <- wsi_vips_tiff_target(
      output = output,
      tile_size = tile_size,
      compression = compression,
      pyramid = isTRUE(pyramid) || identical(format, "pyramidal-tiff") || identical(format, "ome-tiff"),
      bigtiff = bigtiff,
      ome = identical(format, "ome-tiff")
    )
  } else if (isTRUE(pyramid)) {
    wsi_warn("`pyramid = TRUE` is ignored for PNG/JPEG outputs.")
  }

  wsi_vips_copy(input, target)
  invisible(output)
}

#' Create a pyramidal TIFF or OME-TIFF
#'
#' @param input Input file.
#' @param output Output file.
#' @param tile_size TIFF tile size.
#' @param compression TIFF compression.
#' @param bigtiff Use BigTIFF.
#' @param ome Use OME-oriented TIFF options where libvips supports them.
#' @param overwrite Whether to overwrite `output`.
#'
#' @return The output path, invisibly.
#' @export
wsi_pyramid <- function(input, output, tile_size = 512,
                        compression = c("lzw", "jpeg", "deflate", "none"),
                        bigtiff = TRUE, ome = TRUE, overwrite = FALSE) {
  compression <- match.arg(compression)
  wsi_convert(
    input = input,
    output = output,
    format = if (isTRUE(ome)) "ome-tiff" else "pyramidal-tiff",
    backend = "vips",
    tile_size = tile_size,
    compression = compression,
    pyramid = TRUE,
    bigtiff = bigtiff,
    overwrite = overwrite
  )
}
