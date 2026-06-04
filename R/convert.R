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
#' @param subifd Store pyramid layers as SubIFDs for TIFF outputs. This is
#'   enabled automatically for OME-TIFF outputs.
#' @param properties Ask libvips to write image metadata into the TIFF
#'   ImageDescription tag.
#' @param depth Pyramid depth passed to libvips.
#' @param region_shrink Shrink strategy for lower pyramid levels.
#' @param predictor TIFF predictor for lossless compression.
#' @param quality JPEG/WebP quality factor when applicable.
#' @param compression_level Deflate or Zstd compression level when applicable.
#' @param overwrite Whether to overwrite `output`.
#'
#' @return The output path, invisibly.
#' @export
wsi_convert <- function(input, output,
                        format = c("ome-tiff", "tiff", "pyramidal-tiff", "png", "jpeg"),
                        backend = c("auto", "vips"),
                        tile_size = 512,
                        compression = c("lzw", "jpeg", "deflate", "zstd", "webp", "packbits", "none"),
                        pyramid = TRUE,
                        bigtiff = TRUE,
                        subifd = NULL,
                        properties = NULL,
                        depth = c("onepixel", "onetile", "one"),
                        region_shrink = c("mean", "median", "mode", "max", "min", "nearest"),
                        predictor = c("horizontal", "none", "float"),
                        quality = NULL,
                        compression_level = NULL,
                        overwrite = FALSE) {
  input <- wsi_validate_input_path(input)
  output <- wsi_validate_output_path(output, overwrite = overwrite)
  format <- match.arg(format)
  backend <- match.arg(backend)
  compression <- match.arg(compression)
  depth <- match.arg(depth)
  region_shrink <- match.arg(region_shrink)
  predictor <- match.arg(predictor)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  if (!is.null(quality)) {
    quality <- as.integer(wsi_check_scalar_number(quality, "quality", allow_zero = FALSE))
    if (quality < 1L || quality > 100L) {
      wsi_abort("`quality` must be between 1 and 100.")
    }
  }
  if (!is.null(compression_level)) {
    compression_level <- as.integer(wsi_check_scalar_number(compression_level, "compression_level", allow_zero = FALSE))
    max_level <- if (identical(compression, "zstd")) 22L else 9L
    if (!compression %in% c("deflate", "zstd")) {
      wsi_warn("`compression_level` is only used for Deflate and Zstd TIFF compression; ignoring it.")
      compression_level <- NULL
    } else if (compression_level < 1L || compression_level > max_level) {
      wsi_abort(sprintf("`compression_level` must be between 1 and %d for %s compression.", max_level, compression))
    }
  }

  if (backend == "auto") {
    backend <- "vips"
  }
  if (backend != "vips") {
    wsi_abort("Only the libvips conversion backend is implemented in the first milestone.")
  }
  if (!wsi_has_vips()) {
    wsi_abort(
      wsi_backend_action_message(
        "libvips conversion could not start because the libvips backend is not installed.",
        backend = "vips"
      ),
      class = "wsi_backend_unavailable"
    )
  }

  target <- output
  if (format %in% c("ome-tiff", "tiff", "pyramidal-tiff")) {
    write_ome <- identical(format, "ome-tiff")
    write_pyramid <- isTRUE(pyramid) || identical(format, "pyramidal-tiff") || write_ome
    write_subifd <- isTRUE(subifd) || write_ome
    write_properties <- isTRUE(properties) || write_ome
    write_predictor <- if (compression %in% c("lzw", "deflate", "zstd")) predictor else NULL
    target <- wsi_vips_tiff_target(
      output = output,
      tile_size = tile_size,
      compression = compression,
      pyramid = write_pyramid,
      bigtiff = bigtiff,
      ome = write_ome,
      subifd = write_subifd,
      properties = write_properties,
      depth = if (isTRUE(write_pyramid)) depth else NULL,
      region_shrink = if (isTRUE(write_pyramid)) region_shrink else NULL,
      predictor = write_predictor,
      quality = quality,
      compression_level = compression_level
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
#' @param depth Pyramid depth passed to libvips.
#' @param region_shrink Shrink strategy for lower pyramid levels.
#' @param predictor TIFF predictor for lossless compression.
#' @param quality JPEG/WebP quality factor when applicable.
#' @param compression_level Deflate or Zstd compression level when applicable.
#' @param overwrite Whether to overwrite `output`.
#'
#' @return The output path, invisibly.
#' @export
wsi_pyramid <- function(input, output, tile_size = 512,
                        compression = c("lzw", "jpeg", "deflate", "zstd", "webp", "packbits", "none"),
                        bigtiff = TRUE, ome = TRUE,
                        depth = c("onepixel", "onetile", "one"),
                        region_shrink = c("mean", "median", "mode", "max", "min", "nearest"),
                        predictor = c("horizontal", "none", "float"),
                        quality = NULL,
                        compression_level = NULL,
                        overwrite = FALSE) {
  compression <- match.arg(compression)
  depth <- match.arg(depth)
  region_shrink <- match.arg(region_shrink)
  predictor <- match.arg(predictor)
  wsi_convert(
    input = input,
    output = output,
    format = if (isTRUE(ome)) "ome-tiff" else "pyramidal-tiff",
    backend = "vips",
    tile_size = tile_size,
    compression = compression,
    pyramid = TRUE,
    bigtiff = bigtiff,
    depth = depth,
    region_shrink = region_shrink,
    predictor = predictor,
    quality = quality,
    compression_level = compression_level,
    overwrite = overwrite
  )
}

#' Export a pyramidal OME-TIFF
#'
#' Convenience wrapper around [wsi_convert()] for OME-oriented tiled pyramidal
#' TIFF export using libvips. The function enables pyramids, BigTIFF, SubIFD
#' pyramid storage, and TIFF metadata properties by default.
#'
#' @inheritParams wsi_convert
#'
#' @return The output path, invisibly.
#' @export
wsi_export_ome_tiff <- function(input, output,
                                tile_size = 512,
                                compression = c("lzw", "jpeg", "deflate", "zstd", "webp", "packbits", "none"),
                                bigtiff = TRUE,
                                depth = c("onepixel", "onetile", "one"),
                                region_shrink = c("mean", "median", "mode", "max", "min", "nearest"),
                                predictor = c("horizontal", "none", "float"),
                                quality = NULL,
                                compression_level = NULL,
                                overwrite = FALSE) {
  compression <- match.arg(compression)
  depth <- match.arg(depth)
  region_shrink <- match.arg(region_shrink)
  predictor <- match.arg(predictor)
  wsi_convert(
    input = input,
    output = output,
    format = "ome-tiff",
    backend = "vips",
    tile_size = tile_size,
    compression = compression,
    pyramid = TRUE,
    bigtiff = bigtiff,
    subifd = TRUE,
    properties = TRUE,
    depth = depth,
    region_shrink = region_shrink,
    predictor = predictor,
    quality = quality,
    compression_level = compression_level,
    overwrite = overwrite
  )
}
