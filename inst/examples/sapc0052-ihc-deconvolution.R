# Example: test H-DAB deconvolution on SAPC 0052.svs
#
# Run from R with either:
#   Sys.setenv(SAPC0052_PATH = "/path/to/SAPC 0052.svs")
#   source(system.file("examples/sapc0052-ihc-deconvolution.R", package = "wsiTools"))
#
# or from the package source tree:
#   SAPC0052_PATH="/path/to/SAPC 0052.svs" Rscript inst/examples/sapc0052-ihc-deconvolution.R
#
# The viewer opens through a local http://127.0.0.1 server by default so the
# browser can read canvas pixels for interactive stain-channel selection. Set
# SAPC0052_SERVE_VIEWER=FALSE only if you explicitly want a file:// viewer.

library(wsiTools)

`%||%` <- function(x, y) if (is.null(x)) y else x

read_bool_env <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    return(isTRUE(default))
  }
  value <- tolower(trimws(value))
  if (value %in% c("1", "true", "t", "yes", "y", "on")) {
    return(TRUE)
  }
  if (value %in% c("0", "false", "f", "no", "n", "off")) {
    return(FALSE)
  }
  stop("Environment variable ", name, " must be TRUE or FALSE.", call. = FALSE)
}

sapc_candidates <- c(
  Sys.getenv("SAPC0052_PATH", unset = ""),
  "SAPC 0052.svs",
  "SAPC0052.svs",
  "SAPC_0052.svs"
)
sapc_candidates <- sapc_candidates[nzchar(sapc_candidates)]
existing_sapc <- sapc_candidates[file.exists(sapc_candidates)]
slide_path <- existing_sapc[1L]
if (!length(existing_sapc) || is.na(slide_path) || !file.exists(slide_path)) {
  stop(
    paste0(
      "Could not find SAPC 0052.svs. Set SAPC0052_PATH to the full slide path, ",
      "for example Sys.setenv(SAPC0052_PATH = \"/path/to/SAPC 0052.svs\")."
    ),
    call. = FALSE
  )
}

if (!wsi_has_openslide() && !wsi_has_vips()) {
  stop(
    "SAPC0052.svs requires OpenSlide or libvips for region-based reading.",
    call. = FALSE
  )
}

if (!requireNamespace("magick", quietly = TRUE)) {
  stop(
    "The optional package `magick` is required here to read cropped regions into R.",
    call. = FALSE
  )
}

if (!requireNamespace("tiff", quietly = TRUE)) {
  stop(
    "The optional package `tiff` is required here to write the two-channel OME-TIFF.",
    call. = FALSE
  )
}

if (!nzchar(Sys.which("tiffset"))) {
  stop(
    "The command-line tool `tiffset` is required here to embed OME-XML metadata.",
    call. = FALSE
  )
}

output_dir <- Sys.getenv("SAPC0052_OUTPUT_DIR", unset = "SAPC0052_ihc_deconvolution")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write_channel_png <- function(channel, file, invert = TRUE) {
  finite <- is.finite(channel)
  if (!any(finite)) {
    stop("Channel contains no finite values: ", file, call. = FALSE)
  }
  limits <- stats::quantile(channel[finite], probs = c(0.01, 0.99), na.rm = TRUE)
  if (!is.finite(diff(limits)) || diff(limits) <= 0) {
    limits <- range(channel[finite], na.rm = TRUE)
  }
  scaled <- pmin(pmax((channel - limits[[1L]]) / max(diff(limits), .Machine$double.eps), 0), 1)
  if (isTRUE(invert)) {
    scaled <- 1 - scaled
  }

  grDevices::png(file, width = ncol(channel), height = nrow(channel))
  old_par <- graphics::par(mar = c(0, 0, 0, 0))
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  }, add = TRUE)
  colours <- grDevices::gray(as.vector(scaled))
  dim(colours) <- dim(scaled)
  graphics::plot.new()
  graphics::rasterImage(grDevices::as.raster(colours), 0, 0, 1, 1)
  invisible(file)
}

write_array_png <- function(image, file) {
  dims <- dim(image)
  alpha <- if (dims[[3L]] >= 4L) image[, , 4L] else 1
  colours <- grDevices::rgb(
    as.vector(image[, , 1L]),
    as.vector(image[, , 2L]),
    as.vector(image[, , 3L]),
    alpha = as.vector(alpha)
  )
  dim(colours) <- dims[seq_len(2L)]
  raster <- grDevices::as.raster(colours)
  grDevices::png(file, width = dims[[2L]], height = dims[[1L]])
  old_par <- graphics::par(mar = c(0, 0, 0, 0))
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::plot.new()
  graphics::rasterImage(raster, 0, 0, 1, 1)
  invisible(file)
}

xml_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

scale_channel_for_ome <- function(channel, probs = c(0.001, 0.999)) {
  finite <- is.finite(channel)
  if (!any(finite)) {
    stop("Cannot write OME-TIFF because a deconvolved channel has no finite values.", call. = FALSE)
  }
  limits <- stats::quantile(channel[finite], probs = probs, na.rm = TRUE, names = FALSE)
  if (!is.finite(diff(limits)) || diff(limits) <= 0) {
    limits <- range(channel[finite], na.rm = TRUE)
  }
  scaled <- pmin(pmax((channel - limits[[1L]]) / max(diff(limits), .Machine$double.eps), 0), 1)
  scaled[!is.finite(scaled)] <- 0
  scaled
}

write_two_channel_ome_tiff <- function(hematoxylin,
                                       hrp_dab,
                                       file,
                                       channel_names = c("Hematoxylin", "HRP/DAB"),
                                       mpp = c(x = NA_real_, y = NA_real_),
                                       compression = "LZW") {
  if (!identical(dim(hematoxylin), dim(hrp_dab))) {
    stop("Hematoxylin and HRP/DAB channel matrices must have the same dimensions.", call. = FALSE)
  }

  file <- normalizePath(file, mustWork = FALSE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  h <- scale_channel_for_ome(hematoxylin)
  d <- scale_channel_for_ome(hrp_dab)
  tiff::writeTIFF(
    list(h, d),
    file,
    bits.per.sample = 16L,
    compression = compression
  )

  physical <- ""
  if (is.numeric(mpp) && length(mpp) >= 2L && all(is.finite(mpp[1:2])) && all(mpp[1:2] > 0)) {
    physical <- sprintf(
      ' PhysicalSizeX="%.8g" PhysicalSizeXUnit="um" PhysicalSizeY="%.8g" PhysicalSizeYUnit="um"',
      unname(mpp[[1L]]),
      unname(mpp[[2L]])
    )
  }

  ome_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06" ',
    'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ',
    'xsi:schemaLocation="http://www.openmicroscopy.org/Schemas/OME/2016-06 ',
    'http://www.openmicroscopy.org/Schemas/OME/2016-06/ome.xsd">',
    '<Image ID="Image:0" Name="', xml_escape(basename(file)), '">',
    '<Pixels ID="Pixels:0" DimensionOrder="XYCZT" Type="uint16" ',
    'SizeX="', ncol(h), '" SizeY="', nrow(h), '" SizeC="2" SizeZ="1" SizeT="1"',
    physical,
    '>',
    '<Channel ID="Channel:0:0" Name="', xml_escape(channel_names[[1L]]), '" SamplesPerPixel="1"><LightPath/></Channel>',
    '<Channel ID="Channel:0:1" Name="', xml_escape(channel_names[[2L]]), '" SamplesPerPixel="1"><LightPath/></Channel>',
    '<TiffData IFD="0" FirstC="0" FirstZ="0" FirstT="0" PlaneCount="1"/>',
    '<TiffData IFD="1" FirstC="1" FirstZ="0" FirstT="0" PlaneCount="1"/>',
    "</Pixels>",
    "</Image>",
    "</OME>"
  )

  xml_file <- tempfile(fileext = ".ome.xml")
  on.exit(unlink(xml_file), add = TRUE)
  writeLines(ome_xml, xml_file, useBytes = TRUE)

  status <- system2(
    "tiffset",
    args = c("-d", "0", "-sf", "270", xml_file, file),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status") %||% 0L
  if (!identical(as.integer(exit_status), 0L)) {
    stop("Failed to embed OME-XML metadata with tiffset:\n", paste(status, collapse = "\n"), call. = FALSE)
  }

  invisible(file)
}

open_viewer_file <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(FALSE)
  }
  viewer_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  serve_viewer <- read_bool_env("SAPC0052_SERVE_VIEWER", default = TRUE)
  if (isTRUE(serve_viewer)) {
    python <- Sys.which("python3")
    if (!nzchar(python)) {
      python <- Sys.which("python")
    }
    if (nzchar(python)) {
      root <- dirname(viewer_path)
      start_port <- suppressWarnings(as.integer(Sys.getenv("SAPC0052_VIEWER_PORT", unset = "8765")))
      if (is.na(start_port) || start_port < 1024L || start_port > 65535L) {
        stop("Environment variable SAPC0052_VIEWER_PORT must be an integer port between 1024 and 65535.", call. = FALSE)
      }
      port <- start_port
      found_port <- FALSE
      for (candidate in seq.int(start_port, length.out = 50L)) {
        con <- try(
          suppressWarnings(socketConnection(
            host = "127.0.0.1",
            port = candidate,
            open = "r+",
            blocking = TRUE,
            timeout = 0.2
          )),
          silent = TRUE
        )
        if (inherits(con, "try-error")) {
          port <- candidate
          found_port <- TRUE
          break
        }
        close(con)
      }
      if (!isTRUE(found_port)) {
        stop("Could not find a free localhost port for the interactive viewer.", call. = FALSE)
      }
      log_file <- file.path(root, sprintf("wsiTools_viewer_%s.log", port))
      http_args <- c(
        "-m", "http.server", as.character(port),
        "--bind", "127.0.0.1",
        "--directory", root
      )
      launcher <- Sys.which("nohup")
      if (nzchar(launcher)) {
        system2(launcher, args = c(python, http_args), stdout = log_file, stderr = log_file, wait = FALSE)
      } else {
        system2(python, args = http_args, stdout = log_file, stderr = log_file, wait = FALSE)
      }
      Sys.sleep(0.75)
      viewer_url <- paste0(
        "http://127.0.0.1:",
        port,
        "/",
        utils::URLencode(basename(viewer_path), reserved = FALSE)
      )
      ok <- tryCatch(
        {
          utils::browseURL(viewer_url)
          TRUE
        },
        error = function(err) {
          warning(
            "Could not open the localhost viewer automatically: ",
            conditionMessage(err),
            "\nOpen this URL manually: ",
            viewer_url,
            call. = FALSE
          )
          FALSE
        }
      )
      if (isTRUE(ok)) {
        message("Opened interactive viewer: ", viewer_url)
        message("Serving viewer directory: ", root)
        message("Viewer server log: ", log_file)
      }
      return(invisible(ok))
    }
    warning(
      "Python was not found, so the viewer will be opened as a file:// URL. ",
      "Stain channel selection may be blocked by browser canvas security.",
      call. = FALSE
    )
  }

  viewer_url <- paste0("file://", utils::URLencode(viewer_path, reserved = FALSE))
  ok <- tryCatch(
    {
      utils::browseURL(viewer_url)
      TRUE
    },
    error = function(err) {
      warning(
        "Could not open the interactive viewer automatically: ",
        conditionMessage(err),
        "\nOpen this file manually: ",
        viewer_path,
        call. = FALSE
      )
      FALSE
    }
  )
  if (isTRUE(ok)) {
    message("Opened interactive viewer: ", viewer_path)
  }
  invisible(ok)
}

slide <- wsi_open(slide_path, backend = "auto")
on.exit(wsi_close(slide), add = TRUE)

info <- wsi_info(slide)
print(info)

region_width <- as.integer(Sys.getenv("SAPC0052_REGION_WIDTH", unset = "1024"))
region_height <- as.integer(Sys.getenv("SAPC0052_REGION_HEIGHT", unset = "1024"))
region_x <- Sys.getenv("SAPC0052_REGION_X", unset = NA_character_)
region_y <- Sys.getenv("SAPC0052_REGION_Y", unset = NA_character_)

if (is.na(region_x) || is.na(region_y)) {
  x <- floor((info$dimensions[["width"]] - region_width) / 2)
  y <- floor((info$dimensions[["height"]] - region_height) / 2)
} else {
  x <- as.integer(region_x)
  y <- as.integer(region_y)
}

message("Opened slide: ", normalizePath(slide_path, mustWork = FALSE))
message("Reading region: x=", x, " y=", y, " width=", region_width, " height=", region_height)

channels <- wsi_deconvolve_region(
  slide,
  x = x,
  y = y,
  width = region_width,
  height = region_height,
  level = 0,
  format = "channels"
)

composite <- wsi_deconvolve_region(
  slide,
  x = x,
  y = y,
  width = region_width,
  height = region_height,
  level = 0,
  format = "array",
  hematoxylin_colour = "#4b3f99",
  hrp_colour = "#8b5a2b"
)

write_channel_png(channels$hematoxylin, file.path(output_dir, "SAPC0052_hematoxylin.png"))
hrp_channel <- channels$hrp_dab %||% channels$hrp
write_channel_png(hrp_channel, file.path(output_dir, "SAPC0052_hrp_dab.png"))
write_array_png(composite, file.path(output_dir, "SAPC0052_hdab_composite.png"))
ome_tiff <- file.path(output_dir, "SAPC0052_hdab_two_channel.ome.tiff")
write_two_channel_ome_tiff(
  hematoxylin = channels$hematoxylin,
  hrp_dab = hrp_channel,
  file = ome_tiff,
  mpp = wsi_mpp(slide)
)

viewer_mode <- if (wsi_has_vips()) "tiles" else "thumbnail"
viewer_args <- list(
  slide = slide,
  mode = viewer_mode,
  output = file.path(output_dir, "SAPC0052_ihc_viewer.html"),
  # Open after writing so browser-launch errors can be reported without stopping export.
  open = FALSE,
  overwrite = TRUE
)
if (identical(viewer_mode, "tiles")) {
  viewer_args$tile_dir <- file.path(output_dir, "SAPC0052_deepzoom_tiles")
  viewer_args$rebuild <- FALSE
}

html <- do.call(wsi_viewer_ihc, viewer_args)
if (read_bool_env("SAPC0052_OPEN_VIEWER", default = TRUE)) {
  open_viewer_file(html)
}

message("Wrote:")
message("  ", normalizePath(file.path(output_dir, "SAPC0052_hematoxylin.png"), mustWork = FALSE))
message("  ", normalizePath(file.path(output_dir, "SAPC0052_hrp_dab.png"), mustWork = FALSE))
message("  ", normalizePath(file.path(output_dir, "SAPC0052_hdab_composite.png"), mustWork = FALSE))
message("  ", normalizePath(ome_tiff, mustWork = FALSE))
message("  ", normalizePath(html, mustWork = FALSE))
