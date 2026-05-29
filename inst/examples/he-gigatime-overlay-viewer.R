# H&E + GigaTIME/mIHC overlay viewer example
#
# This opens an H&E whole-slide image as the base layer and overlays a
# registered GigaTIME/mIHC OME-TIFF as selectable tiled channels. The full
# images are not loaded into R memory.
#
# You can override the default paths with environment variables:
#   WSITOOLS_HE_SLIDE
#   WSITOOLS_HE_DEEPZOOM
#   WSITOOLS_GIGATIME_OME_TIFF
#   WSITOOLS_GIGATIME_REGISTRATION
#   WSITOOLS_VIEWER_DIR

library(wsiTools)

viewer_dir <- Sys.getenv("WSITOOLS_VIEWER_DIR", "/Users/stefano/Documents/viewer")
he_slide <- Sys.getenv("WSITOOLS_HE_SLIDE", "/Users/stefano/Downloads/AP-GY-26-04_HE.svs")
he_deepzoom <- Sys.getenv("WSITOOLS_HE_DEEPZOOM", file.path(viewer_dir, "apgy2604_he_deepzoom"))
mihc <- Sys.getenv(
  "WSITOOLS_GIGATIME_OME_TIFF",
  "/Users/stefano/Documents/CellPhenotyper/remote_previews/apgy2604_ometiff_jpeg_pyramid/gigatime_probs.ome.tif"
)
registration <- Sys.getenv(
  "WSITOOLS_GIGATIME_REGISTRATION",
  "/Users/stefano/Documents/CellPhenotyper/remote_previews/apgy2604_registration/shift.json"
)

stopifnot(file.exists(mihc), file.exists(registration))
dir.create(viewer_dir, recursive = TRUE, showWarnings = FALSE)

if (exists("apgy2604_gigatime_viewer", envir = .GlobalEnv, inherits = FALSE)) {
  try(wsi_viewer_stop(get("apgy2604_gigatime_viewer", envir = .GlobalEnv)), silent = TRUE)
  rm("apgy2604_gigatime_viewer", envir = .GlobalEnv)
}

out <- file.path(viewer_dir, "apgy2604_he_gigatime_overlay_viewer.html")
has_fast_he_tiles <- file.exists(file.path(he_deepzoom, "slide.dzi")) &&
  dir.exists(file.path(he_deepzoom, "slide_files"))

if (!file.exists(he_slide) && !has_fast_he_tiles) {
  stop(
    "No H&E source is available. Set WSITOOLS_HE_SLIDE to an H&E WSI path, ",
    "or set WSITOOLS_HE_DEEPZOOM to an existing Deep Zoom tile directory.",
    call. = FALSE
  )
}

channel_names <- c("DAPI", "PD-1")
channel_colours <- c("#3B82F6", "#F97316")
channel_visible <- c(FALSE, TRUE)

if (file.exists(he_slide)) {
  viewer <- wsi_viewer_he_mihc(
    he = he_slide,
    mihc = mihc,
    registration = registration,
    channel_names = channel_names,
    colours = channel_colours,
    visible = channel_visible,
    opacity = 0.55,
    dynamic_tiles = !has_fast_he_tiles,
    dynamic_tile_format = "jpg",
    mode = "tiles",
    stain = "he",
    base_layer_name = "H&E",
    tile_dir = if (has_fast_he_tiles) he_deepzoom else NULL,
    rebuild = FALSE,
    tile_format = "jpg",
    output = out,
    overwrite = TRUE,
    open = TRUE,
    wait = FALSE,
    transport = "auto",
    name = "apgy2604_gigatime"
  )
} else {
  # If only prebuilt H&E tiles are available, open those as the base layer and
  # keep the mIHC channels live through the R tile server.
  dzi <- paste(readLines(file.path(he_deepzoom, "slide.dzi"), warn = FALSE), collapse = " ")
  width <- as.numeric(sub('.*Width="([0-9]+)".*', "\\1", dzi))
  height <- as.numeric(sub('.*Height="([0-9]+)".*', "\\1", dzi))
  tile_size <- as.numeric(sub('.*TileSize="([0-9]+)".*', "\\1", dzi))
  overlap <- as.numeric(sub('.*Overlap="([0-9]+)".*', "\\1", dzi))
  max_level <- ceiling(log2(max(width, height)))
  tile_url_base <- if (identical(normalizePath(dirname(out), winslash = "/", mustWork = FALSE),
                                 normalizePath(dirname(he_deepzoom), winslash = "/", mustWork = FALSE))) {
    file.path(basename(he_deepzoom), "slide_files")
  } else {
    paste0("file://", utils::URLencode(normalizePath(file.path(he_deepzoom, "slide_files"), winslash = "/", mustWork = TRUE), reserved = FALSE))
  }
  slide <- wsiTools:::wsi_mock_slide(width = width, height = height, levels = c(1, 4, 16, 64))
  slide$path <- "AP-GY-26-04_HE_prebuilt_deepzoom"
  slide$backend <- "prebuilt-deepzoom"
  channels <- wsi_mihc_channel_sources(
    mihc,
    channel_names = channel_names,
    colours = channel_colours,
    visible = channel_visible,
    opacity = 0.55,
    registration = registration,
    format = "png"
  )
  viewer <- wsi_viewer_live(
    slide,
    mode = "tiles",
    dynamic_tiles = FALSE,
    tile_url_base = tile_url_base,
    tile_url_style = "deepzoom",
    tile_size = if (is.finite(tile_size)) tile_size else 512,
    tile_overlap = if (is.finite(overlap)) overlap else 0,
    tile_format = "jpg",
    max_level = max_level,
    tile_source_label = "prebuilt H&E Deep Zoom tiles",
    channel_sources = channels,
    stain = "none",
    base_layer_name = "H&E",
    output = out,
    overwrite = TRUE,
    open = TRUE,
    wait = FALSE,
    transport = "auto",
    name = "apgy2604_gigatime"
  )
}

assign("apgy2604_gigatime_viewer", viewer, envir = .GlobalEnv)
message("Viewer object saved as `apgy2604_gigatime_viewer`.")
message("Use the top Stains menu to toggle H&E and GigaTIME/mIHC channels.")
message("Inspect channels with: apgy2604_gigatime_viewer$get_channel_settings()")
