#!/usr/bin/env Rscript

# Run this on Chiamaka's Ubuntu workstation, not through the Tauri desktop app.
#
# Default:
#   Rscript /mnt/sata_ssd/benchmarks/open_visiumhd_chiamaka_live.R
#
# Optional overrides:
#   WSITOOLS_VISIUMHD_DIR=/mnt/sata_ssd/benchmarks/VF
#   WSITOOLS_VISIUMHD_PROJECT=breast
#   WSITOOLS_VISIUMHD_IMAGE=/path/to/tissue.btf
#   WSITOOLS_VISIUMHD_SEURAT=/path/to/seurat.rds
#   WSITOOLS_VISIUMHD_GEOJSON=/path/to/cells.geojson
#   WSITOOLS_VISIUMHD_MASK_DOWNSAMPLE=4
#   WSITOOLS_VISIUMHD_OPEN=true
#   WSITOOLS_VISIUMHD_WAIT=true

options(warn = 1)

if (!requireNamespace("wsiTools", quietly = TRUE)) {
  stop(
    "The wsiTools package is not installed. Install it first with:\n",
    "remotes::install_github('tkcaccia/wsiTools', upgrade = 'never')",
    call. = FALSE
  )
}

library(wsiTools)

msg <- function(...) message(sprintf(...))
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || all(is.na(x)) || !nzchar(as.character(x[[1L]]))) y else x

env_path <- function(name) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) return(NULL)
  normalizePath(value, winslash = "/", mustWork = FALSE)
}

truthy <- function(x, default = FALSE) {
  x <- Sys.getenv(x, unset = if (default) "true" else "false")
  tolower(x) %in% c("1", "true", "yes", "y")
}

find_files <- function(root, exts, target = NULL, include = NULL, exclude = NULL) {
  if (!dir.exists(root)) return(character())
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  if (!length(files)) return(character())
  ext_regex <- paste0("\\.(", paste(exts, collapse = "|"), ")$")
  files <- files[grepl(ext_regex, files, ignore.case = TRUE)]
  if (!length(files)) return(character())
  score <- rep(0, length(files))
  lower <- tolower(files)
  if (!is.null(target) && nzchar(target)) {
    score <- score + ifelse(grepl(tolower(target), lower, fixed = TRUE), 100, 0)
  }
  if (!is.null(include)) {
    score <- score + ifelse(grepl(include, lower, ignore.case = TRUE), 25, 0)
  }
  if (!is.null(exclude)) {
    score <- score - ifelse(grepl(exclude, lower, ignore.case = TRUE), 50, 0)
  }
  files[order(-score, nchar(files), files)]
}

first_existing <- function(x) {
  x <- x[!is.na(x) & nzchar(x) & file.exists(x)]
  if (length(x)) normalizePath(x[[1L]], winslash = "/", mustWork = TRUE) else NULL
}

load_r_object <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "rds")) return(readRDS(path))
  env <- new.env(parent = emptyenv())
  loaded <- load(path, envir = env)
  if (!length(loaded)) stop("No object was found in: ", path, call. = FALSE)
  objects <- mget(loaded, envir = env, inherits = FALSE)
  seurat_idx <- vapply(objects, inherits, logical(1), what = "Seurat")
  if (any(seurat_idx)) return(objects[[which(seurat_idx)[[1L]]]])
  objects[[1L]]
}

as_legend <- function(labels) {
  if (is.null(labels) || !nrow(labels)) return(list())
  lapply(seq_len(nrow(labels)), function(i) {
    list(
      value = as.character(labels$value[[i]]),
      label = as.character(labels$key[[i]] %||% labels$name[[i]] %||% paste0("label_", labels$value[[i]])),
      colour = as.character(labels$colour[[i]] %||% labels$color[[i]] %||% "#ffffff"),
      count = if ("count" %in% names(labels)) labels$count[[i]] else NULL
    )
  })
}

cat("\n--- wsiTools backend status ---\n")
print(wsi_backends())
cat("--------------------------------\n\n")

target <- Sys.getenv("WSITOOLS_VISIUMHD_PROJECT", unset = "breast")
project_root <- env_path("WSITOOLS_VISIUMHD_DIR") %||% "/mnt/sata_ssd/benchmarks/VF"

if (!dir.exists(project_root)) {
  stop("Project root was not found: ", project_root, "\nSet WSITOOLS_VISIUMHD_DIR to the VisiumHD project folder.", call. = FALSE)
}

project_dir <- NULL
project_dirs <- list.dirs(project_root, recursive = FALSE, full.names = TRUE)
if (length(project_dirs) && nzchar(target)) {
  matches <- project_dirs[grepl(tolower(target), tolower(basename(project_dirs)), fixed = TRUE)]
  if (length(matches)) project_dir <- normalizePath(matches[[1L]], winslash = "/", mustWork = TRUE)
}
search_root <- project_dir %||% project_root

output_root <- env_path("WSITOOLS_VISIUMHD_OUTPUT_DIR") %||%
  if (!is.null(project_dir)) {
    file.path(project_dir, "wsiTools_viewer")
  } else {
    file.path(project_root, "save", paste0("visiumhd_wsiTools_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  }
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

existing_output_file <- function(pattern) {
  hits <- list.files(output_root, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  hits <- hits[file.exists(hits)]
  if (length(hits)) normalizePath(hits[[1L]], winslash = "/", mustWork = TRUE) else NULL
}

existing_output_dir <- function(pattern) {
  hits <- list.dirs(output_root, recursive = FALSE, full.names = TRUE)
  hits <- hits[grepl(pattern, basename(hits), ignore.case = TRUE)]
  hits <- hits[dir.exists(hits)]
  if (length(hits)) normalizePath(hits[[1L]], winslash = "/", mustWork = TRUE) else NULL
}

msg("Searching VisiumHD files under: %s", search_root)
msg("Project keyword: %s", target)

image_file <- first_existing(env_path("WSITOOLS_VISIUMHD_IMAGE") %||% character())
seurat_file <- first_existing(env_path("WSITOOLS_VISIUMHD_SEURAT") %||% character())
geojson_file <- first_existing(env_path("WSITOOLS_VISIUMHD_GEOJSON") %||% character())

if (is.null(image_file)) {
  image_file <- first_existing(find_files(
    search_root,
    exts = c("btf", "tif", "tiff", "svs", "ome.tif", "ome.tiff"),
    target = target,
    include = "image|tissue|full|hires|high|wsi|btf|ome",
    exclude = "mask|segmentation|annotation|geojson|deepzoom|tile|prob|thumbnail|preview"
  ))
}

if (is.null(seurat_file)) {
  seurat_file <- first_existing(find_files(
    search_root,
    exts = c("rds", "rda", "rdata"),
    target = target,
    include = "seurat|object|visium|spatial",
    exclude = "mask|geojson|annotation|image|tile"
  ))
}

if (is.null(geojson_file)) {
  geojson_file <- first_existing(find_files(
    search_root,
    exts = c("geojson", "json"),
    target = target,
    include = "cell|annotation|segmentation|geojson|medsam|mask|cluster",
    exclude = "scalefactors|metadata|shift|project_outputs|manifest"
  ))
}

spatial_dir <- NULL
spatial_files <- find_files(search_root, exts = c("json", "csv"), target = target, include = "scalefactors_json|tissue_positions")
if (length(spatial_files)) {
  possible <- unique(dirname(spatial_files[grepl("scalefactors_json|tissue_positions", basename(spatial_files), ignore.case = TRUE)]))
  preferred <- possible[basename(possible) == "spatial"]
  if (length(preferred)) {
    spatial_dir <- preferred[[1L]]
  } else if (length(possible)) {
    spatial_dir <- possible[[1L]]
  }
}

if (is.null(image_file)) stop("No high-resolution tissue image was found. Set WSITOOLS_VISIUMHD_IMAGE.", call. = FALSE)
if (is.null(seurat_file)) stop("No Seurat .rds/.rda/.RData object was found. Set WSITOOLS_VISIUMHD_SEURAT.", call. = FALSE)

msg("Image:   %s", image_file)
msg("Seurat:  %s", seurat_file)
msg("GeoJSON: %s", geojson_file %||% "<none>")
msg("Spatial: %s", spatial_dir %||% "<from Seurat object if available>")
msg("Output:  %s", output_root)

seurat_obj <- load_r_object(seurat_file)
slide <- wsi_open(image_file)
info <- wsi_info(slide)
msg("Slide size: %s x %s pixels", info$width %||% slide$dimensions[["width"]], info$height %||% slide$dimensions[["height"]])

channel_sources <- list()

if (!is.null(geojson_file)) {
  if (!wsi_has_vips()) {
    warning("libvips is not available, so the cell GeoJSON cannot be converted to a tiled mask overlay.")
  } else {
    mask_downsample <- as.numeric(Sys.getenv("WSITOOLS_VISIUMHD_MASK_DOWNSAMPLE", unset = "4"))
    if (!is.finite(mask_downsample) || mask_downsample < 1) mask_downsample <- 4
    mask_ds_token <- gsub("\\.", "_", as.character(mask_downsample))
    mask_ome <- existing_output_file(sprintf(".*cell.*annotation.*mask.*ds%s.*\\.ome\\.tiff?$", mask_ds_token)) %||%
      file.path(output_root, sprintf("cell_annotation_mask_ds%s.ome.tif", mask_ds_token))
    msg("Converting cell GeoJSON to coloured mask OME-TIFF at downsample %s.", mask_downsample)
    msg("This is intentionally tiled and does not send every cell polygon to the browser.")

    mask_result <- if (file.exists(mask_ome)) {
      msg("Reusing existing mask: %s", mask_ome)
      legend_file <- existing_output_file(sprintf(".*cell.*annotation.*mask.*(legend|labels).*ds%s.*\\.csv$", mask_ds_token)) %||%
        sub("\\.ome\\.tiff?$", "_labels.csv", mask_ome, ignore.case = TRUE)
      labels <- if (file.exists(legend_file)) utils::read.csv(legend_file, stringsAsFactors = FALSE) else data.frame()
      list(
        output = mask_ome,
        legend = legend_file,
        labels = labels,
        slide_width = as.numeric(slide$dimensions[["width"]]),
        slide_height = as.numeric(slide$dimensions[["height"]]),
        mask_width = ceiling(as.numeric(slide$dimensions[["width"]]) / mask_downsample),
        mask_height = ceiling(as.numeric(slide$dimensions[["height"]]) / mask_downsample),
        downsample = mask_downsample
      )
    } else {
      wsi_geojson_to_mask_tiff(
        geojson = geojson_file,
        output = mask_ome,
        slide = slide,
        downsample = mask_downsample,
        label_by = "class",
        colour = TRUE,
        background_colour = "#000000",
        smooth = TRUE,
        smooth_iterations = 1,
        smooth_max_vertices = 4000,
        format = "ome-tiff",
        pyramid = TRUE,
        tile_size = 512,
        compression = "lzw",
        overwrite = TRUE,
        return_mask = FALSE
      )
    }

    mask_slide <- wsi_open(mask_result$output)
    mask_tile_dir <- existing_output_dir(sprintf(".*cell.*annotation.*mask.*ds%s.*tiles$", mask_ds_token)) %||%
      file.path(output_root, "cell_annotation_mask_deepzoom")
    create_deepzoom <- getFromNamespace("wsi_create_deepzoom_tiles", "wsiTools")
    tile_base <- getFromNamespace("wsi_tile_base_url", "wsiTools")
    dz_max_level <- getFromNamespace("wsi_dz_max_level", "wsiTools")

    msg("Creating/reusing cell mask Deep Zoom tiles: %s", mask_tile_dir)
    mask_tiles <- create_deepzoom(
      slide = mask_slide,
      tile_dir = mask_tile_dir,
      tile_size = 512,
      tile_overlap = 1,
      tile_format = "png",
      quality = 90,
      rebuild = FALSE
    )

    html_output <- file.path(output_root, "visiumhd_live_viewer.html")
    legend <- as_legend(mask_result$labels)
    channel_sources <- list(wsi_channel_source(
      name = "Cell annotation mask",
      id = "visiumhd_cell_annotation_mask",
      type = "deepzoom",
      tile_url_base = tile_base(mask_tile_dir, html_output),
      width = mask_result$mask_width,
      height = mask_result$mask_height,
      tile_size = 512,
      tile_format = "png",
      max_level = dz_max_level(mask_result$mask_width, mask_result$mask_height),
      tile_overlap = as.integer(mask_tiles$overlap %||% 1L),
      visible = TRUE,
      opacity = 0.78,
      colour = "#ffffff",
      metadata = list(
        kind = "mask",
        legend = legend,
        selected_values = vapply(legend, function(x) as.character(x$value), character(1)),
        extent = list(
          x = 0,
          y = 0,
          width = as.numeric(slide$dimensions[["width"]]),
          height = as.numeric(slide$dimensions[["height"]])
        ),
        mask_downsample = mask_downsample,
        source_geojson = geojson_file,
        source_mask = mask_result$output,
        legend_csv = mask_result$legend
      )
    ))
  }
}

html_output <- file.path(output_root, "visiumhd_live_viewer.html")
tile_dir <- existing_output_dir(".*tissue.*tiles$|.*tissue.*deepzoom$") %||%
  file.path(output_root, "tissue_deepzoom")

msg("Opening live tiled viewer. First run may spend time creating full-resolution Deep Zoom tiles.")
msg("Subsequent runs reuse: %s", tile_dir)

viewer <- wsi_viewer_seurat(
  seurat = seurat_obj,
  image = slide,
  spatial_dir = spatial_dir,
  coordinate_scale = "auto",
  reduction = Sys.getenv("WSITOOLS_VISIUMHD_REDUCTION", unset = "pca"),
  dims = c(1, 2),
  max_points = as.integer(Sys.getenv("WSITOOLS_VISIUMHD_MAX_POINTS", unset = "75000")),
  show_spots = FALSE,
  live = TRUE,
  dynamic_tiles = FALSE,
  mode = "tiles",
  tile_dir = tile_dir,
  tile_size = 512,
  tile_format = "jpg",
  quality = 90,
  rebuild = FALSE,
  tile_overlap = 1,
  channel_sources = channel_sources,
  title = sprintf("wsiTools VisiumHD viewer: %s", basename(search_root)),
  output = html_output,
  open = truthy("WSITOOLS_VISIUMHD_OPEN", default = TRUE),
  wait = truthy("WSITOOLS_VISIUMHD_WAIT", default = TRUE),
  transport = "auto",
  overwrite = TRUE
)

msg("\nViewer object created.")
msg("HTML: %s", html_output)
msg("If Firefox did not open automatically, copy the http://127.0.0.1:<port> URL printed above from R.")
msg("The tissue is shown using full-resolution tiles. Cell annotations are shown as a tiled mask layer with a mask legend.")

invisible(viewer)
