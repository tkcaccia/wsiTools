#' Link a Seurat spatial object to a high-resolution image
#'
#' `wsi_link_seurat_image()` extracts spatial spot coordinates and a dimensional
#' reduction, usually PCA, from a Seurat object and maps the spots onto a
#' high-resolution image. Seurat remains an optional dependency: when
#' `SeuratObject` or `Seurat` is installed their accessors are used; otherwise
#' the function falls back to Seurat-like object slots/lists.
#'
#' The linked object can be passed to [wsi_viewer_seurat()] to open the tissue
#' image with a spot overlay and an interactive PCA scatter plot. The image is
#' opened through the usual wsiTools backends and is not loaded fully into R
#' memory by default.
#'
#' @param seurat A Seurat object, or a Seurat-like object with spatial
#'   coordinates and reductions.
#' @param image Path to the high-resolution image, or a `wsi_slide` object.
#' @param image_name Optional Seurat spatial image name. When `NULL`, the first
#'   available spatial image is used.
#' @param spatial_dir Optional 10x Genomics `spatial/` directory. When supplied,
#'   wsiTools looks for `scalefactors_json.json` and `tissue_positions.csv` or
#'   `tissue_positions_list.csv` and uses full-resolution Space Ranger
#'   coordinates when available.
#' @param scalefactors_json Optional path to a 10x Space Ranger
#'   `scalefactors_json.json` file. This is useful when the Seurat object stores
#'   only downsampled image coordinates.
#' @param tissue_positions Optional path to a 10x Space Ranger tissue positions
#'   CSV file. These files contain full-resolution pixel coordinates and are the
#'   preferred source for alignment to an external high-resolution image.
#' @param reduction Dimensional reduction to extract, for example `"pca"`.
#' @param dims Two reduction dimensions to plot.
#' @param coordinate_scale How to map Seurat image coordinates onto `image`.
#'   `"auto"` rescales from the stored Seurat image dimensions when coordinates
#'   appear to be in that preview space. `"none"` uses coordinates as supplied.
#'   `"fullres"` treats coordinates as 10x full-resolution pixels and only
#'   applies a final scale if the external image dimensions differ from the 10x
#'   inferred full-resolution dimensions. `"hires"` and `"lowres"` convert from
#'   10x hires/lowres preview pixels back to full-resolution pixels.
#'   `"seurat_image"` always rescales from the stored Seurat image dimensions.
#'   `"custom"` uses `scale_x` and `scale_y`.
#' @param scale_x,scale_y Custom coordinate scale factors used when
#'   `coordinate_scale = "custom"`.
#' @param coordinate_flip Orientation flip applied after coordinate scaling and
#'   before rotation. Choices are `"none"`, `"vertical"` (`y1 = -y`, displayed
#'   as `image_height - y`) and `"horizontal"` (`x1 = -x`, displayed as
#'   `image_width - x`). The misspelling `"verticcal"` is accepted as an alias.
#' @param coordinate_rotation Clockwise rotation applied after
#'   `coordinate_flip`. Choices are `0`, `90`, `180`, and `270` degrees.
#' @param coordinate_transform Legacy one-step orientation transform kept for
#'   backwards compatibility. Prefer `coordinate_flip` and
#'   `coordinate_rotation` for new code. For example, the old
#'   `"x_neg_y_y_neg_x"` transform is equivalent to
#'   `coordinate_flip = "horizontal"` and `coordinate_rotation = 90`.
#' @param spot_genes Optional character vector of gene names to extract and
#'   send to the viewer for spot colouring. Only these genes are embedded in the
#'   viewer payload; the full expression matrix is never copied into the
#'   browser.
#' @param default_gene Optional gene name used as the initial spot colour
#'   variable. The gene is automatically added to `spot_genes` when needed.
#' @param spot_radius Spot marker radius, in slide pixels. When `NULL`, an
#'   estimate is taken from Seurat scale factors when available.
#' @param max_points Maximum number of spots to keep in the browser payload.
#'   This protects interactive HTML viewers from very large objects.
#' @param colour_by Spot colour mode. `"component_1"` colours by the first
#'   plotted reduction component; `"gene"` colours by `default_gene`; `"none"`
#'   uses one colour.
#'
#' @return A `wsi_seurat_spatial` object containing the slide, spot table, PCA
#'   points, and coordinate mapping metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' library(Seurat)
#' library(SeuratData)
#' brain <- LoadData("stxBrain", type = "anterior1")
#' brain <- SCTransform(brain, assay = "Spatial", verbose = FALSE)
#' brain <- RunPCA(brain, assay = "SCT", verbose = FALSE)
#'
#' linked <- wsi_link_seurat_image(
#'   brain,
#'   "/Users/stefano/Downloads/V1_Mouse_Brain_Sagittal_Anterior_image.tif"
#' )
#' }
wsi_link_seurat_image <- function(seurat, image, image_name = NULL,
                                  spatial_dir = NULL, scalefactors_json = NULL,
                                  tissue_positions = NULL,
                                  reduction = "pca", dims = c(1L, 2L),
                                  coordinate_scale = c("auto", "none", "fullres", "hires", "lowres", "seurat_image", "custom"),
                                  scale_x = NULL, scale_y = NULL,
                                  coordinate_flip = c("none", "vertical", "horizontal"),
                                  coordinate_rotation = c(0, 90, 180, 270),
                                  coordinate_transform = "none",
                                  spot_genes = NULL, default_gene = NULL,
                                  spot_radius = NULL, max_points = 100000L,
                                  colour_by = c("component_1", "gene", "none")) {
  coordinate_flip_missing <- missing(coordinate_flip)
  coordinate_rotation_missing <- missing(coordinate_rotation)
  coordinate_transform_missing <- missing(coordinate_transform)
  colour_by_missing <- missing(colour_by)
  coordinate_scale <- match.arg(coordinate_scale)
  coordinate_transform <- wsi_seurat_coordinate_transform_arg(coordinate_transform)
  coordinate_flip <- wsi_seurat_coordinate_flip_arg(coordinate_flip)
  coordinate_rotation <- wsi_seurat_coordinate_rotation_arg(coordinate_rotation)
  if (!coordinate_transform_missing && !identical(coordinate_transform, "none")) {
    if ((!coordinate_flip_missing && !identical(coordinate_flip, "none")) ||
        (!coordinate_rotation_missing && !identical(coordinate_rotation, 0L))) {
      wsi_abort("Use either legacy `coordinate_transform` or the new `coordinate_flip`/`coordinate_rotation` pair, not both.")
    }
    preset <- wsi_seurat_coordinate_transform_preset(coordinate_transform)
    coordinate_flip <- preset$flip
    coordinate_rotation <- preset$rotation
  }
  spot_genes <- wsi_seurat_gene_vector(spot_genes, "spot_genes")
  default_gene <- wsi_seurat_default_gene_arg(default_gene)
  if (!is.null(default_gene) && !default_gene %in% spot_genes) {
    spot_genes <- c(default_gene, spot_genes)
  }
  if (!is.null(default_gene) && isTRUE(colour_by_missing)) {
    colour_by <- "gene"
  }
  colour_by <- match.arg(colour_by)
  if (identical(colour_by, "gene") && !length(spot_genes)) {
    wsi_abort("`colour_by = \"gene\"` requires `default_gene` or at least one value in `spot_genes`.")
  }
  reduction <- wsi_seurat_check_scalar_character(reduction, "reduction")
  dims <- as.integer(dims)
  if (length(dims) != 2L || anyNA(dims) || any(dims < 1L)) {
    wsi_abort("`dims` must contain two positive reduction dimensions.")
  }
  max_points <- as.integer(wsi_check_scalar_number(max_points, "max_points", allow_zero = FALSE))

  slide <- if (inherits(image, "wsi_slide")) {
    image
  } else {
    wsi_open(image)
  }

  spatial <- wsi_seurat_spatial_files(
    spatial_dir = spatial_dir,
    scalefactors_json = scalefactors_json,
    tissue_positions = tissue_positions
  )
  image_name <- wsi_seurat_image_name(seurat, image_name)
  image_obj <- wsi_seurat_image_object(seurat, image_name)
  scale_factors <- wsi_seurat_scale_factors(image_obj, scalefactors_json = spatial$scalefactors_json)
  coordinates <- wsi_seurat_coordinate_table(
    seurat,
    image_name = image_name,
    image_obj = image_obj,
    tissue_positions = spatial$tissue_positions
  )
  embeddings <- wsi_seurat_embeddings(seurat, reduction = reduction)
  if (ncol(embeddings) < max(dims)) {
    wsi_abort(sprintf(
      "Reduction `%s` has %d dimension%s, but `dims` requests dimension %d.",
      reduction,
      ncol(embeddings),
      if (ncol(embeddings) == 1L) "" else "s",
      max(dims)
    ))
  }

  matched <- wsi_seurat_match_spots(coordinates, embeddings)
  coordinates <- matched$coordinates
  embeddings <- matched$embeddings
  if (!nrow(coordinates)) {
    wsi_abort("No shared spot/barcode identifiers were found between Seurat coordinates and the reduction.")
  }
  gene_expression <- wsi_seurat_gene_expression(
    seurat,
    genes = spot_genes,
    spot_ids = coordinates$barcode,
    default_gene = default_gene
  )

  mapping <- wsi_seurat_coordinate_mapping(
    coordinates = coordinates,
    image_obj = image_obj,
    slide = slide,
    scale_factors = scale_factors,
    coordinate_scale = coordinate_scale,
    scale_x = scale_x,
    scale_y = scale_y
  )
  coordinates$x <- coordinates$x * mapping$scale_x
  coordinates$y <- coordinates$y * mapping$scale_y
  transformed <- wsi_seurat_apply_coordinate_transform(
    x = coordinates$x,
    y = coordinates$y,
    width = as.numeric(slide$dimensions[["width"]]),
    height = as.numeric(slide$dimensions[["height"]]),
    transform = coordinate_transform,
    flip = coordinate_flip,
    rotation = coordinate_rotation
  )
  coordinates$x <- transformed$x
  coordinates$y <- transformed$y
  mapping$coordinate_transform <- transformed$transform
  mapping$coordinate_flip <- transformed$flip
  mapping$coordinate_rotation <- transformed$rotation
  mapping$transform_width <- transformed$width
  mapping$transform_height <- transformed$height
  mapping$transform_rescale_x <- transformed$rescale_x
  mapping$transform_rescale_y <- transformed$rescale_y

  component_values <- embeddings[, dims, drop = FALSE]
  component_names <- colnames(embeddings)[dims]
  if (is.null(component_names) || any(!nzchar(component_names))) {
    component_names <- paste0(toupper(reduction), "_", dims)
  }
  component_colours <- wsi_seurat_gradient(component_values[, 1L])
  base_colours <- if (identical(colour_by, "none")) {
    rep("#2B6CB0", nrow(component_values))
  } else {
    component_colours
  }
  colours <- if (identical(colour_by, "gene")) {
    wsi_seurat_gene_colours(gene_expression, default_gene)
  } else if (identical(colour_by, "component_1")) {
    component_colours
  } else {
    rep("#2B6CB0", nrow(component_values))
  }
  ids <- coordinates$barcode

  spots <- data.frame(
    id = ids,
    label = ids,
    barcode = ids,
    x = coordinates$x,
    y = coordinates$y,
    component_1 = component_values[, 1L],
    component_2 = component_values[, 2L],
    colour = colours,
    color = colours,
    base_colour = base_colours,
    base_color = base_colours,
    class = reduction,
    stringsAsFactors = FALSE
  )
  names(spots)[names(spots) == "component_1"] <- component_names[[1L]]
  names(spots)[names(spots) == "component_2"] <- component_names[[2L]]

  total <- nrow(spots)
  if (total > max_points) {
    idx <- unique(round(seq(1, total, length.out = max_points)))
    spots <- spots[idx, , drop = FALSE]
    component_values <- component_values[idx, , drop = FALSE]
    colours <- colours[idx]
    base_colours <- base_colours[idx]
    gene_expression <- wsi_seurat_subset_gene_expression(gene_expression, idx)
  }
  gene_value_items <- wsi_seurat_gene_value_items(gene_expression)

  plot_points <- data.frame(
    label = spots$label,
    spot_id = spots$id,
    x = component_values[, 1L],
    y = component_values[, 2L],
    slide_x = spots$x,
    slide_y = spots$y,
    colour = spots$colour,
    color = spots$colour,
    base_colour = spots$base_colour,
    base_color = spots$base_colour,
    stringsAsFactors = FALSE
  )
  if (length(gene_value_items)) {
    plot_points$gene_values <- I(gene_value_items)
  }

  if (is.null(spot_radius)) {
    spot_radius <- wsi_seurat_spot_radius(scale_factors = scale_factors, mapping = mapping)
  }
  spot_radius <- as.numeric(wsi_check_scalar_number(spot_radius, "spot_radius", allow_zero = FALSE))

  out <- list(
    slide = slide,
    image_path = slide$path,
    image_name = image_name,
    reduction = reduction,
    dims = dims,
    component_names = component_names,
    coordinate_mapping = mapping,
    gene_expression = gene_expression,
    spot_radius = spot_radius,
    spot_count = total,
    displayed_spot_count = nrow(spots),
    spots = spots,
    pca = list(
      id = paste0("seurat_", wsi_safe_id(reduction, "reduction")),
      label = paste0("Seurat ", toupper(reduction), " plot"),
      reduction = reduction,
      x_label = component_names[[1L]],
      y_label = component_names[[2L]],
      point_count = total,
      points = plot_points
    )
  )
  class(out) <- c("wsi_seurat_spatial", "list")
  out
}

#' Open a Seurat spatial object in the wsiTools viewer
#'
#' `wsi_viewer_seurat()` opens a high-resolution tissue image and overlays
#' Seurat spatial spots. The top **Seurat** menu opens the PCA scatter plot and
#' can show/hide or zoom to the spot overlay.
#'
#' @param seurat A Seurat object.
#' @param image Path to the high-resolution image, or a `wsi_slide` object.
#' @param linked Optional precomputed object from [wsi_link_seurat_image()].
#' @param live Use [wsi_viewer_live()] instead of static [wsi_viewer()]. Live
#'   mode keeps R and the browser synchronized while the session is active.
#' @param dynamic_tiles When `live = TRUE`, serve the image as dynamic tiles.
#' @param mode Viewer mode for static output. `"tiles"` gives full-resolution
#'   Deep Zoom viewing when the backend can create tiles.
#' @param output,open,overwrite Additional viewer options.
#' @param ... Arguments passed to [wsi_link_seurat_image()] and then to
#'   [wsi_viewer()] or [wsi_viewer_live()]. Viewer arguments take precedence
#'   after the linked object is created.
#'
#' @return The HTML path for static mode, or a `wsi_viewer_session` for live
#'   mode.
#' @export
#'
#' @examples
#' \dontrun{
#' viewer <- wsi_viewer_seurat(
#'   brain,
#'   "/Users/stefano/Downloads/V1_Mouse_Brain_Sagittal_Anterior_image.tif",
#'   mode = "tiles"
#' )
#' }
wsi_viewer_seurat <- function(seurat, image, linked = NULL,
                              live = FALSE, dynamic_tiles = live,
                              mode = c("tiles", "thumbnail"),
                              output = NULL, open = interactive(),
                              overwrite = FALSE, ...) {
  dots <- list(...)
  if (!is.logical(live) || length(live) != 1L || is.na(live)) {
    wsi_abort("`live` must be `TRUE` or `FALSE`.")
  }
  mode <- match.arg(mode)
  if (is.null(linked)) {
    link_args <- dots[names(dots) %in% names(formals(wsi_link_seurat_image))]
    dots <- dots[setdiff(names(dots), names(link_args))]
    linked <- do.call(
      wsi_link_seurat_image,
      c(list(seurat = seurat, image = image), link_args)
    )
  } else if (!inherits(linked, "wsi_seurat_spatial")) {
    wsi_abort("`linked` must be an object returned by `wsi_link_seurat_image()`.")
  }

  spot_layer <- wsi_seurat_spots_layer(linked)
  layers <- dots$layers %||% list()
  if (!is.list(layers) || inherits(layers, "data.frame")) {
    layers <- list(layers)
  }
  dots$layers <- c(list(spot_layer), layers)
  dots$seurat <- linked
  dots$output <- output
  dots$open <- open
  dots$overwrite <- overwrite
  dots$title <- dots$title %||% sprintf("wsiTools Seurat viewer: %s", basename(linked$image_path %||% "image"))

  if (isTRUE(live)) {
    dots$dynamic_tiles <- dynamic_tiles
    dots$slide <- linked$slide
    return(do.call(wsi_viewer_live, dots))
  }

  dots$slide <- linked$slide
  dots$mode <- mode
  do.call(wsi_viewer, dots)
}

#' @export
print.wsi_seurat_spatial <- function(x, ...) {
  cat("<wsi_seurat_spatial>\n")
  cat("  image:     ", x$image_path %||% "<slide>", "\n", sep = "")
  cat("  image key: ", x$image_name %||% NA_character_, "\n", sep = "")
  cat("  reduction: ", x$reduction, " dims ", paste(x$dims, collapse = ","), "\n", sep = "")
  cat("  spots:     ", format(x$spot_count, big.mark = ","), "\n", sep = "")
  cat("  displayed: ", format(x$displayed_spot_count, big.mark = ","), "\n", sep = "")
  cat("  mapping:   x*", signif(x$coordinate_mapping$scale_x, 5), " y*", signif(x$coordinate_mapping$scale_y, 5),
      " (", x$coordinate_mapping$method, ")\n", sep = "")
  transform <- x$coordinate_mapping$coordinate_transform %||% "none"
  if (!identical(transform, "none")) {
    cat("  transform: ", transform, "\n", sep = "")
  }
  flip <- x$coordinate_mapping$coordinate_flip %||% "none"
  rotation <- x$coordinate_mapping$coordinate_rotation %||% 0L
  if (!identical(flip, "none") || !identical(as.integer(rotation), 0L)) {
    cat("  orientation: flip=", flip, " rotation=", as.integer(rotation), "\n", sep = "")
  }
  invisible(x)
}

wsi_seurat_spots_layer <- function(linked, visible = TRUE, opacity = 0.85) {
  layer <- wsi_viewer_layer_payload(
    name = "Seurat spatial spots",
    data = linked$spots,
    type = "points",
    visible = visible,
    opacity = opacity,
    colour = "#2B6CB0",
    radius = linked$spot_radius
  )
  gene_values <- wsi_seurat_gene_value_items(linked$gene_expression)
  if (length(gene_values) && length(layer$items)) {
    n <- min(length(gene_values), length(layer$items))
    for (i in seq_len(n)) {
      layer$items[[i]]$gene_values <- gene_values[[i]]
      layer$items[[i]]$base_colour <- linked$spots$base_colour[[i]] %||% layer$items[[i]]$colour
      layer$items[[i]]$base_color <- layer$items[[i]]$base_colour
    }
  } else if (length(layer$items) && "base_colour" %in% names(linked$spots)) {
    n <- min(nrow(linked$spots), length(layer$items))
    for (i in seq_len(n)) {
      layer$items[[i]]$base_colour <- linked$spots$base_colour[[i]] %||% layer$items[[i]]$colour
      layer$items[[i]]$base_color <- layer$items[[i]]$base_colour
    }
  }
  layer$id <- "seurat_spots"
  layer$name <- "Seurat spatial spots"
  layer$source_type <- "seurat_spots"
  layer$metadata <- list(
    reduction = linked$reduction,
    image_name = linked$image_name,
    coordinate_mapping = linked$coordinate_mapping
  )
  layer
}

wsi_viewer_seurat_config <- function(seurat = NULL) {
  if (is.null(seurat)) {
    return(list(enabled = FALSE, plots = list(), spot_count = 0L))
  }
  if (!inherits(seurat, "wsi_seurat_spatial")) {
    wsi_abort("`seurat` viewer configuration must be created by `wsi_link_seurat_image()`.")
  }
  list(
    enabled = TRUE,
    image_name = seurat$image_name,
    reduction = seurat$reduction,
    dims = as.integer(seurat$dims),
    component_names = as.character(seurat$component_names),
    spot_layer_id = "seurat_spots",
    spot_count = as.integer(seurat$spot_count),
    displayed_spot_count = as.integer(seurat$displayed_spot_count),
    spot_radius = as.numeric(seurat$spot_radius),
    gene_expression = wsi_seurat_gene_expression_config(seurat$gene_expression),
    plots = list(seurat$pca)
  )
}

wsi_seurat_slot <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  if (isS4(x) && name %in% methods::slotNames(x)) {
    return(methods::slot(x, name))
  }
  if (is.list(x) && name %in% names(x)) {
    return(x[[name]])
  }
  NULL
}

wsi_seurat_check_scalar_character <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    wsi_abort(sprintf("`%s` must be a single non-empty string.", name))
  }
  x
}

wsi_seurat_try_accessor <- function(namespace, fun, ...) {
  if (!requireNamespace(namespace, quietly = TRUE)) {
    return(NULL)
  }
  f <- getExportedValue(namespace, fun)
  tryCatch(f(...), error = function(e) NULL)
}

wsi_seurat_check_optional_path <- function(path, name) {
  if (is.null(path)) {
    return(NULL)
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    wsi_abort(sprintf("`%s` must be `NULL` or a single non-empty path.", name))
  }
  path
}

wsi_seurat_resolve_spatial_file <- function(path = NULL, spatial_dir = NULL, patterns = character(), name = "file") {
  path <- wsi_seurat_check_optional_path(path, name)
  spatial_dir <- wsi_seurat_check_optional_path(spatial_dir, "spatial_dir")
  if (!is.null(path) && file.exists(path)) {
    return(normalizePath(path, mustWork = TRUE))
  }
  search_dirs <- character()
  if (!is.null(path)) {
    search_dirs <- c(search_dirs, dirname(path))
  }
  if (!is.null(spatial_dir)) {
    search_dirs <- c(search_dirs, spatial_dir)
  }
  search_dirs <- unique(search_dirs[file.exists(search_dirs) & dir.exists(search_dirs)])
  for (dir in search_dirs) {
    files <- list.files(dir, full.names = TRUE)
    if (!length(files)) {
      next
    }
    base <- basename(files)
    for (pattern in patterns) {
      hit <- files[grepl(pattern, base, ignore.case = TRUE)]
      if (length(hit)) {
        return(normalizePath(hit[[1L]], mustWork = TRUE))
      }
    }
  }
  if (!is.null(path)) {
    wsi_abort(sprintf("Could not find `%s`: %s", name, path))
  }
  NULL
}

wsi_seurat_spatial_files <- function(spatial_dir = NULL, scalefactors_json = NULL, tissue_positions = NULL) {
  spatial_dir <- wsi_seurat_check_optional_path(spatial_dir, "spatial_dir")
  if (!is.null(spatial_dir) && (!file.exists(spatial_dir) || !dir.exists(spatial_dir))) {
    wsi_abort(sprintf("`spatial_dir` does not exist or is not a directory: %s", spatial_dir))
  }
  scalefactors_json <- wsi_seurat_resolve_spatial_file(
    scalefactors_json,
    spatial_dir = spatial_dir,
    patterns = c("scalefactors_json\\.json$"),
    name = "scalefactors_json"
  )
  tissue_positions <- wsi_seurat_resolve_spatial_file(
    tissue_positions,
    spatial_dir = spatial_dir,
    patterns = c("^tissue_positions\\.csv$", "^tissue_positions_list\\.csv$"),
    name = "tissue_positions"
  )
  list(
    spatial_dir = if (!is.null(spatial_dir)) normalizePath(spatial_dir, mustWork = TRUE) else NULL,
    scalefactors_json = scalefactors_json,
    tissue_positions = tissue_positions
  )
}

wsi_seurat_images <- function(seurat) {
  out <- wsi_seurat_try_accessor("SeuratObject", "Images", object = seurat) %||%
    wsi_seurat_try_accessor("Seurat", "Images", object = seurat)
  if (!is.null(out)) {
    return(as.character(out))
  }
  images <- wsi_seurat_slot(seurat, "images")
  names(images %||% list())
}

wsi_seurat_image_name <- function(seurat, image_name = NULL) {
  if (!is.null(image_name)) {
    return(wsi_seurat_check_scalar_character(image_name, "image_name"))
  }
  images <- wsi_seurat_images(seurat)
  if (!length(images)) {
    wsi_abort("No spatial images were found in the Seurat object. Supply `image_name` or a Seurat object with an image slot.")
  }
  images[[1L]]
}

wsi_seurat_image_object <- function(seurat, image_name) {
  images <- wsi_seurat_slot(seurat, "images")
  if (is.list(images) && image_name %in% names(images)) {
    return(images[[image_name]])
  }
  NULL
}

wsi_seurat_embeddings <- function(seurat, reduction = "pca") {
  emb <- wsi_seurat_try_accessor("SeuratObject", "Embeddings", object = seurat, reduction = reduction) %||%
    wsi_seurat_try_accessor("Seurat", "Embeddings", object = seurat, reduction = reduction)
  if (is.null(emb)) {
    reductions <- wsi_seurat_slot(seurat, "reductions")
    reduction_obj <- if (is.list(reductions) && reduction %in% names(reductions)) reductions[[reduction]] else NULL
    emb <- wsi_seurat_slot(reduction_obj, "cell.embeddings")
  }
  if (is.null(emb) || (!is.matrix(emb) && !is.data.frame(emb))) {
    wsi_abort(sprintf("Could not extract `%s` embeddings from the Seurat object.", reduction))
  }
  emb <- as.matrix(emb)
  storage.mode(emb) <- "double"
  if (!nrow(emb) || !ncol(emb)) {
    wsi_abort(sprintf("Reduction `%s` does not contain embeddings.", reduction))
  }
  if (is.null(rownames(emb))) {
    rownames(emb) <- as.character(seq_len(nrow(emb)))
  }
  emb
}

wsi_seurat_coordinates_from_accessor <- function(seurat, image_name) {
  out <- wsi_seurat_try_accessor("SeuratObject", "GetTissueCoordinates", object = seurat, image = image_name) %||%
    wsi_seurat_try_accessor("Seurat", "GetTissueCoordinates", object = seurat, image = image_name)
  if (!is.null(out)) {
    return(out)
  }
  wsi_seurat_try_accessor("SeuratObject", "GetTissueCoordinates", object = seurat) %||%
    wsi_seurat_try_accessor("Seurat", "GetTissueCoordinates", object = seurat)
}

wsi_seurat_read_tissue_positions <- function(path) {
  if (is.null(path)) {
    return(NULL)
  }
  first <- readLines(path, n = 1L, warn = FALSE)
  if (!length(first)) {
    wsi_abort(sprintf("Tissue positions file is empty: %s", path))
  }
  fields <- strsplit(first, ",", fixed = TRUE)[[1L]]
  has_header <- any(grepl("barcode|pxl|in_tissue|array", fields, ignore.case = TRUE))
  coords <- tryCatch(
    utils::read.csv(path, header = has_header, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(err) {
      wsi_abort(sprintf("Could not read tissue positions file `%s`: %s", path, conditionMessage(err)))
    }
  )
  if (!has_header) {
    if (ncol(coords) < 6L) {
      wsi_abort("10x tissue positions files without a header must contain at least 6 columns.")
    }
    names(coords)[seq_len(6L)] <- c(
      "barcode", "in_tissue", "array_row", "array_col",
      "pxl_row_in_fullres", "pxl_col_in_fullres"
    )
  }
  barcode_col <- wsi_seurat_first_column(coords, c("barcode", "barcodes", "cell", "cells"))
  x_col <- wsi_seurat_first_column(coords, c("pxl_col_in_fullres", "imagecol", "col", "x", "X"))
  y_col <- wsi_seurat_first_column(coords, c("pxl_row_in_fullres", "imagerow", "row", "y", "Y"))
  if (is.null(barcode_col) || is.null(x_col) || is.null(y_col)) {
    wsi_abort("Tissue positions must contain barcode plus full-resolution row/column coordinate columns.")
  }
  out <- data.frame(
    barcode = as.character(coords[[barcode_col]]),
    x = suppressWarnings(as.numeric(coords[[x_col]])),
    y = suppressWarnings(as.numeric(coords[[y_col]])),
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$x) & is.finite(out$y) & nzchar(out$barcode), , drop = FALSE]
  row.names(out) <- NULL
  attr(out, "coordinate_space") <- if (all(c("pxl_col_in_fullres", "pxl_row_in_fullres") %in% names(coords))) "fullres" else "unknown"
  attr(out, "coordinate_source") <- path
  attr(out, "x_column") <- x_col
  attr(out, "y_column") <- y_col
  out
}

wsi_seurat_coordinate_table <- function(seurat, image_name, image_obj = NULL, tissue_positions = NULL) {
  coords <- wsi_seurat_read_tissue_positions(tissue_positions)
  if (is.null(coords)) {
    coords <- wsi_seurat_coordinates_from_accessor(seurat, image_name)
  }
  if (is.null(coords)) {
    coords <- wsi_seurat_slot(image_obj, "coordinates")
  }
  if (is.null(coords) || !is.data.frame(coords)) {
    wsi_abort("Could not extract spatial coordinates from the Seurat object.")
  }
  coords <- as.data.frame(coords, stringsAsFactors = FALSE)
  barcode <- NULL
  for (candidate in c("barcode", "barcodes", "cell", "cells", "spot", "spot_id")) {
    if (candidate %in% names(coords)) {
      barcode <- as.character(coords[[candidate]])
      break
    }
  }
  if (is.null(barcode)) {
    barcode <- rownames(coords) %||% as.character(seq_len(nrow(coords)))
  }
  x_col <- wsi_seurat_first_column(coords, c("pxl_col_in_fullres", "imagecol", "col", "x", "X"))
  y_col <- wsi_seurat_first_column(coords, c("pxl_row_in_fullres", "imagerow", "row", "y", "Y"))
  if (is.null(x_col) || is.null(y_col)) {
    wsi_abort("Spatial coordinates must contain full-resolution, imagecol/imagerow, x/y, or col/row columns.")
  }
  out <- data.frame(
    barcode = barcode,
    x = suppressWarnings(as.numeric(coords[[x_col]])),
    y = suppressWarnings(as.numeric(coords[[y_col]])),
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$x) & is.finite(out$y) & nzchar(out$barcode), , drop = FALSE]
  row.names(out) <- NULL
  source_space <- if (identical(x_col, "pxl_col_in_fullres") && identical(y_col, "pxl_row_in_fullres")) {
    "fullres"
  } else {
    "unknown"
  }
  attr(out, "coordinate_space") <- attr(coords, "coordinate_space", exact = TRUE) %||% source_space
  attr(out, "coordinate_source") <- attr(coords, "coordinate_source", exact = TRUE) %||% "seurat"
  attr(out, "x_column") <- x_col
  attr(out, "y_column") <- y_col
  out
}

wsi_seurat_first_column <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit)) hit[[1L]] else NULL
}

wsi_seurat_match_spots <- function(coordinates, embeddings) {
  coord_ids <- as.character(coordinates$barcode)
  emb_ids <- rownames(embeddings) %||% as.character(seq_len(nrow(embeddings)))
  idx <- match(coord_ids, emb_ids)
  keep <- !is.na(idx)
  if (!any(keep) && nrow(coordinates) == nrow(embeddings)) {
    coordinates$barcode <- emb_ids
    return(list(coordinates = coordinates, embeddings = embeddings))
  }
  coordinates <- coordinates[keep, , drop = FALSE]
  embeddings <- embeddings[idx[keep], , drop = FALSE]
  row.names(coordinates) <- NULL
  list(coordinates = coordinates, embeddings = embeddings)
}

wsi_seurat_gene_vector <- function(x, name = "spot_genes") {
  if (is.null(x)) {
    return(character())
  }
  if (!is.character(x)) {
    wsi_abort(sprintf("`%s` must be `NULL` or a character vector of gene names.", name))
  }
  x <- unique(trimws(x))
  x <- x[nzchar(x) & !is.na(x)]
  if (!length(x)) {
    return(character())
  }
  x
}

wsi_seurat_default_gene_arg <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    wsi_abort("`default_gene` must be `NULL` or a single non-empty gene name.")
  }
  trimws(x)
}

wsi_seurat_gene_match <- function(requested, available) {
  if (!length(requested) || !length(available)) {
    return(integer())
  }
  idx <- match(requested, available)
  missing <- is.na(idx)
  if (any(missing)) {
    lower_idx <- match(tolower(requested[missing]), tolower(available))
    idx[missing] <- lower_idx
  }
  idx
}

wsi_seurat_fetch_gene_expression <- function(seurat, genes, spot_ids) {
  for (namespace in c("SeuratObject", "Seurat")) {
    fetched <- wsi_seurat_try_accessor(namespace, "FetchData", object = seurat, vars = genes, cells = spot_ids)
    if (is.null(fetched) || !is.data.frame(fetched) || !nrow(fetched)) {
      next
    }
    gene_idx <- wsi_seurat_gene_match(genes, names(fetched))
    keep <- !is.na(gene_idx)
    if (!any(keep)) {
      next
    }
    values <- as.data.frame(fetched[, gene_idx[keep], drop = FALSE], stringsAsFactors = FALSE)
    names(values) <- names(fetched)[gene_idx[keep]]
    row_ids <- rownames(values) %||% character()
    row_idx <- match(spot_ids, row_ids)
    if (all(!is.na(row_idx))) {
      values <- values[row_idx, , drop = FALSE]
    } else if (nrow(values) != length(spot_ids)) {
      next
    }
    mat <- as.matrix(values)
    storage.mode(mat) <- "double"
    rownames(mat) <- spot_ids
    return(mat)
  }
  NULL
}

wsi_seurat_matrix_like <- function(x) {
  dims <- tryCatch(dim(x), error = function(e) NULL)
  length(dims) == 2L && all(dims > 0)
}

wsi_seurat_collect_expression_matrices <- function(seurat) {
  matrices <- list()
  add_matrix <- function(x, name) {
    if (!wsi_seurat_matrix_like(x)) {
      return(invisible(NULL))
    }
    rn <- tryCatch(rownames(x), error = function(e) NULL)
    cn <- tryCatch(colnames(x), error = function(e) NULL)
    if (!length(rn) || !length(cn)) {
      return(invisible(NULL))
    }
    matrices[[length(matrices) + 1L]] <<- list(name = name, matrix = x)
    invisible(NULL)
  }
  visit_container <- function(x, prefix = "object") {
    if (is.null(x)) {
      return(invisible(NULL))
    }
    if (wsi_seurat_matrix_like(x)) {
      add_matrix(x, prefix)
      return(invisible(NULL))
    }
    for (slot_name in c("data", "counts", "scale.data", "scale_data", "expression", "exprs")) {
      add_matrix(wsi_seurat_slot(x, slot_name), paste(prefix, slot_name, sep = "$"))
    }
    if (is.list(x)) {
      for (nm in names(x)) {
        if (nm %in% c("reductions", "images", "meta.data", "meta_data", "coordinates")) {
          next
        }
        value <- x[[nm]]
        if (wsi_seurat_matrix_like(value)) {
          add_matrix(value, paste(prefix, nm, sep = "$"))
        } else if (is.list(value) || isS4(value)) {
          for (slot_name in c("data", "counts", "scale.data", "scale_data", "expression", "exprs")) {
            add_matrix(wsi_seurat_slot(value, slot_name), paste(prefix, nm, slot_name, sep = "$"))
          }
        }
      }
    }
    invisible(NULL)
  }
  assays <- wsi_seurat_slot(seurat, "assays")
  if (is.list(assays) && length(assays)) {
    for (assay_name in names(assays)) {
      visit_container(assays[[assay_name]], paste("assays", assay_name, sep = "$"))
    }
  }
  for (slot_name in c("data", "counts", "expression", "exprs")) {
    add_matrix(wsi_seurat_slot(seurat, slot_name), slot_name)
  }
  visit_container(seurat, "seurat")
  matrices
}

wsi_seurat_matrix_gene_expression <- function(seurat, genes, spot_ids) {
  matrices <- wsi_seurat_collect_expression_matrices(seurat)
  if (!length(matrices)) {
    return(NULL)
  }
  for (entry in matrices) {
    mat <- entry$matrix
    rn <- rownames(mat)
    cn <- colnames(mat)
    gene_idx <- wsi_seurat_gene_match(genes, rn)
    spot_idx <- match(spot_ids, cn)
    if (any(!is.na(gene_idx)) && any(!is.na(spot_idx))) {
      keep_gene <- !is.na(gene_idx)
      keep_spot <- !is.na(spot_idx)
      out <- matrix(NA_real_, nrow = length(spot_ids), ncol = sum(keep_gene))
      colnames(out) <- rn[gene_idx[keep_gene]]
      rownames(out) <- spot_ids
      sub <- as.matrix(mat[gene_idx[keep_gene], spot_idx[keep_spot], drop = FALSE])
      storage.mode(sub) <- "double"
      out[keep_spot, ] <- t(sub)
      return(out)
    }
    gene_idx <- wsi_seurat_gene_match(genes, cn)
    spot_idx <- match(spot_ids, rn)
    if (any(!is.na(gene_idx)) && any(!is.na(spot_idx))) {
      keep_gene <- !is.na(gene_idx)
      keep_spot <- !is.na(spot_idx)
      out <- matrix(NA_real_, nrow = length(spot_ids), ncol = sum(keep_gene))
      colnames(out) <- cn[gene_idx[keep_gene]]
      rownames(out) <- spot_ids
      sub <- as.matrix(mat[spot_idx[keep_spot], gene_idx[keep_gene], drop = FALSE])
      storage.mode(sub) <- "double"
      out[keep_spot, ] <- sub
      return(out)
    }
  }
  NULL
}

wsi_seurat_gene_expression <- function(seurat, genes, spot_ids, default_gene = NULL) {
  genes <- wsi_seurat_gene_vector(genes, "spot_genes")
  if (!length(genes)) {
    return(list(enabled = FALSE, genes = character(), default_gene = NULL, values = matrix(numeric(), nrow = length(spot_ids), ncol = 0L)))
  }
  spot_ids <- as.character(spot_ids)
  values <- wsi_seurat_fetch_gene_expression(seurat, genes, spot_ids) %||%
    wsi_seurat_matrix_gene_expression(seurat, genes, spot_ids)
  if (is.null(values) || !ncol(values)) {
    wsi_abort(sprintf(
      "None of the requested `spot_genes` were found in the Seurat expression data: %s",
      paste(genes, collapse = ", ")
    ))
  }
  requested_idx <- wsi_seurat_gene_match(genes, colnames(values))
  keep <- !is.na(requested_idx)
  missing_genes <- genes[!keep]
  values <- values[, requested_idx[keep], drop = FALSE]
  storage.mode(values) <- "double"
  found <- colnames(values)
  if (length(missing_genes)) {
    wsi_warn(sprintf(
      "Some requested `spot_genes` were not found and will be unavailable in the viewer: %s",
      paste(missing_genes, collapse = ", ")
    ))
  }
  if (!is.null(default_gene)) {
    default_idx <- wsi_seurat_gene_match(default_gene, found)
    if (is.na(default_idx)) {
      wsi_abort(sprintf("`default_gene` was not found in the extracted Seurat expression data: %s", default_gene))
    }
    default_gene <- found[[default_idx]]
  } else {
    default_gene <- found[[1L]]
  }
  list(
    enabled = TRUE,
    genes = found,
    default_gene = default_gene,
    values = values,
    ranges = wsi_seurat_gene_ranges(values)
  )
}

wsi_seurat_gene_ranges <- function(values) {
  if (is.null(values) || !ncol(values)) {
    return(list())
  }
  stats <- lapply(seq_len(ncol(values)), function(i) {
    x <- suppressWarnings(as.numeric(values[, i]))
    finite <- is.finite(x)
    list(
      min = if (any(finite)) min(x[finite]) else NA_real_,
      max = if (any(finite)) max(x[finite]) else NA_real_
    )
  })
  names(stats) <- colnames(values)
  stats
}

wsi_seurat_gene_colours <- function(gene_expression, gene = NULL) {
  if (!isTRUE(gene_expression$enabled) || is.null(gene_expression$values) || !ncol(gene_expression$values)) {
    wsi_abort("No Seurat gene expression values are available for spot colouring.")
  }
  genes <- gene_expression$genes
  gene <- gene %||% gene_expression$default_gene %||% genes[[1L]]
  idx <- wsi_seurat_gene_match(gene, genes)
  if (is.na(idx)) {
    wsi_abort(sprintf("Gene `%s` was not found in the extracted Seurat expression data.", gene))
  }
  values <- suppressWarnings(as.numeric(gene_expression$values[, idx]))
  wsi_seurat_gradient(values, low = "#dbeafe", mid = "#fef3c7", high = "#dc2626")
}

wsi_seurat_subset_gene_expression <- function(gene_expression, idx) {
  if (!isTRUE(gene_expression$enabled) || is.null(gene_expression$values) || !ncol(gene_expression$values)) {
    return(gene_expression)
  }
  gene_expression$values <- gene_expression$values[idx, , drop = FALSE]
  gene_expression
}

wsi_seurat_gene_value_items <- function(gene_expression) {
  if (!isTRUE(gene_expression$enabled) || is.null(gene_expression$values) || !ncol(gene_expression$values)) {
    return(list())
  }
  values <- gene_expression$values
  lapply(seq_len(nrow(values)), function(i) {
    row <- as.list(as.numeric(values[i, ]))
    names(row) <- colnames(values)
    row
  })
}

wsi_seurat_gene_expression_config <- function(gene_expression) {
  if (!isTRUE(gene_expression$enabled)) {
    return(list(enabled = FALSE, genes = character(), default_gene = NULL, ranges = list()))
  }
  list(
    enabled = TRUE,
    genes = as.character(gene_expression$genes %||% character()),
    default_gene = gene_expression$default_gene %||% NULL,
    ranges = gene_expression$ranges %||% list()
  )
}

wsi_seurat_coordinate_transform_arg <- function(transform) {
  if (!is.character(transform) || length(transform) != 1L || is.na(transform) || !nzchar(transform)) {
    wsi_abort("`coordinate_transform` must be a single non-empty string.")
  }
  transform <- tolower(gsub("[ -]+", "_", transform))
  aliases <- c(
    none = "none",
    identity = "none",
    flip_y = "flip_y",
    flip_vertical = "flip_y",
    vertical_flip = "flip_y",
    y_neg_y = "flip_y",
    x_y_y_neg_x = "x_y_y_neg_x",
    y_neg_x = "x_y_y_neg_x",
    rotate_90_cw = "x_y_y_neg_x",
    rot90cw = "x_y_y_neg_x",
    flip_y_rotate_90_cw = "x_y_y_neg_x",
    x_neg_y_y_neg_x = "x_neg_y_y_neg_x",
    neg_y_neg_x = "x_neg_y_y_neg_x",
    y_neg_x_x_neg_y = "x_neg_y_y_neg_x",
    anti_diagonal_flip = "x_neg_y_y_neg_x"
  )
  out <- aliases[[transform]]
  if (is.null(out)) {
    wsi_abort(paste0(
      "`coordinate_transform` must be one of: \"none\", \"flip_y\", ",
      "\"x_y_y_neg_x\", \"x_neg_y_y_neg_x\", \"rotate_90_cw\", ",
      "or \"flip_y_rotate_90_cw\"."
    ))
  }
  unname(out)
}

wsi_seurat_coordinate_transform_preset <- function(transform) {
  transform <- wsi_seurat_coordinate_transform_arg(transform)
  if (identical(transform, "none")) {
    return(list(flip = "none", rotation = 0L))
  }
  if (identical(transform, "flip_y")) {
    return(list(flip = "vertical", rotation = 0L))
  }
  if (identical(transform, "x_y_y_neg_x")) {
    return(list(flip = "none", rotation = 270L))
  }
  if (identical(transform, "x_neg_y_y_neg_x")) {
    return(list(flip = "horizontal", rotation = 90L))
  }
  wsi_abort(sprintf("Unsupported coordinate transform: %s", transform))
}

wsi_seurat_coordinate_flip_arg <- function(flip) {
  if (length(flip) > 1L) {
    defaults <- c("none", "vertical", "horizontal")
    if (identical(as.character(flip), defaults)) {
      flip <- flip[[1L]]
    }
  }
  if (!is.character(flip) || length(flip) != 1L || is.na(flip) || !nzchar(flip)) {
    wsi_abort("`coordinate_flip` must be one of: \"none\", \"vertical\", or \"horizontal\".")
  }
  flip <- tolower(gsub("[ -]+", "_", flip))
  aliases <- c(
    none = "none",
    identity = "none",
    vertical = "vertical",
    verticcal = "vertical",
    flip_y = "vertical",
    y = "vertical",
    horizontal = "horizontal",
    flip_x = "horizontal",
    x = "horizontal"
  )
  out <- aliases[[flip]]
  if (is.null(out)) {
    wsi_abort("`coordinate_flip` must be one of: \"none\", \"vertical\", or \"horizontal\".")
  }
  unname(out)
}

wsi_seurat_coordinate_rotation_arg <- function(rotation) {
  if (length(rotation) > 1L) {
    defaults <- c(0, 90, 180, 270)
    if (identical(as.numeric(rotation), defaults)) {
      rotation <- rotation[[1L]]
    }
  }
  if (is.character(rotation)) {
    rotation <- tolower(gsub("[[:space:]_]+", "", rotation))
    rotation <- sub("degrees?$", "", rotation)
    rotation <- sub("deg$", "", rotation)
  }
  rotation <- suppressWarnings(as.integer(rotation))
  if (length(rotation) != 1L || is.na(rotation) || !(rotation %in% c(0L, 90L, 180L, 270L))) {
    wsi_abort("`coordinate_rotation` must be one of: 0, 90, 180, or 270.")
  }
  rotation
}

wsi_seurat_coordinate_orientation_name <- function(flip, rotation) {
  if (identical(flip, "none") && identical(rotation, 0L)) {
    return("none")
  }
  if (identical(flip, "vertical") && identical(rotation, 0L)) {
    return("flip_y")
  }
  if (identical(flip, "none") && identical(rotation, 270L)) {
    return("x_y_y_neg_x")
  }
  if (identical(flip, "horizontal") && identical(rotation, 90L)) {
    return("x_neg_y_y_neg_x")
  }
  paste0("flip_", flip, "_rotate_", rotation)
}

wsi_seurat_apply_coordinate_transform <- function(x, y, width, height, transform = "none",
                                                  flip = "none", rotation = 0) {
  transform <- wsi_seurat_coordinate_transform_arg(transform)
  flip <- wsi_seurat_coordinate_flip_arg(flip)
  rotation <- wsi_seurat_coordinate_rotation_arg(rotation)
  legacy_transform <- transform
  if (!identical(transform, "none")) {
    preset <- wsi_seurat_coordinate_transform_preset(transform)
    flip <- preset$flip
    rotation <- preset$rotation
  }
  width <- as.numeric(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  height <- as.numeric(wsi_check_scalar_number(height, "height", allow_zero = FALSE))
  x <- as.numeric(x)
  y <- as.numeric(y)
  current_width <- width
  current_height <- height

  if (identical(flip, "horizontal")) {
    x <- current_width - x
  } else if (identical(flip, "vertical")) {
    y <- current_height - y
  }

  if (identical(rotation, 0L)) {
    return(list(
      x = x,
      y = y,
      transform = if (identical(legacy_transform, "none")) wsi_seurat_coordinate_orientation_name(flip, rotation) else legacy_transform,
      flip = flip,
      rotation = rotation,
      width = current_width,
      height = current_height,
      rescale_x = 1,
      rescale_y = 1
    ))
  }
  if (identical(rotation, 90L)) {
    old_x <- x
    old_y <- y
    return(list(
      x = current_height - old_y,
      y = old_x,
      transform = if (identical(legacy_transform, "none")) wsi_seurat_coordinate_orientation_name(flip, rotation) else legacy_transform,
      flip = flip,
      rotation = rotation,
      width = current_height,
      height = current_width,
      rescale_x = 1,
      rescale_y = 1
    ))
  }
  if (identical(rotation, 180L)) {
    return(list(
      x = current_width - x,
      y = current_height - y,
      transform = if (identical(legacy_transform, "none")) wsi_seurat_coordinate_orientation_name(flip, rotation) else legacy_transform,
      flip = flip,
      rotation = rotation,
      width = current_width,
      height = current_height,
      rescale_x = 1,
      rescale_y = 1
    ))
  }
  if (identical(rotation, 270L)) {
    old_x <- x
    old_y <- y
    return(list(
      x = old_y,
      y = current_width - old_x,
      transform = if (identical(legacy_transform, "none")) wsi_seurat_coordinate_orientation_name(flip, rotation) else legacy_transform,
      flip = flip,
      rotation = rotation,
      width = current_height,
      height = current_width,
      rescale_x = 1,
      rescale_y = 1
    ))
  }
  wsi_abort(sprintf("Unsupported coordinate rotation: %s", rotation))
}

wsi_seurat_image_dimensions <- function(image_obj) {
  image <- wsi_seurat_slot(image_obj, "image")
  if (is.null(image)) {
    return(NULL)
  }
  dims <- dim(image)
  if (length(dims) < 2L) {
    return(NULL)
  }
  c(width = as.numeric(dims[[2L]]), height = as.numeric(dims[[1L]]))
}

wsi_seurat_normalize_scale_factors <- function(sf) {
  if (is.null(sf)) {
    return(list())
  }
  if (isS4(sf)) {
    out <- lapply(methods::slotNames(sf), function(name) methods::slot(sf, name))
    names(out) <- methods::slotNames(sf)
    sf <- out
  }
  if (!is.list(sf)) {
    return(list())
  }
  out <- sf
  if (!is.null(out$tissue_hires_scalef)) {
    out$hires <- out$tissue_hires_scalef
  }
  if (!is.null(out$tissue_lowres_scalef)) {
    out$lowres <- out$tissue_lowres_scalef
  }
  if (!is.null(out$spot_diameter_fullres)) {
    out$spot <- out$spot_diameter_fullres
  }
  if (!is.null(out$fiducial_diameter_fullres)) {
    out$fiducial <- out$fiducial_diameter_fullres
  }
  out
}

wsi_seurat_read_scalefactors_json <- function(path) {
  if (is.null(path)) {
    return(list())
  }
  out <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = TRUE),
    error = function(err) {
      wsi_abort(sprintf("Could not read scalefactors JSON `%s`: %s", path, conditionMessage(err)))
    }
  )
  wsi_seurat_normalize_scale_factors(as.list(out))
}

wsi_seurat_scale_factors <- function(image_obj, scalefactors_json = NULL) {
  sf <- wsi_seurat_slot(image_obj, "scale.factors") %||%
    wsi_seurat_slot(image_obj, "scale_factors")
  sf <- wsi_seurat_normalize_scale_factors(sf)
  json_sf <- wsi_seurat_read_scalefactors_json(scalefactors_json)
  wsi_seurat_normalize_scale_factors(utils::modifyList(sf, json_sf, keep.null = TRUE))
}

wsi_seurat_scale_value <- function(scale_factors, name) {
  value <- suppressWarnings(as.numeric(scale_factors[[name]] %||% NA_real_))
  if (length(value) != 1L || !is.finite(value) || value <= 0) {
    return(NA_real_)
  }
  value
}

wsi_seurat_fullres_dimensions <- function(image_obj, slide, scale_factors) {
  slide_dims <- c(
    width = as.numeric(slide$dimensions[["width"]]),
    height = as.numeric(slide$dimensions[["height"]])
  )
  image_dims <- wsi_seurat_image_dimensions(image_obj)
  if (!is.null(image_dims) && all(is.finite(image_dims)) && all(image_dims > 0)) {
    for (scale_name in c("lowres", "hires")) {
      sf <- wsi_seurat_scale_value(scale_factors, scale_name)
      if (is.finite(sf)) {
        inferred <- c(width = image_dims[["width"]] / sf, height = image_dims[["height"]] / sf)
        if (all(is.finite(inferred)) && all(inferred > 0) &&
            max(abs(inferred / slide_dims - 1), na.rm = TRUE) < 0.25) {
          return(inferred)
        }
      }
    }
  }
  slide_dims
}

wsi_seurat_coordinate_mapping <- function(coordinates, image_obj, slide,
                                          scale_factors = list(),
                                          coordinate_scale, scale_x = NULL,
                                          scale_y = NULL) {
  slide_width <- as.numeric(slide$dimensions[["width"]])
  slide_height <- as.numeric(slide$dimensions[["height"]])
  fullres_dims <- wsi_seurat_fullres_dimensions(image_obj, slide, scale_factors)
  fullres_to_slide_x <- slide_width / fullres_dims[["width"]]
  fullres_to_slide_y <- slide_height / fullres_dims[["height"]]
  coordinate_space <- attr(coordinates, "coordinate_space", exact = TRUE) %||% "unknown"
  if (identical(coordinate_scale, "custom")) {
    if (is.null(scale_x) || is.null(scale_y)) {
      wsi_abort("`scale_x` and `scale_y` are required when `coordinate_scale = \"custom\"`.")
    }
    return(list(
      method = "custom",
      scale_x = as.numeric(wsi_check_scalar_number(scale_x, "scale_x", allow_zero = FALSE)),
      scale_y = as.numeric(wsi_check_scalar_number(scale_y, "scale_y", allow_zero = FALSE))
    ))
  }
  if (identical(coordinate_scale, "none")) {
    return(list(method = "none", scale_x = 1, scale_y = 1))
  }
  if (identical(coordinate_scale, "fullres")) {
    return(list(
      method = "fullres",
      scale_x = fullres_to_slide_x,
      scale_y = fullres_to_slide_y,
      fullres_width = unname(fullres_dims[["width"]]),
      fullres_height = unname(fullres_dims[["height"]]),
      coordinate_space = coordinate_space
    ))
  }
  if (coordinate_scale %in% c("hires", "lowres")) {
    sf <- wsi_seurat_scale_value(scale_factors, coordinate_scale)
    if (!is.finite(sf)) {
      wsi_abort(sprintf(
        "`coordinate_scale = \"%s\"` requires `%s` in the Seurat scale factors or scalefactors JSON.",
        coordinate_scale,
        coordinate_scale
      ))
    }
    return(list(
      method = coordinate_scale,
      scale_x = fullres_to_slide_x / sf,
      scale_y = fullres_to_slide_y / sf,
      fullres_width = unname(fullres_dims[["width"]]),
      fullres_height = unname(fullres_dims[["height"]]),
      coordinate_space = coordinate_space
    ))
  }
  seurat_dims <- wsi_seurat_image_dimensions(image_obj)
  if (is.null(seurat_dims) || !all(is.finite(seurat_dims)) || any(seurat_dims <= 0)) {
    return(list(method = "auto_fullres", scale_x = fullres_to_slide_x, scale_y = fullres_to_slide_y))
  }
  sx <- slide_width / seurat_dims[["width"]]
  sy <- slide_height / seurat_dims[["height"]]
  if (identical(coordinate_scale, "seurat_image")) {
    return(list(method = "seurat_image", scale_x = sx, scale_y = sy))
  }
  max_x <- suppressWarnings(max(coordinates$x, na.rm = TRUE))
  max_y <- suppressWarnings(max(coordinates$y, na.rm = TRUE))
  if (identical(coordinate_space, "fullres")) {
    return(list(
      method = "auto_fullres",
      scale_x = fullres_to_slide_x,
      scale_y = fullres_to_slide_y,
      fullres_width = unname(fullres_dims[["width"]]),
      fullres_height = unname(fullres_dims[["height"]]),
      coordinate_space = coordinate_space
    ))
  }
  for (scale_name in c("lowres", "hires")) {
    sf <- wsi_seurat_scale_value(scale_factors, scale_name)
    if (!is.finite(sf)) {
      next
    }
    scaled_dims <- fullres_dims * sf
    if (is.finite(max_x) && is.finite(max_y) &&
        max_x <= scaled_dims[["width"]] * 1.15 &&
        max_y <= scaled_dims[["height"]] * 1.15) {
      return(list(
        method = paste0("auto_", scale_name),
        scale_x = fullres_to_slide_x / sf,
        scale_y = fullres_to_slide_y / sf,
        fullres_width = unname(fullres_dims[["width"]]),
        fullres_height = unname(fullres_dims[["height"]]),
        coordinate_space = coordinate_space
      ))
    }
  }
  if (is.finite(max_x) && is.finite(max_y) &&
      max_x > seurat_dims[["width"]] * 1.15 &&
      max_y > seurat_dims[["height"]] * 1.15) {
    return(list(
      method = "auto_fullres",
      scale_x = fullres_to_slide_x,
      scale_y = fullres_to_slide_y,
      fullres_width = unname(fullres_dims[["width"]]),
      fullres_height = unname(fullres_dims[["height"]]),
      coordinate_space = coordinate_space
    ))
  }
  in_seurat_space <- is.finite(max_x) && is.finite(max_y) &&
    max_x <= seurat_dims[["width"]] * 1.15 &&
    max_y <= seurat_dims[["height"]] * 1.15 &&
    (slide_width > seurat_dims[["width"]] * 1.15 || slide_height > seurat_dims[["height"]] * 1.15)
  if (isTRUE(in_seurat_space)) {
    return(list(method = "auto_seurat_image", scale_x = sx, scale_y = sy))
  }
  list(method = "auto_none", scale_x = 1, scale_y = 1)
}

wsi_seurat_spot_radius <- function(scale_factors, mapping) {
  spot <- suppressWarnings(as.numeric(scale_factors$spot %||% scale_factors$spot_diameter_fullres %||% NA_real_))
  if (length(spot) == 1L && is.finite(spot) && spot > 0) {
    return(max(2, spot * mean(c(mapping$scale_x, mapping$scale_y)) / 2))
  }
  max(6, 28 * mean(c(mapping$scale_x, mapping$scale_y)))
}

wsi_seurat_gradient <- function(values, low = "#2B6CB0", mid = "#F8FAFC", high = "#B91C1C") {
  values <- as.numeric(values)
  if (!length(values)) {
    return(character())
  }
  ok <- is.finite(values)
  if (!any(ok)) {
    return(rep(low, length(values)))
  }
  rng <- range(values[ok], na.rm = TRUE)
  if (!is.finite(diff(rng)) || abs(diff(rng)) < 1e-12) {
    return(rep(low, length(values)))
  }
  t <- (values - rng[[1L]]) / diff(rng)
  t <- pmin(1, pmax(0, t))
  ramp <- grDevices::colorRamp(c(low, mid, high), space = "Lab")
  rgb <- ramp(t)
  out <- grDevices::rgb(rgb[, 1L], rgb[, 2L], rgb[, 3L], maxColorValue = 255)
  out[!ok] <- "#999999"
  out
}
