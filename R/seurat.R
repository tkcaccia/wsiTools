#' Link a Seurat spatial object to a high-resolution image
#'
#' `wsi_link_seurat_image()` extracts spatial spot coordinates and, when
#' available, a dimensional reduction from a Seurat object and maps the spots
#' or cells onto a high-resolution image. If no reduction is present, wsiTools
#' falls back to a coordinate-based spatial plot so the image overlay and live
#' gene lookup still work. Seurat remains an optional dependency: when
#' `SeuratObject` or `Seurat` is installed their accessors are used; otherwise
#' the function falls back to Seurat-like object slots/lists.
#'
#' The linked object can be passed to [wsi_viewer_seurat()] to open the tissue
#' image with a spot overlay and one interactive plot button for each available
#' dimensional reduction. The image is opened through the usual wsiTools
#' backends and is not loaded fully into R memory by default.
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
#'   If the reduction is absent but spatial coordinates are available, the
#'   viewer uses the slide x/y coordinates as a fallback `"spatial"` plot.
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
#'   browser. In live mode, [wsi_viewer_seurat()] can instead retrieve one
#'   selected gene at a time from the active R session, so `spot_genes` may be
#'   left `NULL`.
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
  embeddings <- tryCatch(
    wsi_seurat_embeddings(seurat, reduction = reduction),
    error = function(err) NULL
  )
  if (is.null(embeddings)) {
    seurat_ids <- tryCatch(colnames(seurat), error = function(err) NULL)
    if (is.null(seurat_ids) || !length(seurat_ids)) {
      meta <- wsi_seurat_slot(seurat, "meta.data")
      seurat_ids <- rownames(meta) %||% character()
    }
    if (length(seurat_ids)) {
      coordinates <- coordinates[coordinates$barcode %in% seurat_ids, , drop = FALSE]
    }
    if (!nrow(coordinates)) {
      wsi_abort("No shared spot/cell identifiers were found between Seurat coordinates and the object.")
    }
    embeddings <- cbind(slide_x = coordinates$x, slide_y = coordinates$y)
    rownames(embeddings) <- coordinates$barcode
    reduction <- "spatial"
    dims <- c(1L, 2L)
  } else {
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
  }
  if (!nrow(coordinates)) {
    wsi_abort("No shared spot/barcode identifiers were found between Seurat coordinates and the reduction.")
  }
  feature_type <- wsi_seurat_feature_type(coordinates, source_name = "Seurat")
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
  scale_metadata <- wsi_spatial_scale_metadata(
    slide = slide,
    coordinates = coordinates,
    scale_factors = scale_factors,
    mapping = mapping
  )

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
  cluster_values <- wsi_spatial_clusters(seurat, spot_ids = ids)

  spots <- data.frame(
    id = ids,
    label = ids,
    barcode = ids,
    feature_type = feature_type,
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
    cluster_values <- wsi_spatial_subset_clusters(cluster_values, idx)
  }
  spots <- wsi_spatial_add_cluster_columns(spots, cluster_values)
  gene_value_items <- wsi_seurat_gene_value_items(gene_expression)
  cluster_value_items <- wsi_spatial_cluster_value_items(cluster_values)

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
  if (length(cluster_value_items)) {
    plot_points$cluster_values <- I(cluster_value_items)
  }
  plots <- wsi_spatial_reduction_plots(
    object = seurat,
    source_name = "Seurat",
    spots = spots,
    selected_embeddings = embeddings,
    reduction = reduction,
    dims = dims,
    gene_value_items = gene_value_items,
    cluster_value_items = cluster_value_items
  )
  if (!length(plots)) {
    plots <- list(wsi_spatial_reduction_plot(
      embeddings = embeddings,
      spots = spots,
      reduction = reduction,
      dims = dims,
      source_name = "Seurat",
      gene_value_items = gene_value_items,
      cluster_value_items = cluster_value_items
    ))
    plots <- plots[!vapply(plots, is.null, logical(1))]
  }
  primary_plot <- if (length(plots)) {
    plots[[1L]]
  } else {
    list(
      id = paste0("seurat_", wsi_safe_id(reduction, "reduction")),
      label = paste0("Seurat ", toupper(reduction), " plot"),
      reduction = reduction,
      x_label = component_names[[1L]],
      y_label = component_names[[2L]],
      point_count = total,
      points = plot_points
    )
  }

  if (is.null(spot_radius)) {
    spot_radius <- wsi_seurat_spot_radius(
      scale_factors = scale_factors,
      mapping = mapping,
      scale_metadata = scale_metadata
    )
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
    mpp = scale_metadata$mpp,
    pixel_size = scale_metadata$mpp,
    scale_metadata = scale_metadata,
    gene_expression = gene_expression,
    feature_type = feature_type,
    spot_radius = spot_radius,
    spot_count = total,
    displayed_spot_count = nrow(spots),
    spots = spots,
    clusters = wsi_spatial_cluster_config(cluster_values),
    cluster_fields = attr(cluster_values, "fields", exact = TRUE) %||% wsi_empty_spatial_cluster_fields(),
    cluster_values = cluster_values,
    expression_source = list(
      object = seurat,
      spot_ids = as.character(spots$barcode %||% spots$id),
      feature_type = feature_type
    ),
    reduction_embeddings = embeddings,
    reduction_embedding_name = reduction,
    plots = plots,
    pca = primary_plot
  )
  class(out) <- c("wsi_seurat_spatial", "list")
  out
}

#' Extract Seurat spatial coordinates used by wsiTools
#'
#' `wsi_seurat_coordinates()` returns the spot or cell coordinates stored in a
#' Seurat object. If `image` is supplied, the function applies the same scaling,
#' flipping, rotation, and slide-coordinate mapping used by
#' [wsi_link_seurat_image()] and the interactive viewer.
#'
#' @param seurat A Seurat or Seurat-like object.
#' @param image Optional microscopy image path or `wsi_slide`. When supplied,
#'   output `x` and `y` are level-0 slide coordinates used by the viewer.
#' @param image_name Spatial image name inside the Seurat object. By default the
#'   first spatial image is used when available.
#' @param spatial_dir Optional 10x Genomics `spatial/` directory.
#' @param scalefactors_json Optional 10x Genomics scalefactors JSON file.
#' @param tissue_positions Optional 10x Genomics tissue positions CSV file.
#' @param coordinate_scale,scale_x,scale_y,coordinate_flip,coordinate_rotation,coordinate_transform
#'   Coordinate mapping arguments; see [wsi_link_seurat_image()].
#' @param plot If `TRUE`, draw a quick base R coordinate plot.
#' @param output Optional CSV file path where the coordinates are written.
#'
#' @return A data frame with at least `id`, `barcode`, `x`, and `y`. Attributes
#'   describe the coordinate source, coordinate space, and mapping when known.
#' @export
#'
#' @examples
#' \dontrun{
#' coords <- wsi_seurat_coordinates(seurat_object)
#' head(coords)
#'
#' coords_slide <- wsi_seurat_coordinates(
#'   seurat_object,
#'   image = "tissue_image.btf",
#'   plot = TRUE,
#'   output = "seurat_coordinates.csv"
#' )
#' }
wsi_seurat_coordinates <- function(seurat, image = NULL, image_name = NULL,
                                   spatial_dir = NULL, scalefactors_json = NULL,
                                   tissue_positions = NULL,
                                   coordinate_scale = c("auto", "none", "fullres", "hires", "lowres", "seurat_image", "custom"),
                                   scale_x = NULL, scale_y = NULL,
                                   coordinate_flip = c("none", "vertical", "horizontal"),
                                   coordinate_rotation = c(0, 90, 180, 270),
                                   coordinate_transform = "none",
                                   plot = FALSE,
                                   output = NULL) {
  coordinate_scale <- match.arg(coordinate_scale)
  coordinate_flip <- wsi_seurat_coordinate_flip_arg(coordinate_flip)
  coordinate_rotation <- wsi_seurat_coordinate_rotation_arg(coordinate_rotation)
  coordinate_transform <- wsi_seurat_coordinate_transform_arg(coordinate_transform)
  if (!is.logical(plot) || length(plot) != 1L || is.na(plot)) {
    wsi_abort("`plot` must be `TRUE` or `FALSE`.")
  }

  if (!is.null(image)) {
    linked <- wsi_link_seurat_image(
      seurat = seurat,
      image = image,
      image_name = image_name,
      spatial_dir = spatial_dir,
      scalefactors_json = scalefactors_json,
      tissue_positions = tissue_positions,
      coordinate_scale = coordinate_scale,
      scale_x = scale_x,
      scale_y = scale_y,
      coordinate_flip = coordinate_flip,
      coordinate_rotation = coordinate_rotation,
      coordinate_transform = coordinate_transform,
      colour_by = "none",
      max_points = .Machine$integer.max
    )
    spots <- linked$spots
    out <- data.frame(
      id = as.character(spots$id %||% spots$barcode),
      barcode = as.character(spots$barcode %||% spots$id),
      feature_type = as.character(spots$feature_type %||% linked$feature_type %||% "spot"),
      x = suppressWarnings(as.numeric(spots$x)),
      y = suppressWarnings(as.numeric(spots$y)),
      stringsAsFactors = FALSE
    )
    attr(out, "coordinate_space") <- "level0_slide_pixels"
    attr(out, "coordinate_source") <- attr(linked$coordinate_mapping, "source", exact = TRUE) %||%
      linked$coordinate_mapping$coordinate_space %||% "viewer_mapped"
    attr(out, "coordinate_mapping") <- linked$coordinate_mapping
    attr(out, "slide_width") <- as.numeric(linked$slide$dimensions[["width"]] %||% NA_real_)
    attr(out, "slide_height") <- as.numeric(linked$slide$dimensions[["height"]] %||% NA_real_)
    attr(out, "image_name") <- linked$image_name
    attr(out, "feature_type") <- linked$feature_type
  } else {
    spatial <- wsi_seurat_spatial_files(
      spatial_dir = spatial_dir,
      scalefactors_json = scalefactors_json,
      tissue_positions = tissue_positions
    )
    resolved_image_name <- tryCatch(
      wsi_seurat_image_name(seurat, image_name),
      error = function(err) image_name
    )
    image_obj <- if (!is.null(resolved_image_name)) {
      wsi_seurat_image_object(seurat, resolved_image_name)
    } else {
      NULL
    }
    coords <- wsi_seurat_coordinate_table(
      seurat = seurat,
      image_name = resolved_image_name,
      image_obj = image_obj,
      tissue_positions = spatial$tissue_positions
    )
    out <- data.frame(
      id = as.character(coords$barcode),
      barcode = as.character(coords$barcode),
      x = suppressWarnings(as.numeric(coords$x)),
      y = suppressWarnings(as.numeric(coords$y)),
      stringsAsFactors = FALSE
    )
    attr(out, "coordinate_space") <- attr(coords, "coordinate_space", exact = TRUE) %||% "unknown"
    attr(out, "coordinate_source") <- attr(coords, "coordinate_source", exact = TRUE) %||% "seurat"
    attr(out, "id_column") <- attr(coords, "id_column", exact = TRUE)
    attr(out, "x_column") <- attr(coords, "x_column", exact = TRUE)
    attr(out, "y_column") <- attr(coords, "y_column", exact = TRUE)
    attr(out, "image_name") <- resolved_image_name
  }

  out <- out[is.finite(out$x) & is.finite(out$y) & nzchar(out$barcode), , drop = FALSE]
  row.names(out) <- NULL
  if (!is.null(output)) {
    output <- wsi_validate_output_path(output, overwrite = TRUE)
    utils::write.csv(out, output, row.names = FALSE)
    attr(out, "output") <- normalizePath(output, winslash = "/", mustWork = FALSE)
  }
  if (isTRUE(plot)) {
    graphics::plot(
      out$x,
      out$y,
      asp = 1,
      pch = ".",
      col = "#2563EB",
      xlab = "x",
      ylab = "y",
      ylim = rev(range(out$y, finite = TRUE)),
      main = sprintf("Seurat coordinates (%s)", attr(out, "coordinate_space") %||% "unknown")
    )
  }
  out
}

#' Open a Seurat spatial object in the wsiTools viewer
#'
#' `wsi_viewer_seurat()` opens a high-resolution tissue image and overlays
#' Seurat spatial spots. The top **Seurat** menu opens buttons for the
#' dimensional reductions found in the object, and can show/hide or zoom to the
#' spot overlay. In live mode, typing a gene in the Seurat menu retrieves only
#' that selected gene from R; static HTML viewers use genes embedded with
#' `spot_genes`.
#'
#' @param seurat A Seurat object.
#' @param image Path to the high-resolution image, or a `wsi_slide` object.
#' @param linked Optional precomputed object from [wsi_link_seurat_image()].
#' @param live Use [wsi_viewer_live()] instead of static [wsi_viewer()]. The
#'   default is `TRUE` so R and the browser stay synchronized while the session
#'   is active; set `live = FALSE` to write a static HTML viewer.
#' @param dynamic_tiles When `live = TRUE`, serve the image as dynamic tiles.
#'   The default is `FALSE`: live synchronization stays active, while the base
#'   image and dense spatial masks use prebuilt Deep Zoom tiles for smoother
#'   zooming and panning. Use `TRUE` only as a fallback when prebuilt tiles
#'   cannot be created.
#' @param show_spots Whether to draw the spatial spot layer on top of the
#'   tissue image. Set this to `FALSE` for very dense assays such as Visium HD
#'   when a tiled mask or other aggregate layer is more appropriate than sending
#'   sampled spots to the browser.
#' @param mode Viewer mode for static output. `"tiles"` gives full-resolution
#'   Deep Zoom viewing when the backend can create tiles.
#' @param output,open,overwrite Additional viewer options.
#' @param ... Arguments passed to [wsi_link_seurat_image()] and then to
#'   [wsi_viewer()] or [wsi_viewer_live()]. Viewer arguments take precedence
#'   after the linked object is created.
#'
#' @return A `wsi_viewer_session` by default. If `live = FALSE`, returns the
#'   static HTML path.
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
                              live = TRUE, dynamic_tiles = FALSE,
                              show_spots = TRUE,
                              mode = c("tiles", "thumbnail"),
                              output = NULL, open = interactive(),
                              overwrite = FALSE, ...) {
  dots <- list(...)
  if (!is.logical(live) || length(live) != 1L || is.na(live)) {
    wsi_abort("`live` must be `TRUE` or `FALSE`.")
  }
  if (!is.logical(show_spots) || length(show_spots) != 1L || is.na(show_spots)) {
    wsi_abort("`show_spots` must be `TRUE` or `FALSE`.")
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
  if (is.null(linked$expression_source$object)) {
    linked$expression_source <- list(
      object = seurat,
      spot_ids = as.character(linked$spots$barcode %||% linked$spots$id)
    )
  }
  if (isTRUE(live) && identical(mode, "tiles")) {
    dynamic_tiles <- wsi_prefer_static_spatial_tiles(dynamic_tiles, context = "Seurat viewer")
  }

  layers <- dots$layers %||% list()
  if (!is.list(layers) || inherits(layers, "data.frame")) {
    layers <- list(layers)
  }
  dots$layers <- layers
  if (isTRUE(show_spots)) {
    if (is.null(output)) {
      output <- tempfile(fileext = ".html")
      overwrite <- TRUE
    }
    spot_source <- wsi_seurat_spots_channel_source(
      linked = linked,
      output_dir = file.path(dirname(output), paste0(tools::file_path_sans_ext(basename(output)), "_spatial_masks")),
      output_html = output,
      id = "seurat_spots",
      visible = TRUE,
      opacity = 0.85,
      dynamic = isTRUE(dots$spatial_mask_dynamic %||% FALSE),
      rebuild = isTRUE(dots$rebuild %||% FALSE)
    )
    if (!is.null(spot_source)) {
      channel_sources <- dots$channel_sources %||% list()
      if (!is.list(channel_sources) || inherits(channel_sources, "data.frame")) {
        channel_sources <- list(channel_sources)
      }
      dots$channel_sources <- c(list(spot_source$source), channel_sources)
      linked$spot_layer_id <- spot_source$source$id
      linked$spot_mask <- spot_source$mask
    }
  }
  dots$seurat <- linked
  dots$output <- output
  dots$open <- open
  dots$overwrite <- overwrite
  dots$title <- dots$title %||% sprintf("wsiTools Seurat viewer: %s", basename(linked$image_path %||% "image"))

  if (isTRUE(live)) {
    dots$dynamic_tiles <- dynamic_tiles
    if (isTRUE(dynamic_tiles) && is.null(dots$dynamic_tile_format)) {
      dots$dynamic_tile_format <- "jpg"
    }
    dots$mode <- mode
    dots$slide <- linked$slide
    return(do.call(wsi_viewer_live, dots))
  }

  dots$slide <- linked$slide
  dots$mode <- mode
  do.call(wsi_viewer, dots)
}

#' Open a multi-tissue Seurat project in the wsiTools viewer
#'
#' `wsi_viewer_seurat_project()` links several Seurat spatial images to their
#' corresponding high-resolution tissue images and opens them as one wsiTools
#' project. Each tissue section keeps its own spatial spot overlay, available
#' reduction plots, annotations, and trajectories. In tiled mode, Deep Zoom
#' tiles are generated for each source image with libvips so zooming can reach
#' the full available image resolution without loading the complete image into R
#' memory.
#'
#' @param seurat A Seurat object, Seurat-like object, or a list of one object
#'   per image. Ignored when `linked` is supplied.
#' @param images Named character vector/list of high-resolution image paths or
#'   `wsi_slide` objects. Names are used as Seurat spatial image names when
#'   `image_names` is not supplied.
#' @param linked Optional `wsi_seurat_spatial` object, or named list of objects
#'   returned by [wsi_link_seurat_image()]. Supplying `linked` skips the
#'   linking step.
#' @param image_names Seurat spatial image names, one per image. For multiple
#'   images, either `image_names` or names on `images` should be supplied.
#' @param labels Optional labels shown in the Project panel.
#' @inheritParams wsi_link_seurat_image
#' @param mode Viewer mode. `"tiles"` uses prebuilt Deep Zoom tiles and is the
#'   recommended mode for real WSI or high-resolution tissue images.
#'   `"thumbnail"` is useful for quick tests and mock slides.
#' @param live Use [wsi_viewer_live()] by default so the project can synchronize
#'   annotations, selections, measurements, and spot tables back to R. Set
#'   `live = FALSE` to write a static HTML viewer.
#' @param dynamic_tiles When `live = TRUE`, whether the first active image should
#'   be served from the dynamic tile server instead of the prebuilt tile source.
#'   The default is `FALSE` because prebuilt project tiles are usually smoother.
#' @param wait For live viewers, whether to keep servicing the local R/httpuv
#'   bridge until interrupted.
#' @param transport Live viewer transport passed to [wsi_viewer_live()].
#' @param output,open,overwrite,title Viewer output options.
#' @param width,height Thumbnail/navigator size options passed to the viewer.
#' @param tile_dir Directory where per-image Deep Zoom tile pyramids are stored.
#' @param tile_size,tile_format,quality,rebuild,tile_overlap Deep Zoom tiling
#'   options used in tiled mode.
#' @param roi_class_presets ROI classes used by the annotation UI.
#'
#' @return A `wsi_viewer_session` by default. If `live = FALSE`, returns the
#'   static HTML path.
#' @export
#'
#' @examples
#' \dontrun{
#' images <- c(
#'   anterior1 = "V1_Mouse_Brain_Sagittal_Anterior_image.tif",
#'   anterior2 = "V1_Mouse_Brain_Sagittal_Anterior_Section_2_image.tif"
#' )
#'
#' viewer <- wsi_viewer_seurat_project(
#'   brain.merge,
#'   images = images,
#'   image_names = names(images),
#'   output = "seurat_project.html",
#'   mode = "tiles",
#'   wait = FALSE
#' )
#' }
wsi_viewer_seurat_project <- function(seurat = NULL, images = NULL, linked = NULL,
                                      image_names = NULL, labels = NULL,
                                      spatial_dir = NULL,
                                      scalefactors_json = NULL,
                                      tissue_positions = NULL,
                                      reduction = "pca", dims = c(1L, 2L),
                                      coordinate_scale = c("auto", "none", "fullres", "hires", "lowres", "seurat_image", "custom"),
                                      scale_x = NULL, scale_y = NULL,
                                      coordinate_flip = c("none", "vertical", "horizontal"),
                                      coordinate_rotation = c(0, 90, 180, 270),
                                      coordinate_transform = "none",
                                      spot_genes = NULL, default_gene = NULL,
                                      spot_radius = NULL, max_points = 100000L,
                                      colour_by = c("component_1", "gene", "none"),
                                      mode = c("tiles", "thumbnail"),
                                      live = TRUE, dynamic_tiles = FALSE,
                                      wait = interactive(),
                                      transport = c("auto", "websocket", "polling"),
                                      output = NULL, open = interactive(),
                                      overwrite = FALSE,
                                      title = "wsiTools Seurat project viewer",
                                      width = 1600, height = NULL,
                                      tile_dir = NULL, tile_size = 512,
                                      tile_format = c("jpg", "png"),
                                      quality = 90, rebuild = FALSE,
                                      tile_overlap = NULL,
                                      roi_class_presets = wsi_roi_class_presets()) {
  mode <- match.arg(mode)
  transport <- match.arg(transport)
  tile_format <- match.arg(tile_format)
  coordinate_scale <- match.arg(coordinate_scale)
  coordinate_flip <- wsi_seurat_coordinate_flip_arg(coordinate_flip)
  coordinate_rotation <- wsi_seurat_coordinate_rotation_arg(coordinate_rotation)
  colour_by <- match.arg(colour_by)
  if (!is.logical(live) || length(live) != 1L || is.na(live)) {
    wsi_abort("`live` must be `TRUE` or `FALSE`.")
  }
  if (!is.logical(dynamic_tiles) || length(dynamic_tiles) != 1L || is.na(dynamic_tiles)) {
    wsi_abort("`dynamic_tiles` must be `TRUE` or `FALSE`.")
  }
  if (isTRUE(live) && identical(mode, "tiles")) {
    dynamic_tiles <- wsi_prefer_static_spatial_tiles(dynamic_tiles, context = "Seurat project viewer")
  }
  if (!is.logical(wait) || length(wait) != 1L || is.na(wait)) {
    wsi_abort("`wait` must be `TRUE` or `FALSE`.")
  }
  width <- as.integer(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  if (!is.null(height)) {
    height <- as.integer(wsi_check_scalar_number(height, "height", allow_zero = FALSE))
  }
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  quality <- as.integer(wsi_check_scalar_number(quality, "quality", allow_zero = FALSE))
  if (!is.null(tile_overlap)) {
    tile_overlap <- as.integer(wsi_check_scalar_number(tile_overlap, "tile_overlap"))
    if (tile_overlap >= tile_size) {
      wsi_abort("`tile_overlap` must be smaller than `tile_size`.")
    }
  }

  if (is.null(output)) {
    output <- tempfile(fileext = ".html")
    overwrite <- TRUE
  }
  output <- wsi_validate_output_path(output, overwrite = overwrite)

  linked <- if (is.null(linked)) {
    wsi_link_seurat_project_sections(
      seurat = seurat,
      images = images,
      image_names = image_names,
      spatial_dir = spatial_dir,
      scalefactors_json = scalefactors_json,
      tissue_positions = tissue_positions,
      reduction = reduction,
      dims = dims,
      coordinate_scale = coordinate_scale,
      scale_x = scale_x,
      scale_y = scale_y,
      coordinate_flip = coordinate_flip,
      coordinate_rotation = coordinate_rotation,
      coordinate_transform = coordinate_transform,
      spot_genes = spot_genes,
      default_gene = default_gene,
      spot_radius = spot_radius,
      max_points = max_points,
      colour_by = colour_by
    )
  } else {
    wsi_normalize_seurat_project_linked(linked)
  }
  if (!length(linked)) {
    wsi_abort("No linked Seurat sections were supplied.")
  }

  labels <- wsi_seurat_project_labels(labels, linked)
  names(linked) <- labels
  records <- wsi_seurat_project_records(
    linked,
    output = output,
    labels = labels,
    mode = mode,
    width = min(width, 768L),
    height = height,
    tile_dir = tile_dir,
    tile_size = tile_size,
    tile_format = tile_format,
    quality = quality,
    rebuild = rebuild,
    tile_overlap = tile_overlap,
    dynamic_channel_sources = FALSE
  )

  project_prediction <- wsi_prediction_context(
    spatial = wsi_seurat_project_prediction_context(linked, records)
  )
  first <- wsi_seurat_project_scoped_linked(
    linked[[1L]],
    records[[1L]],
    item_index = 0L,
    section = NULL,
    section_index = -1L
  )
  first_record <- records[[1L]]
  first$spot_layer_id <- first_record$seurat$spot_layer_id %||% "seurat_spots"
  project_channel_sources <- unlist(lapply(records, function(record) record$channel_sources %||% list()), recursive = FALSE)
  project_tile_sources <- if (isTRUE(live) && isTRUE(dynamic_tiles) && identical(mode, "tiles")) {
    wsi_seurat_project_dynamic_tile_sources(linked, records)
  } else {
    NULL
  }
  records <- lapply(records, function(record) {
    record$channel_sources <- NULL
    record$spot_mask <- NULL
    record
  })

  if (identical(mode, "thumbnail")) {
    viewer_args <- list(
      slide = first$slide,
      width = width,
      height = height,
      output = output,
      open = open,
      title = title,
      overwrite = TRUE,
      mode = "thumbnail",
      roi_class_presets = roi_class_presets,
      project_images = records,
      layers = list(),
      channel_sources = project_channel_sources,
      seurat = first
    )
    if (isTRUE(live)) {
      viewer_args$dynamic_tiles <- FALSE
      viewer_args$wait <- wait
      viewer_args$transport <- transport
      viewer_args$name <- "wsi_seurat_project_live_state"
      viewer_args$prediction_context <- project_prediction
      viewer_args$proximity_context <- project_prediction
      return(do.call(wsi_viewer_live, viewer_args))
    }
    return(do.call(wsi_viewer, viewer_args))
  }

  viewer_args <- list(
    slide = first$slide,
    width = width,
    height = height,
    output = output,
    open = open,
    title = title,
    overwrite = TRUE,
    mode = "tiles",
    tile_size = first_record$tile_size %||% tile_size,
    tile_format = first_record$tile_format %||% tile_format,
    tile_url_base = first_record$tile_url_base,
    tile_url_template = first_record$tile_url_template,
    tile_url_style = first_record$tile_url_style %||% "deepzoom",
    tile_overlap = first_record$tile_overlap %||% 1L,
    max_level = first_record$max_level,
    tile_source_label = "Seurat project Deep Zoom tiles",
    roi_class_presets = roi_class_presets,
    project_images = records,
    layers = list(),
    channel_sources = project_channel_sources,
    seurat = first
  )
  if (isTRUE(live)) {
    viewer_args$dynamic_tiles <- dynamic_tiles
    if (isTRUE(dynamic_tiles)) {
      viewer_args$dynamic_tile_format <- "jpg"
    }
    viewer_args$wait <- wait
    viewer_args$transport <- transport
    viewer_args$name <- "wsi_seurat_project_live_state"
    viewer_args$prediction_context <- project_prediction
    viewer_args$proximity_context <- project_prediction
    viewer_args$project_tile_sources <- project_tile_sources
    return(do.call(wsi_viewer_live, viewer_args))
  }
  do.call(wsi_viewer, viewer_args)
}

wsi_seurat_project_dynamic_tile_sources <- function(linked, records,
                                                    tile_size = 512,
                                                    tile_overlap = 1,
                                                    format = "jpg") {
  Map(function(item, record) {
    source <- wsi_dynamic_tile_source(
      item$slide,
      slide_id = record$id %||% record$path %||% item$image_name %||% NULL,
      tile_size = record$tile_size %||% tile_size,
      tile_overlap = record$tile_overlap %||% tile_overlap,
      format = record$tile_format %||% format
    )
    source$name <- record$label %||% source$id
    source$metadata <- list(
      project_item_id = record$id %||% source$id,
      id = record$id %||% source$id,
      label = record$label %||% source$name,
      path = record$path %||% item$image_path %||% "",
      backend = record$backend %||% item$slide$backend %||% "dynamic",
      type = record$type %||% "seurat_spatial_section",
      status = "live dynamic tiles",
      message = "Full-resolution image tiles are served on demand by the live R session.",
      active = isTRUE(record$active),
      mpp = record$mpp %||% item$mpp %||% item$pixel_size %||% NULL,
      objective_power = record$objective_power %||% NULL
    )
    source
  }, linked, records)
}

#' @export
print.wsi_seurat_spatial <- function(x, ...) {
  source_name <- x$source_name %||% "Seurat"
  feature_plural <- if (identical(x$feature_type %||% "spot", "cell")) "cells" else "spots"
  cat("<", tolower(source_name), "_spatial>\n", sep = "")
  cat("  image:     ", x$image_path %||% "<slide>", "\n", sep = "")
  cat("  image key: ", x$image_name %||% NA_character_, "\n", sep = "")
  cat("  reduction: ", x$reduction, " dims ", paste(x$dims, collapse = ","), "\n", sep = "")
  cat("  ", feature_plural, ":     ", format(x$spot_count, big.mark = ","), "\n", sep = "")
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

wsi_prefer_static_spatial_tiles <- function(dynamic_tiles, context = "spatial viewer") {
  if (!isTRUE(dynamic_tiles)) {
    return(FALSE)
  }
  force_dynamic <- identical(Sys.getenv("WSITOOLS_FORCE_DYNAMIC_TILES", unset = "false"), "true")
  if (isTRUE(force_dynamic)) {
    return(TRUE)
  }
  if (wsi_has_vips()) {
    wsi_warn(paste0(
      "Using prebuilt Deep Zoom tiles for smoother ",
      context,
      " performance. Dynamic tiles remain available only when ",
      "`Sys.setenv(WSITOOLS_FORCE_DYNAMIC_TILES = \"true\")` is set."
    ))
    return(FALSE)
  }
  TRUE
}

wsi_seurat_spots_layer <- function(linked, visible = TRUE, opacity = 0.85) {
  source_name <- linked$source_name %||% "Seurat"
  feature_type <- linked$feature_type %||% "spot"
  feature_plural <- if (identical(feature_type, "cell")) "cells" else "spots"
  feature_count <- suppressWarnings(as.integer(linked$spot_count %||% nrow(linked$spots) %||% 0L))
  layer <- wsi_seurat_spatial_mask_layer(
    linked = linked,
    name = paste(source_name, "spatial", feature_plural),
    id = "seurat_spots",
    visible = visible,
    opacity = opacity,
    feature_count = feature_count
  )
  layer$metadata <- c(
    layer$metadata %||% list(),
    list(
      source_name = source_name,
      feature_type = feature_type,
      feature_label = feature_plural,
      reduction = linked$reduction,
      image_name = linked$image_name,
      coordinate_mapping = linked$coordinate_mapping,
      mpp = linked$mpp %||% linked$pixel_size %||% NULL,
      scale_metadata = linked$scale_metadata %||% NULL,
      feature_count = feature_count,
      display_mode = "raster_mask",
      vector_rendering = FALSE
    )
  )
  layer
}

wsi_seurat_spots_channel_source <- function(linked,
                                            output_dir,
                                            output_html = NULL,
                                            name = NULL,
                                            id = "seurat_spots",
                                            visible = TRUE,
                                            opacity = 0.85,
                                            tile_size = 254,
                                            max_dimension = 8192L,
                                            radius_scale = 0.5,
                                            alpha = 0.65,
                                            dynamic = FALSE,
                                            rebuild = FALSE,
                                            overwrite = FALSE,
                                            target_path = NULL,
                                            project_image_id = NULL) {
  if (!wsi_has_vips()) {
    wsi_warn(
      wsi_backend_action_message(
        "Spatial coordinate OME-TIFF overlays require libvips. The viewer will open without the tiled coordinate mask.",
        backend = "vips"
      )
    )
    return(NULL)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_dir)) {
    wsi_abort(sprintf("Could not create spatial coordinate mask directory: %s", output_dir))
  }
  source_name <- linked$source_name %||% "Spatial"
  feature_type <- linked$feature_type %||% "spot"
  feature_plural <- if (identical(feature_type, "cell")) "cells" else "spots"
  name <- name %||% paste(source_name, "spatial", feature_plural, "mask")
  id <- wsi_channel_source_id(id, name)
  stem <- wsi_safe_id(id, "seurat_spots")
  mask_output <- file.path(output_dir, paste0(stem, ".ome.tif"))
  tile_dir <- file.path(output_dir, paste0(stem, "_deepzoom"))
  raster_result <- wsi_seurat_spatial_mask_raster(
    linked = linked,
    max_dimension = max_dimension,
    radius_scale = radius_scale,
    alpha = alpha
  )
  ome_tile_size <- max(16L, min(512L, tile_size, raster_result$mask_width, raster_result$mask_height))
  ome_tile_size <- max(16L, as.integer(floor(ome_tile_size / 16L) * 16L))
  if (!file.exists(mask_output) || isTRUE(rebuild)) {
    temp_png <- tempfile(fileext = ".png")
    on.exit(unlink(temp_png), add = TRUE)
    old_par <- NULL
    grDevices::png(temp_png, width = raster_result$mask_width, height = raster_result$mask_height, bg = "transparent")
    tryCatch({
      old_par <- graphics::par(no.readonly = TRUE)
      graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
      graphics::plot.new()
      graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = NA)
      graphics::rasterImage(grDevices::as.raster(raster_result$raster), 0, 0, 1, 1, interpolate = FALSE)
    }, finally = {
      if (!is.null(old_par)) {
        try(graphics::par(old_par), silent = TRUE)
      }
      grDevices::dev.off()
    })
    wsi_convert(
      input = temp_png,
      output = mask_output,
      format = "ome-tiff",
      backend = "vips",
      tile_size = ome_tile_size,
      compression = "lzw",
      pyramid = TRUE,
      bigtiff = TRUE,
      overwrite = TRUE
    )
  }
  mask_slide <- wsi_open(mask_output)
  slide_path <- target_path %||% linked$image_path %||% linked$slide$path %||% NULL
  if (!is.null(slide_path) && nzchar(slide_path)) {
    slide_path <- normalizePath(slide_path, winslash = "/", mustWork = FALSE)
  }
  legend <- list(list(
    label = feature_plural,
    value = "1",
    colour = "#2B6CB0",
    count = raster_result$feature_count
  ))
  source_metadata <- list(
    kind = "mask",
    source_type = "seurat_spots",
    transparent_background = TRUE,
    legend = legend,
    selected_values = "1",
    extent = list(x = 0, y = 0, width = raster_result$slide_width, height = raster_result$slide_height),
    mask_downsample = raster_result$downsample,
    source_mask = normalizePath(mask_output, winslash = "/", mustWork = FALSE),
    target_path = slide_path,
    base_slide_path = slide_path,
    project_image_id = project_image_id %||% "active_project_image",
    feature_type = feature_type,
    feature_count = raster_result$feature_count,
    represented_count = raster_result$represented_count,
    display_mode = "ome_tiff_mask"
  )
  if (isTRUE(dynamic)) {
    source <- wsi_dynamic_tile_source(
      mask_slide,
      slide_id = id,
      tile_size = tile_size,
      tile_overlap = 1,
      format = "png"
    )
    source$name <- name
    source$kind <- "mask"
    source$visible <- visible
    source$opacity <- opacity
    source$colour <- "#ffffff"
    source$extent <- source_metadata$extent
    source$metadata <- source_metadata
    return(list(
      source = source,
      mask = list(
        output = normalizePath(mask_output, winslash = "/", mustWork = FALSE),
        mask_width = raster_result$mask_width,
        mask_height = raster_result$mask_height,
        slide_width = raster_result$slide_width,
        slide_height = raster_result$slide_height,
        downsample = raster_result$downsample,
        represented_count = raster_result$represented_count
      ),
      tiles = NULL
    ))
  }
  tiles <- wsi_create_deepzoom_tiles(
    slide = mask_slide,
    tile_dir = tile_dir,
    tile_size = tile_size,
    tile_overlap = 1,
    tile_format = "png",
    quality = 90,
    rebuild = isTRUE(rebuild)
  )
  mask_info <- file.info(mask_output)
  mask_cache_key <- paste(
    normalizePath(mask_output, winslash = "/", mustWork = FALSE),
    suppressWarnings(as.numeric(mask_info$size %||% NA_real_)),
    if (!is.null(mask_info$mtime)) format(mask_info$mtime, tz = "UTC", usetz = TRUE) else NA_character_,
    sep = "|"
  )
  source_metadata$cache_key <- mask_cache_key
  source <- wsi_channel_source(
    name = name,
    id = id,
    type = "deepzoom",
    tile_url_base = if (!is.null(output_html)) wsi_tile_base_url(tile_dir, output_html) else wsi_file_url(tiles$tiles),
    width = raster_result$mask_width,
    height = raster_result$mask_height,
    tile_size = tile_size,
    tile_format = "png",
    max_level = wsi_dz_max_level(raster_result$mask_width, raster_result$mask_height),
    tile_overlap = as.integer(tiles$overlap %||% 1L),
    visible = visible,
    opacity = opacity,
    colour = "#ffffff",
    metadata = source_metadata
  )
  source$cache_key <- mask_cache_key
  list(
    source = source,
    mask = list(
      output = normalizePath(mask_output, winslash = "/", mustWork = FALSE),
      mask_width = raster_result$mask_width,
      mask_height = raster_result$mask_height,
      slide_width = raster_result$slide_width,
      slide_height = raster_result$slide_height,
      downsample = raster_result$downsample,
      represented_count = raster_result$represented_count
    ),
    tiles = tiles
  )
}

wsi_seurat_spatial_mask_raster <- function(linked,
                                           colours = NULL,
                                           max_dimension = 4096L,
                                           radius_scale = 0.5,
                                           alpha = 0.65) {
  spots <- linked$spots
  feature_count <- suppressWarnings(as.integer(linked$spot_count %||% nrow(spots) %||% 0L))
  slide_width <- suppressWarnings(as.numeric(linked$slide$dimensions[["width"]] %||% max(spots$x, na.rm = TRUE)))
  slide_height <- suppressWarnings(as.numeric(linked$slide$dimensions[["height"]] %||% max(spots$y, na.rm = TRUE)))
  if (!is.finite(slide_width) || slide_width <= 0) {
    slide_width <- max(1, suppressWarnings(max(as.numeric(spots$x), na.rm = TRUE)))
  }
  if (!is.finite(slide_height) || slide_height <= 0) {
    slide_height <- max(1, suppressWarnings(max(as.numeric(spots$y), na.rm = TRUE)))
  }
  max_dimension <- max(256L, as.integer(max_dimension %||% 4096L))
  downsample <- max(1, ceiling(max(slide_width, slide_height) / max_dimension))
  mask_width <- max(1L, as.integer(ceiling(slide_width / downsample)))
  mask_height <- max(1L, as.integer(ceiling(slide_height / downsample)))
  raster <- matrix("#00000000", nrow = mask_height, ncol = mask_width)
  x <- suppressWarnings(as.numeric(spots$x %||% spots$slide_x))
  y <- suppressWarnings(as.numeric(spots$y %||% spots$slide_y))
  keep <- is.finite(x) & is.finite(y) & x >= 0 & y >= 0 & x <= slide_width & y <= slide_height
  if (any(keep)) {
    x <- x[keep]
    y <- y[keep]
    if (is.null(colours)) {
      colours <- spots$colour %||% spots$color %||% spots$base_colour %||% spots$base_color %||% "#2B6CB0"
      colours <- as.character(colours)[keep]
    } else {
      colours <- rep(as.character(colours), length.out = nrow(spots))[keep]
    }
    colours <- vapply(colours, wsi_colour_to_hex, character(1), name = "colour")
    colours <- grDevices::adjustcolor(colours, alpha.f = max(0, min(1, alpha)))
    px <- pmin(mask_width, pmax(1L, as.integer(round(x / downsample)) + 1L))
    py <- pmin(mask_height, pmax(1L, as.integer(round(y / downsample)) + 1L))
    radius_values <- suppressWarnings(as.numeric(spots$radius %||% spots$spot_radius %||% linked$spot_radius %||% NA_real_))
    if (length(radius_values) != nrow(spots)) {
      fallback_radius <- if (length(radius_values) && is.finite(radius_values[[1L]])) {
        radius_values[[1L]]
      } else {
        linked$spot_radius %||% 2
      }
      radius_values <- rep(fallback_radius, nrow(spots))
    }
    radius_values <- radius_values[keep]
    radius_values[!is.finite(radius_values) | radius_values <= 0] <- linked$spot_radius %||% 2
    pr <- pmax(1L, as.integer(round(radius_values * radius_scale / downsample)))
    for (i in seq_along(px)) {
      rr <- pr[[i]]
      x0 <- max(1L, px[[i]] - rr)
      x1 <- min(mask_width, px[[i]] + rr)
      y0 <- max(1L, py[[i]] - rr)
      y1 <- min(mask_height, py[[i]] + rr)
      if (rr <= 1L) {
        raster[py[[i]], px[[i]]] <- colours[[i]]
      } else {
        xs <- x0:x1
        ys <- y0:y1
        disk <- outer(ys - py[[i]], xs - px[[i]], function(a, b) a * a + b * b <= rr * rr)
        sub <- raster[ys, xs, drop = FALSE]
        sub[disk] <- colours[[i]]
        raster[ys, xs] <- sub
      }
    }
  }
  list(
    raster = raster,
    slide_width = slide_width,
    slide_height = slide_height,
    mask_width = mask_width,
    mask_height = mask_height,
    downsample = downsample,
    feature_count = feature_count,
    represented_count = sum(keep)
  )
}

wsi_seurat_spatial_mask_layer <- function(linked, name, id = "seurat_spots",
                                          colours = NULL, visible = TRUE,
                                          opacity = 0.85,
                                          feature_count = NULL,
                                          max_dimension = 4096L,
                                          radius_scale = 0.5,
                                          alpha = 0.65) {
  raster_result <- wsi_seurat_spatial_mask_raster(
    linked = linked,
    colours = colours,
    max_dimension = max_dimension,
    radius_scale = radius_scale,
    alpha = alpha
  )
  feature_count <- feature_count %||% raster_result$feature_count
  layer <- wsi_viewer_layer_payload(
    name = name,
    data = grDevices::as.raster(raster_result$raster),
    type = "image",
    slide = linked$slide,
    visible = visible,
    opacity = opacity,
    extent = c(xmin = 0, ymin = 0, xmax = raster_result$slide_width, ymax = raster_result$slide_height)
  )
  layer$id <- id
  layer$source_type <- "seurat_spots"
  layer$count <- feature_count
  layer$metadata <- list(
    kind = "spatial_coordinate_mask",
    raster_width = raster_result$mask_width,
    raster_height = raster_result$mask_height,
    downsample = raster_result$downsample,
    represented_count = raster_result$represented_count,
    feature_count = feature_count,
    radius_scale = radius_scale,
    transparent_background = TRUE
  )
  layer
}

wsi_seurat_plot_browser_payload <- function(plot, max_points = 20000L) {
  if (!is.list(plot)) {
    return(plot)
  }
  points <- plot$points %||% NULL
  if (!is.data.frame(points) || !nrow(points)) {
    return(plot)
  }
  original_count <- nrow(points)
  drop_cols <- intersect(
    names(points),
    c("cluster_values", "base_colour", "base_color")
  )
  if (length(drop_cols)) {
    points[drop_cols] <- NULL
  }
  max_points <- max(1000L, as.integer(max_points %||% 20000L))
  if (nrow(points) > max_points) {
    idx <- unique(as.integer(round(seq(1, nrow(points), length.out = max_points))))
    points <- points[idx, , drop = FALSE]
    plot$sampled <- TRUE
  } else {
    plot$sampled <- FALSE
  }
  plot$points <- points
  plot$point_count <- as.integer(original_count)
  plot$displayed_point_count <- as.integer(nrow(points))
  plot
}

wsi_viewer_seurat_config <- function(seurat = NULL) {
  if (is.null(seurat)) {
    return(list(enabled = FALSE, plots = list(), spot_count = 0L))
  }
  if (!inherits(seurat, "wsi_seurat_spatial") && !inherits(seurat, "wsi_spatial_object")) {
    wsi_abort("`seurat` viewer configuration must be created by `wsi_link_seurat_image()` or another wsiTools spatial-object linker.")
  }
  plots <- seurat$plots %||% list()
  if (!length(plots) && !is.null(seurat$pca)) {
    plots <- list(seurat$pca)
  }
  plot_max_points <- if (!is.null(seurat$spot_mask)) 15000L else 50000L
  plots <- lapply(plots, wsi_seurat_plot_browser_payload, max_points = plot_max_points)
  list(
    enabled = TRUE,
    source_name = seurat$source_name %||% "Seurat",
    feature_type = seurat$feature_type %||% "spot",
    feature_label = if (identical(seurat$feature_type %||% "spot", "cell")) "cells" else "spots",
    image_name = seurat$image_name,
    reduction = seurat$reduction,
    dims = as.integer(seurat$dims),
    component_names = as.character(seurat$component_names),
    spot_layer_id = seurat$spot_layer_id %||% "seurat_spots",
    spot_count = as.integer(seurat$spot_count),
    displayed_spot_count = as.integer(seurat$displayed_spot_count),
    spot_radius = as.numeric(seurat$spot_radius),
    mpp = seurat$mpp %||% seurat$pixel_size %||% NULL,
    pixel_size = seurat$pixel_size %||% seurat$mpp %||% NULL,
    scale_metadata = seurat$scale_metadata %||% NULL,
    gene_expression = wsi_seurat_gene_expression_config(seurat$gene_expression),
    clusters = seurat$clusters %||% wsi_spatial_cluster_config(seurat$cluster_values %||% data.frame()),
    plots = plots
  )
}

wsi_normalize_seurat_project_linked <- function(linked) {
  if (inherits(linked, "wsi_seurat_spatial")) {
    linked <- list(linked)
  }
  if (!is.list(linked) || inherits(linked, "data.frame")) {
    wsi_abort("`linked` must be a `wsi_seurat_spatial` object or a list of them.")
  }
  if (!length(linked)) {
    wsi_abort("`linked` must contain at least one linked Seurat section.")
  }
  for (i in seq_along(linked)) {
    if (!inherits(linked[[i]], "wsi_seurat_spatial")) {
      wsi_abort("Every entry in `linked` must be returned by `wsi_link_seurat_image()`.")
    }
  }
  linked
}

wsi_link_seurat_project_sections <- function(seurat, images, image_names = NULL,
                                             spatial_dir = NULL,
                                             scalefactors_json = NULL,
                                             tissue_positions = NULL,
                                             reduction = "pca", dims = c(1L, 2L),
                                             coordinate_scale = "auto",
                                             scale_x = NULL, scale_y = NULL,
                                             coordinate_flip = "none",
                                             coordinate_rotation = 0,
                                             coordinate_transform = "none",
                                             spot_genes = NULL, default_gene = NULL,
                                             spot_radius = NULL, max_points = 100000L,
                                             colour_by = "component_1") {
  if (is.null(seurat)) {
    wsi_abort("`seurat` is required when `linked` is not supplied.")
  }
  images <- wsi_seurat_project_images(images)
  n <- length(images)
  image_names <- wsi_seurat_project_image_names(images, image_names)
  objects <- wsi_seurat_project_objects(seurat, n)

  linked <- vector("list", n)
  for (i in seq_len(n)) {
    image_name_i <- image_names[[i]]
    if (is.na(image_name_i) || !nzchar(image_name_i)) {
      image_name_i <- NULL
    }
    linked[[i]] <- wsi_link_seurat_image(
      seurat = objects[[i]],
      image = images[[i]],
      image_name = image_name_i,
      spatial_dir = wsi_seurat_project_index_value(spatial_dir, i, n, "spatial_dir"),
      scalefactors_json = wsi_seurat_project_index_value(scalefactors_json, i, n, "scalefactors_json"),
      tissue_positions = wsi_seurat_project_index_value(tissue_positions, i, n, "tissue_positions"),
      reduction = reduction,
      dims = dims,
      coordinate_scale = coordinate_scale,
      scale_x = scale_x,
      scale_y = scale_y,
      coordinate_flip = coordinate_flip,
      coordinate_rotation = coordinate_rotation,
      coordinate_transform = coordinate_transform,
      spot_genes = spot_genes,
      default_gene = default_gene,
      spot_radius = spot_radius,
      max_points = max_points,
      colour_by = colour_by
    )
  }
  if (length(image_names) == n && !anyNA(image_names)) {
    names(linked) <- image_names
  }
  linked
}

wsi_seurat_project_images <- function(images) {
  if (is.null(images) || !length(images)) {
    wsi_abort("`images` must contain one or more image paths or `wsi_slide` objects.")
  }
  if (inherits(images, "wsi_slide")) {
    return(list(images))
  }
  if (is.character(images)) {
    if (anyNA(images) || any(!nzchar(images))) {
      wsi_abort("`images` must not contain missing or empty paths.")
    }
    out <- as.list(images)
    names(out) <- names(images)
    return(out)
  }
  if (is.list(images) && !inherits(images, "data.frame")) {
    for (i in seq_along(images)) {
      item <- images[[i]]
      ok <- inherits(item, "wsi_slide") ||
        (is.character(item) && length(item) == 1L && !is.na(item) && nzchar(item))
      if (!ok) {
        wsi_abort("Each `images` entry must be a file path or a `wsi_slide` object.")
      }
    }
    return(images)
  }
  wsi_abort("`images` must be a character vector, list, or `wsi_slide` object.")
}

wsi_seurat_project_image_names <- function(images, image_names = NULL) {
  n <- length(images)
  if (is.null(image_names)) {
    image_names <- names(images)
  }
  if (is.null(image_names) || length(image_names) != n ||
      anyNA(image_names) || any(!nzchar(image_names))) {
    if (n == 1L) {
      return(NA_character_)
    }
    wsi_abort("For multiple Seurat images, supply `image_names` or use a named `images` vector/list.")
  }
  as.character(image_names)
}

wsi_seurat_project_objects <- function(seurat, n) {
  is_single <- isS4(seurat) ||
    inherits(seurat, "Seurat") ||
    (is.list(seurat) && any(c("images", "reductions", "assays") %in% names(seurat)))
  if (is_single) {
    return(rep(list(seurat), n))
  }
  if (is.list(seurat) && length(seurat) == n) {
    return(seurat)
  }
  if (is.list(seurat) && length(seurat) == 1L) {
    return(rep(seurat, n))
  }
  wsi_abort("`seurat` must be one object or a list with one object per image.")
}

wsi_seurat_project_index_value <- function(x, i, n, name) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x) && !inherits(x, "data.frame")) {
    if (length(x) == 1L) {
      return(x[[1L]])
    }
    if (length(x) == n) {
      return(x[[i]])
    }
    wsi_abort(sprintf("`%s` must have length 1 or match the number of images.", name))
  }
  if (length(x) == 1L) {
    return(x[[1L]])
  }
  if (length(x) == n) {
    return(x[[i]])
  }
  wsi_abort(sprintf("`%s` must have length 1 or match the number of images.", name))
}

wsi_seurat_project_labels <- function(labels = NULL, linked) {
  n <- length(linked)
  if (is.null(labels)) {
    labels <- names(linked)
    if (is.null(labels) || length(labels) != n) {
      labels <- rep("", n)
    }
    fallback <- vapply(seq_along(linked), function(i) {
      linked[[i]]$image_name %||%
        tools::file_path_sans_ext(basename(linked[[i]]$image_path %||% "")) %||%
        sprintf("Tissue %d", i)
    }, character(1))
    labels[is.na(labels) | !nzchar(labels)] <- fallback[is.na(labels) | !nzchar(labels)]
    labels[is.na(labels) | !nzchar(labels)] <- sprintf("Tissue %d", which(is.na(labels) | !nzchar(labels)))
    return(as.character(labels))
  }
  if (!is.character(labels) || length(labels) != n || anyNA(labels) || any(!nzchar(labels))) {
    wsi_abort("`labels` must be `NULL` or a character vector with one label per linked section.")
  }
  labels
}

wsi_seurat_project_record_key <- function(record, section = NULL) {
  item_id <- wsi_safe_id(record$id %||% record$path %||% record$label %||% "image", "image")
  section_id <- if (is.null(section)) {
    "image"
  } else {
    wsi_safe_id(section$id %||% section$label %||% "section", "section")
  }
  paste(item_id, section_id, sep = "::")
}

wsi_seurat_project_scoped_linked <- function(linked, record, item_index,
                                             section = NULL,
                                             section_index = -1L) {
  scoped <- linked
  spots <- scoped$spots
  if (is.data.frame(spots) && nrow(spots)) {
    project_key <- wsi_seurat_project_record_key(record, section)
    project_image <- as.character(record$label %||% record$id %||% record$path %||% "")
    project_section <- if (is.null(section)) "" else {
      as.character(section$label %||% section$id %||% "")
    }
    original_id <- as.character(spots$original_id %||% spots$barcode %||% spots$id %||% spots$label %||% seq_len(nrow(spots)))
    spots$original_id <- original_id
    spots$feature_id <- as.character(spots$feature_id %||% spots$barcode %||% original_id)
    spots$project_key <- project_key
    spots$wsi_project_key <- project_key
    spots$project_image <- project_image
    spots$project_section <- project_section
    spots$image_id <- as.character(record$id %||% project_image)
    spots$section_id <- if (is.null(section)) "image" else as.character(section$id %||% project_section)
    spots$sample_id <- as.character(scoped$image_name %||% project_image)
    spots$project_image_index <- as.integer(item_index)
    spots$project_section_index <- as.integer(section_index)
    spots$id <- paste(project_key, original_id, sep = "::")
    scoped$spots <- spots
    scoped$displayed_spot_count <- nrow(spots)
  }
  scoped$project_key <- wsi_seurat_project_record_key(record, section)
  scoped$project_image <- as.character(record$label %||% record$id %||% record$path %||% "")
  scoped$project_section <- if (is.null(section)) "" else as.character(section$label %||% section$id %||% "")
  scoped$project_image_index <- as.integer(item_index)
  scoped$project_section_index <- as.integer(section_index)
  scoped
}

wsi_seurat_project_rbind_fill <- function(tables) {
  tables <- tables[vapply(tables, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
  if (!length(tables)) {
    return(data.frame())
  }
  columns <- unique(unlist(lapply(tables, names), use.names = FALSE))
  aligned <- lapply(tables, function(x) {
    missing <- setdiff(columns, names(x))
    for (column in missing) {
      x[[column]] <- NA
    }
    x[, columns, drop = FALSE]
  })
  out <- do.call(rbind, aligned)
  row.names(out) <- NULL
  out
}

wsi_seurat_project_prediction_context <- function(linked, records) {
  if (!length(linked) || !length(records)) {
    return(NULL)
  }
  n <- min(length(linked), length(records))
  scoped <- lapply(seq_len(n), function(i) {
    wsi_seurat_project_scoped_linked(
      linked[[i]],
      records[[i]],
      item_index = i - 1L,
      section = NULL,
      section_index = -1L
    )
  })
  spots <- wsi_seurat_project_rbind_fill(lapply(scoped, function(x) x$spots %||% data.frame()))
  out <- scoped[[1L]]
  out$spots <- spots
  out$spot_count <- sum(vapply(scoped, function(x) as.integer(x$spot_count %||% nrow(x$spots %||% data.frame())), integer(1)))
  out$displayed_spot_count <- nrow(spots)
  out$project_sections <- scoped
  out$source_name <- paste0(out$source_name %||% "Spatial", " project")
  class(out) <- unique(c("wsi_spatial_project", class(out)))
  out
}

wsi_seurat_project_records <- function(linked, output, labels,
                                       mode = c("tiles", "thumbnail"),
                                       width = 768, height = NULL,
                                       tile_dir = NULL, tile_size = 512,
                                       tile_format = c("jpg", "png"),
                                       quality = 90, rebuild = FALSE,
                                       tile_overlap = NULL,
                                       dynamic_channel_sources = FALSE) {
  mode <- match.arg(mode)
  tile_format <- match.arg(tile_format)
  if (identical(mode, "tiles")) {
    tile_dir <- tile_dir %||% wsi_default_tile_dir(output)
    if (!dir.exists(tile_dir)) {
      dir.create(tile_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!dir.exists(tile_dir)) {
      wsi_abort(sprintf("Could not create tile directory: %s", tile_dir))
    }
  }

  records <- vector("list", length(linked))
  for (i in seq_along(linked)) {
    item <- linked[[i]]
    label <- labels[[i]]
    slide <- item$slide
    wsi_check_slide(slide)
    record <- list(
      id = paste0("seurat_project_", wsi_safe_id(label, paste0("section_", i))),
      label = label,
      path = if (!is.null(slide$path) && length(slide$path) == 1L && !is.na(slide$path)) slide$path else "",
      backend = slide$backend,
      type = "seurat_spatial_section",
      status = sprintf("%s spatial spots", item$source_name %||% "Seurat"),
      width = unname(as.numeric(slide$dimensions[["width"]])),
      height = unname(as.numeric(slide$dimensions[["height"]])),
      mpp = item$mpp %||% item$pixel_size %||% NULL,
      pixel_size = item$pixel_size %||% item$mpp %||% NULL,
      content_bbox = wsi_seurat_project_spot_bbox(item)
    )

    if (identical(mode, "thumbnail")) {
      preview <- tryCatch(
        wsi_viewer_thumbnail_data_uri(slide, width = width, height = height),
        error = function(err) NULL
      )
      record$image_data_uri <- preview
      record$navigator_image_data_uri <- preview
    } else {
      if (identical(slide$backend, "mock")) {
        wsi_abort("Tiled Seurat project viewing is not available for mock slides; use `mode = \"thumbnail\"` for tests.")
      }
      section_dir <- file.path(tile_dir, sprintf("%02d_%s", i, wsi_safe_id(label, "section")))
      requested_overlap <- tile_overlap %||% 1L
      tiles <- wsi_create_deepzoom_tiles(
        slide = slide,
        tile_dir = section_dir,
        tile_size = tile_size,
        tile_overlap = requested_overlap,
        tile_format = tile_format,
        quality = quality,
        rebuild = rebuild
      )
      record$tile_size <- as.integer(tile_size)
      record$tile_format <- tile_format
      record$tile_url_base <- wsi_seurat_project_tile_base_url(section_dir, output)
      record$tile_url_template <- NULL
      record$tile_url_style <- "deepzoom"
      record$tile_overlap <- as.integer(tiles$overlap %||% requested_overlap)
      record$min_level <- 0L
      record$max_level <- wsi_dz_max_level(record$width, record$height)
      record$navigator_image_data_uri <- wsi_viewer_navigator_data_uri(slide, width = 512)
    }
    scoped_item <- wsi_seurat_project_scoped_linked(
      item,
      record,
      item_index = i - 1L,
      section = NULL,
      section_index = -1L
    )
    spot_channel <- NULL
    spot_channel_id <- paste0(record$id, "_seurat_spots")
    scoped_item$spot_layer_id <- spot_channel_id
    if (identical(mode, "tiles")) {
      spot_channel <- wsi_seurat_spots_channel_source(
        linked = scoped_item,
        output_dir = file.path(tile_dir, "spatial_masks"),
        output_html = output,
        id = spot_channel_id,
        name = paste(label, scoped_item$source_name %||% "Spatial", "mask"),
        visible = i == 1L,
        opacity = 0.85,
        dynamic = isTRUE(dynamic_channel_sources),
        rebuild = isTRUE(rebuild),
        target_path = record$path,
        project_image_id = record$id
      )
    }
    record$project_key <- scoped_item$project_key
    record$layers <- list()
    record$channel_sources <- if (is.null(spot_channel)) list() else list(spot_channel$source)
    record$spot_mask <- if (is.null(spot_channel)) NULL else spot_channel$mask
    record$seurat <- wsi_viewer_seurat_config(scoped_item)
    records[[i]] <- record
  }
  records[[1L]]$active <- TRUE
  records
}

wsi_seurat_project_tile_base_url <- function(tile_dir, output) {
  tile_files <- file.path(tile_dir, "slide_files")
  output_dir <- normalizePath(dirname(output), winslash = "/", mustWork = FALSE)
  tile_files_norm <- normalizePath(tile_files, winslash = "/", mustWork = FALSE)
  prefix <- paste0(gsub("/+$", "", output_dir), "/")
  if (startsWith(tile_files_norm, prefix)) {
    return(wsi_url_encode_path(substring(tile_files_norm, nchar(prefix) + 1L)))
  }
  wsi_file_url(tile_files)
}

wsi_seurat_project_spot_bbox <- function(linked, pad_fraction = 0.05) {
  spots <- linked$spots
  if (is.null(spots) || !nrow(spots) || !all(c("x", "y") %in% names(spots))) {
    return(NULL)
  }
  x <- as.numeric(spots$x)
  y <- as.numeric(spots$y)
  keep <- is.finite(x) & is.finite(y)
  if (!any(keep)) {
    return(NULL)
  }
  x <- x[keep]
  y <- y[keep]
  slide <- linked$slide
  width <- as.numeric(slide$dimensions[["width"]])
  height <- as.numeric(slide$dimensions[["height"]])
  xr <- range(x)
  yr <- range(y)
  pad <- max(
    as.numeric(linked$spot_radius %||% 8) * 6,
    diff(xr) * pad_fraction,
    diff(yr) * pad_fraction,
    1
  )
  list(
    xmin = max(0, xr[[1L]] - pad),
    ymin = max(0, yr[[1L]] - pad),
    xmax = min(width, xr[[2L]] + pad),
    ymax = min(height, yr[[2L]] + pad)
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

wsi_seurat_coordinates_from_boundaries <- function(image_obj) {
  boundaries <- wsi_seurat_slot(image_obj, "boundaries")
  if (is.null(boundaries) || !length(boundaries)) {
    return(NULL)
  }
  candidates <- c("centroids", "Centroids", names(boundaries))
  candidates <- unique(candidates[nzchar(candidates %||% "")])
  for (name in candidates) {
    boundary <- boundaries[[name]]
    if (is.null(boundary)) {
      next
    }
    coords <- wsi_seurat_slot(boundary, "coords")
    cells <- wsi_seurat_slot(boundary, "cells")
    if (is.null(coords) || (!is.matrix(coords) && !is.data.frame(coords))) {
      next
    }
    coords <- as.data.frame(coords, stringsAsFactors = FALSE)
    if (!nrow(coords) || ncol(coords) < 2L) {
      next
    }
    x_col <- wsi_seurat_first_column(coords, c("x", "X", "imagecol", "col"))
    y_col <- wsi_seurat_first_column(coords, c("y", "Y", "imagerow", "row"))
    if (is.null(x_col) || is.null(y_col)) {
      x_col <- names(coords)[[1L]]
      y_col <- names(coords)[[2L]]
    }
    if (is.null(cells) || length(cells) != nrow(coords)) {
      cells <- rownames(coords) %||% as.character(seq_len(nrow(coords)))
    }
    out <- data.frame(
      barcode = as.character(cells),
      x = suppressWarnings(as.numeric(coords[[x_col]])),
      y = suppressWarnings(as.numeric(coords[[y_col]])),
      stringsAsFactors = FALSE
    )
    out <- out[is.finite(out$x) & is.finite(out$y) & nzchar(out$barcode), , drop = FALSE]
    if (!nrow(out)) {
      next
    }
    row.names(out) <- NULL
    attr(out, "coordinate_space") <- "fullres"
    attr(out, "coordinate_source") <- paste0("seurat_image_boundaries$", name)
    attr(out, "registered_coordinates") <- TRUE
    attr(out, "id_column") <- "cells"
    attr(out, "x_column") <- x_col
    attr(out, "y_column") <- y_col
    return(out)
  }
  NULL
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
  attr(out, "id_column") <- barcode_col
  attr(out, "x_column") <- x_col
  attr(out, "y_column") <- y_col
  out
}

wsi_seurat_coordinates_from_metadata <- function(seurat) {
  meta <- wsi_seurat_slot(seurat, "meta.data")
  if (is.null(meta) || !is.data.frame(meta) || !nrow(meta)) {
    return(NULL)
  }
  coords <- as.data.frame(meta, stringsAsFactors = FALSE)
  id_col <- wsi_seurat_first_column(coords, c(
    "barcode", "barcodes", "cell", "cells", "cell_id", "cellid",
    "spot", "spot_id", "feature_id"
  ))
  ids <- if (!is.null(id_col)) {
    as.character(coords[[id_col]])
  } else {
    rownames(coords) %||% as.character(seq_len(nrow(coords)))
  }
  name_map <- stats::setNames(names(coords), tolower(gsub("[^[:alnum:]]+", "", names(coords))))
  find_col <- function(candidates) {
    keys <- tolower(gsub("[^[:alnum:]]+", "", candidates))
    hit <- name_map[keys]
    hit <- hit[!is.na(hit)]
    if (length(hit)) unname(hit[[1L]]) else NULL
  }
  pairs <- list(
    list(x = c("registered_x", "registration_x", "x_registered", "x_registration"),
         y = c("registered_y", "registration_y", "y_registered", "y_registration"),
         space = "fullres", registered = TRUE),
    list(x = c("slide_x", "x_slide", "slide_col", "slide_column"),
         y = c("slide_y", "y_slide", "slide_row"),
         space = "fullres", registered = TRUE),
    list(x = c("global_x", "x_global"),
         y = c("global_y", "y_global"),
         space = "fullres", registered = TRUE),
    list(x = c("fullres_x", "x_fullres", "pxl_col_in_fullres"),
         y = c("fullres_y", "y_fullres", "pxl_row_in_fullres"),
         space = "fullres", registered = TRUE),
    list(x = c("centroid_x", "x_centroid", "cell_x", "x_cell", "center_x", "x_center"),
         y = c("centroid_y", "y_centroid", "cell_y", "y_cell", "center_y", "y_center"),
         space = "unknown", registered = TRUE),
    list(x = c("image_x", "x_image", "imagecol", "col"),
         y = c("image_y", "y_image", "imagerow", "row"),
         space = "unknown", registered = FALSE),
    list(x = c("x"),
         y = c("y"),
         space = "unknown", registered = FALSE)
  )
  for (pair in pairs) {
    x_col <- find_col(pair$x)
    y_col <- find_col(pair$y)
    if (is.null(x_col) || is.null(y_col)) {
      next
    }
    out <- data.frame(
      barcode = ids,
      x = suppressWarnings(as.numeric(coords[[x_col]])),
      y = suppressWarnings(as.numeric(coords[[y_col]])),
      stringsAsFactors = FALSE
    )
    out <- out[is.finite(out$x) & is.finite(out$y) & nzchar(out$barcode), , drop = FALSE]
    if (!nrow(out)) {
      next
    }
    row.names(out) <- NULL
    attr(out, "coordinate_space") <- pair$space
    attr(out, "coordinate_source") <- "seurat_meta.data"
    attr(out, "registered_coordinates") <- isTRUE(pair$registered)
    attr(out, "id_column") <- id_col %||% "rownames(meta.data)"
    attr(out, "x_column") <- x_col
    attr(out, "y_column") <- y_col
    return(out)
  }
  NULL
}

wsi_seurat_coordinate_table <- function(seurat, image_name, image_obj = NULL, tissue_positions = NULL) {
  coords <- wsi_seurat_read_tissue_positions(tissue_positions)
  meta_coords <- NULL
  if (is.null(coords)) {
    meta_coords <- wsi_seurat_coordinates_from_metadata(seurat)
    if (isTRUE(attr(meta_coords, "registered_coordinates", exact = TRUE))) {
      coords <- meta_coords
    }
  }
  if (is.null(coords)) {
    coords <- wsi_seurat_coordinates_from_accessor(seurat, image_name)
  }
  if (is.null(coords)) {
    coords <- wsi_seurat_slot(image_obj, "coordinates")
  }
  if (is.null(coords)) {
    coords <- wsi_seurat_coordinates_from_boundaries(image_obj)
  }
  if (is.null(coords)) {
    coords <- meta_coords %||% wsi_seurat_coordinates_from_metadata(seurat)
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
  attr(out, "id_column") <- attr(coords, "id_column", exact = TRUE) %||% {
    if ("cell" %in% names(coords)) "cell" else if ("cells" %in% names(coords)) "cells" else "barcode"
  }
  attr(out, "x_column") <- x_col
  attr(out, "y_column") <- y_col
  out
}

wsi_seurat_feature_type <- function(coordinates, source_name = "Seurat") {
  ids <- as.character(coordinates$barcode %||% character())
  id_column <- tolower(as.character(attr(coordinates, "id_column", exact = TRUE) %||% ""))
  source_name <- tolower(as.character(source_name %||% ""))
  cell_like_ids <- length(ids) && mean(grepl("(^cell(id)?[_:-])|(^cell[0-9])|cellid", tolower(ids))) > 0.5
  if (id_column %in% c("cell", "cells", "cell_id", "cellid") ||
      grepl("cell", source_name, fixed = TRUE) ||
      isTRUE(cell_like_ids)) {
    return("cell")
  }
  "spot"
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

wsi_seurat_feature_alias_table <- function(object) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    return(NULL)
  }
  row_data <- tryCatch(
    as.data.frame(SummarizedExperiment::rowData(object), stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (!is.data.frame(row_data) || !nrow(row_data)) {
    return(NULL)
  }
  feature_ids <- rownames(row_data)
  if (!length(feature_ids) || any(!nzchar(feature_ids))) {
    feature_ids <- as.character(row_data$gene_id %||% row_data$id %||% row_data$feature_id %||% character())
  }
  if (length(feature_ids) != nrow(row_data) || !any(nzchar(feature_ids))) {
    return(NULL)
  }
  alias_columns <- intersect(
    c(
      "gene_name", "gene_symbol", "symbol", "external_gene_name", "feature_name",
      "name", "gene_id", "id", "feature_id", "gene_search"
    ),
    names(row_data)
  )
  alias_columns <- alias_columns[vapply(row_data[alias_columns], function(x) {
    is.character(x) || is.factor(x) || is.numeric(x) || is.integer(x)
  }, logical(1))]
  if (!length(alias_columns)) {
    return(NULL)
  }
  list(
    feature_ids = as.character(feature_ids),
    data = row_data[alias_columns],
    preferred = intersect(
      c("gene_name", "gene_symbol", "symbol", "external_gene_name", "feature_name", "name"),
      alias_columns
    )
  )
}

wsi_seurat_alias_values <- function(x) {
  values <- as.character(x %||% character())
  values <- unlist(strsplit(values, "\\s*[;,|]\\s*"), use.names = FALSE)
  values <- unique(trimws(values))
  values[nzchar(values) & !is.na(values)]
}

wsi_seurat_feature_alias_index <- function(alias_table, available) {
  if (is.null(alias_table) || !length(available)) {
    return(integer())
  }
  rows <- match(as.character(available), alias_table$feature_ids)
  ok <- which(!is.na(rows))
  if (!length(ok)) {
    return(integer())
  }
  index <- integer()
  add_alias <- function(alias, value) {
    alias <- tolower(wsi_seurat_alias_values(alias))
    alias <- alias[!alias %in% names(index)]
    if (length(alias)) {
      index[alias] <<- as.integer(value)
    }
  }
  for (i in ok) {
    add_alias(available[[i]], i)
    for (column in names(alias_table$data)) {
      add_alias(alias_table$data[[column]][[rows[[i]]]], i)
    }
  }
  index
}

wsi_seurat_gene_match_with_alias <- function(requested, available, alias_table = NULL) {
  idx <- wsi_seurat_gene_match(requested, available)
  if (!length(idx) || is.null(alias_table) || !any(is.na(idx))) {
    return(idx)
  }
  alias_index <- wsi_seurat_feature_alias_index(alias_table, available)
  if (!length(alias_index)) {
    return(idx)
  }
  missing <- which(is.na(idx))
  for (i in missing) {
    aliases <- unique(c(
      requested[[i]],
      gsub("_", "-", requested[[i]], fixed = TRUE),
      gsub("-", "_", requested[[i]], fixed = TRUE),
      gsub("\\.", "-", requested[[i]]),
      gsub("\\.", "_", requested[[i]])
    ))
    hit <- unname(alias_index[tolower(wsi_seurat_alias_values(aliases))])
    hit <- hit[!is.na(hit)]
    if (length(hit)) {
      idx[[i]] <- as.integer(hit[[1L]])
    }
  }
  idx
}

wsi_seurat_feature_display_names <- function(available, alias_table = NULL) {
  available <- as.character(available %||% character())
  if (is.null(alias_table) || !length(available)) {
    return(available)
  }
  rows <- match(available, alias_table$feature_ids)
  out <- available
  preferred <- alias_table$preferred %||% character()
  for (i in seq_along(out)) {
    row <- rows[[i]]
    if (is.na(row)) {
      next
    }
    for (column in preferred) {
      values <- wsi_seurat_alias_values(alias_table$data[[column]][[row]])
      if (length(values)) {
        out[[i]] <- values[[1L]]
        break
      }
    }
  }
  make.unique(out)
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
  dims <- tryCatch(suppressWarnings(dim(x)), error = function(e) NULL)
  if (length(dims) != 2L || any(dims <= 0)) {
    return(FALSE)
  }
  if (is.matrix(x) || is.data.frame(x) || inherits(x, "Matrix")) {
    return(TRUE)
  }
  tryCatch(
    {
      suppressWarnings(
        x[seq_len(min(1L, dims[[1L]])), seq_len(min(1L, dims[[2L]])), drop = FALSE]
      )
      TRUE
    },
    error = function(e) FALSE
  )
}

wsi_seurat_collect_expression_matrices <- function(seurat) {
  matrices <- list()
  if (inherits(seurat, "wsi_spatialexperiment_expression_source")) {
    requested_assay_name <- seurat$assay_name %||% NULL
    seurat <- seurat$object
  } else {
    requested_assay_name <- NULL
  }
  feature_aliases <- wsi_seurat_feature_alias_table(seurat)
  add_matrix <- function(x, name) {
    if (!wsi_seurat_matrix_like(x)) {
      return(invisible(NULL))
    }
    rn <- tryCatch(rownames(x), error = function(e) NULL)
    cn <- tryCatch(colnames(x), error = function(e) NULL)
    if (!length(rn) || !length(cn)) {
      return(invisible(NULL))
    }
    matrices[[length(matrices) + 1L]] <<- list(
      name = name,
      matrix = x,
      feature_aliases = feature_aliases
    )
    invisible(NULL)
  }
  add_seurat_layers <- function(x, prefix, assay_name = NULL) {
    if (!requireNamespace("SeuratObject", quietly = TRUE) || is.null(x)) {
      return(invisible(NULL))
    }
    layer_names <- tryCatch(
      if (is.null(assay_name)) {
        SeuratObject::Layers(x)
      } else {
        SeuratObject::Layers(x, assay = assay_name)
      },
      error = function(e) character()
    )
    layer_names <- unique(as.character(layer_names %||% character()))
    layer_names <- layer_names[nzchar(layer_names) & !is.na(layer_names)]
    if (!length(layer_names)) {
      return(invisible(NULL))
    }
    preferred <- c("counts", "data", "normalized", "logcounts", "normcounts")
    layer_names <- c(intersect(preferred, layer_names), setdiff(layer_names, preferred))
    for (layer_name in layer_names) {
      value <- tryCatch(
        if (is.null(assay_name)) {
          SeuratObject::LayerData(x, layer = layer_name)
        } else {
          SeuratObject::LayerData(x, assay = assay_name, layer = layer_name)
        },
        error = function(e) NULL
      )
      add_matrix(value, paste(prefix, "layers", layer_name, sep = "$"))
    }
    invisible(NULL)
  }
  visit_expression_container <- function(x, prefix = "expression", depth = 0L,
                                         seen = character(), include_scaled = FALSE) {
    if (is.null(x) || depth > 8L) {
      return(invisible(NULL))
    }
    key <- tryCatch(
      wsi_spatial_seen_key(x),
      error = function(e) paste(prefix, typeof(x), length(x), sep = ":")
    )
    if (key %in% seen) {
      return(invisible(NULL))
    }
    seen <- c(seen, key)
    if (wsi_seurat_matrix_like(x)) {
      add_matrix(x, prefix)
      return(invisible(NULL))
    }
    children <- list()
    if (isS4(x)) {
      slots <- tryCatch(methods::slotNames(x), error = function(e) character())
      for (slot_name in slots) {
        value <- tryCatch(methods::slot(x, slot_name), error = function(e) NULL)
        if (!is.null(value)) {
          children[[slot_name]] <- value
        }
      }
    }
    if (is.list(x)) {
      children <- c(children, x)
    }
    if (!length(children)) {
      return(invisible(NULL))
    }
    nms <- names(children)
    if (is.null(nms)) {
      nms <- paste0("item", seq_along(children))
    }
    names(children) <- nms
    preferred_names <- unique(c(
      requested_assay_name %||% character(),
      "normalized", "data", "logcounts", "normcounts",
      "raw", "counts", "scaled", "scale.data", "scale_data",
      "exprMat", "expression", "exprs", "rna", "RNA", "cell", "assays"
    ))
    ordered_names <- c(intersect(preferred_names, nms), setdiff(nms, preferred_names))
    for (nm in ordered_names) {
      if (!isTRUE(include_scaled) &&
          nm %in% c("scaled", "scale.data", "scale_data") &&
          !identical(nm, requested_assay_name)) {
        next
      }
      visit_expression_container(
        children[[nm]],
        prefix = paste(prefix, nm, sep = "$"),
        depth = depth + 1L,
        seen = seen,
        include_scaled = include_scaled
      )
    }
    invisible(NULL)
  }
  expression_root <- wsi_seurat_slot(seurat, "expression")
  if (!is.null(expression_root)) {
    expression_count <- length(matrices)
    visit_expression_container(expression_root, "expression", include_scaled = FALSE)
    if (length(matrices) == expression_count) {
      visit_expression_container(expression_root, "expression", include_scaled = TRUE)
    }
  }
  if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    se_assays <- tryCatch(SummarizedExperiment::assays(seurat), error = function(e) NULL)
    if (!is.null(se_assays) && length(se_assays)) {
      assay_names <- names(se_assays) %||% paste0("assay_", seq_along(se_assays))
      if (!is.null(requested_assay_name) && requested_assay_name %in% assay_names) {
        add_matrix(se_assays[[requested_assay_name]], paste("assays", requested_assay_name, sep = "$"))
      }
      for (i in seq_along(se_assays)) {
        add_matrix(se_assays[[i]], paste("assays", assay_names[[i]], sep = "$"))
      }
    } else {
      add_matrix(tryCatch(SummarizedExperiment::assay(seurat), error = function(e) NULL), "assay")
    }
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
      add_seurat_layers(assays[[assay_name]], paste("assays", assay_name, sep = "$"))
      visit_container(assays[[assay_name]], paste("assays", assay_name, sep = "$"))
    }
  }
  seurat_assays <- if (requireNamespace("SeuratObject", quietly = TRUE)) {
    tryCatch(SeuratObject::Assays(seurat), error = function(e) character())
  } else {
    character()
  }
  for (assay_name in setdiff(as.character(seurat_assays %||% character()), names(assays) %||% character())) {
    add_seurat_layers(seurat, paste("assays", assay_name, sep = "$"), assay_name = assay_name)
  }
  add_seurat_layers(seurat, "seurat")
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
    gene_idx <- wsi_seurat_gene_match_with_alias(genes, rn, entry$feature_aliases)
    spot_idx <- match(spot_ids, cn)
    if (any(!is.na(gene_idx)) && any(!is.na(spot_idx))) {
      keep_gene <- !is.na(gene_idx)
      keep_spot <- !is.na(spot_idx)
      out <- matrix(NA_real_, nrow = length(spot_ids), ncol = sum(keep_gene))
      colnames(out) <- genes[keep_gene]
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

wsi_seurat_gene_expression <- function(seurat, genes, spot_ids, default_gene = NULL,
                                       object_label = "Seurat") {
  genes <- wsi_seurat_gene_vector(genes, "spot_genes")
  if (!length(genes)) {
    return(list(enabled = FALSE, genes = character(), default_gene = NULL, values = matrix(numeric(), nrow = length(spot_ids), ncol = 0L)))
  }
  spot_ids <- as.character(spot_ids)
  values <- wsi_seurat_fetch_gene_expression(seurat, genes, spot_ids) %||%
    wsi_seurat_matrix_gene_expression(seurat, genes, spot_ids)
  if (is.null(values) || !ncol(values)) {
    wsi_abort(sprintf(
      "None of the requested `spot_genes` were found in the %s expression data: %s",
      object_label,
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
      "Some requested `spot_genes` were not found in the %s object and will be unavailable in the viewer: %s",
      object_label,
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

wsi_seurat_live_gene_available <- function(seurat = NULL) {
  inherits(seurat, "wsi_seurat_spatial") && !is.null(seurat$expression_source$object)
}

wsi_seurat_dynamic_gene_payload <- function(linked, gene) {
  source_name <- as.character(linked$source_name %||% "spatial object")
  if (!inherits(linked, "wsi_seurat_spatial")) {
    wsi_abort("Dynamic spatial gene lookup requires an object from a wsiTools spatial-image linker.")
  }
  gene <- wsi_seurat_default_gene_arg(gene)
  source <- linked$expression_source %||% list()
  object <- source$object %||% NULL
  if (is.null(object)) {
    wsi_abort(
      paste(
        sprintf("This viewer was not created with a live %s expression source.", source_name),
        "Reopen it with `live = TRUE` or preload a small gene set with `spot_genes`."
      )
    )
  }
  spots <- linked$spots
  spot_ids <- as.character(source$spot_ids %||% spots$barcode %||% spots$id)
  if (!length(spot_ids)) {
    wsi_abort(sprintf("No %s spot/cell identifiers are available for dynamic gene lookup.", source_name))
  }
  feature_type <- source$feature_type %||% linked$feature_type %||% {
    if ("feature_type" %in% names(spots)) spots$feature_type[[1L]] else NULL
  }
  feature_type <- feature_type %||% {
    tryCatch(wsi_seurat_feature_type(spots, source_name = source_name), error = function(err) "spot")
  }
  feature_type <- if (identical(as.character(feature_type), "cell")) "cell" else "spot"
  feature_label <- if (identical(feature_type, "cell")) "cell" else "spot"
  feature_plural <- paste0(feature_label, "s")
  gene_expression <- wsi_seurat_gene_expression(
    object,
    genes = gene,
    spot_ids = spot_ids,
    default_gene = gene,
    object_label = source_name
  )
  actual_gene <- gene_expression$default_gene %||% gene_expression$genes[[1L]]
  idx <- wsi_seurat_gene_match(actual_gene, colnames(gene_expression$values))
  if (is.na(idx)) {
    wsi_abort(sprintf("Gene `%s` was not found in the %s expression data.", gene, source_name))
  }
  values <- suppressWarnings(as.numeric(gene_expression$values[, idx]))
  colours <- wsi_seurat_gene_colours(gene_expression, actual_gene)
  if (nrow(spots) != length(values)) {
    spots <- spots[seq_len(min(nrow(spots), length(values))), , drop = FALSE]
    values <- values[seq_len(nrow(spots))]
    colours <- colours[seq_len(nrow(spots))]
  }
  points <- lapply(seq_len(nrow(spots)), function(i) {
    x <- suppressWarnings(as.numeric(spots$x[[i]] %||% spots$slide_x[[i]] %||% NA_real_))
    y <- suppressWarnings(as.numeric(spots$y[[i]] %||% spots$slide_y[[i]] %||% NA_real_))
    radius <- suppressWarnings(as.numeric(spots$radius[[i]] %||% spots$spot_radius[[i]] %||% linked$spot_radius %||% NA_real_))
    list(
      id = as.character(spots$id[[i]] %||% ""),
      label = as.character(spots$label[[i]] %||% spots$id[[i]] %||% ""),
      barcode = as.character(spots$barcode[[i]] %||% spots$id[[i]] %||% ""),
      feature_type = feature_type,
      x = if (is.finite(x)) x else NA_real_,
      y = if (is.finite(y)) y else NA_real_,
      slide_x = if (is.finite(x)) x else NA_real_,
      slide_y = if (is.finite(y)) y else NA_real_,
      radius = if (is.finite(radius)) radius else NA_real_,
      value = if (is.finite(values[[i]])) values[[i]] else NA_real_,
      colour = as.character(colours[[i]] %||% "#d1d5db")
    )
  })
  gene_layer <- tryCatch({
    linked_for_layer <- linked
    linked_for_layer$spots$colour <- colours
    linked_for_layer$spots$color <- colours
    linked_for_layer$spots$radius <- suppressWarnings(as.numeric(linked_for_layer$spots$radius %||% linked$spot_radius %||% NA_real_))
    layer <- wsi_seurat_spatial_mask_layer(
      linked = linked_for_layer,
      name = sprintf("%s %s %s expression", source_name, feature_plural, actual_gene),
      id = if (identical(feature_type, "cell")) "seurat_cell_gene_expression" else "seurat_gene_expression",
      colours = colours,
      visible = TRUE,
      opacity = 0.9,
      feature_count = nrow(spots),
      radius_scale = 0.5,
      alpha = 0.85
    )
    layer$source_type <- if (identical(feature_type, "cell")) "seurat_cell_gene_expression" else "seurat_gene_expression"
    layer$metadata <- c(
      layer$metadata %||% list(),
      list(
        gene = actual_gene,
        feature_type = feature_type,
        feature_label = feature_plural,
        range = gene_expression$ranges[[actual_gene]] %||% list(min = NA_real_, max = NA_real_),
        positive_count = sum(values > 0, na.rm = TRUE),
        display_mode = "raster_mask",
        vector_rendering = FALSE
      )
    )
    unclass(layer)
  }, error = function(err) NULL)
  list(
    ok = TRUE,
    gene = as.character(actual_gene),
    requested_gene = as.character(gene),
    feature_type = feature_type,
    feature_label = feature_label,
    feature_plural = feature_plural,
    range = gene_expression$ranges[[actual_gene]] %||% list(min = NA_real_, max = NA_real_),
    count = length(points),
    image_layer = gene_layer,
    points = points
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

wsi_spatial_scale_metadata <- function(slide, coordinates,
                                       scale_factors = list(), mapping = NULL,
                                       visium_center_spacing_um = 100,
                                       visium_spot_diameter_um = 55) {
  slide_mpp <- wsi_viewer_mpp_payload(tryCatch(wsi_mpp(slide), error = function(err) NULL))
  if (!is.null(slide_mpp)) {
    return(list(
      mpp = slide_mpp,
      pixel_size = slide_mpp,
      source = "image_metadata",
      inferred = FALSE
    ))
  }
  inferred <- wsi_spatial_infer_visium_mpp(
    coordinates = coordinates,
    scale_factors = scale_factors,
    mapping = mapping,
    center_spacing_um = visium_center_spacing_um,
    spot_diameter_um = visium_spot_diameter_um
  )
  if (!is.null(inferred)) {
    inferred$pixel_size <- inferred$mpp
    return(inferred)
  }
  list(
    mpp = NULL,
    pixel_size = NULL,
    source = "unavailable",
    inferred = FALSE
  )
}

wsi_spatial_infer_visium_mpp <- function(coordinates, scale_factors = list(),
                                         mapping = NULL,
                                         center_spacing_um = 100,
                                         spot_diameter_um = 55) {
  spacing_px <- wsi_spatial_visium_center_spacing_pixels(coordinates)
  if (!is.null(spacing_px)) {
    mpp <- center_spacing_um / spacing_px$spacing_pixels
    return(list(
      mpp = list(x = unname(mpp), y = unname(mpp)),
      source = "visium_center_spacing",
      inferred = TRUE,
      center_spacing_um = unname(center_spacing_um),
      center_spacing_pixels = unname(spacing_px$spacing_pixels),
      spacing_quality = spacing_px$quality,
      spot_diameter_um = unname(spot_diameter_um)
    ))
  }

  spot <- suppressWarnings(as.numeric(scale_factors$spot %||% scale_factors$spot_diameter_fullres %||% NA_real_))
  if (length(spot) == 1L && is.finite(spot) && spot > 0 && !is.null(mapping)) {
    sx <- suppressWarnings(as.numeric(mapping$scale_x %||% NA_real_))
    sy <- suppressWarnings(as.numeric(mapping$scale_y %||% NA_real_))
    scale <- mean(c(sx, sy), na.rm = TRUE)
    if (is.finite(scale) && scale > 0) {
      spot_px <- spot * scale
      mpp <- spot_diameter_um / spot_px
      return(list(
        mpp = list(x = unname(mpp), y = unname(mpp)),
        source = "visium_spot_diameter",
        inferred = TRUE,
        spot_diameter_um = unname(spot_diameter_um),
        spot_diameter_pixels = unname(spot_px)
      ))
    }
  }
  NULL
}

wsi_spatial_visium_center_spacing_pixels <- function(coordinates, max_queries = 5000L) {
  if (is.null(coordinates) || !all(c("x", "y") %in% names(coordinates))) {
    return(NULL)
  }
  x <- suppressWarnings(as.numeric(coordinates$x))
  y <- suppressWarnings(as.numeric(coordinates$y))
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  n <- length(x)
  if (n < 3L) {
    return(NULL)
  }
  candidate_idx <- if (n > 100000L) unique(round(seq(1, n, length.out = 50000L))) else seq_len(n)
  candidate_x <- x[candidate_idx]
  candidate_y <- y[candidate_idx]
  query_n <- if (n > 15000L) min(1200L, n) else min(as.integer(max_queries), n)
  query_idx <- if (n > query_n) unique(round(seq(1, n, length.out = query_n))) else seq_len(n)
  query_n <- length(query_idx)
  nearest <- rep(Inf, query_n)
  chunk_size <- if (length(candidate_idx) > 50000L) 64L else 128L

  for (start in seq(1L, query_n, by = chunk_size)) {
    rows <- start:min(query_n, start + chunk_size - 1L)
    idx <- query_idx[rows]
    dx <- outer(x[idx], candidate_x, "-")
    dy <- outer(y[idx], candidate_y, "-")
    d2 <- dx * dx + dy * dy
    self <- match(idx, candidate_idx)
    self_rows <- which(!is.na(self))
    if (length(self_rows)) {
      d2[cbind(self_rows, self[self_rows])] <- Inf
    }
    nearest[rows] <- sqrt(apply(d2, 1L, min, na.rm = TRUE))
  }

  nearest <- nearest[is.finite(nearest) & nearest > sqrt(.Machine$double.eps)]
  if (length(nearest) < 3L) {
    return(NULL)
  }
  spacing <- stats::median(nearest, na.rm = TRUE)
  if (!is.finite(spacing) || spacing <= 0) {
    return(NULL)
  }
  q <- stats::quantile(nearest, c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 7)
  robust_cv <- (q[[2L]] - q[[1L]]) / max(spacing, .Machine$double.eps)
  if (!is.finite(robust_cv) || robust_cv > 0.25) {
    return(NULL)
  }
  list(
    spacing_pixels = unname(spacing),
    quality = list(
      nearest_count = length(nearest),
      robust_cv = unname(robust_cv),
      q25 = unname(q[[1L]]),
      q75 = unname(q[[2L]])
    )
  )
}

wsi_seurat_spot_radius <- function(scale_factors, mapping, scale_metadata = NULL) {
  spot <- suppressWarnings(as.numeric(scale_factors$spot %||% scale_factors$spot_diameter_fullres %||% NA_real_))
  if (length(spot) == 1L && is.finite(spot) && spot > 0) {
    return(max(2, spot * mean(c(mapping$scale_x, mapping$scale_y)) / 2))
  }
  if (identical(scale_metadata$source %||% NULL, "visium_center_spacing")) {
    spacing_px <- suppressWarnings(as.numeric(scale_metadata$center_spacing_pixels %||% NA_real_))
    diameter_um <- suppressWarnings(as.numeric(scale_metadata$spot_diameter_um %||% 55))
    spacing_um <- suppressWarnings(as.numeric(scale_metadata$center_spacing_um %||% 100))
    if (length(spacing_px) == 1L && is.finite(spacing_px) && spacing_px > 0 &&
        length(diameter_um) == 1L && is.finite(diameter_um) && diameter_um > 0 &&
        length(spacing_um) == 1L && is.finite(spacing_um) && spacing_um > 0) {
      return(max(2, spacing_px * diameter_um / spacing_um / 2))
    }
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
