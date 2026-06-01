#' Link a Giotto spatial object to a high-resolution image
#'
#' `wsi_link_giotto_image()` extracts spatial spot/cell coordinates and a
#' dimensional reduction from a Giotto object, then maps those positions onto a
#' high-resolution image for the wsiTools viewer. Giotto is optional: if Giotto
#' is unavailable or the object layout is unusual, pass `coordinates` and
#' `embeddings` explicitly.
#'
#' @param giotto A Giotto object, Giotto-like object, or list-like object.
#' @param image Path to the high-resolution image, or a `wsi_slide` object.
#' @param image_name Optional image/section label stored in the linked object.
#' @param coordinates Optional data frame of coordinates. It should contain a
#'   spot/cell identifier plus x/y columns. Common Giotto columns such as
#'   `cell_ID`, `sdimx`, and `sdimy` are recognised.
#' @param embeddings Optional dimensional-reduction matrix or data frame. Rows
#'   should be named by spot/cell id, or an id column such as `cell_ID` should
#'   be present.
#' @param spat_unit,feat_type Optional Giotto selectors passed to Giotto
#'   accessors when available.
#' @inheritParams wsi_link_seurat_image
#'
#' @return A linked spatial object that can be passed to
#'   [wsi_viewer_giotto()] or [wsi_viewer()].
#' @export
#'
#' @examples
#' \dontrun{
#' linked <- wsi_link_giotto_image(gobject, "tissue.tif", reduction = "pca")
#' wsi_viewer_giotto(gobject, "tissue.tif", linked = linked)
#' }
wsi_link_giotto_image <- function(giotto, image, image_name = NULL,
                                  coordinates = NULL, embeddings = NULL,
                                  spat_unit = NULL, feat_type = NULL,
                                  reduction = "pca", dims = c(1L, 2L),
                                  coordinate_scale = c("auto", "none", "fullres", "hires", "lowres", "seurat_image", "custom"),
                                  scale_x = NULL, scale_y = NULL,
                                  coordinate_flip = c("none", "vertical", "horizontal"),
                                  coordinate_rotation = c(0, 90, 180, 270),
                                  coordinate_transform = "none",
                                  spot_genes = NULL, default_gene = NULL,
                                  spot_radius = NULL, max_points = 100000L,
                                  colour_by = c("component_1", "gene", "none")) {
  coordinate_scale <- match.arg(coordinate_scale)
  coordinate_flip <- wsi_seurat_coordinate_flip_arg(coordinate_flip)
  coordinate_rotation <- wsi_seurat_coordinate_rotation_arg(coordinate_rotation)
  colour_by <- match.arg(colour_by)
  coordinates <- coordinates %||% wsi_giotto_coordinates(
    giotto,
    spat_unit = spat_unit,
    feat_type = feat_type
  )
  embeddings <- embeddings %||% wsi_giotto_embeddings(
    giotto,
    reduction = reduction,
    spat_unit = spat_unit,
    feat_type = feat_type
  )
  wsi_link_spatial_table_image(
    object = giotto,
    image = image,
    source_name = "Giotto",
    image_name = image_name %||% "giotto",
    coordinates = coordinates,
    embeddings = embeddings,
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

#' Open a Giotto spatial object in the wsiTools viewer
#'
#' @inheritParams wsi_viewer_seurat
#' @param giotto A Giotto object.
#' @param linked Optional object returned by [wsi_link_giotto_image()].
#' @param ... Arguments passed to [wsi_link_giotto_image()] and then to
#'   [wsi_viewer()] or [wsi_viewer_live()].
#'
#' @return The HTML path for static mode, or a `wsi_viewer_session` for live
#'   mode.
#' @export
wsi_viewer_giotto <- function(giotto, image, linked = NULL,
                              live = FALSE, dynamic_tiles = live,
                              mode = c("tiles", "thumbnail"),
                              output = NULL, open = interactive(),
                              overwrite = FALSE, ...) {
  wsi_viewer_spatial_linked(
    object = giotto,
    image = image,
    linked = linked,
    linker = wsi_link_giotto_image,
    source_name = "Giotto",
    live = live,
    dynamic_tiles = dynamic_tiles,
    mode = match.arg(mode),
    output = output,
    open = open,
    overwrite = overwrite,
    ...
  )
}

#' Link a SpatialExperiment object to a high-resolution image
#'
#' `wsi_link_spatialexperiment_image()` extracts spatial coordinates and a
#' dimensional reduction from a `SpatialExperiment` object, then maps spots onto
#' a high-resolution image. `SpatialExperiment`, `SingleCellExperiment`, and
#' `SummarizedExperiment` are optional runtime dependencies; explicit
#' `coordinates` and `embeddings` can be supplied for lightweight or mocked
#' objects.
#'
#' @param spe A SpatialExperiment object, or SpatialExperiment-like object.
#' @param image Path to the high-resolution image, or a `wsi_slide` object.
#' @param image_name Optional image/section label stored in the linked object.
#' @param sample_id Optional `sample_id` used to subset multi-sample
#'   SpatialExperiment objects before extracting coordinates and reductions.
#' @param coordinates Optional coordinate data frame or matrix.
#' @param embeddings Optional dimensional-reduction matrix or data frame.
#' @param assay_name Optional assay name used for gene expression lookup.
#' @inheritParams wsi_link_seurat_image
#'
#' @return A linked spatial object that can be passed to
#'   [wsi_viewer_spatialexperiment()] or [wsi_viewer()].
#' @export
#'
#' @examples
#' \dontrun{
#' linked <- wsi_link_spatialexperiment_image(spe, "tissue.tif", reduction = "PCA")
#' wsi_viewer_spatialexperiment(spe, "tissue.tif", linked = linked)
#' }
wsi_link_spatialexperiment_image <- function(spe, image, image_name = NULL,
                                             sample_id = NULL,
                                             coordinates = NULL,
                                             embeddings = NULL,
                                             assay_name = NULL,
                                             reduction = "PCA", dims = c(1L, 2L),
                                             coordinate_scale = c("auto", "none", "fullres", "hires", "lowres", "seurat_image", "custom"),
                                             scale_x = NULL, scale_y = NULL,
                                             coordinate_flip = c("none", "vertical", "horizontal"),
                                             coordinate_rotation = c(0, 90, 180, 270),
                                             coordinate_transform = "none",
                                             spot_genes = NULL, default_gene = NULL,
                                             spot_radius = NULL, max_points = 100000L,
                                             colour_by = c("component_1", "gene", "none")) {
  coordinate_scale <- match.arg(coordinate_scale)
  coordinate_flip <- wsi_seurat_coordinate_flip_arg(coordinate_flip)
  coordinate_rotation <- wsi_seurat_coordinate_rotation_arg(coordinate_rotation)
  colour_by <- match.arg(colour_by)
  spe_subset <- wsi_spatialexperiment_subset(spe, sample_id = sample_id)
  coordinates <- coordinates %||% wsi_spatialexperiment_coordinates(spe_subset)
  embeddings <- embeddings %||% wsi_spatialexperiment_embeddings(spe_subset, reduction = reduction)
  expression_object <- if (is.null(assay_name)) {
    spe_subset
  } else {
    structure(list(object = spe_subset, assay_name = assay_name), class = "wsi_spatialexperiment_expression_source")
  }
  wsi_link_spatial_table_image(
    object = expression_object,
    image = image,
    source_name = "SpatialExperiment",
    image_name = image_name %||% sample_id %||% "spatialexperiment",
    coordinates = coordinates,
    embeddings = embeddings,
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

#' Open a SpatialExperiment object in the wsiTools viewer
#'
#' @inheritParams wsi_viewer_seurat
#' @param spe A SpatialExperiment object.
#' @param linked Optional object returned by
#'   [wsi_link_spatialexperiment_image()].
#' @param ... Arguments passed to [wsi_link_spatialexperiment_image()] and then
#'   to [wsi_viewer()] or [wsi_viewer_live()].
#'
#' @return The HTML path for static mode, or a `wsi_viewer_session` for live
#'   mode.
#' @export
wsi_viewer_spatialexperiment <- function(spe, image, linked = NULL,
                                         live = FALSE, dynamic_tiles = live,
                                         mode = c("tiles", "thumbnail"),
                                         output = NULL, open = interactive(),
                                         overwrite = FALSE, ...) {
  wsi_viewer_spatial_linked(
    object = spe,
    image = image,
    linked = linked,
    linker = wsi_link_spatialexperiment_image,
    source_name = "SpatialExperiment",
    live = live,
    dynamic_tiles = dynamic_tiles,
    mode = match.arg(mode),
    output = output,
    open = open,
    overwrite = overwrite,
    ...
  )
}

wsi_viewer_spatial_linked <- function(object, image, linked, linker, source_name,
                                      live, dynamic_tiles, mode, output, open,
                                      overwrite, ...) {
  dots <- list(...)
  if (!is.logical(live) || length(live) != 1L || is.na(live)) {
    wsi_abort("`live` must be `TRUE` or `FALSE`.")
  }
  if (is.null(linked)) {
    link_args <- dots[names(dots) %in% names(formals(linker))]
    dots <- dots[setdiff(names(dots), names(link_args))]
    linked <- do.call(linker, c(list(object, image), link_args))
  } else if (!inherits(linked, "wsi_spatial_object") && !inherits(linked, "wsi_seurat_spatial")) {
    wsi_abort(sprintf("`linked` must be an object returned by `wsi_link_%s_image()`.", tolower(source_name)))
  }
  if (is.null(linked$expression_source$object)) {
    linked$expression_source <- list(
      object = object,
      spot_ids = as.character(linked$spots$barcode %||% linked$spots$id)
    )
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
  dots$title <- dots$title %||% sprintf("wsiTools %s viewer: %s", source_name, basename(linked$image_path %||% "image"))

  if (isTRUE(live)) {
    dots$dynamic_tiles <- dynamic_tiles
    dots$slide <- linked$slide
    return(do.call(wsi_viewer_live, dots))
  }

  dots$slide <- linked$slide
  dots$mode <- mode
  do.call(wsi_viewer, dots)
}

wsi_link_spatial_table_image <- function(object, image, source_name, image_name,
                                         coordinates, embeddings, reduction, dims,
                                         coordinate_scale, scale_x, scale_y,
                                         coordinate_flip, coordinate_rotation,
                                         coordinate_transform, spot_genes,
                                         default_gene, spot_radius, max_points,
                                         colour_by) {
  coordinate_transform <- wsi_seurat_coordinate_transform_arg(coordinate_transform)
  coordinate_flip <- wsi_seurat_coordinate_flip_arg(coordinate_flip)
  coordinate_rotation <- wsi_seurat_coordinate_rotation_arg(coordinate_rotation)
  spot_genes <- wsi_seurat_gene_vector(spot_genes, "spot_genes")
  default_gene <- wsi_seurat_default_gene_arg(default_gene)
  if (!is.null(default_gene) && !default_gene %in% spot_genes) {
    spot_genes <- c(default_gene, spot_genes)
  }
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
  coordinates <- wsi_spatial_coordinate_table(coordinates, source_name = source_name)
  embeddings <- wsi_spatial_embedding_matrix(embeddings, reduction = reduction, source_name = source_name)
  if (ncol(embeddings) < max(dims)) {
    wsi_abort(sprintf(
      "%s reduction `%s` has %d dimension%s, but `dims` requests dimension %d.",
      source_name,
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
    wsi_abort(sprintf("No shared spot/cell identifiers were found between %s coordinates and the reduction.", source_name))
  }
  gene_expression <- wsi_seurat_gene_expression(
    object,
    genes = spot_genes,
    spot_ids = coordinates$barcode,
    default_gene = default_gene,
    object_label = source_name
  )

  mapping <- wsi_seurat_coordinate_mapping(
    coordinates = coordinates,
    image_obj = NULL,
    slide = slide,
    scale_factors = list(),
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
    spot_radius <- max(6, 28 * mean(c(mapping$scale_x, mapping$scale_y)))
  }
  spot_radius <- as.numeric(wsi_check_scalar_number(spot_radius, "spot_radius", allow_zero = FALSE))

  out <- list(
    slide = slide,
    image_path = slide$path,
    image_name = image_name,
    source_name = source_name,
    reduction = reduction,
    dims = dims,
    component_names = component_names,
    coordinate_mapping = mapping,
    gene_expression = gene_expression,
    spot_radius = spot_radius,
    spot_count = total,
    displayed_spot_count = nrow(spots),
    spots = spots,
    expression_source = list(
      object = object,
      spot_ids = as.character(spots$barcode %||% spots$id)
    ),
    pca = list(
      id = paste0(tolower(wsi_safe_id(source_name, "spatial")), "_", wsi_safe_id(reduction, "reduction")),
      label = paste0(source_name, " ", toupper(reduction), " plot"),
      reduction = reduction,
      x_label = component_names[[1L]],
      y_label = component_names[[2L]],
      point_count = total,
      points = plot_points
    )
  )
  class(out) <- c("wsi_spatial_object", "wsi_seurat_spatial", "list")
  out
}

wsi_spatial_coordinate_table <- function(coords, source_name = "spatial object") {
  if (is.null(coords)) {
    wsi_abort(sprintf("Could not extract spatial coordinates from the %s object. Supply `coordinates` explicitly.", source_name))
  }
  coords <- as.data.frame(coords, stringsAsFactors = FALSE)
  barcode <- NULL
  for (candidate in c("barcode", "barcodes", "cell_ID", "cell_id", "cell", "cells", "spot", "spot_id", "sample_id", "id", "ID")) {
    if (candidate %in% names(coords)) {
      barcode <- as.character(coords[[candidate]])
      break
    }
  }
  if (is.null(barcode)) {
    barcode <- rownames(coords) %||% as.character(seq_len(nrow(coords)))
  }
  x_col <- wsi_seurat_first_column(coords, c("pxl_col_in_fullres", "imagecol", "col", "x", "X", "sdimx", "sdimX", "spatial_x", "array_col"))
  y_col <- wsi_seurat_first_column(coords, c("pxl_row_in_fullres", "imagerow", "row", "y", "Y", "sdimy", "sdimY", "spatial_y", "array_row"))
  if (is.null(x_col) || is.null(y_col)) {
    wsi_abort(sprintf("%s coordinates must contain recognised x/y columns.", source_name))
  }
  out <- data.frame(
    barcode = barcode,
    x = suppressWarnings(as.numeric(coords[[x_col]])),
    y = suppressWarnings(as.numeric(coords[[y_col]])),
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$x) & is.finite(out$y) & nzchar(out$barcode), , drop = FALSE]
  row.names(out) <- NULL
  attr(out, "coordinate_space") <- if (all(c("pxl_col_in_fullres", "pxl_row_in_fullres") %in% names(coords))) "fullres" else "unknown"
  attr(out, "coordinate_source") <- tolower(source_name)
  attr(out, "x_column") <- x_col
  attr(out, "y_column") <- y_col
  out
}

wsi_spatial_embedding_matrix <- function(embeddings, reduction = "pca", source_name = "spatial object") {
  if (is.null(embeddings)) {
    wsi_abort(sprintf("Could not extract `%s` embeddings from the %s object. Supply `embeddings` explicitly.", reduction, source_name))
  }
  if (is.data.frame(embeddings)) {
    ids <- NULL
    id_col <- wsi_seurat_first_column(embeddings, c("barcode", "barcodes", "cell_ID", "cell_id", "cell", "cells", "spot", "spot_id", "sample_id", "id", "ID"))
    if (!is.null(id_col)) {
      ids <- as.character(embeddings[[id_col]])
      embeddings <- embeddings[setdiff(names(embeddings), id_col)]
    }
    numeric_cols <- vapply(embeddings, is.numeric, logical(1))
    embeddings <- embeddings[, numeric_cols, drop = FALSE]
    emb <- as.matrix(embeddings)
    if (!is.null(ids) && length(ids) == nrow(emb)) {
      rownames(emb) <- ids
    }
  } else {
    emb <- as.matrix(embeddings)
  }
  storage.mode(emb) <- "double"
  if (!nrow(emb) || !ncol(emb)) {
    wsi_abort(sprintf("Reduction `%s` from the %s object does not contain embeddings.", reduction, source_name))
  }
  if (is.null(rownames(emb))) {
    rownames(emb) <- as.character(seq_len(nrow(emb)))
  }
  if (is.null(colnames(emb))) {
    colnames(emb) <- paste0(toupper(reduction), "_", seq_len(ncol(emb)))
  }
  emb
}

wsi_optional_accessor <- function(namespace, fun, ..., .args = list()) {
  if (!requireNamespace(namespace, quietly = TRUE)) {
    return(NULL)
  }
  f <- tryCatch(getExportedValue(namespace, fun), error = function(e) NULL)
  if (!is.function(f)) {
    return(NULL)
  }
  args <- c(list(...), .args)
  args <- args[!vapply(args, is.null, logical(1))]
  tryCatch(do.call(f, args), error = function(e) NULL)
}

wsi_giotto_coordinates <- function(giotto, spat_unit = NULL, feat_type = NULL) {
  args <- list(spat_unit = spat_unit, feat_type = feat_type)
  for (namespace in c("Giotto", "GiottoClass")) {
    out <- wsi_optional_accessor(namespace, "spatLocs", gobject = giotto, .args = args) %||%
      wsi_optional_accessor(namespace, "spatLocs", giotto, .args = args) %||%
      wsi_optional_accessor(namespace, "getSpatialLocations", gobject = giotto, .args = args) %||%
      wsi_optional_accessor(namespace, "getSpatialLocations", giotto, .args = args)
    if (!is.null(out)) {
      return(out)
    }
  }
  wsi_find_spatial_coordinate_table(
    giotto,
    names = c("spatial_locs", "spatial_locations", "spat_locs", "spatLocs", "coordinates", "cell_metadata")
  )
}

wsi_giotto_embeddings <- function(giotto, reduction = "pca", spat_unit = NULL, feat_type = NULL) {
  args <- list(reduction = reduction, spat_unit = spat_unit, feat_type = feat_type)
  for (namespace in c("Giotto", "GiottoClass")) {
    out <- wsi_optional_accessor(namespace, "getDimReduction", gobject = giotto, .args = args) %||%
      wsi_optional_accessor(namespace, "getDimReduction", giotto, .args = args) %||%
      wsi_optional_accessor(namespace, "getDimensionReduction", gobject = giotto, .args = args) %||%
      wsi_optional_accessor(namespace, "getDimensionReduction", giotto, .args = args)
    if (!is.null(out)) {
      return(wsi_spatial_extract_embedding_payload(out, reduction = reduction))
    }
  }
  wsi_find_spatial_embedding_table(
    giotto,
    reduction = reduction,
    names = c("dimension_reduction", "dim_reduction", "dimReduction", "reductions", reduction)
  )
}

wsi_spatialexperiment_subset <- function(spe, sample_id = NULL) {
  if (is.null(sample_id)) {
    return(spe)
  }
  sample_id <- wsi_seurat_check_scalar_character(sample_id, "sample_id")
  cd <- wsi_optional_accessor("SummarizedExperiment", "colData", x = spe)
  if (is.null(cd)) {
    wsi_abort("`sample_id` subsetting requires a SpatialExperiment/SummarizedExperiment object with `colData()`.")
  }
  cd <- as.data.frame(cd)
  sample_col <- wsi_seurat_first_column(cd, c("sample_id", "sample", "section", "image_id"))
  if (is.null(sample_col)) {
    wsi_abort("Could not find a `sample_id`/`sample` column in `colData()`.")
  }
  keep <- as.character(cd[[sample_col]]) == sample_id
  if (!any(keep)) {
    wsi_abort(sprintf("No SpatialExperiment columns matched `sample_id = \"%s\"`.", sample_id))
  }
  spe[, keep, drop = FALSE]
}

wsi_spatialexperiment_coordinates <- function(spe) {
  coords <- wsi_optional_accessor("SpatialExperiment", "spatialCoords", x = spe)
  if (is.null(coords)) {
    coords <- wsi_find_spatial_coordinate_table(spe, names = c("spatialCoords", "spatial_coords", "coordinates"))
  }
  if (is.null(coords)) {
    return(NULL)
  }
  coords <- as.data.frame(coords, stringsAsFactors = FALSE)
  ids <- tryCatch(colnames(spe), error = function(e) NULL)
  if (!is.null(ids) && length(ids) == nrow(coords) && is.null(wsi_seurat_first_column(coords, c("barcode", "cell", "spot", "id", "sample_id")))) {
    coords$barcode <- as.character(ids)
  }
  coords
}

wsi_spatialexperiment_embeddings <- function(spe, reduction = "PCA") {
  reduction_names <- wsi_optional_accessor("SingleCellExperiment", "reducedDimNames", x = spe)
  reduction_type <- reduction
  if (length(reduction_names)) {
    hit <- reduction_names[match(tolower(reduction), tolower(reduction_names), nomatch = 0L)]
    if (length(hit)) {
      reduction_type <- hit[[1L]]
    }
  }
  emb <- wsi_optional_accessor("SingleCellExperiment", "reducedDim", x = spe, type = reduction_type)
  if (is.null(emb)) {
    reduced <- wsi_seurat_slot(spe, "reducedDims") %||% wsi_seurat_slot(spe, "reduced_dims")
    if (is.list(reduced) && length(reduced)) {
      hit <- names(reduced)[match(tolower(reduction), tolower(names(reduced)), nomatch = 0L)]
      emb <- if (length(hit)) reduced[[hit[[1L]]]] else reduced[[1L]]
    }
  }
  if (is.null(emb)) {
    return(NULL)
  }
  ids <- tryCatch(colnames(spe), error = function(e) NULL)
  emb <- wsi_spatial_extract_embedding_payload(emb, reduction = reduction)
  if (!is.null(ids) && length(ids) == nrow(emb) && is.null(rownames(emb))) {
    rownames(emb) <- as.character(ids)
  }
  emb
}

wsi_spatial_extract_embedding_payload <- function(x, reduction = "pca") {
  for (slot_name in c("coordinates", "embeddings", "cell.embeddings", "cell_embeddings", "data")) {
    value <- wsi_seurat_slot(x, slot_name)
    if (wsi_seurat_matrix_like(value) || is.data.frame(value)) {
      return(value)
    }
  }
  if (wsi_seurat_matrix_like(x) || is.data.frame(x)) {
    return(x)
  }
  if (is.list(x)) {
    for (nm in c(reduction, toupper(reduction), "coordinates", "embeddings", "data")) {
      value <- x[[nm]]
      if (wsi_seurat_matrix_like(value) || is.data.frame(value)) {
        return(value)
      }
    }
  }
  x
}

wsi_find_spatial_coordinate_table <- function(x, names = character(), depth = 0L, seen = character()) {
  if (depth > 4L || is.null(x)) {
    return(NULL)
  }
  key <- paste0(class(x)[[1L]] %||% typeof(x), ":", utils::capture.output(utils::str(x, max.level = 0L))[1L] %||% "")
  if (key %in% seen) {
    return(NULL)
  }
  seen <- c(seen, key)
  if (is.data.frame(x)) {
    if (!is.null(wsi_seurat_first_column(x, c("x", "X", "sdimx", "sdimX", "spatial_x", "imagecol", "pxl_col_in_fullres"))) &&
        !is.null(wsi_seurat_first_column(x, c("y", "Y", "sdimy", "sdimY", "spatial_y", "imagerow", "pxl_row_in_fullres")))) {
      return(x)
    }
  }
  children <- wsi_spatial_children(x)
  if (!length(children)) {
    return(NULL)
  }
  preferred <- intersect(names, names(children))
  for (nm in c(preferred, setdiff(names(children), preferred))) {
    out <- wsi_find_spatial_coordinate_table(children[[nm]], names = names, depth = depth + 1L, seen = seen)
    if (!is.null(out)) {
      return(out)
    }
  }
  NULL
}

wsi_find_spatial_embedding_table <- function(x, reduction = "pca", names = character(), depth = 0L, seen = character()) {
  if (depth > 5L || is.null(x)) {
    return(NULL)
  }
  key <- paste0(class(x)[[1L]] %||% typeof(x), ":", utils::capture.output(utils::str(x, max.level = 0L))[1L] %||% "")
  if (key %in% seen) {
    return(NULL)
  }
  seen <- c(seen, key)
  if (wsi_spatial_embedding_candidate(x, reduction = reduction)) {
    return(x)
  }
  children <- wsi_spatial_children(x)
  if (!length(children)) {
    return(NULL)
  }
  reduction_names <- names(children)[grepl(reduction, names(children), ignore.case = TRUE)]
  preferred <- unique(c(intersect(names, names(children)), reduction_names))
  for (nm in c(preferred, setdiff(names(children), preferred))) {
    out <- wsi_find_spatial_embedding_table(children[[nm]], reduction = reduction, names = names, depth = depth + 1L, seen = seen)
    if (!is.null(out)) {
      return(out)
    }
  }
  NULL
}

wsi_spatial_embedding_candidate <- function(x, reduction = "pca") {
  if (!(wsi_seurat_matrix_like(x) || is.data.frame(x))) {
    return(FALSE)
  }
  dims <- dim(x)
  if (length(dims) != 2L || dims[[2L]] < 2L || dims[[2L]] > 200L || dims[[1L]] < 1L) {
    return(FALSE)
  }
  if (is.data.frame(x)) {
    numeric_cols <- vapply(x, is.numeric, logical(1))
    return(sum(numeric_cols) >= 2L)
  }
  TRUE
}

wsi_spatial_children <- function(x) {
  children <- list()
  if (is.list(x)) {
    children <- x
  }
  if (isS4(x)) {
    slots <- methods::slotNames(x)
    for (slot_name in slots) {
      value <- tryCatch(methods::slot(x, slot_name), error = function(e) NULL)
      if (!is.null(value)) {
        children[[slot_name]] <- value
      }
    }
  }
  children
}
