#' Link a spatial omics object to a high-resolution image
#'
#' `wsi_link_spatial_image()` is the shared entry point for linking spatial
#' omics coordinates, dimensional reductions, and optional expression data to a
#' tissue image. It dispatches to the existing Seurat, Giotto, or
#' SpatialExperiment linkers while preserving their specialised arguments.
#'
#' Use `object_type = "auto"` for ordinary Seurat, Giotto, and
#' SpatialExperiment objects. For lightweight lists or unusual objects, set
#' `object_type` explicitly.
#'
#' @param object A Seurat, Giotto, SpatialExperiment, or compatible object.
#' @param image Path to the high-resolution image, or a `wsi_slide` object.
#' @param object_type Object family. `"auto"` tries to infer the backend.
#' @param ... Arguments passed to the selected linker, for example
#'   `coordinates`, `embeddings`, `reduction`, `spot_genes`, `default_gene`,
#'   `coordinate_flip`, and `coordinate_rotation`.
#'
#' @return A linked spatial object accepted by [wsi_viewer_spatial()] and the
#'   lower-level viewer functions.
#' @export
#'
#' @examples
#' \dontrun{
#' linked <- wsi_link_spatial_image(brain, "tissue.tif", object_type = "seurat")
#' viewer <- wsi_viewer_spatial(brain, "tissue.tif", live = TRUE)
#' }
wsi_link_spatial_image <- function(object, image,
                                   object_type = c("auto", "seurat", "giotto", "spatialexperiment"),
                                   ...) {
  object_type <- wsi_spatial_object_type(object, object_type)
  switch(
    object_type,
    seurat = wsi_link_seurat_image(object, image, ...),
    giotto = wsi_link_giotto_image(object, image, ...),
    spatialexperiment = wsi_link_spatialexperiment_image(object, image, ...)
  )
}

#' Open a spatial omics object in the wsiTools viewer
#'
#' `wsi_viewer_spatial()` is a common viewer entry point for Seurat, Giotto,
#' and SpatialExperiment-style objects. Static mode writes an HTML viewer.
#' Live mode starts the R/httpuv bridge, enabling on-demand gene lookup from R
#' without serialising the full expression matrix into the browser.
#'
#' @inheritParams wsi_link_spatial_image
#' @param linked Optional linked object returned by [wsi_link_spatial_image()]
#'   or one of the specialised linkers.
#' @inheritParams wsi_viewer_giotto
#'
#' @return The HTML path for static mode, or a `wsi_viewer_session` for live
#'   mode.
#' @export
wsi_viewer_spatial <- function(object, image, linked = NULL,
                               object_type = c("auto", "seurat", "giotto", "spatialexperiment"),
                               live = FALSE, dynamic_tiles = live,
                               mode = c("tiles", "thumbnail"),
                               output = NULL, open = interactive(),
                               overwrite = FALSE, ...) {
  object_type <- if (!is.null(linked) && inherits(linked, "wsi_seurat_spatial")) {
    tolower(as.character(linked$source_name %||% "auto"))
  } else {
    wsi_spatial_object_type(object, object_type)
  }
  object_type <- switch(
    object_type,
    seurat = "seurat",
    giotto = "giotto",
    spatialexperiment = "spatialexperiment",
    spatial_experiment = "spatialexperiment",
    wsi_abort(sprintf("Unsupported linked spatial source: %s", object_type))
  )
  switch(
    object_type,
    seurat = wsi_viewer_seurat(
      seurat = object,
      image = image,
      linked = linked,
      live = live,
      dynamic_tiles = dynamic_tiles,
      mode = mode,
      output = output,
      open = open,
      overwrite = overwrite,
      ...
    ),
    giotto = wsi_viewer_giotto(
      giotto = object,
      image = image,
      linked = linked,
      live = live,
      dynamic_tiles = dynamic_tiles,
      mode = mode,
      output = output,
      open = open,
      overwrite = overwrite,
      ...
    ),
    spatialexperiment = wsi_viewer_spatialexperiment(
      spe = object,
      image = image,
      linked = linked,
      live = live,
      dynamic_tiles = dynamic_tiles,
      mode = mode,
      output = output,
      open = open,
      overwrite = overwrite,
      ...
    )
  )
}

wsi_spatial_object_type <- function(object,
                                    object_type = c("auto", "seurat", "giotto", "spatialexperiment")) {
  object_type <- match.arg(object_type)
  if (!identical(object_type, "auto")) {
    return(object_type)
  }
  if (inherits(object, "Seurat") ||
      wsi_spatial_has_field(object, c("images", "reductions", "assays"))) {
    return("seurat")
  }
  if (inherits(object, "giotto") ||
      wsi_spatial_has_field(object, c("spatial_locs", "dimension_reduction", "spatial_info"))) {
    return("giotto")
  }
  if (inherits(object, "SpatialExperiment") ||
      wsi_spatial_has_field(object, c("spatialCoords", "reducedDims", "colData"))) {
    return("spatialexperiment")
  }
  wsi_abort(
    paste(
      "Could not infer the spatial object type.",
      "Set `object_type` to \"seurat\", \"giotto\", or \"spatialexperiment\"."
    )
  )
}

wsi_spatial_has_field <- function(object, fields) {
  names_or_slots <- character()
  if (is.list(object)) {
    names_or_slots <- names(object) %||% character()
  }
  if (isS4(object)) {
    names_or_slots <- union(names_or_slots, tryCatch(methods::slotNames(object), error = function(e) character()))
  }
  any(fields %in% names_or_slots)
}

wsi_spatial_reduction_plots <- function(object, source_name, spots,
                                        selected_embeddings, reduction, dims,
                                        gene_value_items = list(),
                                        cluster_value_items = list()) {
  reduction <- wsi_seurat_check_scalar_character(reduction, "reduction")
  source_key <- gsub("[^a-z0-9]+", "", tolower(source_name))
  reduction_names <- switch(
    source_key,
    seurat = wsi_seurat_reduction_names(object),
    giotto = wsi_giotto_reduction_names(object),
    spatialexperiment = wsi_spatialexperiment_reduction_names(object),
    character()
  )
  reduction_names <- unique(c(reduction, reduction_names))
  reduction_names <- reduction_names[nzchar(reduction_names) & !is.na(reduction_names)]
  if (!length(reduction_names)) {
    reduction_names <- reduction
  }

  plots <- list()
  seen <- character()
  for (name in reduction_names) {
    key <- tolower(name)
    if (key %in% seen) {
      next
    }
    seen <- c(seen, key)
    embeddings <- if (identical(tolower(name), tolower(reduction))) {
      selected_embeddings
    } else {
      tryCatch(
        switch(
          source_key,
          seurat = wsi_seurat_embeddings(object, reduction = name),
          giotto = wsi_giotto_embeddings(object, reduction = name),
          spatialexperiment = wsi_spatialexperiment_embeddings(
            if (inherits(object, "wsi_spatialexperiment_expression_source")) object$object else object,
            reduction = name
          ),
          NULL
        ),
        error = function(e) NULL
      )
    }
    plot <- wsi_spatial_reduction_plot(
      embeddings = embeddings,
      spots = spots,
      reduction = name,
      dims = dims,
      source_name = source_name,
      gene_value_items = gene_value_items,
      cluster_value_items = cluster_value_items
    )
    if (!is.null(plot)) {
      plots[[length(plots) + 1L]] <- plot
    }
  }
  plots
}

wsi_spatial_reduction_plot <- function(embeddings, spots, reduction, dims,
                                       source_name, gene_value_items = list(),
                                       cluster_value_items = list()) {
  if (is.null(embeddings) || is.null(spots) || !nrow(spots)) {
    return(NULL)
  }
  emb <- tryCatch(as.matrix(embeddings), error = function(e) NULL)
  if (is.null(emb) || length(dim(emb)) != 2L || !nrow(emb) || ncol(emb) < max(dims)) {
    return(NULL)
  }
  storage.mode(emb) <- "double"
  spot_ids <- as.character(spots$barcode %||% spots$id %||% spots$label)
  emb_ids <- rownames(emb) %||% as.character(seq_len(nrow(emb)))
  idx <- match(spot_ids, emb_ids)
  keep <- !is.na(idx)
  if (!any(keep) && nrow(emb) == nrow(spots)) {
    idx <- seq_len(nrow(spots))
    keep <- rep(TRUE, nrow(spots))
  }
  if (!any(keep)) {
    return(NULL)
  }
  spot_subset <- spots[keep, , drop = FALSE]
  values <- emb[idx[keep], dims, drop = FALSE]
  component_names <- colnames(emb)[dims]
  if (is.null(component_names) || any(!nzchar(component_names))) {
    component_names <- paste0(toupper(reduction), "_", dims)
  }
  colours <- wsi_seurat_gradient(values[, 1L])
  plot_points <- data.frame(
    label = as.character(spot_subset$label %||% spot_subset$id %||% spot_subset$barcode),
    spot_id = as.character(spot_subset$id %||% spot_subset$barcode %||% spot_subset$label),
    x = values[, 1L],
    y = values[, 2L],
    slide_x = as.numeric(spot_subset$x),
    slide_y = as.numeric(spot_subset$y),
    colour = colours,
    color = colours,
    base_colour = colours,
    base_color = colours,
    stringsAsFactors = FALSE
  )
  if (length(gene_value_items)) {
    subset_gene_values <- gene_value_items[which(keep)]
    if (length(subset_gene_values)) {
      plot_points$gene_values <- I(subset_gene_values)
    }
  }
  if (length(cluster_value_items)) {
    subset_cluster_values <- cluster_value_items[which(keep)]
    if (length(subset_cluster_values)) {
      plot_points$cluster_values <- I(subset_cluster_values)
    }
  }
  list(
    id = paste0(tolower(wsi_safe_id(source_name, "spatial")), "_", wsi_safe_id(reduction, "reduction")),
    label = paste0(source_name, " ", wsi_reduction_label(reduction), " plot"),
    reduction = reduction,
    x_label = component_names[[1L]],
    y_label = component_names[[2L]],
    point_count = nrow(plot_points),
    points = plot_points
  )
}

wsi_reduction_label <- function(reduction) {
  reduction <- as.character(reduction %||% "reduction")
  known <- c(
    pca = "PCA",
    umap = "UMAP",
    tsne = "tSNE",
    t_sne = "tSNE",
    kodama = "KODAMA"
  )
  key <- tolower(reduction)
  if (key %in% names(known)) {
    return(known[[key]])
  }
  reduction
}

wsi_seurat_reduction_names <- function(seurat) {
  out <- character()
  for (namespace in c("SeuratObject", "Seurat")) {
    value <- wsi_seurat_try_accessor(namespace, "Reductions", object = seurat) %||%
      wsi_seurat_try_accessor(namespace, "Reductions", seurat)
    if (length(value)) {
      out <- c(out, as.character(value))
    }
  }
  reductions <- wsi_seurat_slot(seurat, "reductions")
  if (is.list(reductions)) {
    out <- c(out, names(reductions) %||% character())
  }
  unique(out[nzchar(out) & !is.na(out)])
}

wsi_giotto_reduction_names <- function(giotto) {
  root <- wsi_seurat_slot(giotto, "dimension_reduction") %||%
    wsi_seurat_slot(giotto, "dim_reduction") %||%
    wsi_seurat_slot(giotto, "dimReduction")
  unique(wsi_giotto_collect_reduction_names(root))
}

wsi_giotto_collect_reduction_names <- function(x, parent_name = NULL,
                                               depth = 0L, seen = character()) {
  if (is.null(x) || depth > 8L) {
    return(character())
  }
  key <- tryCatch(
    paste(parent_name %||% "", depth, wsi_spatial_seen_key(x), sep = ":"),
    error = function(e) paste(parent_name %||% "", typeof(x), length(x), depth, sep = ":")
  )
  if (key %in% seen) {
    return(character())
  }
  seen <- c(seen, key)
  if (isS4(x)) {
    coords <- tryCatch(wsi_seurat_slot(x, "coordinates"), error = function(e) NULL)
    method <- tryCatch(wsi_seurat_slot(x, "reduction_method"), error = function(e) NULL)
    name <- tryCatch(wsi_seurat_slot(x, "name"), error = function(e) NULL)
    label <- as.character(method %||% name %||% parent_name %||% character())
    if (!is.null(coords) && wsi_spatial_embedding_candidate(coords) && length(label) && nzchar(label[[1L]])) {
      return(label[[1L]])
    }
  }
  out <- character()
  children <- list()
  if (is.list(x)) {
    children <- x
  }
  if (isS4(x)) {
    for (slot_name in methods::slotNames(x)) {
      children[[slot_name]] <- tryCatch(methods::slot(x, slot_name), error = function(e) NULL)
    }
  }
  if (!length(children)) {
    return(out)
  }
  nms <- names(children)
  if (is.null(nms)) {
    nms <- rep(NA_character_, length(children))
  }
  for (i in seq_along(children)) {
    child_name <- if (!is.na(nms[[i]]) && nzchar(nms[[i]])) nms[[i]] else parent_name
    out <- c(out, wsi_giotto_collect_reduction_names(children[[i]], parent_name = child_name, depth = depth + 1L, seen = seen))
  }
  out[nzchar(out) & !is.na(out)]
}

wsi_spatialexperiment_reduction_names <- function(spe) {
  if (inherits(spe, "wsi_spatialexperiment_expression_source")) {
    spe <- spe$object
  }
  out <- wsi_optional_accessor("SingleCellExperiment", "reducedDimNames", x = spe)
  if (length(out)) {
    return(unique(as.character(out)))
  }
  reduced <- wsi_seurat_slot(spe, "reducedDims") %||% wsi_seurat_slot(spe, "reduced_dims")
  if (is.list(reduced)) {
    return(unique(as.character(names(reduced) %||% character())))
  }
  character()
}

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

#' Open a multi-slide SpatialExperiment project in the wsiTools viewer
#'
#' `wsi_viewer_spatialexperiment_project()` links a multi-sample
#' `SpatialExperiment` object to one high-resolution tissue image per sample
#' and opens the sections as a single wsiTools project. Each project item keeps
#' its own spot overlay and only the dimensional-reduction buttons that are
#' present in the object, such as PCA, t-SNE, UMAP, or custom reductions.
#'
#' @param spe A SpatialExperiment object, or SpatialExperiment-like object with
#'   `colData()`, `spatialCoords()`, and `reducedDims()`.
#' @param images Named character vector/list of high-resolution image paths or
#'   `wsi_slide` objects. Names are used as `sample_id` values when
#'   `sample_ids` is not supplied.
#' @param sample_ids Sample identifiers matching a `sample_id`, `sample`,
#'   `section`, or `image_id` column in `colData(spe)`. When `NULL`, names on
#'   `images` are used; if those are missing, unique sample IDs are inferred
#'   from `colData(spe)` when their number matches `images`.
#' @param image_names Optional image/section names stored in the linked objects.
#'   Defaults to `sample_ids`.
#' @param labels Optional labels shown in the Project panel.
#' @param assay_name Optional assay name used for live gene expression lookup.
#' @inheritParams wsi_link_spatialexperiment_image
#' @inheritParams wsi_viewer_seurat_project
#'
#' @return The HTML viewer path.
#' @export
#'
#' @examples
#' \dontrun{
#' images <- c(
#'   `151507` = "151507_full_image.tif",
#'   `151508` = "151508_full_image.tif"
#' )
#' html <- wsi_viewer_spatialexperiment_project(
#'   spe,
#'   images = images,
#'   coordinate_scale = "none",
#'   output = "spatialexperiment_project.html"
#' )
#' }
wsi_viewer_spatialexperiment_project <- function(spe = NULL, images = NULL,
                                                 linked = NULL,
                                                 sample_ids = NULL,
                                                 image_names = NULL,
                                                 labels = NULL,
                                                 assay_name = NULL,
                                                 reduction = "PCA",
                                                 dims = c(1L, 2L),
                                                 coordinate_scale = c("auto", "none", "fullres", "hires", "lowres", "seurat_image", "custom"),
                                                 scale_x = NULL,
                                                 scale_y = NULL,
                                                 coordinate_flip = c("none", "vertical", "horizontal"),
                                                 coordinate_rotation = c(0, 90, 180, 270),
                                                 coordinate_transform = "none",
                                                 spot_genes = NULL,
                                                 default_gene = NULL,
                                                 spot_radius = NULL,
                                                 max_points = 100000L,
                                                 colour_by = c("component_1", "gene", "none"),
                                                 mode = c("tiles", "thumbnail"),
                                                 output = NULL,
                                                 open = interactive(),
                                                 overwrite = FALSE,
                                                 title = "wsiTools SpatialExperiment project viewer",
                                                 width = 1600,
                                                 height = NULL,
                                                 tile_dir = NULL,
                                                 tile_size = 512,
                                                 tile_format = c("jpg", "png"),
                                                 quality = 90,
                                                 rebuild = FALSE,
                                                 tile_overlap = NULL,
                                                 roi_class_presets = wsi_roi_class_presets()) {
  mode <- match.arg(mode)
  tile_format <- match.arg(tile_format)
  coordinate_scale <- match.arg(coordinate_scale)
  coordinate_flip <- wsi_seurat_coordinate_flip_arg(coordinate_flip)
  coordinate_rotation <- wsi_seurat_coordinate_rotation_arg(coordinate_rotation)
  colour_by <- match.arg(colour_by)
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
    wsi_link_spatialexperiment_project_sections(
      spe = spe,
      images = images,
      sample_ids = sample_ids,
      image_names = image_names,
      assay_name = assay_name,
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
    wsi_abort("No linked SpatialExperiment sections were supplied.")
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
    tile_overlap = tile_overlap
  )

  first <- linked[[1L]]
  first_record <- records[[1L]]
  first_layer <- wsi_seurat_spots_layer(first)
  first_layer$project_scoped <- TRUE

  if (identical(mode, "thumbnail")) {
    return(wsi_viewer(
      first$slide,
      width = width,
      height = height,
      output = output,
      open = open,
      title = title,
      overwrite = TRUE,
      mode = "thumbnail",
      roi_class_presets = roi_class_presets,
      project_images = records,
      layers = list(first_layer),
      seurat = first
    ))
  }

  wsi_viewer(
    first$slide,
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
    tile_source_label = "SpatialExperiment project Deep Zoom tiles",
    roi_class_presets = roi_class_presets,
    project_images = records,
    layers = list(first_layer),
    seurat = first
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

wsi_link_spatialexperiment_project_sections <- function(spe, images,
                                                        sample_ids = NULL,
                                                        image_names = NULL,
                                                        assay_name = NULL,
                                                        reduction = "PCA",
                                                        dims = c(1L, 2L),
                                                        coordinate_scale = "auto",
                                                        scale_x = NULL,
                                                        scale_y = NULL,
                                                        coordinate_flip = "none",
                                                        coordinate_rotation = 0,
                                                        coordinate_transform = "none",
                                                        spot_genes = NULL,
                                                        default_gene = NULL,
                                                        spot_radius = NULL,
                                                        max_points = 100000L,
                                                        colour_by = "component_1") {
  if (is.null(spe)) {
    wsi_abort("`spe` is required when `linked` is not supplied.")
  }
  images <- wsi_seurat_project_images(images)
  n <- length(images)
  sample_ids <- wsi_spatialexperiment_project_sample_ids(spe, images, sample_ids)
  image_names <- wsi_spatialexperiment_project_image_names(sample_ids, image_names)

  linked <- vector("list", n)
  for (i in seq_len(n)) {
    linked[[i]] <- wsi_link_spatialexperiment_image(
      spe = spe,
      image = images[[i]],
      image_name = image_names[[i]],
      sample_id = sample_ids[[i]],
      assay_name = assay_name,
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
  names(linked) <- image_names
  linked
}

wsi_spatialexperiment_project_sample_ids <- function(spe, images, sample_ids = NULL) {
  n <- length(images)
  if (!is.null(sample_ids)) {
    if (!is.character(sample_ids) || length(sample_ids) != n ||
        anyNA(sample_ids) || any(!nzchar(sample_ids))) {
      wsi_abort("`sample_ids` must be `NULL` or a character vector with one non-empty sample ID per image.")
    }
    return(as.character(sample_ids))
  }
  image_names <- names(images)
  if (!is.null(image_names) && length(image_names) == n &&
      !anyNA(image_names) && all(nzchar(image_names))) {
    return(as.character(image_names))
  }
  inferred <- wsi_spatialexperiment_sample_ids(spe)
  if (length(inferred) == n) {
    return(inferred)
  }
  wsi_abort(
    paste(
      "Could not infer `sample_ids` for the SpatialExperiment project.",
      "Supply `sample_ids`, or name `images` with values from the SpatialExperiment sample_id column."
    )
  )
}

wsi_spatialexperiment_project_image_names <- function(sample_ids, image_names = NULL) {
  n <- length(sample_ids)
  if (is.null(image_names)) {
    return(as.character(sample_ids))
  }
  if (!is.character(image_names) || length(image_names) != n ||
      anyNA(image_names) || any(!nzchar(image_names))) {
    wsi_abort("`image_names` must be `NULL` or a character vector with one non-empty name per image.")
  }
  as.character(image_names)
}

wsi_spatialexperiment_sample_ids <- function(spe) {
  cd <- wsi_optional_accessor("SummarizedExperiment", "colData", x = spe)
  if (is.null(cd)) {
    return(character())
  }
  cd <- as.data.frame(cd)
  sample_col <- wsi_seurat_first_column(cd, c("sample_id", "sample", "section", "image_id"))
  if (is.null(sample_col)) {
    return(character())
  }
  unique(as.character(cd[[sample_col]]))
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
  scale_metadata <- wsi_spatial_scale_metadata(
    slide = slide,
    coordinates = coordinates,
    scale_factors = list(),
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
  cluster_values <- wsi_spatial_clusters(object, spot_ids = ids)

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
    object = object,
    source_name = source_name,
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
      source_name = source_name,
      gene_value_items = gene_value_items,
      cluster_value_items = cluster_value_items
    ))
    plots <- plots[!vapply(plots, is.null, logical(1))]
  }
  primary_plot <- if (length(plots)) {
    plots[[1L]]
  } else {
    list(
      id = paste0(tolower(wsi_safe_id(source_name, "spatial")), "_", wsi_safe_id(reduction, "reduction")),
      label = paste0(source_name, " ", toupper(reduction), " plot"),
      reduction = reduction,
      x_label = component_names[[1L]],
      y_label = component_names[[2L]],
      point_count = total,
      points = plot_points
    )
  }

  if (is.null(spot_radius)) {
    spot_radius <- wsi_seurat_spot_radius(
      scale_factors = list(),
      mapping = mapping,
      scale_metadata = scale_metadata
    )
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
    mpp = scale_metadata$mpp,
    pixel_size = scale_metadata$mpp,
    scale_metadata = scale_metadata,
    gene_expression = gene_expression,
    spot_radius = spot_radius,
    spot_count = total,
    displayed_spot_count = nrow(spots),
    spots = spots,
    clusters = wsi_spatial_cluster_config(cluster_values),
    cluster_fields = attr(cluster_values, "fields", exact = TRUE) %||% wsi_empty_spatial_cluster_fields(),
    cluster_values = cluster_values,
    expression_source = list(
      object = object,
      spot_ids = as.character(spots$barcode %||% spots$id)
    ),
    plots = plots,
    pca = primary_plot
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
  direct_root <- wsi_seurat_slot(giotto, "spatial_locs")
  if (is.null(direct_root)) {
    direct_root <- giotto
  }
  direct <- wsi_find_spatial_coordinate_table(
    direct_root,
    names = c("spatial_locs", "spatial_locations", "spat_locs", "spatLocs", "coordinates", "cell_metadata")
  )
  if (!is.null(direct)) {
    return(wsi_giotto_image_coordinates(direct))
  }

  args <- list(spat_unit = spat_unit, feat_type = feat_type)
  for (namespace in c("Giotto", "GiottoClass")) {
    out <- wsi_optional_accessor(namespace, "spatLocs", gobject = giotto, .args = args) %||%
      wsi_optional_accessor(namespace, "spatLocs", giotto, .args = args) %||%
      wsi_optional_accessor(namespace, "getSpatialLocations", gobject = giotto, .args = args) %||%
      wsi_optional_accessor(namespace, "getSpatialLocations", giotto, .args = args)
    if (!is.null(out)) {
      return(wsi_giotto_image_coordinates(out))
    }
  }
  out <- wsi_find_spatial_coordinate_table(
    giotto,
    names = c("spatial_locs", "spatial_locations", "spat_locs", "spatLocs", "coordinates", "cell_metadata")
  )
  wsi_giotto_image_coordinates(out)
}

wsi_giotto_embeddings <- function(giotto, reduction = "pca", spat_unit = NULL, feat_type = NULL) {
  direct_root <- wsi_seurat_slot(giotto, "dimension_reduction")
  if (is.null(direct_root)) {
    direct_root <- wsi_seurat_slot(giotto, "dim_reduction")
  }
  if (is.null(direct_root)) {
    direct_root <- wsi_seurat_slot(giotto, "dimReduction")
  }
  direct <- wsi_giotto_direct_embedding(direct_root, reduction = reduction)
  if (!is.null(direct)) {
    return(wsi_spatial_extract_embedding_payload(direct, reduction = reduction))
  }

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

wsi_giotto_direct_embedding <- function(x, reduction = "pca", depth = 0L) {
  if (is.null(x) || depth > 6L) {
    return(NULL)
  }
  if (isS4(x)) {
    method <- tryCatch(wsi_seurat_slot(x, "reduction_method"), error = function(e) NULL)
    coords <- tryCatch(wsi_seurat_slot(x, "coordinates"), error = function(e) NULL)
    if (!is.null(coords) && (is.null(method) || identical(tolower(as.character(method)), tolower(reduction)))) {
      if (wsi_spatial_embedding_candidate(coords, reduction = reduction)) {
        return(coords)
      }
    }
  }
  if (wsi_spatial_embedding_candidate(x, reduction = reduction)) {
    return(x)
  }
  if (!is.list(x)) {
    return(NULL)
  }
  nms <- names(x)
  preferred <- character()
  if (!is.null(nms)) {
    preferred <- nms[match(tolower(reduction), tolower(nms), nomatch = 0L)]
    preferred <- unique(c(preferred, nms[grepl(reduction, nms, ignore.case = TRUE)]))
  }
  for (nm in c(preferred, setdiff(nms %||% character(), preferred))) {
    out <- wsi_giotto_direct_embedding(x[[nm]], reduction = reduction, depth = depth + 1L)
    if (!is.null(out)) {
      return(out)
    }
  }
  NULL
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
  key <- wsi_spatial_seen_key(x)
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
  key <- wsi_spatial_seen_key(x)
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

wsi_spatial_seen_key <- function(x) {
  class_name <- class(x)[[1L]] %||% typeof(x)
  fields <- tryCatch(
    {
      if (isS4(x)) {
        methods::slotNames(x)
      } else {
        names(x)
      }
    },
    error = function(e) character()
  )
  paste(class_name, typeof(x), length(x), paste(fields, collapse = ","), sep = ":")
}

wsi_giotto_image_coordinates <- function(coords) {
  if (is.null(coords)) {
    return(NULL)
  }
  if (isS4(coords)) {
    extracted <- wsi_seurat_slot(coords, "coordinates")
    if (!is.null(extracted)) {
      coords <- extracted
    }
  }
  if (!is.data.frame(coords)) {
    return(coords)
  }
  y_col <- wsi_seurat_first_column(coords, c("sdimy", "sdimY"))
  if (!is.null(y_col)) {
    y <- suppressWarnings(as.numeric(coords[[y_col]]))
    if (length(y) && all(is.finite(y)) && max(y) <= 0 && min(y) < 0) {
      coords[[y_col]] <- -y
      attr(coords, "coordinate_transform") <- "giotto_negative_sdimy_flipped"
    }
  }
  coords
}
