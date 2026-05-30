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
#' @param reduction Dimensional reduction to extract, for example `"pca"`.
#' @param dims Two reduction dimensions to plot.
#' @param coordinate_scale How to map Seurat image coordinates onto `image`.
#'   `"auto"` rescales from the stored Seurat image dimensions when coordinates
#'   appear to be in that preview space. `"none"` uses coordinates as supplied.
#'   `"seurat_image"` always rescales from the stored Seurat image dimensions.
#'   `"custom"` uses `scale_x` and `scale_y`.
#' @param scale_x,scale_y Custom coordinate scale factors used when
#'   `coordinate_scale = "custom"`.
#' @param spot_radius Spot marker radius, in slide pixels. When `NULL`, an
#'   estimate is taken from Seurat scale factors when available.
#' @param max_points Maximum number of spots to keep in the browser payload.
#'   This protects interactive HTML viewers from very large objects.
#' @param colour_by Spot colour mode. `"component_1"` colours by the first
#'   plotted reduction component; `"none"` uses one colour.
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
                                  reduction = "pca", dims = c(1L, 2L),
                                  coordinate_scale = c("auto", "none", "seurat_image", "custom"),
                                  scale_x = NULL, scale_y = NULL,
                                  spot_radius = NULL, max_points = 100000L,
                                  colour_by = c("component_1", "none")) {
  coordinate_scale <- match.arg(coordinate_scale)
  colour_by <- match.arg(colour_by)
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

  image_name <- wsi_seurat_image_name(seurat, image_name)
  image_obj <- wsi_seurat_image_object(seurat, image_name)
  coordinates <- wsi_seurat_coordinate_table(seurat, image_name = image_name, image_obj = image_obj)
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

  mapping <- wsi_seurat_coordinate_mapping(
    coordinates = coordinates,
    image_obj = image_obj,
    slide = slide,
    coordinate_scale = coordinate_scale,
    scale_x = scale_x,
    scale_y = scale_y
  )
  coordinates$x <- coordinates$x * mapping$scale_x
  coordinates$y <- coordinates$y * mapping$scale_y

  component_values <- embeddings[, dims, drop = FALSE]
  component_names <- colnames(embeddings)[dims]
  if (is.null(component_names) || any(!nzchar(component_names))) {
    component_names <- paste0(toupper(reduction), "_", dims)
  }
  colours <- if (identical(colour_by, "component_1")) {
    wsi_seurat_gradient(component_values[, 1L])
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
  }

  plot_points <- data.frame(
    label = spots$label,
    spot_id = spots$id,
    x = component_values[, 1L],
    y = component_values[, 2L],
    slide_x = spots$x,
    slide_y = spots$y,
    colour = spots$colour,
    color = spots$colour,
    stringsAsFactors = FALSE
  )

  if (is.null(spot_radius)) {
    spot_radius <- wsi_seurat_spot_radius(image_obj, mapping = mapping)
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

wsi_seurat_coordinate_table <- function(seurat, image_name, image_obj = NULL) {
  coords <- wsi_seurat_coordinates_from_accessor(seurat, image_name)
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

wsi_seurat_scale_factors <- function(image_obj) {
  sf <- wsi_seurat_slot(image_obj, "scale.factors") %||%
    wsi_seurat_slot(image_obj, "scale_factors")
  if (is.null(sf)) {
    return(list())
  }
  if (isS4(sf)) {
    out <- lapply(methods::slotNames(sf), function(name) methods::slot(sf, name))
    names(out) <- methods::slotNames(sf)
    return(out)
  }
  if (is.list(sf)) {
    return(sf)
  }
  list()
}

wsi_seurat_coordinate_mapping <- function(coordinates, image_obj, slide,
                                          coordinate_scale, scale_x = NULL,
                                          scale_y = NULL) {
  slide_width <- as.numeric(slide$dimensions[["width"]])
  slide_height <- as.numeric(slide$dimensions[["height"]])
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
  seurat_dims <- wsi_seurat_image_dimensions(image_obj)
  if (is.null(seurat_dims) || !all(is.finite(seurat_dims)) || any(seurat_dims <= 0)) {
    return(list(method = "none", scale_x = 1, scale_y = 1))
  }
  sx <- slide_width / seurat_dims[["width"]]
  sy <- slide_height / seurat_dims[["height"]]
  if (identical(coordinate_scale, "seurat_image")) {
    return(list(method = "seurat_image", scale_x = sx, scale_y = sy))
  }
  max_x <- suppressWarnings(max(coordinates$x, na.rm = TRUE))
  max_y <- suppressWarnings(max(coordinates$y, na.rm = TRUE))
  in_seurat_space <- is.finite(max_x) && is.finite(max_y) &&
    max_x <= seurat_dims[["width"]] * 1.15 &&
    max_y <= seurat_dims[["height"]] * 1.15 &&
    (slide_width > seurat_dims[["width"]] * 1.15 || slide_height > seurat_dims[["height"]] * 1.15)
  if (isTRUE(in_seurat_space)) {
    return(list(method = "auto_seurat_image", scale_x = sx, scale_y = sy))
  }
  list(method = "auto_none", scale_x = 1, scale_y = 1)
}

wsi_seurat_spot_radius <- function(image_obj, mapping) {
  sf <- wsi_seurat_scale_factors(image_obj)
  spot <- suppressWarnings(as.numeric(sf$spot %||% sf$spot_diameter_fullres %||% NA_real_))
  if (length(spot) == 1L && is.finite(spot) && spot > 0) {
    return(max(2, spot / 2))
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
