# Same-section adapters carry normalized metadata, never expression matrices.
wsi_review_spatial_source <- function(object, trusted = FALSE) {
  reference <- list(type = "embedded-review-data")
  if (is.character(object) && length(object) == 1L) {
    if (!isTRUE(trusted)) wsi_abort("Reading an RDS attachment requires trusted=TRUE; select only a trusted local file.")
    if (grepl("^[[:alpha:]][[:alnum:]+.-]*://", object) || !file.exists(object) || dir.exists(object))
      wsi_abort("Spatial attachment must be an accessible local RDS file.")
    limit <- getOption("wsiTools.review_max_rds_bytes", 256 * 1024^2)
    if (!is.numeric(limit) || length(limit) != 1L || !is.finite(limit) || limit <= 0 || file.info(object)$size > limit)
      wsi_abort("Spatial RDS exceeds wsiTools.review_max_rds_bytes; supply a bounded metadata/reduction export or explicitly raise the limit.")
    path <- normalizePath(object, winslash = "/", mustWork = TRUE)
    reference <- list(type = "trusted-local-rds", path = path, sha256 = wsi_experiment_sha256(path))
    object <- tryCatch(readRDS(path), error = function(e) wsi_abort(paste("Cannot read spatial RDS:", conditionMessage(e))))
    if (!identical(wsi_experiment_sha256(path), reference$sha256)) wsi_abort("Spatial RDS changed while reading.")
  }
  if (!is.list(object) && !isS4(object)) wsi_abort("Spatial attachment must be a Seurat object or a compatible metadata/coordinates list.")
  list(object = object, reference = reference)
}

wsi_review_spatial_coordinates <- function(object, coordinates = NULL, image_name = NULL) {
  images <- if (is.null(coordinates)) wsi_seurat_images(object) else character()
  if (length(images) > 1L && is.null(image_name))
    wsi_abort("This object has multiple spatial images; select image_name explicitly for the same tissue section.")
  if (is.null(coordinates) && length(images) <= 1L) coordinates <- wsi_seurat_slot(object, "coordinates")
  if (is.null(coordinates) && !length(images)) {
    # Reuse the existing metadata column conventions but retain every raw row;
    # the display helper otherwise removes invalid coordinates silently.
    discovered <- wsi_seurat_coordinates_from_metadata(object)
    if (!is.null(discovered)) {
      metadata <- wsi_seurat_slot(object, "meta.data")
      id <- attr(discovered, "id_column", exact = TRUE)
      coordinates <- data.frame(id = if (id %in% names(metadata)) as.character(metadata[[id]]) else rownames(metadata),
        x = metadata[[attr(discovered, "x_column", exact = TRUE)]],
        y = metadata[[attr(discovered, "y_column", exact = TRUE)]], stringsAsFactors = FALSE)
      attr(coordinates, "coordinate_space") <- attr(discovered, "coordinate_space", exact = TRUE)
      attr(coordinates, "coordinate_source") <- "seurat_meta.data"
    }
  }
  if (is.null(coordinates)) {
    # Legacy display extraction may remove invalid rows. Capture the selected
    # image's unfiltered identities first, so a persistent import cannot silently
    # become a smaller dataset or fall back to unrelated global metadata.
    expected_ids <- NULL
    if (length(images)) {
      selected_image <- wsi_seurat_image_name(object, image_name)
      image_object <- wsi_seurat_image_object(object, selected_image)
      raw <- wsi_seurat_coordinates_from_accessor(object, selected_image) %||%
        wsi_seurat_slot(image_object, "coordinates")
      if (is.null(raw)) {
        boundaries <- wsi_seurat_slot(image_object, "boundaries") %||% list()
        for (name in unique(c("centroids", "Centroids", names(boundaries)))) {
          boundary <- boundaries[[name]]
          candidate <- wsi_seurat_slot(boundary, "coords")
          if ((!is.matrix(candidate) && !is.data.frame(candidate)) || !nrow(candidate) || ncol(candidate) < 2L) next
          cells <- wsi_seurat_slot(boundary, "cells")
          raw <- as.data.frame(candidate, stringsAsFactors = FALSE)
          if (!is.null(cells) && length(cells) == nrow(raw)) raw$barcode <- as.character(cells)
          break
        }
      }
      if ((!is.data.frame(raw) && !is.matrix(raw)) || !nrow(raw))
        wsi_abort("The selected spatial image has no identifiable coordinate table; global metadata was not used. Supply explicit same-section coordinates.")
      raw <- as.data.frame(raw, stringsAsFactors = FALSE)
      id <- wsi_seurat_first_column(raw, c("id", "barcode", "cell_id", "cell_ID", "spot_id", "cell", "cells"))
      expected_ids <- if (!is.null(id)) as.character(raw[[id]]) else wsi_seurat_coordinate_ids(raw)
      if (length(expected_ids) != nrow(raw) || anyNA(expected_ids) || any(!nzchar(trimws(expected_ids))) || anyDuplicated(expected_ids))
        wsi_abort("Selected-image coordinates contain invalid/missing rows or IDs; no rows were imported.")
    }
    coordinates <- wsi_seurat_coordinates(object, image_name = image_name)
    if (!is.null(expected_ids) && (nrow(coordinates) != length(expected_ids) ||
        !setequal(as.character(coordinates$id), expected_ids)))
      wsi_abort("Selected-image coordinates contain invalid/missing rows; no rows were imported.")
  }
  if (!is.data.frame(coordinates) && !is.matrix(coordinates)) wsi_abort("Spatial coordinates must be an explicit table.")
  attrs <- attributes(coordinates)
  coordinates <- as.data.frame(coordinates, stringsAsFactors = FALSE)
  id <- wsi_seurat_first_column(coordinates, c("id", "barcode", "cell_id", "cell_ID", "spot_id"))
  ids <- if (!is.null(id)) as.character(coordinates[[id]]) else rownames(coordinates)
  if (is.null(id) && identical(ids, as.character(seq_len(nrow(coordinates)))))
    wsi_abort("Spatial coordinates require explicit stable barcodes/IDs; generated row numbers are not identities.")
  if (!length(ids) || anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids))
    wsi_abort("Spatial coordinate IDs must be nonempty and unique within the declared tissue dataset.")
  x <- wsi_seurat_first_column(coordinates, c("x", "slide_x", "fullres_x", "pxl_col_in_fullres", "imagecol"))
  y <- wsi_seurat_first_column(coordinates, c("y", "slide_y", "fullres_y", "pxl_row_in_fullres", "imagerow"))
  if (is.null(x) || is.null(y)) wsi_abort("Spatial coordinate table has no recognized X/Y columns; supply an explicit id/x/y table.")
  list(ids = ids, x = wsi_experiment_review_numeric(coordinates[[x]], "Spatial X"),
    y = wsi_experiment_review_numeric(coordinates[[y]], "Spatial Y"),
    metadata = list(coordinate_source = attrs$coordinate_source %||% "explicit_table",
      declared_coordinate_space = attrs$coordinate_space %||% "unknown", id_column = id %||% "rownames",
      x_column = x, y_column = y, image_name = image_name))
}

wsi_review_spatial_mapping <- function(image, mapping) {
  if (!is.list(mapping) || !isTRUE(mapping$confirmed))
    wsi_abort("Confirm the same-section coordinate mapping explicitly: coordinate_mapping needs confirmed=TRUE, image_sha256, frame_id and coordinate_space='native_pixels' (or source_pixels with an affine). Unknown coordinate space is never guessed.")
  if (!identical(tolower(mapping$image_sha256 %||% ""), image$sha256) || !identical(mapping$frame_id, image$frame_id))
    wsi_abort("Spatial mapping image SHA-256/frame_id differs from the active H&E image.")
  if (!is.null(mapping$same_section) && !isTRUE(mapping$same_section)) wsi_abort("Only explicitly same-section spatial attachments are supported.")
  space <- mapping$coordinate_space
  if (!is.character(space) || length(space) != 1L || !space %in% c("native_pixels", "level0_slide_pixels", "source_pixels"))
    wsi_abort("Unknown coordinate space: confirm native_pixels or supply source_pixels with an explicit affine.")
  affine <- mapping$affine
  if (is.null(affine)) {
    if (space == "source_pixels") wsi_abort("source_pixels require an explicit finite 3x3 affine to the declared image frame.")
    affine <- diag(3L)
  } else if (is.list(affine)) affine <- do.call(rbind, lapply(affine, unlist, use.names = FALSE))
  if (!is.matrix(affine) || !identical(dim(affine), c(3L, 3L)) || !is.numeric(affine) ||
      any(!is.finite(affine)) || any(abs(affine[3L, ] - c(0, 0, 1)) > 1e-10) || abs(det(affine)) < 1e-12)
    wsi_abort("Coordinate mapping must contain an invertible finite affine with final row [0,0,1].")
  if (space != "source_pixels" && any(abs(affine - diag(3L)) > 1e-10))
    wsi_abort("A nonidentity transform must be declared as source_pixels; native_pixels already refer to the image frame.")
  list(affine = affine, metadata = list(image_sha256 = image$sha256, frame_id = image$frame_id,
    coordinate_space = space, affine = lapply(seq_len(3L), function(i) as.list(affine[i, ])),
    confirmed = TRUE, same_section = TRUE))
}

#' Inspect a trusted spatial attachment without mapping it to an image
#' @param object Loaded Seurat/list object or trusted local RDS path.
#' @param trusted Explicit permission to deserialize a local RDS file.
#' @param coordinates Optional explicit coordinate table.
#' @param image_name Spatial image to use when the object contains several.
#' @return Coordinate source/count and available metadata/reduction names only.
#' @export
wsi_review_inspect_spatial <- function(object, trusted = FALSE, coordinates = NULL, image_name = NULL) {
  source <- wsi_review_spatial_source(object, trusted)
  coords <- wsi_review_spatial_coordinates(source$object, coordinates, image_name)
  metadata <- wsi_seurat_slot(source$object, "meta.data") %||% wsi_seurat_slot(source$object, "metadata")
  reductions <- wsi_seurat_slot(source$object, "reductions") %||% list()
  list(observation_count = length(coords$ids), coordinate_metadata = coords$metadata,
    metadata_fields = as.list(names(metadata) %||% character()),
    reduction_names = as.list(names(reductions) %||% character()), source_reference = source$reference,
    mapping_confirmed = FALSE)
}

#' Attach same-section spatial metadata and reductions to human review
#'
#' Native-pixel confirmation binds coordinates to the exact image SHA-256 and
#' frame. Source pixels instead require an explicit affine. No scaling, image
#' registration, barcode matching across sections, expression export or conversion
#' of cluster membership into expert labels is performed automatically.
#'
#' @param context A validated review context.
#' @inheritParams wsi_review_inspect_spatial
#' @param tissue_id Exact target tissue ID in context.
#' @param dataset_id Stable local dataset identity, distinct within this tissue.
#' @param coordinate_mapping List with confirmed=TRUE, image_sha256, frame_id,
#'   coordinate_space (native_pixels/source_pixels), optional same_section=TRUE,
#'   and a 3x3 affine for source_pixels. May be stored in a list-like object's
#'   wsiTools_review_mapping field. Confirmation is never inferred.
#' @param cluster_columns Explicit categorical metadata fields; NULL selects
#'   conventional available cluster fields only.
#' @param reduction_names Explicit Seurat reduction names; NULL includes none.
#' @param reductions Named matrices/data frames with stable row names, or
#'   named lists containing cell.embeddings. Only two explicit plotted dimensions
#'   are included and rows join by ID.
#' @param observation_unit spot or cell. Spatial observations never become tiles.
#' @param footprint_width_px,footprint_height_px Optional explicitly calibrated
#'   native-pixel footprint dimensions. Both must be supplied together.
#' @param max_observations Total context observation limit, without sampling.
#' @return A new bounded JSON-compatible review context. The original object and
#'   context are untouched; source references and mapping metadata are retained.
#' @export
wsi_review_attach_spatial <- function(context, object, tissue_id, dataset_id = "spatial",
  coordinates = NULL, coordinate_mapping = NULL, cluster_columns = NULL,
  reduction_names = NULL, reductions = NULL, image_name = NULL,
  observation_unit = "spot", trusted = FALSE, footprint_width_px = NULL,
  footprint_height_px = NULL, max_observations = 10000L) {
  context <- wsi_validate_review_context(context)
  tissue_id <- wsi_review_text(tissue_id, "tissue_id")
  dataset_id <- wsi_review_text(dataset_id, "dataset_id")
  index <- match(tissue_id, vapply(context$images, `[[`, character(1), "tissue_id"))
  if (is.na(index)) wsi_abort("Select an existing tissue_id for the same-section attachment.")
  if (!is.character(observation_unit) || length(observation_unit) != 1L || !observation_unit %in% c("spot", "cell"))
    wsi_abort("Spatial attachment observation_unit must be spot or cell, never tile/token.")
  if (any(vapply(context$observations, function(row) identical(row$tissue_id, tissue_id) &&
      identical(row$dataset_id, dataset_id), logical(1)))) wsi_abort("This tissue already has the declared dataset_id; choose a distinct stable ID.")
  source <- wsi_review_spatial_source(object, trusted)
  coordinate_mapping <- coordinate_mapping %||% wsi_seurat_slot(source$object, "wsiTools_review_mapping")
  mapping <- wsi_review_spatial_mapping(context$images[[index]], coordinate_mapping)
  coords <- wsi_review_spatial_coordinates(source$object, coordinates, image_name)
  count <- length(coords$ids)
  max_observations <- wsi_review_number(max_observations, "max_observations", TRUE, TRUE)
  if (length(context$observations) + count > max_observations)
    wsi_abort("Spatial attachment exceeds max_observations; provide an explicitly bounded same-section dataset. No rows were sampled.")
  if (xor(is.null(footprint_width_px), is.null(footprint_height_px))) wsi_abort("Declare both footprint dimensions or neither.")
  if (!is.null(footprint_width_px)) {
    wsi_review_number(footprint_width_px, "footprint_width_px", TRUE)
    wsi_review_number(footprint_height_px, "footprint_height_px", TRUE)
  }
  xy <- cbind(coords$x, coords$y, 1) %*% t(mapping$affine)
  metadata <- wsi_seurat_slot(source$object, "meta.data") %||% wsi_seurat_slot(source$object, "metadata")
  if (is.null(cluster_columns)) cluster_columns <- intersect(c("seurat_clusters", "cluster", "clusters", "leiden", "annotation"), names(metadata))
  cluster_columns <- wsi_review_ids(as.character(cluster_columns), "cluster_columns")
  if (length(cluster_columns)) {
    if (!is.data.frame(metadata) || any(!cluster_columns %in% names(metadata))) wsi_abort("Requested cluster metadata columns are unavailable.")
    id <- wsi_seurat_first_column(metadata, c("id", "barcode", "cell_id", "cell_ID", "spot_id"))
    ids <- if (is.null(id)) rownames(metadata) else as.character(metadata[[id]])
    if (is.null(ids) || anyNA(ids) || anyDuplicated(ids) || any(!coords$ids %in% ids))
      wsi_abort("Metadata must uniquely identify every attached observation by stable ID.")
    metadata <- metadata[match(coords$ids, ids), cluster_columns, drop = FALSE]
  }
  global <- wsi_experiment_namespace_id(context$experiment_id %||% context$context_id, tissue_id, dataset_id, coords$ids)
  observations <- lapply(seq_len(count), function(i) {
    clusters <- stats::setNames(lapply(cluster_columns, function(column) {
      value <- metadata[[column]][[i]]
      if (length(value) != 1L || (!is.atomic(value) && !is.factor(value))) wsi_abort("Cluster metadata must be scalar categorical values.")
      if (is.na(value)) NULL else as.character(value)
    }), cluster_columns)
    row <- list(id = global[[i]], original_id = coords$ids[[i]], tissue_id = tissue_id,
      frame_id = context$images[[index]]$frame_id, dataset_id = dataset_id,
      observation_unit = observation_unit, x = xy[i, 1L], y = xy[i, 2L], cluster_values = clusters)
    if (!is.null(footprint_width_px)) { row$footprint_width_px <- footprint_width_px; row$footprint_height_px <- footprint_height_px }
    row
  })
  if (is.null(reductions)) reductions <- list()
  if (!is.list(reductions) || (length(reductions) && (is.null(names(reductions)) || any(!nzchar(names(reductions))) || anyDuplicated(names(reductions)))))
    wsi_abort("reductions must be an explicitly named list.")
  reduction_names <- wsi_review_ids(as.character(reduction_names %||% character()), "reduction_names")
  for (name in reduction_names) {
    if (name %in% names(reductions)) wsi_abort("Reduction was supplied twice.")
    reductions[[name]] <- wsi_seurat_embeddings(source$object, name)
  }
  added_reductions <- lapply(names(reductions), function(name) {
    matrix <- reductions[[name]]
    if (is.list(matrix) && !is.data.frame(matrix)) matrix <- matrix$cell.embeddings
    if (!is.matrix(matrix) && !is.data.frame(matrix)) wsi_abort("Spatial reductions must contain explicit embedding matrices.")
    if (isTRUE(attr(matrix, "wsi_synthetic_ids"))) wsi_abort("Reduction has generated row IDs; stable barcodes are required.")
    ids <- rownames(matrix)
    if (is.null(ids) || anyNA(ids) || anyDuplicated(ids) || any(!coords$ids %in% ids) || ncol(matrix) < 2L)
      wsi_abort("Spatial reductions must uniquely identify every attached observation and contain two dimensions.")
    matrix <- as.matrix(matrix[match(coords$ids, ids), 1:2, drop = FALSE])
    x <- wsi_experiment_review_numeric(matrix[, 1L], "Spatial reduction dimension 1")
    y <- wsi_experiment_review_numeric(matrix[, 2L], "Spatial reduction dimension 2")
    list(id = wsi_experiment_namespace_id(context$experiment_id %||% context$context_id, tissue_id, dataset_id, name),
      label = paste(context$images[[index]]$name %||% tissue_id, dataset_id, name, sep = " \u00b7 "),
      observation_unit = observation_unit, dataset_id = dataset_id,
      frame_id = context$images[[index]]$frame_id,
      points = lapply(seq_len(count), function(i) list(id = global[[i]], x = x[[i]], y = y[[i]])))
  })
  attachment <- list(dataset_id = dataset_id, tissue_id = tissue_id, source_reference = source$reference,
    mapping = mapping$metadata, coordinate_metadata = coords$metadata, observation_unit = observation_unit,
    cluster_columns = as.list(cluster_columns), reduction_names = as.list(names(reductions)),
    observation_count = count, reconstruction = "Normalized observations and reductions are embedded in the bounded review context; expression matrices are not serialized.")
  provenance <- context$provenance
  provenance$spatial_attachments <- c(provenance$spatial_attachments %||% list(), list(attachment))
  if (identical(provenance$availability$status, "unavailable")) provenance$availability$status <- "partial"
  wsi_review_context(context$images, c(context$observations, observations),
    c(context$reductions, added_reductions), context$context_id, context$experiment_id, provenance)
}

#' @rdname wsi_review_attach_spatial
#' @param seurat Loaded Seurat/Seurat-like object or trusted local RDS.
#' @export
wsi_review_attach_seurat <- function(context, seurat, tissue_id, ...) {
  wsi_review_attach_spatial(context, object = seurat, tissue_id = tissue_id, ...)
}

#' Build review metadata from an already linked spatial image
#' @param linked A spatial image object created by the existing wsiTools linkers.
#' @param tissue_id Stable same-section identity.
#' @param frame_id Explicit native frame ID, or an identity derived from image bytes.
#' @param image_sha256 Optional expected image digest; current bytes are checked.
#' @param context_id Optional stable review context ID.
#' @param max_observations Bounded displayed observation limit; no sampling added.
#' @param confirmed Explicit confirmation of the existing source-to-image mapping.
#'   Without it, metadata remain available but observations are not promoted into
#'   a review frame. The linker's review_confirmed flag is also accepted.
#' @return A bounded review context retaining the linker's explicit mapping.
#' @export
wsi_review_context_from_linked <- function(linked, tissue_id = "section-1", frame_id = NULL,
  image_sha256 = NULL, context_id = NULL, max_observations = 10000L, confirmed = FALSE) {
  if (!is.list(linked) || !inherits(linked$slide, "wsi_slide")) wsi_abort("Expected a linked spatial image with its source slide.")
  path <- linked$image_path %||% linked$slide$path
  path <- wsi_review_text(path, "linked image path")
  if (!file.exists(path) || dir.exists(path)) wsi_abort("Linked image bytes are unavailable; an explicit image identity is required.")
  hash <- wsi_experiment_sha256(path)
  if (!is.null(image_sha256) && !identical(tolower(image_sha256), hash)) wsi_abort("Linked image SHA-256 mismatch.")
  frame_id <- frame_id %||% paste0("native:", hash)
  # A spacing estimate inferred from RNA geometry is not source image calibration.
  mpp <- wsi_viewer_mpp_payload(tryCatch(wsi_mpp(linked$slide), error = function(e) NULL))
  if (is.numeric(mpp) && length(mpp) == 1L) mpp <- list(x = mpp, y = mpp)
  mpp_x <- if (is.list(mpp)) mpp$x %||% mpp$mpp_x else NULL
  mpp_y <- if (is.list(mpp)) mpp$y %||% mpp$mpp_y else NULL
  calibrated <- is.numeric(mpp_x) && length(mpp_x) == 1L && is.finite(mpp_x) && mpp_x > 0 &&
    is.numeric(mpp_y) && length(mpp_y) == 1L && is.finite(mpp_y) && mpp_y > 0
  image <- list(tissue_id = tissue_id, name = linked$image_name %||% tissue_id, frame_id = frame_id, sha256 = hash,
    width = unname(linked$slide$dimensions[["width"]]), height = unname(linked$slide$dimensions[["height"]]),
    path = normalizePath(path, winslash = "/"), coordinate_convention = "pixel-edge-origin_centres-in-native-pixels",
    calibration_status = if (calibrated) "available" else "unavailable")
  if (calibrated) { image$mpp_x <- mpp_x; image$mpp_y <- mpp_y }
  provenance <- list(source_reference = list(type = "linked-spatial-image", path = image$path, sha256 = hash),
    linked_coordinate_mapping = linked$coordinate_mapping,
    availability = list(status = "available"))
  if (!isTRUE(confirmed) && !isTRUE(linked$coordinate_mapping$review_confirmed)) {
    provenance$availability <- list(status = "unavailable", reason = paste(
      "Confirm the same-section source-to-image mapping before review; existing automatic display mappings are not expert registration evidence."))
    return(wsi_review_context(list(image), list(), context_id = context_id %||% paste0("spatial:", substr(hash, 1L, 24L)), provenance = provenance))
  }
  context <- wsi_review_context(list(image), list(), context_id = context_id %||% paste0("spatial:", substr(hash, 1L, 24L)), provenance = provenance)
  spots <- linked$spots
  if (!is.data.frame(spots) || !nrow(spots)) return(context)
  if (!is.null(linked$spot_count) && linked$spot_count != nrow(spots))
    wsi_abort("Linked object has sampled observations; request an explicitly bounded unsampled source before review.")
  if (!is.list(linked$coordinate_mapping)) wsi_abort("Linked spatial object has no explicit source-to-image mapping.")
  object <- list(coordinates = data.frame(id = as.character(spots$id %||% spots$barcode), x = spots$x, y = spots$y))
  fields <- if (is.list(spots$cluster_values)) unique(unlist(lapply(spots$cluster_values, names), use.names = FALSE)) else character()
  metadata <- data.frame(id = object$coordinates$id, stringsAsFactors = FALSE)
  for (field in fields) metadata[[field]] <- vapply(spots$cluster_values, function(values) as.character(values[[field]] %||% NA_character_), character(1))
  object$metadata <- metadata
  reductions <- list()
  for (plot in linked$plots %||% list()) {
    points <- wsi_review_rows(plot$points, "linked reduction points")
    if (!length(points)) next
    ids <- vapply(points, function(point) as.character(point$id %||% point$barcode), character(1))
    matrix <- cbind(vapply(points, `[[`, numeric(1), "x"), vapply(points, `[[`, numeric(1), "y"))
    rownames(matrix) <- ids
    reductions[[wsi_review_text(plot$id, "linked reduction id")]] <- matrix
  }
  context <- wsi_review_attach_spatial(context, object, tissue_id, dataset_id = "linked-spatial",
    coordinate_mapping = list(confirmed = TRUE, image_sha256 = hash, frame_id = frame_id,
      coordinate_space = "native_pixels", same_section = TRUE),
    observation_unit = if (identical(linked$feature_type, "cell")) "cell" else "spot",
    cluster_columns = fields, reductions = reductions, max_observations = max_observations)
  context
}
