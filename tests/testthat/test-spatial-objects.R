test_that("Giotto-like objects can be linked with explicit coordinates", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800)
  ids <- paste0("spot", 1:4)
  coords <- data.frame(
    cell_ID = ids,
    sdimx = c(100, 200, 300, 400),
    sdimy = c(150, 250, 350, 450)
  )
  embeddings <- data.frame(
    cell_ID = ids,
    PC_1 = c(-1, -0.5, 0.5, 1),
    PC_2 = c(0.2, 0.4, 0.8, 1.2)
  )
  expression <- matrix(
    c(1, 2, 3, 4, 4, 3, 2, 1),
    nrow = 2,
    dimnames = list(c("GeneA", "GeneB"), ids)
  )

  linked <- wsi_link_giotto_image(
    list(expression = expression),
    slide,
    coordinates = coords,
    embeddings = embeddings,
    spot_genes = "GeneA",
    default_gene = "GeneA"
  )

  expect_s3_class(linked, "wsi_spatial_object")
  expect_s3_class(linked, "wsi_seurat_spatial")
  expect_equal(linked$source_name, "Giotto")
  expect_equal(nrow(linked$spots), 4)
  expect_equal(linked$component_names, c("PC_1", "PC_2"))
  expect_true(linked$gene_expression$enabled)
})

test_that("Giotto and SpatialExperiment cluster metadata can be inspected", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800)
  ids <- paste0("spot", 1:4)
  coords <- data.frame(
    cell_ID = ids,
    sdimx = c(100, 200, 300, 400),
    sdimy = c(150, 250, 350, 450)
  )
  embeddings <- data.frame(
    cell_ID = ids,
    PC_1 = c(-1, -0.5, 0.5, 1),
    PC_2 = c(0.2, 0.4, 0.8, 1.2)
  )
  giotto_like <- list(
    cell_metadata = data.frame(
      cell_ID = ids,
      leiden = c("0", "0", "1", "2"),
      stringsAsFactors = FALSE
    )
  )

  giotto_fields <- wsi_spatial_cluster_fields(giotto_like, spot_ids = ids)
  expect_equal(giotto_fields$field, "leiden")
  expect_equal(giotto_fields$storage, "cell_metadata")

  linked <- wsi_link_giotto_image(
    giotto_like,
    slide,
    coordinates = coords,
    embeddings = embeddings
  )
  expect_true(linked$clusters$enabled)
  expect_equal(linked$cluster_values$leiden, c("0", "0", "1", "2"))

  spe_like <- list(
    colData = data.frame(
      barcode = ids,
      cell_type = c("tumour", "stroma", "stroma", "normal"),
      stringsAsFactors = FALSE
    )
  )
  spe_fields <- wsi_spatial_cluster_fields(spe_like, spot_ids = ids)
  expect_equal(spe_fields$field, "cell_type")
  expect_equal(spe_fields$storage, "colData")
  spe_clusters <- wsi_spatial_clusters(spe_like, spot_ids = ids, field = "cell_type")
  expect_equal(spe_clusters$cell_type, c("tumour", "stroma", "stroma", "normal"))
})

test_that("Giotto slot-style coordinates with negative sdimy are mapped to image y", {
  slide <- wsiTools:::wsi_mock_slide(width = 500, height = 400)
  ids <- c("cell_a", "cell_b")
  coords <- data.frame(
    cell_ID = ids,
    sdimx = c(100, 250),
    sdimy = c(-40, -180)
  )
  embeddings <- matrix(
    c(1, 0, -1, 2),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(ids, c("PC_1", "PC_2"))
  )
  giotto_like <- list(
    spatial_locs = list(cell = list(raw = coords)),
    dimension_reduction = list(cells = list(cell = list(rna = list(pca = embeddings))))
  )

  linked <- wsi_link_giotto_image(
    giotto_like,
    slide,
    image_name = "mock giotto",
    reduction = "pca",
    coordinate_scale = "none"
  )

  expect_equal(linked$spots$x, c(100, 250))
  expect_equal(linked$spots$y, c(40, 180))
  expect_equal(linked$coordinate_mapping$method, "none")
})

test_that("Giotto exprMat-style expression can be fetched one gene at a time", {
  if (!methods::isClass("wsi_test_giotto_expr_obj")) {
    methods::setClass("wsi_test_giotto_expr_obj", slots = c(exprMat = "matrix"))
  }
  ids <- c("cell_a", "cell_b", "cell_c")
  expr <- methods::new(
    "wsi_test_giotto_expr_obj",
    exprMat = matrix(
      c(0, 1, 2, 5, 6, 7),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("Cryzl2", "Mbp"), ids)
    )
  )
  giotto_like <- list(
    expression = list(cell = list(rna = list(normalized = expr)))
  )

  values <- wsiTools:::wsi_seurat_gene_expression(
    giotto_like,
    genes = "Cryzl2",
    spot_ids = ids,
    default_gene = "Cryzl2",
    object_label = "Giotto"
  )

  expect_true(values$enabled)
  expect_equal(values$genes, "Cryzl2")
  expect_equal(as.numeric(values$values[, "Cryzl2"]), c(0, 1, 2))
})

test_that("generic spatial linker dispatches to Giotto architecture", {
  slide <- wsiTools:::wsi_mock_slide(width = 500, height = 400)
  ids <- c("cell_a", "cell_b")
  coords <- data.frame(cell_ID = ids, sdimx = c(100, 250), sdimy = c(120, 260))
  embeddings <- matrix(
    c(1, 0, -1, 2),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(ids, c("PC_1", "PC_2"))
  )
  expr <- matrix(
    c(3, 4, 9, 10),
    nrow = 2,
    dimnames = list(c("Cryzl2", "Mbp"), ids)
  )
  giotto_like <- list(expression = list(cell = list(rna = list(normalized = list(exprMat = expr)))))

  linked <- wsi_link_spatial_image(
    giotto_like,
    slide,
    object_type = "giotto",
    coordinates = coords,
    embeddings = embeddings,
    spot_genes = "Cryzl2"
  )

  expect_s3_class(linked, "wsi_spatial_object")
  expect_equal(linked$source_name, "Giotto")
  expect_true(linked$gene_expression$enabled)
})

test_that("Giotto viewer controls expose only present dimensional reductions", {
  if (!methods::isClass("wsi_test_giotto_dim_obj")) {
    methods::setClass("wsi_test_giotto_dim_obj", slots = c(reduction_method = "character", coordinates = "matrix"))
  }
  slide <- wsiTools:::wsi_mock_slide(width = 500, height = 400)
  ids <- c("cell_a", "cell_b", "cell_c")
  coords <- data.frame(cell_ID = ids, sdimx = c(100, 250, 300), sdimy = c(120, 260, 310))
  pca <- matrix(
    c(1, 0, -1, 2, 0, 3),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(ids, c("PC_1", "PC_2"))
  )
  kodama <- matrix(
    c(5, 1, 6, 2, 7, 3),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(ids, c("KODAMA_1", "KODAMA_2"))
  )
  giotto_like <- list(
    spatial_locs = list(cell = list(raw = coords)),
    dimension_reduction = list(
      cells = list(cell = list(rna = list(
        pca = list(pca = methods::new("wsi_test_giotto_dim_obj", reduction_method = "pca", coordinates = pca)),
        KODAMA = list(KODAMA = methods::new("wsi_test_giotto_dim_obj", reduction_method = "KODAMA", coordinates = kodama))
      )))
    )
  )

  linked <- wsi_link_giotto_image(
    giotto_like,
    slide,
    reduction = "pca",
    coordinate_scale = "none"
  )
  config <- list(seurat = wsiTools:::wsi_viewer_seurat_config(linked))
  controls <- wsiTools:::wsi_viewer_seurat_controls(config)

  expect_equal(vapply(linked$plots, function(x) x$reduction, character(1)), c("pca", "KODAMA"))
  expect_match(controls, ">PCA<", fixed = TRUE)
  expect_match(controls, ">KODAMA<", fixed = TRUE)
  expect_match(controls, "seuratRegistrationOpen", fixed = TRUE)
  expect_match(controls, "seuratRegistrationSave", fixed = TRUE)
  expect_false(grepl(">UMAP<", controls, fixed = TRUE))

  html <- tempfile(fileext = ".html")
  wsi_viewer(
    slide,
    output = html,
    open = FALSE,
    overwrite = TRUE,
    mode = "thumbnail",
    layers = list(wsiTools:::wsi_seurat_spots_layer(linked)),
    seurat = linked
  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "\"managed_analysis_project\":true", fixed = TRUE)
  expect_match(text, "seuratRegistrationWindow", fixed = TRUE)
  expect_match(text, "seuratRegistrationDragDrop", fixed = TRUE)
  expect_match(text, "seuratRegistrationMoveStepUp", fixed = TRUE)
  expect_match(text, "seuratRegistrationScaleStepUp", fixed = TRUE)
  expect_match(text, "seuratRegistrationRotateSmallCw", fixed = TRUE)
  expect_match(text, "restoreSeuratRegistrationUndo", fixed = TRUE)
  expect_match(text, "Save spatial object", fixed = TRUE)
  expect_match(text, "spatialObjectSaveDialog", fixed = TRUE)
  expect_match(text, "Coordinate-to-annotation CSV", fixed = TRUE)
  expect_match(text, "Spatial object file name", fixed = TRUE)
  expect_match(text, "spatialObjectSaveFileName", fixed = TRUE)
  expect_match(text, "spatialObjectPathWithFileName", fixed = TRUE)
  expect_match(text, "annotations:annotationGeojson", fixed = TRUE)
  expect_match(text, "overwriteConfirmed", fixed = TRUE)
  expect_match(text, "overwrite_required", fixed = TRUE)
  expect_match(text, "A file already exists at:\\n\\n", fixed = TRUE)
  expect_match(text, "Do you want to replace it?", fixed = TRUE)
  expect_match(text, "viewerErrorText", fixed = TRUE)
  expect_equal(lengths(regmatches(text, gregexpr("function roiFeature(", text, fixed = TRUE))), 1L)
  expect_match(text, "spatial_object_save_url", fixed = TRUE)
  expect_match(text, "spatial_registration_updated", fixed = TRUE)
  expect_match(text, "spatial_object_save_requested", fixed = TRUE)
  expect_false(grepl("id=\"projectOpenImage\"", text, fixed = TRUE))
  expect_false(grepl("id=\"projectImageFile\"", text, fixed = TRUE))
})

test_that("spatial object save response writes CSV and full R object", {
  registration_payload <- list(
    format = "csv",
    output = tempfile(fileext = ".csv"),
    spatial_registration = list(
      source = "Seurat",
      count = 2L,
      changed_count = 1L,
      coordinates = list(
        list(
          source = "Seurat", layer_id = "seurat_spots", layer_name = "Seurat spots",
          item_index = 1L, id = "spot_1", label = "spot_1",
          x = 12, y = 34, original_x = 10, original_y = 30, changed = TRUE
        ),
        list(
          source = "Seurat", layer_id = "seurat_spots", layer_name = "Seurat spots",
          item_index = 2L, id = "spot_2", label = "spot_2",
          x = 20, y = 40, original_x = 20, original_y = 40, changed = FALSE
        )
      )
    )
  )
  spatial <- structure(
    list(expression_source = list(object = list(
      meta.data = data.frame(value = c(1, 2), row.names = c("spot_1", "spot_2")),
      images = list(slice1 = list(coordinates = data.frame(
        imagecol = c(10, 20),
        imagerow = c(30, 40),
        row.names = c("spot_1", "spot_2")
      )))
    ))),
    class = "wsi_seurat_spatial"
  )
  state <- wsiTools:::wsi_new_viewer_state(name = "save_response_state", envir = new.env(parent = emptyenv()))
  state$rois <- data.frame(
    roi_id = "roi_tumour",
    name = "Tumour",
    class = "tumour",
    object_type = "annotation",
    color = "#ff0000",
    classification_color = "#ff0000",
    is_locked = FALSE,
    geometry_type = "Polygon",
    xmin = 0,
    ymin = 0,
    xmax = 15,
    ymax = 35,
    crs = NA_character_,
    stringsAsFactors = FALSE
  )
  state$rois$coordinates <- I(list(list(rbind(
    c(0, 0), c(15, 0), c(15, 35), c(0, 35), c(0, 0)
  ))))
  state$rois$measurements <- I(list(list()))
  state$rois$properties <- I(list(list()))
  state$rois$geometry <- I(list(list()))
  state$rois$feature <- I(list(list()))
  class(state$rois) <- c("wsi_roi", class(state$rois))

  csv_result <- wsiTools:::wsi_spatial_object_save_response(spatial, registration_payload, state = state)
  expect_true(file.exists(csv_result$file))
  csv <- utils::read.csv(csv_result$file, stringsAsFactors = FALSE)
  expect_equal(nrow(csv), 2L)
  expect_equal(csv$id, c("spot_1", "spot_2"))
  expect_equal(nrow(state$spatial_registration), 2L)

  rds_path <- tempfile(fileext = ".rds")
  rds_payload <- registration_payload
  rds_payload$format <- "rds"
  rds_payload$output <- rds_path
  rds_result <- wsiTools:::wsi_spatial_object_save_response(spatial, rds_payload, state = state)
  expect_true(file.exists(rds_result$file))
  expect_error(
    wsiTools:::wsi_spatial_object_save_response(spatial, rds_payload, state = state),
    "already exists",
    class = "wsi_overwrite_required"
  )
  rds_payload$overwrite <- TRUE
  overwrite_result <- wsiTools:::wsi_spatial_object_save_response(
    spatial,
    rds_payload,
    state = state
  )
  expect_true(overwrite_result$overwrite)
  saved <- readRDS(rds_result$file)
  expect_equal(saved$meta.data$registered_x, c(12, 20))
  expect_equal(saved$meta.data$registered_y, c(34, 40))
  expect_equal(saved$meta.data$wsi_registration_changed, c(TRUE, FALSE))
  expect_equal(saved$images$slice1$coordinates$x, c(12, 20))
  expect_equal(saved$images$slice1$coordinates$y, c(34, 40))
  expect_equal(saved$images$slice1$coordinates$imagecol, c(12, 20))
  expect_equal(saved$images$slice1$coordinates$imagerow, c(34, 40))
  expect_equal(saved$meta.data$wsi_annotation, c("tumour", "Unassigned"))
  expect_equal(saved$meta.data$wsi_annotation_id, c("roi_tumour", NA))
  expect_equal(saved$meta.data$wsi_annotation_name, c("Tumour", NA))
  expect_equal(saved$meta.data$wsi_annotation_index, c(1L, NA_integer_))
  expect_equal(saved$misc$wsiTools$annotation_association$assigned, 1L)
  expect_equal(saved$misc$wsiTools$annotation_association$unassigned, 1L)
  expect_equal(length(saved$misc$wsiTools$annotation_rois$features), 1L)
  expect_equal(rds_result$association_count, 2L)
  expect_equal(rds_result$assigned_count, 1L)
  expect_equal(nrow(state$annotation_spots), 1L)
  expect_equal(state$annotation_spots$spot_id, "spot_1")
  expect_s3_class(attr(saved, "wsi_spatial_registration"), "wsi_spatial_registration")

  invalid_overwrite <- rds_payload
  invalid_overwrite$output <- tempfile(fileext = ".rds")
  invalid_overwrite$overwrite <- "yes"
  expect_error(
    wsiTools:::wsi_spatial_object_save_response(spatial, invalid_overwrite, state = state),
    "must be one TRUE or FALSE"
  )

  association_path <- tempfile(fileext = ".csv")
  association_payload <- registration_payload
  association_payload$format <- "annotation_csv"
  association_payload$output <- association_path
  association_result <- wsiTools:::wsi_spatial_object_save_response(
    spatial,
    association_payload,
    state = state
  )
  expect_true(file.exists(association_result$file))
  association <- utils::read.csv(association_result$file, stringsAsFactors = FALSE)
  expect_equal(association$point_id, c("spot_1", "spot_2"))
  expect_equal(association$annotation_class, c("tumour", "Unassigned"))
})

test_that("SpatialExperiment-like objects can be linked with explicit coordinates", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800)
  ids <- paste0("spot", 1:3)
  coords <- data.frame(
    barcode = ids,
    x = c(10, 20, 30),
    y = c(40, 50, 60)
  )
  embeddings <- matrix(
    c(1, 2, 3, 3, 2, 1),
    ncol = 2,
    dimnames = list(ids, c("UMAP_1", "UMAP_2"))
  )

  linked <- wsi_link_spatialexperiment_image(
    list(),
    slide,
    coordinates = coords,
    embeddings = embeddings,
    reduction = "UMAP"
  )

  expect_s3_class(linked, "wsi_spatial_object")
  expect_equal(linked$source_name, "SpatialExperiment")
	  expect_equal(nrow(linked$spots), 3)
	  expect_equal(linked$component_names, c("UMAP_1", "UMAP_2"))
	})

test_that("SpatialExperiment gene symbols are resolved from rowData aliases", {
  skip_if_not_installed("SpatialExperiment")
  skip_if_not_installed("S4Vectors")

  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800)
  ids <- paste0("spot", 1:4, "-151507")
  assays <- matrix(
    c(0, 1, 2, 3, 5, 0, 4, 1),
    nrow = 2,
    dimnames = list(c("ENSG000001", "ENSG000002"), ids)
  )
  spe <- SpatialExperiment::SpatialExperiment(
    assays = list(logcounts = assays),
    rowData = S4Vectors::DataFrame(
      gene_id = c("ENSG000001", "ENSG000002"),
      gene_name = c("GeneA", "GeneB"),
      gene_search = c("GeneA; ENSG000001", "GeneB; ENSG000002")
    ),
    colData = S4Vectors::DataFrame(sample_id = rep("151507", length(ids))),
    spatialCoords = matrix(
      c(10, 20, 30, 40, 40, 50, 60, 70),
      ncol = 2,
      dimnames = list(sub("-151507$", "-1", ids), c("pxl_col_in_fullres", "pxl_row_in_fullres"))
    )
  )
  embeddings <- matrix(
    c(1, 2, 3, 4, 4, 3, 2, 1),
    ncol = 2,
    dimnames = list(ids, c("PCA_1", "PCA_2"))
  )

  linked <- wsi_link_spatialexperiment_image(
    spe,
    slide,
    sample_id = "151507",
    embeddings = embeddings
  )
  payload <- wsiTools:::wsi_seurat_dynamic_gene_payload(linked, "GeneB")
  expect_equal(payload$gene, "GeneB")
  expect_equal(vapply(payload$points, `[[`, numeric(1), "value"), c(1, 3, 0, 1))

  values <- wsiTools:::wsi_prediction_feature_vector(
    wsi_prediction_context(spatial = linked),
    source_id = "spatial:raw",
    ids = linked$spots$id,
    feature = "GeneB",
    points = wsiTools:::wsi_prediction_points(wsi_prediction_context(spatial = linked), "spatial:points")
  )
  expect_equal(as.numeric(values), c(1, 3, 0, 1))
  expect_equal(attr(values, "feature"), "GeneB")
})

test_that("spatial-object spot tile previews are centered on spots and respect selected ROIs", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  linked <- list(
    slide = slide,
    source_name = "SpatialExperiment",
    spots = data.frame(
      barcode = c("spot_a", "spot_b", "edge"),
      label = c("A", "B", "edge"),
      x = c(100, 900, 20),
      y = c(100, 700, 20),
      stringsAsFactors = FALSE
    )
  )
  class(linked) <- c("wsi_spatial_object", "list")

  expect_warning(
    grid <- wsiTools:::wsi_spatial_tile_grid(linked, tile_size = 100, units = "px"),
    "Dropped 1 coordinate tile"
  )

  expect_s3_class(grid, "wsi_tile_preview")
  expect_equal(nrow(grid), 2)
  expect_equal(grid$spot_id, c("spot_a", "spot_b"))
  expect_equal(grid$x, c(50L, 850L))
  expect_equal(grid$y, c(50L, 650L))
  expect_equal(grid$width, c(100L, 100L))
  expect_equal(grid$height, c(100L, 100L))

  expect_warning(
    microns <- wsiTools:::wsi_spatial_tile_grid(linked, tile_size = 50, units = "um"),
    "Dropped 1 coordinate tile"
  )
  expect_equal(microns$width, c(200L, 200L))
  expect_equal(microns$height, c(200L, 200L))

  geojson <- tempfile(fileext = ".geojson")
  writeLines(
    paste0(
      '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"name":"roi_a","classification":{"name":"tumour"}},',
      '"geometry":{"type":"Polygon","coordinates":[[[0,0],[250,0],[250,250],[0,250],[0,0]]]}}]}'
    ),
    geojson
  )
  roi <- wsi_read_geojson(geojson)
  expect_warning(
    filtered <- wsiTools:::wsi_spatial_tile_grid(linked, tile_size = 100, units = "px", roi = roi),
    "Dropped 1 coordinate tile"
  )

  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$spot_id, "spot_a")

  payload_grid <- wsiTools:::wsi_spatial_tile_payload_grid(list(list(
    tile_id = "spot_a",
    x = 50,
    y = 50,
    width = 100,
    height = 100,
    tissue_fraction = NULL,
    output_file = "spot_a.png"
  )))
  expect_s3_class(payload_grid, "wsi_tile_preview")
  expect_equal(nrow(payload_grid), 1)
  expect_true(is.na(payload_grid$tissue_fraction[[1]]))
})

test_that("spatial spot tile exports write a compact spot-to-file index", {
  manifest <- data.frame(
    tile_id = c("spot_a", "spot_b"),
    file = file.path(tempdir(), c("spot_a.png", "spot_b.png")),
    x = c(50L, 850L),
    y = c(50L, 650L),
    width = c(100L, 100L),
    height = c(100L, 100L),
    level = c(0L, 0L),
    spot_id = c("AAAC-1", "AAAC-2"),
    spot_label = c("A", "B"),
    project_image = c("section 1", "section 1"),
    project_section = c("anterior", "anterior"),
    stringsAsFactors = FALSE
  )

  index <- wsiTools:::wsi_spatial_tile_index(manifest)
  expect_equal(index$spot_id, manifest$spot_id)
  expect_equal(index$image_file, c("spot_a.png", "spot_b.png"))
  expect_equal(index$image_path, manifest$file)
  expect_equal(index$tile_id, manifest$tile_id)
  expect_equal(index$project_section, manifest$project_section)

  index_file <- tempfile(fileext = ".csv")
  wsiTools:::wsi_write_spatial_tile_index_file(manifest, index_file, overwrite = FALSE)
  expect_true(file.exists(index_file))
  saved <- utils::read.csv(index_file, stringsAsFactors = FALSE)
  expect_equal(saved$spot_id, manifest$spot_id)
  expect_equal(saved$image_file, c("spot_a.png", "spot_b.png"))
  expect_equal(saved$image_path, manifest$file)
})

test_that("SpatialExperiment project viewer uses one scoped spot layer per section", {
  slide1 <- wsiTools:::wsi_mock_slide(width = 100, height = 90, levels = c(1, 2))
  slide2 <- wsiTools:::wsi_mock_slide(width = 120, height = 100, levels = c(1, 2))
  ids <- c("a", "b")
  linked <- list(
    section1 = list(
      slide = slide1,
      image_path = "section1.tif",
      image_name = "section1",
      source_name = "SpatialExperiment",
      reduction = "PCA",
      dims = c(1L, 2L),
      component_names = c("PC_1", "PC_2"),
      coordinate_mapping = list(method = "none"),
      spot_radius = 5,
      spot_count = 2L,
      displayed_spot_count = 2L,
      gene_expression = list(enabled = FALSE, genes = character(), values = NULL),
      spots = data.frame(
        id = ids,
        label = ids,
        barcode = ids,
        x = c(20, 60),
        y = c(30, 70),
        PC_1 = c(-1, 1),
        PC_2 = c(0, 2),
        colour = c("#2B6CB0", "#2B6CB0"),
        base_colour = c("#2B6CB0", "#2B6CB0"),
        stringsAsFactors = FALSE
      ),
      plots = list(list(
        id = "spe_pca",
        label = "SpatialExperiment PCA plot",
        reduction = "PCA",
        x_label = "PC_1",
        y_label = "PC_2",
        point_count = 2L,
        points = list()
      ))
    ),
    section2 = list(
      slide = slide2,
      image_path = "section2.tif",
      image_name = "section2",
      source_name = "SpatialExperiment",
      reduction = "UMAP_neighbors15",
      dims = c(1L, 2L),
      component_names = c("UMAP1", "UMAP2"),
      coordinate_mapping = list(method = "none"),
      spot_radius = 5,
      spot_count = 2L,
      displayed_spot_count = 2L,
      gene_expression = list(enabled = FALSE, genes = character(), values = NULL),
      spots = data.frame(
        id = ids,
        label = ids,
        barcode = ids,
        x = c(25, 65),
        y = c(35, 75),
        UMAP1 = c(-2, 2),
        UMAP2 = c(1, 3),
        colour = c("#2B6CB0", "#2B6CB0"),
        base_colour = c("#2B6CB0", "#2B6CB0"),
        stringsAsFactors = FALSE
      ),
      plots = list(list(
        id = "spe_umap",
        label = "SpatialExperiment UMAP plot",
        reduction = "UMAP_neighbors15",
        x_label = "UMAP1",
        y_label = "UMAP2",
        point_count = 2L,
        points = list()
      ))
    )
  )
  class(linked[[1]]) <- c("wsi_seurat_spatial", "wsi_spatial_object", "list")
  class(linked[[2]]) <- c("wsi_seurat_spatial", "wsi_spatial_object", "list")

  output <- tempfile(fileext = ".html")
  html <- wsi_viewer_spatialexperiment_project(
    linked = linked,
    mode = "thumbnail",
    live = FALSE,
    output = output,
    open = FALSE,
    overwrite = TRUE
  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")

  expect_true(file.exists(html))
  expect_match(text, "section1", fixed = TRUE)
  expect_match(text, "section2", fixed = TRUE)
  expect_match(text, "SpatialExperiment spatial spots", fixed = TRUE)
  expect_match(text, "UMAP_neighbors15", fixed = TRUE)
  expect_match(text, "const radius=2.4;points.forEach", fixed = TRUE)
  expect_false(grepl("140/Math.sqrt(points.length)", text, fixed = TRUE))
  expect_false(grepl("radius*1.25", text, fixed = TRUE))
  expect_match(text, "\"managed_analysis_project\":true", fixed = TRUE)
  expect_false(grepl("id=\"projectOpenImage\"", text, fixed = TRUE))
  expect_false(grepl("id=\"projectImageFile\"", text, fixed = TRUE))
})
