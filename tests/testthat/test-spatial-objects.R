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
  expect_false(grepl("id=\"projectOpenImage\"", text, fixed = TRUE))
  expect_false(grepl("id=\"projectImageFile\"", text, fixed = TRUE))
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
  expect_match(text, "\"managed_analysis_project\":true", fixed = TRUE)
  expect_false(grepl("id=\"projectOpenImage\"", text, fixed = TRUE))
  expect_false(grepl("id=\"projectImageFile\"", text, fixed = TRUE))
})
