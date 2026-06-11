test_that("spatial viewer wrappers default to live R synchronization", {
  expect_true(identical(formals(wsi_viewer_seurat)$live, TRUE))
  expect_true(identical(formals(wsi_viewer_spatial)$live, TRUE))
  expect_true(identical(formals(wsi_viewer_giotto)$live, TRUE))
  expect_true(identical(formals(wsi_viewer_spatialexperiment)$live, TRUE))
  expect_true(identical(formals(wsi_viewer_seurat_project)$live, TRUE))
  expect_true(identical(formals(wsi_viewer_spatialexperiment_project)$live, TRUE))
})

test_that("Seurat spatial objects can be linked to high-resolution slide coordinates", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5, 2, 1.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b", "spot_c"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b", "spot_c"),
          imagecol = c(10, 20, 30),
          imagerow = c(5, 15, 25)
        ),
        image = array(0, dim = c(50, 100, 3)),
        scale.factors = list(spot = 12)
      )
    )
  )
  slide <- wsi_mock_slide(width = 1000, height = 500, levels = c(1, 2))

  linked <- wsi_link_seurat_image(seurat_like, slide)

  expect_s3_class(linked, "wsi_seurat_spatial")
  expect_equal(linked$image_name, "anterior1")
  expect_equal(linked$coordinate_mapping$method, "auto_seurat_image")
  expect_equal(linked$coordinate_mapping$scale_x, 10)
  expect_equal(linked$coordinate_mapping$scale_y, 10)
  expect_equal(linked$spots$x, c(100, 200, 300))
  expect_equal(linked$spots$y, c(50, 150, 250))
  expect_equal(linked$pca$x_label, "PC_1")
  expect_equal(linked$pca$y_label, "PC_2")
  expect_equal(nrow(linked$pca$points), 3)

  layer <- wsiTools:::wsi_seurat_spots_layer(linked)
  expect_equal(layer$id, "seurat_spots")
  expect_equal(layer$source_type, "seurat_spots")
  expect_equal(length(layer$items), 3)
  expect_true(length(unique(vapply(layer$items, `[[`, character(1), "colour"))) > 1)
})

test_that("Seurat Visium spot spacing supplies scale when image metadata has no mpp", {
  embeddings <- matrix(
    c(-1, 0, 1, 0, 0, -1, 0, 1),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b", "spot_c", "spot_d"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b", "spot_c", "spot_d"),
          imagecol = c(100, 200, 100, 200),
          imagerow = c(100, 100, 200, 200)
        )
      )
    )
  )
  slide <- wsi_mock_slide(width = 400, height = 400, levels = c(1, 2))
  slide$properties[["openslide.mpp-x"]] <- NULL
  slide$properties[["openslide.mpp-y"]] <- NULL

  linked <- wsi_link_seurat_image(seurat_like, slide)

  expect_equal(wsi_mpp(slide), c(x = NA_real_, y = NA_real_))
  expect_equal(linked$scale_metadata$source, "visium_center_spacing")
  expect_true(linked$scale_metadata$inferred)
  expect_equal(linked$scale_metadata$center_spacing_pixels, 100)
  expect_equal(linked$mpp, list(x = 1, y = 1))
  expect_equal(linked$spot_radius, 27.5)

  config <- wsiTools:::wsi_viewer_seurat_config(linked)
  expect_equal(config$mpp, list(x = 1, y = 1))

  output <- tempfile(fileext = ".html")
  wsi_viewer(
    linked$slide,
    mode = "thumbnail",
    output = output,
    open = FALSE,
    overwrite = TRUE,
    layers = list(wsiTools:::wsi_seurat_spots_layer(linked)),
    seurat = linked
  )
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "\"mpp\":{\"x\":1,\"y\":1}", fixed = TRUE)
})

test_that("Seurat spots can be coloured by selected gene expression values", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5, 2, 1.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b", "spot_c"), c("PC_1", "PC_2"))
  )
  expression <- matrix(
    c(0, 2, 4, 5, 4, 3),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("Mbp", "Plp1"), c("spot_a", "spot_b", "spot_c"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    assays = list(Spatial = list(data = expression)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b", "spot_c"),
          imagecol = c(10, 20, 30),
          imagerow = c(5, 15, 25)
        ),
        image = array(0, dim = c(50, 100, 3)),
        scale.factors = list(spot = 12)
      )
    )
  )
  slide <- wsi_mock_slide(width = 1000, height = 500, levels = c(1, 2))

  linked <- wsi_link_seurat_image(
    seurat_like,
    slide,
    spot_genes = c("Mbp", "Plp1"),
    default_gene = "Mbp"
  )

  expect_true(linked$gene_expression$enabled)
  expect_equal(linked$gene_expression$genes, c("Mbp", "Plp1"))
  expect_equal(as.numeric(linked$gene_expression$values[, "Mbp"]), c(0, 2, 4))
  expect_equal(linked$gene_expression$default_gene, "Mbp")
  expect_true(length(unique(linked$spots$colour)) > 1)
  expect_equal(linked$pca$points$gene_values[[1]]$Mbp, 0)

  layer <- wsiTools:::wsi_seurat_spots_layer(linked)
  expect_equal(layer$items[[2]]$gene_values$Mbp, 2)
  expect_match(layer$items[[2]]$base_colour, "^#")

  config <- wsiTools:::wsi_viewer_seurat_config(linked)
  expect_true(config$gene_expression$enabled)
  expect_equal(config$gene_expression$default_gene, "Mbp")
})

test_that("Seurat clustering metadata is detected and exposed to the viewer", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5, 2, 1.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b", "spot_c"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    meta.data = data.frame(
      seurat_clusters = c("stroma", "tumour", "stroma"),
      row.names = c("spot_a", "spot_b", "spot_c")
    ),
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b", "spot_c"),
          imagecol = c(10, 20, 30),
          imagerow = c(5, 15, 25)
        ),
        image = array(0, dim = c(50, 100, 3)),
        scale.factors = list(spot = 12)
      )
    )
  )
  slide <- wsi_mock_slide(width = 1000, height = 500, levels = c(1, 2))

  fields <- wsi_spatial_cluster_fields(seurat_like, spot_ids = c("spot_a", "spot_b", "spot_c"))
  expect_s3_class(fields, "wsi_spatial_cluster_fields")
  expect_equal(fields$field, "seurat_clusters")
  expect_equal(fields$storage, "meta.data")
  expect_equal(fields$n_clusters, 2L)

  clusters <- wsi_spatial_clusters(seurat_like, spot_ids = c("spot_a", "spot_b", "spot_c"))
  expect_s3_class(clusters, "wsi_spatial_clusters")
  expect_equal(clusters$seurat_clusters, c("stroma", "tumour", "stroma"))

  linked <- wsi_link_seurat_image(seurat_like, slide)
  expect_true(linked$clusters$enabled)
  expect_equal(linked$clusters$default_field, "seurat_clusters")
  expect_equal(linked$cluster_values$seurat_clusters, c("stroma", "tumour", "stroma"))
  expect_equal(linked$pca$points$cluster_values[[2]]$seurat_clusters, "tumour")

  layer <- wsiTools:::wsi_seurat_spots_layer(linked)
  expect_equal(layer$items[[1]]$cluster_values$seurat_clusters, "stroma")

  config <- wsiTools:::wsi_viewer_seurat_config(linked)
  expect_true(config$clusters$enabled)
  expect_equal(config$clusters$fields[[1]]$field, "seurat_clusters")
})

test_that("live Seurat gene payload fetches one selected gene without preloading all genes", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5, 2, 1.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b", "spot_c"), c("PC_1", "PC_2"))
  )
  expression <- matrix(
    c(0, 2, 4, 5, 4, 3, 9, 0, 1),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("Mbp", "Plp1", "Gad1"), c("spot_a", "spot_b", "spot_c"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    assays = list(Spatial = list(data = expression)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b", "spot_c"),
          imagecol = c(10, 20, 30),
          imagerow = c(5, 15, 25)
        ),
        image = array(0, dim = c(50, 100, 3)),
        scale.factors = list(spot = 12)
      )
    )
  )
  slide <- wsi_mock_slide(width = 1000, height = 500, levels = c(1, 2))

  linked <- wsi_link_seurat_image(seurat_like, slide)
  expect_false(linked$gene_expression$enabled)
  expect_true(wsiTools:::wsi_seurat_live_gene_available(linked))

  payload <- wsiTools:::wsi_seurat_dynamic_gene_payload(linked, "Gad1")

  expect_true(payload$ok)
  expect_equal(payload$gene, "Gad1")
  expect_equal(payload$feature_type, "spot")
  expect_equal(payload$feature_plural, "spots")
  expect_equal(payload$count, 3)
  expect_equal(vapply(payload$points, `[[`, numeric(1), "value"), c(9, 0, 1))
  expect_equal(vapply(payload$points, `[[`, numeric(1), "x"), linked$spots$x)
  expect_equal(vapply(payload$points, `[[`, numeric(1), "y"), linked$spots$y)
  expect_true(all(is.finite(vapply(payload$points, `[[`, numeric(1), "radius"))))
})

test_that("live Seurat gene payload identifies cell-level spatial data", {
  embeddings <- matrix(
    c(1, 0, 0, 1),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("cellid_001", "cellid_002"), c("PC_1", "PC_2"))
  )
  expression <- matrix(
    c(3, 9),
    nrow = 1,
    dimnames = list("CRABP2", c("cellid_001", "cellid_002"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    assays = list(Spatial = list(data = expression)),
    images = list(
      breast = list(
        coordinates = data.frame(
          cell = c("cellid_001", "cellid_002"),
          imagecol = c(100, 200),
          imagerow = c(150, 250)
        ),
        image = array(0, dim = c(50, 100, 3))
      )
    )
  )
  slide <- wsi_mock_slide(width = 1000, height = 500, levels = c(1, 2))

  linked <- wsi_link_seurat_image(seurat_like, slide)
  payload <- wsiTools:::wsi_seurat_dynamic_gene_payload(linked, "CRABP2")

  expect_equal(linked$feature_type, "cell")
  expect_equal(payload$feature_type, "cell")
  expect_equal(payload$feature_plural, "cells")
  expect_true(all(vapply(payload$points, `[[`, character(1), "feature_type") == "cell"))
  expect_equal(vapply(payload$points, `[[`, numeric(1), "value"), c(3, 9))
})

test_that("live Seurat gene payload infers cells for manually constructed linked objects", {
  expression <- matrix(
    c(1, 4),
    nrow = 1,
    dimnames = list("CRABP2", c("cellid_001", "cellid_002"))
  )
  seurat_like <- list(assays = list(Spatial = list(data = expression)))
  linked <- list(
    source_name = "Seurat",
    spots = data.frame(
      id = c("cellid_001", "cellid_002"),
      label = c("cellid_001", "cellid_002"),
      barcode = c("cellid_001", "cellid_002"),
      x = c(10, 20),
      y = c(30, 40),
      stringsAsFactors = FALSE
    ),
    spot_radius = 8,
    expression_source = list(
      object = seurat_like,
      spot_ids = c("cellid_001", "cellid_002")
    )
  )
  class(linked) <- c("wsi_seurat_spatial", "list")

  payload <- wsiTools:::wsi_seurat_dynamic_gene_payload(linked, "CRABP2")

  expect_equal(payload$feature_type, "cell")
  expect_equal(payload$feature_plural, "cells")
})

test_that("live Seurat gene endpoint preserves the gene query parameter", {
  query <- wsiTools:::wsi_http_query_params("gene=Cryzl2&q=Gad1")

  expect_equal(query$gene, "Cryzl2")
  expect_equal(query$q, "Gad1")
  expect_null(wsiTools:::wsi_dynamic_tile_query_settings("gene=Cryzl2")$gene)
})

test_that("10x tissue positions and scalefactors align Seurat spots to full-resolution images", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b"),
          imagecol = c(10, 20),
          imagerow = c(5, 15)
        ),
        image = array(0, dim = c(50, 100, 3)),
        scale.factors = list(lowres = 0.1, spot = 20)
      )
    )
  )
  spatial_dir <- tempfile("spatial")
  dir.create(spatial_dir)
  writeLines(
    '{"spot_diameter_fullres": 80, "tissue_lowres_scalef": 0.1}',
    file.path(spatial_dir, "scalefactors_json.json")
  )
  utils::write.table(
    data.frame(
      barcode = c("spot_a", "spot_b", "other"),
      in_tissue = c(1, 1, 0),
      array_row = c(0, 0, 0),
      array_col = c(0, 1, 2),
      pxl_row_in_fullres = c(120, 360, 999),
      pxl_col_in_fullres = c(240, 480, 999)
    ),
    file.path(spatial_dir, "tissue_positions.csv"),
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )
  slide <- wsi_mock_slide(width = 1000, height = 500, levels = c(1, 2))

  linked <- wsi_link_seurat_image(seurat_like, slide, spatial_dir = spatial_dir)

  expect_equal(linked$coordinate_mapping$method, "auto_fullres")
  expect_equal(linked$coordinate_mapping$scale_x, 1)
  expect_equal(linked$coordinate_mapping$scale_y, 1)
  expect_equal(linked$spots$x, c(240, 480))
  expect_equal(linked$spots$y, c(120, 360))
  expect_equal(linked$spot_radius, 40)
})

test_that("Seurat object scale factors are used before external spatial files", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b"),
          imagecol = c(240, 480),
          imagerow = c(120, 360)
        ),
        image = array(0, dim = c(100, 100, 3)),
        scale.factors = list(lowres = 0.1, spot = 80)
      )
    )
  )
  slide <- wsi_mock_slide(width = 1000, height = 1000, levels = c(1, 2))

  linked <- wsi_link_seurat_image(seurat_like, slide)

  expect_equal(linked$coordinate_mapping$method, "auto_fullres")
  expect_equal(linked$spots$x, c(240, 480))
  expect_equal(linked$spots$y, c(120, 360))
  expect_equal(linked$spot_radius, 40)
})

test_that("Seurat spot coordinates can be transformed with x1 equals y and y1 equals negative x", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b"),
          imagecol = c(100, 250),
          imagerow = c(20, 80)
        ),
        image = array(0, dim = c(100, 300, 3))
      )
    )
  )
  slide <- wsi_mock_slide(width = 300, height = 100, levels = c(1, 2))

  linked <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_transform = "x_y_y_neg_x"
  )

  expect_equal(linked$coordinate_mapping$coordinate_transform, "x_y_y_neg_x")
  expect_equal(linked$spots$x, c(20, 80))
  expect_equal(linked$spots$y, c(200, 50))

  alias <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_transform = "rotate_90_cw"
  )
  expect_equal(alias$spots$x, linked$spots$x)
  expect_equal(alias$spots$y, linked$spots$y)
})

test_that("Seurat spot coordinates can be flipped vertically", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b"),
          imagecol = c(100, 250),
          imagerow = c(20, 80)
        ),
        image = array(0, dim = c(100, 300, 3))
      )
    )
  )
  slide <- wsi_mock_slide(width = 300, height = 100, levels = c(1, 2))

  linked <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_transform = "flip_y"
  )

  expect_equal(linked$coordinate_mapping$coordinate_transform, "flip_y")
  expect_equal(linked$spots$x, c(100, 250))
  expect_equal(linked$spots$y, c(80, 20))

  alias <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_transform = "y_neg_y"
  )
  expect_equal(alias$spots$x, linked$spots$x)
  expect_equal(alias$spots$y, linked$spots$y)
})

test_that("Seurat spot coordinates can be transformed with x1 equals negative y and y1 equals negative x", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b"),
          imagecol = c(100, 250),
          imagerow = c(20, 80)
        ),
        image = array(0, dim = c(100, 300, 3))
      )
    )
  )
  slide <- wsi_mock_slide(width = 300, height = 100, levels = c(1, 2))

  linked <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_transform = "x_neg_y_y_neg_x"
  )

  expect_equal(linked$coordinate_mapping$coordinate_transform, "x_neg_y_y_neg_x")
  expect_equal(linked$spots$x, c(80, 20))
  expect_equal(linked$spots$y, c(200, 50))

  alias <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_transform = "neg_y_neg_x"
  )
  expect_equal(alias$spots$x, linked$spots$x)
  expect_equal(alias$spots$y, linked$spots$y)
})

test_that("Seurat spot orientation can be controlled with separate flip and rotation arguments", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b"),
          imagecol = c(100, 250),
          imagerow = c(20, 80)
        ),
        image = array(0, dim = c(100, 300, 3))
      )
    )
  )
  slide <- wsi_mock_slide(width = 300, height = 100, levels = c(1, 2))

  negative_swap <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_flip = "horizontal",
    coordinate_rotation = 90
  )
  expect_equal(negative_swap$coordinate_mapping$coordinate_flip, "horizontal")
  expect_equal(negative_swap$coordinate_mapping$coordinate_rotation, 90L)
  expect_equal(negative_swap$spots$x, c(80, 20))
  expect_equal(negative_swap$spots$y, c(200, 50))

  negative_swap_then_180 <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_flip = "horizontal",
    coordinate_rotation = 270
  )
  expect_equal(negative_swap_then_180$coordinate_mapping$coordinate_flip, "horizontal")
  expect_equal(negative_swap_then_180$coordinate_mapping$coordinate_rotation, 270L)
  expect_equal(negative_swap_then_180$spots$x, c(20, 80))
  expect_equal(negative_swap_then_180$spots$y, c(100, 250))

  vertical_alias <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_flip = "verticcal",
    coordinate_rotation = 0
  )
  expect_equal(vertical_alias$coordinate_mapping$coordinate_flip, "vertical")
  expect_equal(vertical_alias$spots$x, c(100, 250))
  expect_equal(vertical_alias$spots$y, c(80, 20))

  expect_error(
    wsi_link_seurat_image(
      seurat_like,
      slide,
      coordinate_scale = "none",
      coordinate_flip = "horizontal",
      coordinate_transform = "flip_y"
    ),
    "Use either legacy"
  )
})

test_that("missing scalefactors path can be recovered from a spatial directory", {
  embeddings <- matrix(
    c(-1, 0, 1, 1),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("a", "b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    meta.data = data.frame(
      seurat_clusters = c("0", "1"),
      row.names = c("a", "b")
    ),
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(slice = list(
      coordinates = data.frame(barcode = c("a", "b"), imagecol = c(20, 80), imagerow = c(30, 70)),
      image = array(0, dim = c(100, 100, 3))
    ))
  )
  spatial_dir <- tempfile("spatial")
  dir.create(spatial_dir)
  writeLines(
    '{"tissue_lowres_scalef": 0.1, "spot_diameter_fullres": 60}',
    file.path(spatial_dir, "prefix_scalefactors_json.json")
  )
  slide <- wsi_mock_slide(width = 1000, height = 1000, levels = c(1, 2))

  linked <- wsi_link_seurat_image(
    seurat_like,
    slide,
    scalefactors_json = file.path(spatial_dir, "scalefactors_json.json")
  )

  expect_equal(linked$coordinate_mapping$method, "auto_lowres")
  expect_equal(linked$spots$x, c(200, 800))
  expect_equal(linked$spots$y, c(300, 700))
})

test_that("Seurat viewer exposes spot layer and reduction controls", {
  embeddings <- matrix(
    c(-1, 0, 1, 1),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("a", "b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(slice = list(
      coordinates = data.frame(barcode = c("a", "b"), imagecol = c(20, 80), imagerow = c(30, 70)),
      image = array(0, dim = c(100, 100, 3))
    ))
  )
  slide <- wsi_mock_slide(width = 100, height = 100, levels = c(1, 2))
  linked <- wsi_link_seurat_image(seurat_like, slide, coordinate_scale = "none", spot_radius = 4)
  html <- tempfile(fileext = ".html")

  wsi_viewer(
    slide,
    output = html,
    open = FALSE,
    overwrite = TRUE,
    mode = "thumbnail",
	    layers = list(wsiTools:::wsi_seurat_spots_layer(linked)),
	    seurat = linked,
	    seurat_gene_url = "http://127.0.0.1:8788/seurat-gene",
	    spatial_tile_export_url = "http://127.0.0.1:8788/spatial-tiles"
	  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")

  expect_match(text, "Seurat", fixed = TRUE)
  expect_match(text, "seurat_spots", fixed = TRUE)
	  expect_match(text, "seurat_gene_url", fixed = TRUE)
	  expect_match(text, "spatial_tile_export_url", fixed = TRUE)
	  expect_match(text, "Type any gene name", fixed = TRUE)
	  expect_match(text, "hasGenes=seuratEnabled()&&(genes.length>0||dynamic)", fixed = TRUE)
	  expect_false(grepl("hasGenes=has&&(genes.length>0||dynamic)", text, fixed = TRUE))
  expect_match(text, "JSON.stringify({gene:String(gene||'').trim()})", fixed = TRUE)
  expect_match(text, "seuratPlotWindow", fixed = TRUE)
  expect_match(text, "Current tissue", fixed = TRUE)
  expect_match(text, "All tissues", fixed = TRUE)
  expect_match(text, "bindSeuratControls", fixed = TRUE)
  expect_match(text, "openSeuratPlot(Number(btn.dataset.plotIndex||0));if(typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(e.currentTarget);", fixed = TRUE)
  expect_match(text, "seuratGeneInput", fixed = TRUE)
	  expect_match(text, "seuratClusterSelect", fixed = TRUE)
	  expect_match(text, "Spot-centered tiles", fixed = TRUE)
	  expect_match(text, "spatialTileWindow", fixed = TRUE)
	  expect_match(text, "spatialTilePreview", fixed = TRUE)
	  expect_match(text, "saveSpatialTiles", fixed = TRUE)
	  expect_match(text, "spatial_spot_tile_preview", fixed = TRUE)
	  expect_match(text, "Colour by cluster", fixed = TRUE)
  expect_match(text, "seurat_cluster_coloured", fixed = TRUE)
  expect_match(text, "seuratSelectionPayload", fixed = TRUE)
  expect_match(text, "Draw a lasso around reduction points", fixed = TRUE)
	  expect_match(text, "\"managed_analysis_project\":true", fixed = TRUE)
	  expect_false(grepl("id=\"projectOpenImage\"", text, fixed = TRUE))
	  expect_false(grepl("id=\"projectImageFile\"", text, fixed = TRUE))
	  expect_false(grepl("This managed analysis project is defined from R", text, fixed = TRUE))
	  expect_false(grepl("Seurat/Giotto/SpatialExperiment/CellPhenotyper viewer from R", text, fixed = TRUE))
})

test_that("Seurat viewer only shows buttons for available reductions", {
  embeddings <- matrix(
    c(-1, 0, 1, 1, -2, 2, 2, -1),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("a", "b", "c", "d"), c("PC_1", "PC_2"))
  )
  umap <- matrix(
    c(1, 3, 2, 4, 5, 1, 6, 2),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("a", "b", "c", "d"), c("UMAP_1", "UMAP_2"))
  )
  seurat_like <- list(
    reductions = list(
      pca = list(cell.embeddings = embeddings),
      umap = list(cell.embeddings = umap)
    ),
    images = list(slice1 = list(
      coordinates = data.frame(barcode = c("a", "b", "c", "d"), imagecol = c(20, 80, 30, 60), imagerow = c(30, 70, 45, 85)),
      image = array(0, dim = c(100, 100, 3))
    ))
  )
  slide <- wsi_mock_slide(width = 100, height = 100, levels = c(1, 2))
  linked <- wsi_link_seurat_image(seurat_like, slide, image_name = "slice1", reduction = "pca")
  config <- list(seurat = wsiTools:::wsi_viewer_seurat_config(linked))
  controls <- wsiTools:::wsi_viewer_seurat_controls(config)

  expect_equal(vapply(linked$plots, function(x) x$reduction, character(1)), c("pca", "umap"))
  expect_match(controls, "data-plot-index=\"0\"", fixed = TRUE)
  expect_match(controls, ">PCA<", fixed = TRUE)
  expect_match(controls, ">UMAP<", fixed = TRUE)
  expect_false(grepl(">tSNE<", controls, fixed = TRUE))
})

test_that("long reduction names are compacted in viewer controls", {
  embeddings <- matrix(
    c(-1, 0, 1, 1, -2, 2, 2, -1),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("a", "b", "c", "d"), c("RD_1", "RD_2"))
  )
  long_reduction <- "TSNE_PERPLEXITY50"
  seurat_like <- list(
    reductions = setNames(list(list(cell.embeddings = embeddings)), long_reduction),
    images = list(slice1 = list(
      coordinates = data.frame(barcode = c("a", "b", "c", "d"), imagecol = c(20, 80, 30, 60), imagerow = c(30, 70, 45, 85)),
      image = array(0, dim = c(100, 100, 3))
    ))
  )
  slide <- wsi_mock_slide(width = 100, height = 100, levels = c(1, 2))
  linked <- wsi_link_seurat_image(seurat_like, slide, image_name = "slice1", reduction = long_reduction)
  config <- list(seurat = wsiTools:::wsi_viewer_seurat_config(linked))
  controls <- wsiTools:::wsi_viewer_seurat_controls(config)

  expect_equal(wsiTools:::wsi_reduction_display_label(long_reduction), "TSNE...TY50")
  expect_match(controls, "TSNE...TY50", fixed = TRUE)
  expect_match(controls, paste0("Open the ", long_reduction, " reduction plot"), fixed = TRUE)
  expect_false(grepl(paste0(">", long_reduction, "<"), controls, fixed = TRUE))
})

test_that("multi-image Seurat projects keep per-section overlays", {
  embeddings <- matrix(
    c(-1, 0, 1, 1, -2, 2, 2, -1),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("a", "b", "c", "d"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      slice1 = list(
        coordinates = data.frame(barcode = c("a", "b"), imagecol = c(20, 80), imagerow = c(30, 70)),
        image = array(0, dim = c(100, 100, 3))
      ),
      slice2 = list(
        coordinates = data.frame(barcode = c("c", "d"), imagecol = c(15, 60), imagerow = c(20, 75)),
        image = array(0, dim = c(100, 100, 3))
      )
    )
  )
  slides <- list(
    slice1 = wsi_mock_slide(width = 100, height = 100, levels = c(1, 2)),
    slice2 = wsi_mock_slide(width = 120, height = 90, levels = c(1, 2))
  )
  output <- tempfile(fileext = ".html")

  html <- wsi_viewer_seurat_project(
    seurat_like,
    images = slides,
    image_names = names(slides),
    mode = "thumbnail",
    live = FALSE,
    output = output,
    open = FALSE,
    overwrite = TRUE
  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")

  expect_true(file.exists(html))
  expect_match(text, "slice1", fixed = TRUE)
  expect_match(text, "slice2", fixed = TRUE)
  expect_match(text, "seurat_spots", fixed = TRUE)
  expect_match(text, "spatial spots", fixed = TRUE)
  expect_match(text, "applyProjectSeurat", fixed = TRUE)
  expect_match(text, "seuratAllTissuePlotPoints", fixed = TRUE)
  expect_match(text, "seuratEffectivePlotScope", fixed = TRUE)
  expect_match(text, "seuratSelectionProjectWide", fixed = TRUE)
  expect_match(text, "refreshSeuratSelectionAfterProjectSwitch", fixed = TRUE)
  expect_match(text, "updateSeuratSpotHighlights();return true;", fixed = TRUE)
  expect_match(text, "scope==='all'||seuratPointInActiveTissue", fixed = TRUE)
  expect_match(text, "project_image_index", fixed = TRUE)
  expect_match(text, "project_section_index", fixed = TRUE)
  expect_match(text, "multiViewDrawLayers", fixed = TRUE)
  expect_match(text, "multiViewLayerItemMatchesPane", fixed = TRUE)
  expect_match(text, "multiViewApplyStainToPane", fixed = TRUE)
  expect_match(text, "applyMultiViewImageTransform", fixed = TRUE)
  expect_match(text, "project_image:String(p.tissue_label||'')", fixed = TRUE)
  expect_match(text, "multiViewDrawVectorLayer", fixed = TRUE)
  expect_match(text, "requested_scope:seuratPlotScope", fixed = TRUE)
  expect_match(text, "plotPanel&&plotPanel.classList.contains('open')", fixed = TRUE)
  expect_match(text, "renderSeuratPlotWindow", fixed = TRUE)
  expect_match(text, "seurat_plot_scope_changed", fixed = TRUE)
  expect_false(grepl("!allAvailable&&seuratPlotScope==='all'", text, fixed = TRUE))
  expect_false(grepl("cfg.seurat=next||{enabled:false,plots:[],spot_count:0};if(typeof seuratSelectedLabels", text, fixed = TRUE))
  expect_match(text, "\"managed_analysis_project\":true", fixed = TRUE)
  expect_false(grepl("id=\"projectOpenImage\"", text, fixed = TRUE))
  expect_false(grepl("id=\"projectImageFile\"", text, fixed = TRUE))
})

test_that("Seurat project records expose source tiles without reopening paths", {
  linked <- list(
    first = list(
      slide = wsi_mock_slide(width = 1000, height = 800, levels = c(1, 2)),
      image_path = "first.tif",
      image_name = "first",
      source_name = "Seurat",
      reduction = "pca",
      dims = c(1L, 2L),
      component_names = c("PC_1", "PC_2"),
      coordinate_mapping = list(method = "none"),
      spot_radius = 6,
      spot_count = 2L,
      displayed_spot_count = 2L,
      gene_expression = list(enabled = FALSE, genes = character(), values = NULL),
      spots = data.frame(
        id = c("a", "b"),
        label = c("a", "b"),
        barcode = c("a", "b"),
        x = c(100, 200),
        y = c(120, 220),
        PC_1 = c(0, 1),
        PC_2 = c(1, 0),
        colour = c("#000000", "#ffffff"),
        base_colour = c("#000000", "#ffffff"),
        stringsAsFactors = FALSE
      ),
      pca = list(id = "seurat_pca", label = "PCA", x_label = "PC_1", y_label = "PC_2", point_count = 2L, points = list())
    )
  )
  class(linked[[1]]) <- c("wsi_seurat_spatial", "list")

  records <- wsiTools:::wsi_seurat_project_records(
    linked,
    output = tempfile(fileext = ".html"),
    labels = "First section",
    mode = "thumbnail"
  )

  expect_length(records, 1)
  expect_true(records[[1]]$seurat$enabled)
  expect_true(records[[1]]$layers[[1]]$project_scoped)
  expect_equal(records[[1]]$content_bbox$xmin, 64)
  expect_equal(records[[1]]$content_bbox$ymin, 84)
})

test_that("Seurat selection events are accepted by the live bridge validator", {
  payload <- wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "seurat_spots_selected",
    sequence = 1,
    seurat_selection = list(labels = list("a", "b"), count = 2, matched_count = 2)
  ))

  expect_equal(payload$event, "seurat_spots_selected")

  gene_payload <- wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "seurat_gene_coloured",
    sequence = 2,
    detail = list(gene = "Mbp")
  ))
  expect_equal(gene_payload$event, "seurat_gene_coloured")

  cluster_payload <- wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "seurat_cluster_coloured",
    sequence = 3,
    detail = list(field = "seurat_clusters")
  ))
  expect_equal(cluster_payload$event, "seurat_cluster_coloured")
})
