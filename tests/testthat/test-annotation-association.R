test_that("annotation association assigns points to ROI polygons", {
  rois <- data.frame(
    roi_id = c("roi_a", "roi_b"),
    name = c("Tumour", "Stroma"),
    class = c("tumour", "stroma"),
    geometry_type = c("Polygon", "Polygon"),
    xmin = c(0, 20),
    ymin = c(0, 0),
    xmax = c(10, 30),
    ymax = c(10, 10),
    stringsAsFactors = FALSE
  )
  rois$coordinates <- list(
    list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))),
    list(rbind(c(20, 0), c(30, 0), c(30, 10), c(20, 10), c(20, 0)))
  )
  class(rois) <- c("wsi_roi", class(rois))
  points <- data.frame(
    spot_id = c("inside_a", "inside_b", "outside"),
    spot_x = c(5, 25, 15),
    spot_y = c(5, 5, 5),
    stringsAsFactors = FALSE
  )

  assigned <- wsi_associate_annotations(points, rois, engine = "r")
  expect_s3_class(assigned, "wsi_annotation_association")
  expect_equal(assigned$annotation_id, c("roi_a", "roi_b", NA))

  assigned_auto <- wsi_associate_annotations(points, rois, engine = "auto")
  expect_equal(assigned_auto$annotation_id, assigned$annotation_id)
})

test_that("annotation association can return a matrix and CSV", {
  rois <- data.frame(
    roi_id = "roi_a",
    name = "Tumour",
    class = "tumour",
    geometry_type = "Polygon",
    xmin = 0,
    ymin = 0,
    xmax = 10,
    ymax = 10,
    stringsAsFactors = FALSE
  )
  rois$coordinates <- list(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))))
  class(rois) <- c("wsi_roi", class(rois))
  points <- data.frame(id = c("a", "b"), x = c(5, 50), y = c(5, 50))

  mat <- wsi_associate_annotations(points, rois, output = "matrix", engine = "r")
  expect_equal(dim(mat), c(2L, 1L))
  expect_equal(colnames(mat), "roi_a")
  expect_equal(as.integer(mat[, 1]), c(1L, 0L))

  csv <- tempfile(fileext = ".csv")
  out <- wsi_associate_annotations(points, rois, file = csv, engine = "r")
  expect_true(file.exists(csv))
  expect_equal(nrow(utils::read.csv(csv)), nrow(out))
})

test_that("Seurat cells can be annotated and saved without a viewer", {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("Matrix")

  counts <- Matrix::Matrix(matrix(
    c(1, 2, 3),
    nrow = 1,
    dimnames = list("gene1", c("cell_a", "cell_b", "cell_c"))
  ), sparse = TRUE)
  metadata <- data.frame(
    x = c(5, 25, 50),
    y = c(5, 5, 50),
    row.names = colnames(counts)
  )
  object <- SeuratObject::CreateSeuratObject(counts = counts, meta.data = metadata)

  rois <- data.frame(
    roi_id = c("roi_a", "line_a"),
    name = c("Tumour", "Measurement line"),
    class = c("tumour", "line"),
    geometry_type = c("Polygon", "LineString"),
    xmin = c(0, 20),
    ymin = c(0, 0),
    xmax = c(10, 30),
    ymax = c(10, 10),
    stringsAsFactors = FALSE
  )
  rois$coordinates <- list(
    list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))),
    rbind(c(20, 0), c(30, 10))
  )
  class(rois) <- c("wsi_roi", class(rois))

  input <- tempfile(fileext = ".rds")
  output <- tempfile(fileext = ".rds")
  csv <- tempfile(fileext = ".csv")
  saveRDS(object, input)
  annotated <- wsi_annotate_seurat(
    input,
    rois,
    output = output,
    association_csv = csv,
    verbose = FALSE
  )

  expect_equal(
    as.character(annotated$wsi_annotation),
    c("tumour", "Unassigned", "Unassigned")
  )
  expect_equal(as.character(annotated$wsi_annotation_id), c("roi_a", NA, NA))
  expect_equal(annotated@misc$wsiTools$annotation_association$assigned, 1L)
  expect_equal(annotated@misc$wsiTools$annotation_association$ignored_non_area_annotations, 1L)
  expect_true(file.exists(output))
  expect_true(file.exists(csv))
  expect_equal(nrow(utils::read.csv(csv)), 3L)
  expect_equal(as.character(readRDS(output)$wsi_annotation), as.character(annotated$wsi_annotation))
})
