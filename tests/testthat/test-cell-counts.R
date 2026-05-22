wsi_counts_test_rois <- function() {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "tumour-1",
          "properties": {"name": "Tumour", "classification": {"name": "tumour"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [10, 0], [10, 10], [0, 10], [0, 0]]]
          }
        },
        {
          "type": "Feature",
          "id": "stroma-1",
          "properties": {"name": "Stroma", "classification": {"name": "stroma"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[20, 0], [30, 0], [30, 10], [20, 10], [20, 0]]]
          }
        }
      ]
    }',
    path
  )
  wsi_read_geojson(path)
}

wsi_counts_test_channels <- function(cells) {
  hematoxylin <- matrix(0, nrow = 20, ncol = 60)
  hrp_dab <- matrix(0, nrow = 20, ncol = 60)
  h_values <- c(0.1, 0.2, 0.3, 0.4)
  d_values <- c(0.05, 0.5, 0.1, 0.8)
  for (i in seq_len(nrow(cells))) {
    row <- floor(cells$y[[i]]) + 1L
    col <- floor(cells$x[[i]]) + 1L
    hematoxylin[row, col] <- h_values[[i]]
    hrp_dab[row, col] <- d_values[[i]]
  }
  structure(
    list(
      hematoxylin = hematoxylin,
      hrp_dab = hrp_dab,
      channel_metadata = list(
        list(id = "hematoxylin", name = "Hematoxylin"),
        list(id = "hrp_dab", name = "HRP/DAB")
      )
    ),
    class = "wsi_ihc_channels"
  )
}

test_that("wsi_cell_counts combines centroids, ROIs, and channel measurements", {
  rois <- wsi_counts_test_rois()
  cells <- data.frame(
    cell_id = paste0("c", 1:4),
    x = c(2.5, 5.5, 25.5, 50.5),
    y = c(2.5, 5.5, 5.5, 2.5),
    stringsAsFactors = FALSE
  )
  channels <- wsi_counts_test_channels(cells)

  counts <- wsi_cell_counts(
    wsi_mock_slide(),
    segmentation = cells,
    channels = channels,
    rois = rois,
    positive_threshold = c(hematoxylin = 0.15, hrp_dab = 0.3),
    pixel_size = 1
  )

  expect_s3_class(counts, "wsi_cell_counts")
  expect_equal(nrow(counts$cell_table), 4)
  expect_equal(dim(counts$counts_matrix), c(4, 2))
  expect_equal(colnames(counts$counts_matrix), c("hematoxylin", "hrp_dab"))
  expect_equal(counts$cell_table$roi_id, c("tumour-1", "tumour-1", "stroma-1", NA))
  expect_equal(counts$cell_table$channel_hematoxylin, c(0.1, 0.2, 0.3, 0.4))
  expect_equal(counts$cell_table$channel_hrp_dab, c(0.05, 0.5, 0.1, 0.8))
  expect_equal(counts$roi_counts$cell_count, c(2L, 1L))
  expect_equal(counts$roi_counts$channel_hrp_dab_positive_count, c(1L, 0L))
  expect_true("unassigned" %in% counts$class_counts$class)
  expect_equal(counts$class_counts$cell_count[match("tumour", counts$class_counts$class)], 2L)
  expect_equal(as.data.frame(counts)$cell_id, cells$cell_id)
})

test_that("wsi_cell_counts writes CSV tables and accepts polygon segmentations", {
  rois <- wsi_counts_test_rois()
  cells <- data.frame(cell_id = c("n1", "n2"), x = c(4, 24), y = c(4, 4))
  class(cells) <- c("wsi_segmentation_centroids", "wsi_segmentation", class(cells))
  segmentation <- wsi_segmentation_to_rois(cells, radius = 2, label = "nucleus")
  output_dir <- tempfile("cell-counts")

  counts <- wsi_cell_counts(
    segmentation = segmentation,
    channels = list(marker = matrix(0.7, nrow = 12, ncol = 32)),
    rois = rois,
    output_dir = output_dir,
    overwrite = TRUE
  )

  expect_s3_class(counts, "wsi_cell_counts")
  expect_equal(nrow(counts$cell_table), 2)
  expect_true(all(counts$cell_table$cell_area_px2 > 0))
  expect_equal(counts$cell_table$cell_class, c("nucleus", "nucleus"))
  expect_equal(counts$roi_counts$cell_count, c(1L, 1L))
  expect_true(all(file.exists(counts$files)))
  expect_true(file.exists(file.path(output_dir, "wsi_cell_counts_cell_table.csv")))
  expect_true(file.exists(file.path(output_dir, "wsi_cell_counts_counts_matrix.csv")))
})
