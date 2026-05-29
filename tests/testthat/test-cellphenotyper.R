test_that("CellPhenotyper project reader resolves input and cells", {
  root <- file.path(tempdir(), "cellphenotyper_project")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "00_execution"), recursive = TRUE)
  dir.create(file.path(root, "01_input", "sample"), recursive = TRUE)
  dir.create(file.path(root, "05_cell_assignments", "sample"), recursive = TRUE)

  input <- file.path(root, "01_input", "sample", "sample.ome.tif")
  file.create(input)
  manifest <- data.frame(
    output_id = "input_1",
    stage_id = "input",
    stage_title = "Input Conversion",
    stage_folder = "01_input",
    relative_path = "sample/sample.ome.tif",
    absolute_path = "/remote/sample.ome.tif",
    size_bytes = 1,
    check.names = FALSE
  )
  utils::write.table(
    manifest,
    file.path(root, "00_execution", "project_outputs.tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  cells <- data.frame(
    label = c(1, 2),
    x = c(10, 20),
    y = c(30, 40),
    x_orig = c(110, 120),
    y_orig = c(130, 140),
    polygon_label = c("tumour", "stroma")
  )
  utils::write.csv(
    cells,
    file.path(root, "05_cell_assignments", "sample", "sample_objects_assigned.csv"),
    row.names = FALSE
  )

  project <- wsi_read_cellphenotyper_project(root)
  expect_s3_class(project, "wsi_cellphenotyper_project")
  expect_equal(project$input_image, normalizePath(input, mustWork = TRUE))
  expect_equal(project$cell_count, 2L)
  expect_equal(project$cells$x, c(110, 120))
  expect_equal(project$cells$y, c(130, 140))
  expect_equal(project$cells$class, c("tumour", "stroma"))
})

test_that("CellPhenotyper layer and menu are included in viewer HTML", {
  slide <- wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  cells <- data.frame(
    id = c("cell_1", "cell_2"),
    x = c(100, 250),
    y = c(120, 260),
    class = c("cell", "cell")
  )
  layer <- wsi_cellphenotyper_cell_layer(cells, radius = 7)
  html <- tempfile(fileext = ".html")
  wsi_viewer(
    slide,
    output = html,
    open = FALSE,
    overwrite = TRUE,
    mode = "thumbnail",
    layers = list(layer),
    cellphenotyper = list(
      enabled = TRUE,
      stardist_layer_id = "cellphenotyper_stardist_cells",
      cell_count = 2L
    )
  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "CellPhenotyper StarDist", fixed = TRUE)
  expect_match(text, "cellToggle", fixed = TRUE)
  expect_match(text, "cellphenotyper_stardist_cells", fixed = TRUE)
  expect_match(text, "bindCellControls", fixed = TRUE)
})
