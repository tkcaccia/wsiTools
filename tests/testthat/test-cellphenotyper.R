write_cellphenotyper_test_png <- function(path) {
  grDevices::png(path, width = 80, height = 80)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::rect(0, 0, 1, 1, col = "#f5f5f5", border = NA)
}

test_that("CellPhenotyper project reader resolves input and cells", {
  root <- file.path(tempdir(), "cellphenotyper_project")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "00_execution"), recursive = TRUE)
  dir.create(file.path(root, "01_input", "sample"), recursive = TRUE)
  dir.create(file.path(root, "05_cell_assignments", "sample"), recursive = TRUE)
  dir.create(file.path(root, "03_gigatime", "sample", "gigatime_sample"), recursive = TRUE)

  input <- file.path(root, "01_input", "sample", "sample.ome.tif")
  file.create(input)
  panel <- file.path(root, "03_gigatime", "sample", "gigatime_sample", "gigatime_5marker_channel_panels.png")
  write_cellphenotyper_test_png(panel)
  manifest <- data.frame(
    output_id = c("input_1", "gigatime_1"),
    stage_id = c("input", "gigatime"),
    stage_title = c("Input Conversion", "GigaTIME Virtual mIHC"),
    stage_folder = c("01_input", "03_gigatime"),
    relative_path = c(
      "sample/sample.ome.tif",
      "sample/gigatime_sample/gigatime_5marker_channel_panels.png"
    ),
    absolute_path = c("/remote/sample.ome.tif", "/remote/gigatime_5marker_channel_panels.png"),
    size_bytes = c(1, 1),
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
  expect_equal(project$files$gigatime_panel, normalizePath(panel, mustWork = TRUE))
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

test_that("CellPhenotyper GigaTIME panel is added as a project image", {
  root <- file.path(tempdir(), "cellphenotyper_project_panel")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "00_execution"), recursive = TRUE)
  dir.create(file.path(root, "01_input", "sample"), recursive = TRUE)
  dir.create(file.path(root, "03_gigatime", "sample", "gigatime_sample"), recursive = TRUE)

  input <- file.path(root, "01_input", "sample", "sample.ome.tif")
  file.create(input)
  panel <- file.path(root, "03_gigatime", "sample", "gigatime_sample", "gigatime_5marker_channel_panels.png")
  write_cellphenotyper_test_png(panel)

  manifest <- data.frame(
    output_id = c("input_1", "gigatime_1"),
    stage_id = c("input", "gigatime"),
    stage_title = c("Input Conversion", "GigaTIME Virtual mIHC"),
    stage_folder = c("01_input", "03_gigatime"),
    relative_path = c(
      "sample/sample.ome.tif",
      "sample/gigatime_sample/gigatime_5marker_channel_panels.png"
    ),
    absolute_path = c("", ""),
    size_bytes = c(1, 1),
    check.names = FALSE
  )
  utils::write.table(
    manifest,
    file.path(root, "00_execution", "project_outputs.tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  project <- wsi_read_cellphenotyper_project(root, load_cells = FALSE)
  slide <- wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  slide$path <- project$input_image
  html <- tempfile(fileext = ".html")
  wsi_viewer(
    slide,
    output = html,
    open = FALSE,
    overwrite = TRUE,
    mode = "thumbnail",
    project_images = wsi_cellphenotyper_project_images(project)
  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "GigaTIME channel panels", fixed = TRUE)
  expect_match(text, "gigatime_5marker_channel_panels.png", fixed = TRUE)
})
