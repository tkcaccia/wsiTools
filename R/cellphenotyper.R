wsi_cellphenotyper_manifest_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    wsi_abort("`path` must be a CellPhenotyper output directory or `00_execution/project_outputs.tsv` file.")
  }
  path <- path.expand(path)
  if (file.exists(path) && !dir.exists(path)) {
    if (!identical(basename(path), "project_outputs.tsv")) {
      wsi_abort("CellPhenotyper project files must be named `project_outputs.tsv`.")
    }
    return(normalizePath(path, mustWork = TRUE))
  }
  manifest <- file.path(path, "00_execution", "project_outputs.tsv")
  if (!file.exists(manifest)) {
    wsi_abort(sprintf("Could not find CellPhenotyper manifest: %s", manifest), class = "wsi_file_not_found")
  }
  normalizePath(manifest, mustWork = TRUE)
}

wsi_cellphenotyper_project_root <- function(manifest_path) {
  normalizePath(dirname(dirname(manifest_path)), mustWork = TRUE)
}

wsi_cellphenotyper_read_manifest <- function(manifest_path) {
  manifest <- tryCatch(
    utils::read.delim(
      manifest_path,
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      quote = "",
      comment.char = "",
      fileEncoding = "UTF-8"
    ),
    error = function(err) {
      wsi_abort(sprintf("Could not read CellPhenotyper manifest `%s`: %s", manifest_path, conditionMessage(err)))
    }
  )
  required <- c("output_id", "stage_id", "stage_folder", "relative_path")
  missing <- setdiff(required, names(manifest))
  if (length(missing)) {
    wsi_abort(sprintf(
      "CellPhenotyper manifest is missing required column%s: %s",
      if (length(missing) == 1L) "" else "s",
      paste(missing, collapse = ", ")
    ))
  }
  char_cols <- vapply(manifest, is.character, logical(1))
  manifest[char_cols] <- lapply(manifest[char_cols], wsi_clean_text)
  manifest
}

wsi_cellphenotyper_resolve_output <- function(root, stage_folder, relative_path, absolute_path = NA_character_) {
  candidates <- c(
    file.path(root, stage_folder, relative_path),
    file.path(root, relative_path),
    absolute_path
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) {
    return(normalizePath(existing[[1L]], mustWork = TRUE))
  }
  normalizePath(candidates[[1L]], mustWork = FALSE)
}

wsi_cellphenotyper_resolve_manifest <- function(root, manifest) {
  absolute_path <- manifest$absolute_path %||% rep(NA_character_, nrow(manifest))
  manifest$resolved_path <- mapply(
    wsi_cellphenotyper_resolve_output,
    root = root,
    stage_folder = manifest$stage_folder,
    relative_path = manifest$relative_path,
    absolute_path = absolute_path,
    USE.NAMES = FALSE
  )
  manifest$file_exists <- file.exists(manifest$resolved_path)
  manifest
}

wsi_cellphenotyper_image_extensions <- function() {
  c(
    "svs", "ndpi", "scn", "mrxs", "bif", "tif", "tiff", "btf",
    "ome.tif", "ome.tiff", "qptiff", "png", "jpg", "jpeg", "webp",
    "czi", "lif", "vsi", "dcm", "dicom"
  )
}

wsi_cellphenotyper_file_ext2 <- function(path) {
  lower <- tolower(basename(path))
  if (grepl("\\.ome\\.tiff?$", lower)) {
    return(sub("^.*\\.(ome\\.tiff?)$", "\\1", lower))
  }
  tools::file_ext(lower)
}

wsi_cellphenotyper_find_input <- function(manifest) {
  ext <- vapply(manifest$resolved_path, wsi_cellphenotyper_file_ext2, character(1))
  is_image <- ext %in% wsi_cellphenotyper_image_extensions()
  input_rows <- manifest$file_exists & is_image & (
    grepl("^input", manifest$stage_id, ignore.case = TRUE) |
      grepl("input", manifest$output_id, ignore.case = TRUE)
  )
  if (!any(input_rows)) {
    input_rows <- manifest$file_exists & is_image
  }
  if (!any(input_rows)) {
    return(NA_character_)
  }
  manifest$resolved_path[which(input_rows)[[1L]]]
}

wsi_cellphenotyper_find_first_existing <- function(paths) {
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(NA_character_)
  }
  normalizePath(paths[[1L]], mustWork = TRUE)
}

wsi_cellphenotyper_find_cell_table <- function(root, manifest) {
  manifest_candidates <- manifest$resolved_path[
    manifest$file_exists &
      grepl("\\.(csv|tsv|txt)$", manifest$resolved_path, ignore.case = TRUE) &
      grepl("objects|cells|centroid|stardist|assignment", manifest$resolved_path, ignore.case = TRUE)
  ]
  candidates <- c(
    Sys.glob(file.path(root, "05_cell_assignments", "*", "*objects_assigned.csv")),
    Sys.glob(file.path(root, "02_stardist", "*", "stardist_out", "objects.csv")),
    Sys.glob(file.path(root, "02_stardist", "*", "*objects*.csv")),
    manifest_candidates
  )
  wsi_cellphenotyper_find_first_existing(candidates)
}

wsi_cellphenotyper_find_shift <- function(root, cell_table = NA_character_) {
  candidates <- character()
  if (!is.na(cell_table) && nzchar(cell_table)) {
    candidates <- c(candidates, file.path(dirname(cell_table), "shift.json"))
  }
  candidates <- c(candidates, Sys.glob(file.path(root, "02_stardist", "*", "stardist_out", "shift.json")))
  wsi_cellphenotyper_find_first_existing(candidates)
}

wsi_cellphenotyper_read_shift <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(list(dx = 0, dy = 0))
  }
  data <- tryCatch(jsonlite::fromJSON(path), error = function(err) NULL)
  offset <- data$offset_crop_to_original %||% list()
  list(
    dx = suppressWarnings(as.numeric(offset$dx %||% 0)),
    dy = suppressWarnings(as.numeric(offset$dy %||% 0))
  )
}

wsi_cellphenotyper_numeric_column <- function(data, candidates) {
  lower <- tolower(names(data))
  for (candidate in tolower(candidates)) {
    hit <- match(candidate, lower)
    if (!is.na(hit)) {
      value <- suppressWarnings(as.numeric(data[[hit]]))
      if (any(is.finite(value))) {
        return(value)
      }
    }
  }
  rep(NA_real_, nrow(data))
}

wsi_cellphenotyper_read_cells <- function(path, shift = list(dx = 0, dy = 0)) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  ext <- tolower(tools::file_ext(path))
  cells <- tryCatch(
    {
      if (identical(ext, "tsv") || identical(ext, "txt")) {
        utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
      }
    },
    error = function(err) {
      wsi_abort(sprintf("Could not read CellPhenotyper cell table `%s`: %s", path, conditionMessage(err)))
    }
  )
  if (!nrow(cells)) {
    return(cells)
  }
  char_cols <- vapply(cells, is.character, logical(1))
  cells[char_cols] <- lapply(cells[char_cols], wsi_clean_text)
  x_orig <- wsi_cellphenotyper_numeric_column(cells, c("x_orig", "global_x", "slide_x"))
  y_orig <- wsi_cellphenotyper_numeric_column(cells, c("y_orig", "global_y", "slide_y"))
  x_local <- wsi_cellphenotyper_numeric_column(cells, c("x", "centroid_x"))
  y_local <- wsi_cellphenotyper_numeric_column(cells, c("y", "centroid_y"))
  x <- if (any(is.finite(x_orig))) x_orig else x_local + as.numeric(shift$dx %||% 0)
  y <- if (any(is.finite(y_orig))) y_orig else y_local + as.numeric(shift$dy %||% 0)
  keep <- is.finite(x) & is.finite(y)
  if (!any(keep)) {
    wsi_abort("The CellPhenotyper cell table does not contain usable `x`/`y` or `x_orig`/`y_orig` centroid coordinates.")
  }
  cells <- cells[keep, , drop = FALSE]
  cells$cellphenotyper_x <- x[keep]
  cells$cellphenotyper_y <- y[keep]
  cells$x <- x[keep]
  cells$y <- y[keep]
  if (!"id" %in% names(cells)) {
    label <- if ("label" %in% names(cells)) as.character(cells$label) else seq_len(nrow(cells))
    cells$id <- paste0("cell_", label)
  }
  if (!"class" %in% names(cells)) {
    cells$class <- if ("polygon_label" %in% names(cells)) as.character(cells$polygon_label) else "StarDist cell"
  }
  attr(cells, "source_file") <- normalizePath(path, mustWork = TRUE)
  class(cells) <- unique(c("wsi_cellphenotyper_cells", "wsi_segmentation_centroids", "wsi_segmentation", class(cells)))
  cells
}

wsi_cellphenotyper_find_stardist_roi <- function(manifest, root) {
  manifest_candidates <- manifest$resolved_path[
    manifest$file_exists &
      grepl("\\.(geojson|json)$", manifest$resolved_path, ignore.case = TRUE) &
      grepl("stardist|roi", manifest$resolved_path, ignore.case = TRUE)
  ]
  candidates <- c(
    manifest_candidates,
    Sys.glob(file.path(root, "02_stardist", "*", "*.geojson")),
    Sys.glob(file.path(root, "02_stardist", "*", "stardist_out", "*original.geojson"))
  )
  wsi_cellphenotyper_find_first_existing(candidates)
}

wsi_cellphenotyper_find_preview <- function(manifest) {
  previews <- manifest$resolved_path[
    manifest$file_exists &
      grepl("\\.(png|jpg|jpeg|webp)$", manifest$resolved_path, ignore.case = TRUE) &
      grepl("preview|stardist", manifest$resolved_path, ignore.case = TRUE)
  ]
  wsi_cellphenotyper_find_first_existing(previews)
}

wsi_cellphenotyper_cell_layer <- function(cells, radius = 6, colour = "#38BDF8",
                                          opacity = 0.75, visible = FALSE) {
  if (is.null(cells) || !is.data.frame(cells) || !nrow(cells)) {
    return(NULL)
  }
  layer <- wsi_viewer_layer_payload(
    "StarDist cells",
    cells,
    type = "points",
    visible = visible,
    opacity = opacity,
    colour = colour,
    radius = radius
  )
  layer$id <- "cellphenotyper_stardist_cells"
  layer$source_type <- "cellphenotyper_stardist"
  layer$count <- nrow(cells)
  layer
}

wsi_cellphenotyper_viewer_config <- function(project, layer_id = "cellphenotyper_stardist_cells") {
  list(
    enabled = TRUE,
    project_root = project$root,
    manifest_path = project$manifest_path,
    input_image = project$input_image,
    stardist_layer_id = layer_id,
    stardist_cells = project$files$cell_table %||% NA_character_,
    stardist_roi = project$files$stardist_roi %||% NA_character_,
    cell_count = as.integer(project$cell_count %||% 0L)
  )
}

wsi_viewer_layers_config <- function(layers = NULL) {
  if (is.null(layers)) {
    return(list())
  }
  if (inherits(layers, "wsi_viewer_layer")) {
    layer <- unclass(layers)
    return(list(layer))
  }
  if (is.data.frame(layers) || is.matrix(layers) || inherits(layers, "wsi_roi")) {
    return(list(unclass(wsi_viewer_layer_payload("Layer", layers))))
  }
  if (!is.list(layers)) {
    wsi_abort("`layers` must be `NULL`, a viewer layer, data frame, matrix, ROI object, or list of layers.")
  }
  lapply(layers, function(layer) {
    if (inherits(layer, "wsi_viewer_layer")) {
      return(unclass(layer))
    }
    if (!is.list(layer) || is.data.frame(layer)) {
      return(unclass(wsi_viewer_layer_payload("Layer", layer)))
    }
    unclass(layer)
  })
}

wsi_viewer_cellphenotyper_config <- function(cellphenotyper = NULL) {
  if (is.null(cellphenotyper)) {
    return(list(enabled = FALSE))
  }
  if (inherits(cellphenotyper, "wsi_cellphenotyper_project")) {
    return(wsi_cellphenotyper_viewer_config(cellphenotyper))
  }
  if (!is.list(cellphenotyper)) {
    wsi_abort("`cellphenotyper` must be `NULL`, a CellPhenotyper project, or a list.")
  }
  cellphenotyper$enabled <- isTRUE(cellphenotyper$enabled %||% TRUE)
  cellphenotyper
}

#' Open a CellPhenotyper project
#'
#' `wsi_read_cellphenotyper_project()` reads a CellPhenotyper output directory
#' by using `00_execution/project_outputs.tsv` as the project manifest. It
#' resolves local output paths, identifies the input image, and loads the
#' StarDist centroid table when available. The large label image is not loaded
#' into memory.
#'
#' `wsi_viewer_cellphenotyper()` opens the input image in the interactive
#' wsiTools viewer and adds a top **Cells** menu that can show or hide the
#' CellPhenotyper/StarDist cell segmentation overlay.
#'
#' @param path CellPhenotyper output directory or path to
#'   `00_execution/project_outputs.tsv`.
#' @param load_cells Whether to read the StarDist/cell-assignment centroid
#'   table.
#' @param project A project object returned by
#'   `wsi_read_cellphenotyper_project()`, or a CellPhenotyper output directory.
#' @param output Output HTML file for the viewer.
#' @param open Whether to open the viewer in a browser.
#' @param overwrite Whether to overwrite `output` if it already exists.
#' @param mode Viewer mode passed to [wsi_viewer()]. Use `"tiles"` for
#'   full-resolution Deep Zoom viewing when libvips is available.
#' @param backend Backend passed to [wsi_open()].
#' @param cell_radius Display radius, in slide pixels, for StarDist cell
#'   centroids.
#' @param cell_colour Cell overlay colour.
#' @param cell_opacity Cell overlay opacity.
#' @param ... Additional arguments passed to [wsi_viewer()].
#'
#' @return `wsi_read_cellphenotyper_project()` returns a
#'   `wsi_cellphenotyper_project` object. `wsi_viewer_cellphenotyper()` returns
#'   the HTML viewer path invisibly.
#' @export
wsi_read_cellphenotyper_project <- function(path, load_cells = TRUE) {
  manifest_path <- wsi_cellphenotyper_manifest_path(path)
  root <- wsi_cellphenotyper_project_root(manifest_path)
  manifest <- wsi_cellphenotyper_read_manifest(manifest_path)
  manifest <- wsi_cellphenotyper_resolve_manifest(root, manifest)
  input_image <- wsi_cellphenotyper_find_input(manifest)
  if (is.na(input_image) || !nzchar(input_image)) {
    wsi_abort("Could not find a local input image in the CellPhenotyper project outputs.")
  }
  cell_table <- wsi_cellphenotyper_find_cell_table(root, manifest)
  shift_path <- wsi_cellphenotyper_find_shift(root, cell_table)
  shift <- wsi_cellphenotyper_read_shift(shift_path)
  cells <- if (isTRUE(load_cells)) wsi_cellphenotyper_read_cells(cell_table, shift = shift) else NULL
  project <- list(
    root = root,
    manifest_path = manifest_path,
    manifest = manifest,
    input_image = input_image,
    cells = cells,
    cell_count = if (is.data.frame(cells)) nrow(cells) else 0L,
    files = list(
      cell_table = cell_table,
      shift = shift_path,
      stardist_roi = wsi_cellphenotyper_find_stardist_roi(manifest, root),
      stardist_preview = wsi_cellphenotyper_find_preview(manifest)
    )
  )
  class(project) <- "wsi_cellphenotyper_project"
  project
}

#' @rdname wsi_read_cellphenotyper_project
#' @export
read_cellphenotyper_project <- wsi_read_cellphenotyper_project

#' @rdname wsi_read_cellphenotyper_project
#' @export
wsi_viewer_cellphenotyper <- function(project, output = NULL, open = interactive(),
                                      overwrite = FALSE,
                                      mode = c("tiles", "thumbnail"),
                                      backend = c("auto", "openslide", "vips"),
                                      cell_radius = 6,
                                      cell_colour = "#38BDF8",
                                      cell_opacity = 0.75,
                                      ...) {
  if (!inherits(project, "wsi_cellphenotyper_project")) {
    project <- wsi_read_cellphenotyper_project(project)
  }
  mode <- match.arg(mode)
  backend <- match.arg(backend)
  if (is.null(output)) {
    output <- file.path(project$root, "cellphenotyper_wsiTools_viewer.html")
  }
  layers <- list(wsi_cellphenotyper_cell_layer(
    project$cells,
    radius = cell_radius,
    colour = cell_colour,
    opacity = cell_opacity,
    visible = FALSE
  ))
  layers <- Filter(Negate(is.null), layers)
  slide <- wsi_open(project$input_image, backend = backend)
  on.exit(wsi_close(slide), add = TRUE)
  wsi_viewer(
    slide,
    output = output,
    open = open,
    overwrite = overwrite,
    mode = mode,
    title = sprintf("CellPhenotyper: %s", basename(project$root)),
    layers = layers,
    cellphenotyper = wsi_cellphenotyper_viewer_config(project),
    ...
  )
}

#' @rdname wsi_read_cellphenotyper_project
#' @export
viewer_cellphenotyper <- wsi_viewer_cellphenotyper

#' @export
print.wsi_cellphenotyper_project <- function(x, ...) {
  cat("<wsi_cellphenotyper_project>\n")
  cat("  root: ", x$root, "\n", sep = "")
  cat("  input: ", x$input_image, "\n", sep = "")
  cat("  outputs: ", nrow(x$manifest), "\n", sep = "")
  cat("  StarDist cells: ", format(x$cell_count %||% 0L, big.mark = ","), "\n", sep = "")
  if (!is.na(x$files$cell_table %||% NA_character_)) {
    cat("  cell table: ", x$files$cell_table, "\n", sep = "")
  }
  invisible(x)
}
