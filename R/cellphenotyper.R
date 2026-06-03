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
    cells$class <- if ("polygon_label" %in% names(cells)) as.character(cells$polygon_label) else "CellPhenotyper cell"
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

wsi_cellphenotyper_find_stage_file <- function(manifest, stage_id, pattern,
                                               fallback_pattern = NULL) {
  if (!nrow(manifest)) {
    return(NA_character_)
  }
  stage_hit <- grepl(stage_id, manifest$stage_id, ignore.case = TRUE) |
    grepl(stage_id, manifest$output_id, ignore.case = TRUE) |
    grepl(stage_id, manifest$stage_folder, ignore.case = TRUE)
  path_hit <- grepl(pattern, manifest$resolved_path, ignore.case = TRUE)
  candidates <- manifest$resolved_path[manifest$file_exists & stage_hit & path_hit]
  found <- wsi_cellphenotyper_find_first_existing(candidates)
  if (!is.na(found)) {
    return(found)
  }
  if (!is.null(fallback_pattern)) {
    fallback <- manifest$resolved_path[
      manifest$file_exists & stage_hit &
        grepl(fallback_pattern, manifest$resolved_path, ignore.case = TRUE)
    ]
    return(wsi_cellphenotyper_find_first_existing(fallback))
  }
  NA_character_
}

wsi_cellphenotyper_find_gigatime_panel <- function(manifest) {
  wsi_cellphenotyper_find_stage_file(
    manifest,
    stage_id = "gigatime",
    pattern = "gigatime_.*channel_panels\\.(png|jpg|jpeg|webp)$",
    fallback_pattern = "channel_panels\\.(png|jpg|jpeg|webp)$|composite_preview\\.(png|jpg|jpeg|webp)$"
  )
}

wsi_cellphenotyper_find_gigatime_probs <- function(manifest) {
  wsi_cellphenotyper_find_stage_file(
    manifest,
    stage_id = "gigatime",
    pattern = "gigatime_.*probs\\.ome\\.tiff?$",
    fallback_pattern = "probs\\.ome\\.tiff?$"
  )
}

wsi_cellphenotyper_find_gigatime_channels <- function(manifest) {
  wsi_cellphenotyper_find_stage_file(
    manifest,
    stage_id = "gigatime",
    pattern = "gigatime_.*channels\\.json$",
    fallback_pattern = "channels\\.json$"
  )
}

wsi_cellphenotyper_find_gigatime_metadata <- function(manifest) {
  wsi_cellphenotyper_find_stage_file(
    manifest,
    stage_id = "gigatime",
    pattern = "gigatime_.*metadata\\.json$",
    fallback_pattern = "metadata\\.json$"
  )
}

wsi_cellphenotyper_empty_kodama_geojson <- function() {
  data.frame(
    label = character(),
    path = character(),
    profile = character(),
    output_id = character(),
    stage_id = character(),
    stringsAsFactors = FALSE
  )
}

wsi_cellphenotyper_empty_kodama_plots <- function() {
  data.frame(
    label = character(),
    path = character(),
    profile = character(),
    plot_type = character(),
    cluster_csv = character(),
    output_id = character(),
    stage_id = character(),
    stringsAsFactors = FALSE
  )
}

wsi_cellphenotyper_empty_grandqc_geojson <- function() {
  data.frame(
    label = character(),
    path = character(),
    output_id = character(),
    stage_id = character(),
    stringsAsFactors = FALSE
  )
}

wsi_cellphenotyper_kodama_label <- function(path, output_id = "", stage_id = "") {
  text <- paste(basename(path), output_id %||% "", stage_id %||% "")
  profile <- if (grepl("fine", text, ignore.case = TRUE)) {
    "Fine"
  } else if (grepl("standard", text, ignore.case = TRUE)) {
    "Standard"
  } else {
    "Refined"
  }
  sprintf("KODAMA %s MedSAM", profile)
}

wsi_cellphenotyper_kodama_profile <- function(path) {
  text <- basename(path %||% "")
  if (grepl("fine", text, ignore.case = TRUE)) {
    "fine"
  } else if (grepl("standard", text, ignore.case = TRUE)) {
    "standard"
  } else {
    "refined"
  }
}

wsi_cellphenotyper_kodama_plot_type <- function(path) {
  text <- basename(path %||% "")
  if (grepl("cluster", text, ignore.case = TRUE)) {
    "cluster"
  } else if (grepl("medsam", text, ignore.case = TRUE)) {
    "medsam"
  } else {
    "membership"
  }
}

wsi_cellphenotyper_kodama_plot_label <- function(path) {
  profile <- tools::toTitleCase(wsi_cellphenotyper_kodama_profile(path))
  type <- switch(
    wsi_cellphenotyper_kodama_plot_type(path),
    medsam = "MedSAM",
    cluster = "Cluster",
    "Membership"
  )
  sprintf("KODAMA %s %s plot", profile, type)
}

wsi_cellphenotyper_find_kodama_embedding <- function(manifest, root) {
  candidates <- character()
  if (nrow(manifest)) {
    manifest_text <- paste(
      manifest$output_id %||% "",
      manifest$stage_id %||% "",
      manifest$stage_folder %||% "",
      manifest$resolved_path %||% ""
    )
    hit <- manifest$file_exists &
      grepl("\\.(rda|rdata)$", manifest$resolved_path, ignore.case = TRUE) &
      grepl("kodama", manifest_text, ignore.case = TRUE)
    candidates <- manifest$resolved_path[hit]
  }
  fallback <- if (dir.exists(root)) {
    list.files(
      root,
      pattern = "kodama.*\\.(rda|rdata)$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    character()
  }
  paths <- unique(c(candidates, fallback))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(NA_character_)
  }
  normalizePath(paths[[1L]], mustWork = TRUE)
}

wsi_cellphenotyper_find_kodama_cluster_csv <- function(manifest, root, profile) {
  profile <- tolower(profile %||% "")
  candidates <- character()
  if (nrow(manifest)) {
    manifest_text <- paste(
      manifest$output_id %||% "",
      manifest$stage_id %||% "",
      manifest$stage_folder %||% "",
      manifest$resolved_path %||% ""
    )
    hit <- manifest$file_exists &
      grepl("\\.csv$", manifest$resolved_path, ignore.case = TRUE) &
      grepl("cluster", manifest_text, ignore.case = TRUE) &
      !grepl("summary", manifest_text, ignore.case = TRUE)
    if (nzchar(profile)) {
      hit <- hit & grepl(profile, manifest_text, ignore.case = TRUE)
    }
    candidates <- manifest$resolved_path[hit]
  }
  fallback <- if (dir.exists(root) && nzchar(profile)) {
    list.files(
      root,
      pattern = paste0(profile, ".*cluster.*\\.csv$"),
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    character()
  }
  paths <- unique(c(candidates, fallback))
  paths <- paths[!grepl("summary", basename(paths), ignore.case = TRUE)]
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(NA_character_)
  }
  normalizePath(paths[[1L]], mustWork = TRUE)
}

wsi_cellphenotyper_kodama_palette <- function() {
  c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
    "#A65628", "#F781BF", "#999999", "#1B9E77", "#D95F02",
    "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D",
    "#666666", "#8DD3C7", "#FB8072", "#80B1D3", "#B3DE69"
  )
}

wsi_cellphenotyper_kodama_colour <- function(cluster) {
  if (!length(cluster)) {
    return(character())
  }
  palette <- wsi_cellphenotyper_kodama_palette()
  raw <- as.character(cluster)
  idx <- suppressWarnings(as.integer(raw))
  bad <- is.na(idx) | idx < 1L
  if (any(bad)) {
    idx[bad] <- vapply(raw[bad], function(value) {
      chars <- utf8ToInt(enc2utf8(value))
      if (!length(chars)) {
        return(1L)
      }
      as.integer((sum(chars * seq_along(chars)) %% length(palette)) + 1L)
    }, integer(1L))
  }
  palette[((idx - 1L) %% length(palette)) + 1L]
}

wsi_cellphenotyper_read_kodama_plot_points <- function(embedding, cluster_csv, max_points = 75000L) {
  if (is.na(embedding) || !nzchar(embedding) || !file.exists(embedding)) {
    return(NULL)
  }
  if (is.na(cluster_csv) || !nzchar(cluster_csv) || !file.exists(cluster_csv)) {
    return(NULL)
  }
  env <- new.env(parent = emptyenv())
  ok <- tryCatch({
    load(embedding, envir = env)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok) || !exists("vis", envir = env, inherits = FALSE)) {
    return(NULL)
  }
  vis <- get("vis", envir = env, inherits = FALSE)
  if (!is.matrix(vis) && !is.data.frame(vis)) {
    return(NULL)
  }
  vis <- as.matrix(vis)
  if (ncol(vis) < 2L || !nrow(vis)) {
    return(NULL)
  }
  kodama_x <- suppressWarnings(as.numeric(vis[, 1L]))
  kodama_y <- suppressWarnings(as.numeric(vis[, 2L]))
  labels <- rownames(vis)
  if (is.null(labels) && exists("ann", envir = env, inherits = FALSE)) {
    ann <- get("ann", envir = env, inherits = FALSE)
    if (is.data.frame(ann) && "label" %in% names(ann) && nrow(ann) == nrow(vis)) {
      labels <- as.character(ann$label)
    }
  }
  if (is.null(labels)) {
    labels <- as.character(seq_len(nrow(vis)))
  }
  clusters <- tryCatch(
    utils::read.csv(cluster_csv, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (!is.data.frame(clusters) || !all(c("label", "cluster") %in% names(clusters))) {
    return(NULL)
  }
  cluster_idx <- match(as.character(labels), as.character(clusters$label))
  cluster <- clusters$cluster[cluster_idx]
  keep <- is.finite(kodama_x) & is.finite(kodama_y) & !is.na(cluster)
  if (!any(keep)) {
    return(NULL)
  }
  colours <- wsi_cellphenotyper_kodama_colour(cluster[keep])
  points <- data.frame(
    label = as.character(labels[keep]),
    x = kodama_x[keep],
    y = kodama_y[keep],
    cluster = as.character(cluster[keep]),
    class = paste0("color_", as.character(cluster[keep])),
    colour = colours,
    color = colours,
    stringsAsFactors = FALSE
  )
  total <- nrow(points)
  if (total > max_points) {
    idx <- unique(round(seq(1, total, length.out = max_points)))
    points <- points[idx, , drop = FALSE]
  }
  row.names(points) <- NULL
  attr(points, "total_points") <- total
  points
}

wsi_cellphenotyper_find_kodama_geojson <- function(manifest) {
  if (!nrow(manifest)) {
    return(wsi_cellphenotyper_empty_kodama_geojson())
  }
  manifest_text <- paste(
    manifest$output_id %||% "",
    manifest$stage_id %||% "",
    manifest$stage_folder %||% "",
    manifest$resolved_path %||% ""
  )
  geojson_hit <- manifest$file_exists &
    grepl("\\.geojson$", manifest$resolved_path, ignore.case = TRUE) &
    grepl("kodama|medsam|cluster_geojson|grown_mask_smooth_class|refined", manifest_text, ignore.case = TRUE)
  if (!any(geojson_hit)) {
    return(wsi_cellphenotyper_empty_kodama_geojson())
  }
  rows <- manifest[geojson_hit, , drop = FALSE]
  rows <- rows[!duplicated(rows$resolved_path), , drop = FALSE]
  profile <- ifelse(
    grepl("fine", rows$resolved_path, ignore.case = TRUE),
    "fine",
    ifelse(grepl("standard", rows$resolved_path, ignore.case = TRUE), "standard", "refined")
  )
  data.frame(
    label = mapply(
      wsi_cellphenotyper_kodama_label,
      rows$resolved_path,
      rows$output_id %||% "",
      rows$stage_id %||% "",
      USE.NAMES = FALSE
    ),
    path = normalizePath(rows$resolved_path, mustWork = TRUE),
    profile = profile,
    output_id = rows$output_id,
    stage_id = rows$stage_id,
    stringsAsFactors = FALSE
  )
}

wsi_cellphenotyper_find_kodama_plots <- function(manifest, root) {
  manifest_candidates <- character()
  if (nrow(manifest)) {
    manifest_text <- paste(
      manifest$output_id %||% "",
      manifest$stage_id %||% "",
      manifest$stage_folder %||% "",
      manifest$resolved_path %||% ""
    )
    plot_hit <- manifest$file_exists &
      grepl("\\.(png|jpg|jpeg|webp)$", manifest$resolved_path, ignore.case = TRUE) &
      grepl("kodama.*membership|membership.*kodama", manifest_text, ignore.case = TRUE)
    manifest_candidates <- manifest$resolved_path[plot_hit]
  }
  fallback <- if (dir.exists(root)) {
    list.files(
      root,
      pattern = "kodama.*membership.*\\.(png|jpg|jpeg|webp)$|membership.*kodama.*\\.(png|jpg|jpeg|webp)$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    character()
  }
  paths <- unique(c(manifest_candidates, fallback))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(wsi_cellphenotyper_empty_kodama_plots())
  }
  rows <- data.frame(
    label = vapply(paths, wsi_cellphenotyper_kodama_plot_label, character(1)),
    path = normalizePath(paths, mustWork = TRUE),
    profile = vapply(paths, wsi_cellphenotyper_kodama_profile, character(1)),
    plot_type = vapply(paths, wsi_cellphenotyper_kodama_plot_type, character(1)),
    cluster_csv = NA_character_,
    output_id = NA_character_,
    stage_id = NA_character_,
    stringsAsFactors = FALSE
  )
  rows$cluster_csv <- vapply(
    rows$profile,
    function(profile) wsi_cellphenotyper_find_kodama_cluster_csv(manifest, root, profile),
    character(1)
  )
  if (nrow(manifest)) {
    idx <- match(rows$path, normalizePath(manifest$resolved_path, mustWork = FALSE))
    hit <- !is.na(idx)
    rows$output_id[hit] <- manifest$output_id[idx[hit]]
    rows$stage_id[hit] <- manifest$stage_id[idx[hit]]
  }
  order <- order(
    match(rows$profile, c("fine", "standard", "refined"), nomatch = 99L),
    match(rows$plot_type, c("cluster", "medsam", "membership"), nomatch = 99L),
    rows$label
  )
  rows <- rows[order, , drop = FALSE]
  row.names(rows) <- NULL
  rows
}

wsi_cellphenotyper_find_grandqc_geojson <- function(manifest, root) {
  manifest_candidates <- character()
  if (nrow(manifest)) {
    manifest_text <- paste(
      manifest$output_id %||% "",
      manifest$stage_id %||% "",
      manifest$stage_folder %||% "",
      manifest$resolved_path %||% ""
    )
    grandqc_hit <- manifest$file_exists &
      grepl("\\.geojson$", manifest$resolved_path, ignore.case = TRUE) &
      grepl("grandqc", manifest_text, ignore.case = TRUE)
    manifest_candidates <- manifest$resolved_path[grandqc_hit]
  }
  fallback <- c(
    Sys.glob(file.path(root, "01a_grandqc", "*", "*", "*grandqc*.geojson")),
    Sys.glob(file.path(root, "01a_grandqc", "*", "*grandqc*.geojson")),
    Sys.glob(file.path(root, "01a_grandqc", "*grandqc*.geojson"))
  )
  paths <- unique(c(manifest_candidates, fallback))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(wsi_cellphenotyper_empty_grandqc_geojson())
  }
  rows <- data.frame(
    label = paste0("GrandQC ", tools::file_path_sans_ext(basename(paths))),
    path = normalizePath(paths, mustWork = TRUE),
    output_id = NA_character_,
    stage_id = NA_character_,
    stringsAsFactors = FALSE
  )
  if (nrow(manifest)) {
    idx <- match(rows$path, normalizePath(manifest$resolved_path, mustWork = FALSE))
    hit <- !is.na(idx)
    rows$output_id[hit] <- manifest$output_id[idx[hit]]
    rows$stage_id[hit] <- manifest$stage_id[idx[hit]]
  }
  rows
}

wsi_cellphenotyper_read_json <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE), error = function(err) NULL)
}

wsi_cellphenotyper_read_geojson <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(err) NULL)
}

wsi_cellphenotyper_geojson_feature_count <- function(geojson) {
  features <- geojson$features %||% list()
  if (is.list(features)) length(features) else 0L
}

wsi_cellphenotyper_shift_bbox <- function(bbox, dx = 0, dy = 0) {
  if (!is.numeric(bbox) || length(bbox) < 4L) {
    return(bbox)
  }
  out <- bbox
  out[[1L]] <- out[[1L]] + dx
  out[[2L]] <- out[[2L]] + dy
  out[[3L]] <- out[[3L]] + dx
  out[[4L]] <- out[[4L]] + dy
  out
}

wsi_cellphenotyper_shift_geojson <- function(geojson, dx = 0, dy = 0) {
  dx <- suppressWarnings(as.numeric(dx %||% 0))
  dy <- suppressWarnings(as.numeric(dy %||% 0))
  if (!is.list(geojson) || !is.finite(dx) || !is.finite(dy) || (dx == 0 && dy == 0)) {
    return(geojson)
  }
  out <- geojson
  if (!is.null(out$bbox)) {
    out$bbox <- wsi_cellphenotyper_shift_bbox(out$bbox, dx = dx, dy = dy)
  }
  features <- out$features %||% list()
  if (is.list(features) && length(features)) {
    out$features <- lapply(features, function(feature) {
      if (is.list(feature$geometry) && !is.null(feature$geometry$coordinates)) {
        feature$geometry$coordinates <- wsi_offset_coordinates(feature$geometry$coordinates, dx = dx, dy = dy)
      }
      if (!is.null(feature$bbox)) {
        feature$bbox <- wsi_cellphenotyper_shift_bbox(feature$bbox, dx = dx, dy = dy)
      }
      props <- feature$properties %||% list()
      wsi_tools_meta <- props$wsiTools %||% list()
      if (!is.list(wsi_tools_meta) || is.data.frame(wsi_tools_meta)) {
        wsi_tools_meta <- list()
      }
      props$wsiTools <- utils::modifyList(
        wsi_tools_meta,
        list(
          coordinate_space = "slide",
          source_coordinate_space = "cellphenotyper_crop",
          shift_dx = dx,
          shift_dy = dy
        ),
        keep.null = TRUE
      )
      feature$properties <- props
      feature
    })
  }
  out
}

wsi_cellphenotyper_kodama_shift <- function(project) {
  extent <- wsi_cellphenotyper_gigatime_extent(project)
  if (is.numeric(extent) && length(extent) >= 2L &&
      all(is.finite(extent[c("x", "y")]))) {
    return(c(dx = unname(extent[["x"]]), dy = unname(extent[["y"]])))
  }
  shift_path <- project$files$shift %||% NA_character_
  shift <- wsi_cellphenotyper_read_shift(shift_path)
  c(dx = as.numeric(shift$dx %||% 0), dy = as.numeric(shift$dy %||% 0))
}

wsi_cellphenotyper_gigatime_channel_names <- function(project) {
  channels_path <- project$files$gigatime_channels %||% NA_character_
  channels <- wsi_cellphenotyper_read_json(channels_path)
  if (is.character(channels) && length(channels)) {
    return(channels)
  }
  metadata <- wsi_cellphenotyper_read_json(project$files$gigatime_metadata %||% NA_character_)
  metadata_channels <- metadata$store_channels %||% metadata$channels %||% NULL
  if (is.character(metadata_channels) && length(metadata_channels)) {
    return(metadata_channels)
  }
  NULL
}

wsi_cellphenotyper_gigatime_extent <- function(project) {
  shift_path <- project$files$shift %||% NA_character_
  if (!is.na(shift_path) && nzchar(shift_path) && file.exists(shift_path)) {
    shift_extent <- tryCatch(wsi_registration_extent(shift_path), error = function(err) NULL)
    if (is.numeric(shift_extent) && length(shift_extent) == 4L && all(is.finite(shift_extent))) {
      names(shift_extent) <- c("x", "y", "width", "height")
      return(shift_extent)
    }
  }
  metadata <- wsi_cellphenotyper_read_json(project$files$gigatime_metadata %||% NA_character_)
  if (!is.list(metadata)) {
    return(NULL)
  }
  shape <- metadata$original_shape_yx %||% NULL
  if (is.numeric(shape) && length(shape) >= 2L && all(is.finite(shape[seq_len(2L)]))) {
    return(c(x = 0, y = 0, width = as.numeric(shape[[2L]]), height = as.numeric(shape[[1L]])))
  }
  NULL
}

wsi_cellphenotyper_bind_channels_to_slide <- function(channels, slide) {
  if (is.null(channels) || !inherits(channels, "wsi_mihc_channel_sources")) {
    return(channels)
  }
  base_path <- as.character(slide$path %||% "")
  base_project_id <- paste0("project_slide_", wsi_project_id(basename(base_path)))
  channels$dynamic_sources <- lapply(channels$dynamic_sources, function(source) {
    source$metadata <- utils::modifyList(
      source$metadata %||% list(),
      list(
        target_path = base_path,
        base_path = base_path,
        base_slide_path = base_path,
        slide_path = base_path,
        project_item_id = base_project_id,
        target_project_item_id = base_project_id,
        target_role = "base",
        source_type = "cellphenotyper_gigatime"
      ),
      keep.null = TRUE
    )
    source
  })
  channels
}

wsi_cellphenotyper_gigatime_channel_sources <- function(project, slide,
                                                        colours = NULL,
                                                        opacity = 0.55,
                                                        visible = TRUE,
                                                        tile_size = 512,
                                                        format = "png") {
  probs <- project$files$gigatime_probs %||% NA_character_
  if (is.na(probs) || !nzchar(probs) || !file.exists(probs)) {
    return(NULL)
  }
  channels <- wsi_mihc_channel_sources(
    probs,
    channel_names = wsi_cellphenotyper_gigatime_channel_names(project),
    colours = colours,
    opacity = opacity,
    visible = visible,
    tile_size = tile_size,
    format = format,
    extent = wsi_cellphenotyper_gigatime_extent(project)
  )
  wsi_cellphenotyper_bind_channels_to_slide(channels, slide)
}

wsi_cellphenotyper_project_images <- function(project, include_gigatime_panel = FALSE) {
  gigatime_panel <- project$files$gigatime_panel %||% NA_character_
  if (!isTRUE(include_gigatime_panel) ||
      is.na(gigatime_panel) || !nzchar(gigatime_panel) || !file.exists(gigatime_panel)) {
    return(list())
  }
  list(list(
    id = "cellphenotyper_gigatime_panel",
    label = "GigaTIME channel panels",
    path = gigatime_panel,
    backend = "cellphenotyper",
    type = "gigatime-panel",
    status = "manifest output",
    role = "gigatime",
    stage = "03_gigatime"
  ))
}

wsi_cellphenotyper_cell_layer <- function(cells, radius = 6, colour = "#38BDF8",
                                          opacity = 0.75, visible = FALSE) {
  if (is.null(cells) || !is.data.frame(cells) || !nrow(cells)) {
    return(NULL)
  }
  layer <- wsi_viewer_layer_payload(
    "CellPhenotyper cells",
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

wsi_cellphenotyper_kodama_config <- function(project) {
  files <- project$files$kodama_geojson %||% wsi_cellphenotyper_empty_kodama_geojson()
  plot_files <- project$files$kodama_plots %||% wsi_cellphenotyper_empty_kodama_plots()
  embedding <- project$files$kodama_embedding %||% NA_character_
  shift <- wsi_cellphenotyper_kodama_shift(project)
  geojsons <- list()
  if (is.data.frame(files) && nrow(files)) {
    geojsons <- lapply(seq_len(nrow(files)), function(i) {
      geojson <- wsi_cellphenotyper_read_geojson(files$path[[i]])
      if (is.null(geojson)) {
        return(NULL)
      }
      geojson <- wsi_cellphenotyper_shift_geojson(geojson, dx = shift[["dx"]], dy = shift[["dy"]])
      id <- wsi_safe_id(paste("kodama", files$profile[[i]], tools::file_path_sans_ext(basename(files$path[[i]])), sep = "_"), "kodama")
      list(
        id = id,
        label = files$label[[i]],
        profile = files$profile[[i]],
        path = files$path[[i]],
        output_id = files$output_id[[i]],
        stage_id = files$stage_id[[i]],
        shift_dx = unname(shift[["dx"]]),
        shift_dy = unname(shift[["dy"]]),
        coordinate_space = "slide",
        feature_count = wsi_cellphenotyper_geojson_feature_count(geojson),
        geojson = geojson
      )
    })
  }
  geojsons <- Filter(Negate(is.null), geojsons)
  plots <- list()
  if (is.data.frame(plot_files) && nrow(plot_files)) {
    plots <- lapply(seq_len(nrow(plot_files)), function(i) {
      path <- plot_files$path[[i]]
      if (is.na(path) || !nzchar(path) || !file.exists(path)) {
        return(NULL)
      }
      points <- wsi_cellphenotyper_read_kodama_plot_points(embedding, plot_files$cluster_csv[[i]])
      list(
        id = wsi_safe_id(paste("kodama_plot", plot_files$profile[[i]], plot_files$plot_type[[i]], sep = "_"), "kodama_plot"),
        label = plot_files$label[[i]],
        path = path,
        profile = plot_files$profile[[i]],
        plot_type = plot_files$plot_type[[i]],
        cluster_csv = plot_files$cluster_csv[[i]],
        embedding = embedding,
        output_id = plot_files$output_id[[i]],
        stage_id = plot_files$stage_id[[i]],
        points = points,
        point_count = if (is.null(points)) 0L else as.integer(attr(points, "total_points") %||% nrow(points))
      )
    })
    plots <- Filter(Negate(is.null), plots)
  }
  list(
    enabled = length(geojsons) > 0L || length(plots) > 0L,
    geojsons = geojsons,
    plots = plots
  )
}

wsi_cellphenotyper_grandqc_config <- function(project) {
  files <- project$files$grandqc_geojson %||% wsi_cellphenotyper_empty_grandqc_geojson()
  if (!is.data.frame(files) || !nrow(files)) {
    return(list(enabled = FALSE, geojsons = list()))
  }
  geojsons <- lapply(seq_len(nrow(files)), function(i) {
    geojson <- wsi_cellphenotyper_read_geojson(files$path[[i]])
    if (is.null(geojson)) {
      return(NULL)
    }
    id <- wsi_safe_id(
      paste("grandqc", tools::file_path_sans_ext(basename(files$path[[i]])), sep = "_"),
      "grandqc"
    )
    list(
      id = id,
      label = files$label[[i]],
      path = files$path[[i]],
      output_id = files$output_id[[i]],
      stage_id = files$stage_id[[i]],
      coordinate_space = "slide",
      feature_count = wsi_cellphenotyper_geojson_feature_count(geojson),
      geojson = geojson
    )
  })
  geojsons <- Filter(Negate(is.null), geojsons)
  list(
    enabled = length(geojsons) > 0L,
    geojsons = geojsons
  )
}

wsi_cellphenotyper_viewer_config <- function(project, layer_id = "cellphenotyper_stardist_cells") {
  list(
    enabled = TRUE,
    project_type = "cellphenotyper",
    is_project = TRUE,
    project_root = project$root,
    manifest_path = project$manifest_path,
    input_image = project$input_image,
    gigatime_panel = project$files$gigatime_panel %||% NA_character_,
    gigatime_probs = project$files$gigatime_probs %||% NA_character_,
    gigatime_channels = project$files$gigatime_channels %||% NA_character_,
    gigatime_metadata = project$files$gigatime_metadata %||% NA_character_,
    stardist_layer_id = layer_id,
    stardist_cells = project$files$cell_table %||% NA_character_,
    stardist_roi = project$files$stardist_roi %||% NA_character_,
    kodama = wsi_cellphenotyper_kodama_config(project),
    grandqc = wsi_cellphenotyper_grandqc_config(project),
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
#' CellPhenotyper centroid table when available. The large label image is not loaded
#' into memory. GigaTIME OME-TIFF probability output, preview files, and
#' MedSAM-refined KODAMA GeoJSON annotations, KODAMA membership plot PNGs,
#' KODAMA RData embeddings, and cluster CSV files are resolved from the same
#' manifest when present.
#'
#' `wsi_viewer_cellphenotyper()` opens the input image in the interactive
#' wsiTools viewer and adds a top **Cells** menu that can show or hide the
#' CellPhenotyper cell segmentation overlay. When
#' `gigatime_probs.ome.tif` is available, it is shown as live tiled mIHC
#' channel overlays on top of the H&E image and controlled from the top
#' **Stains** menu. When refined KODAMA GeoJSON or membership plots are
#' available, the top **CellPhenotyper** menu can import them as editable viewer
#' annotations and can open membership plots in a floating window. The default
#' KODAMA plot view redraws the KODAMA embedding points using the same cluster
#' colours as the imported GeoJSON annotations, with a spatial GeoJSON redraw
#' fallback. Users can drag over KODAMA points to highlight the matching
#' CellPhenotyper cells on the slide; in live sessions this selected-cell set is
#' synced back to R. When GrandQC GeoJSON is available, the top **Artifacts**
#' menu imports those QC regions instead of running browser-side artifact
#' detection.
#'
#' @param path CellPhenotyper output directory or path to
#'   `00_execution/project_outputs.tsv`.
#' @param load_cells Whether to read the CellPhenotyper cell-assignment centroid
#'   table.
#' @param project A project object returned by
#'   `wsi_read_cellphenotyper_project()`, or a CellPhenotyper output directory.
#' @param output Output HTML file for the viewer.
#' @param open Whether to open the viewer in a browser.
#' @param overwrite Whether to overwrite `output` if it already exists.
#' @param mode Viewer mode passed to [wsi_viewer()]. Use `"tiles"` for
#'   full-resolution Deep Zoom viewing when libvips is available.
#' @param backend Backend passed to [wsi_open()].
#' @param cell_radius Display radius, in slide pixels, for CellPhenotyper cell
#'   centroids.
#' @param cell_colour Cell overlay colour.
#' @param cell_opacity Cell overlay opacity.
#' @param gigatime_overlay Whether to overlay the GigaTIME probability OME-TIFF
#'   as tiled mIHC channels when available.
#' @param gigatime_colours Optional display colours for the GigaTIME channels.
#' @param gigatime_opacity Initial GigaTIME channel opacity.
#' @param gigatime_visible Initial GigaTIME channel visibility.
#' @param live Whether to use the live `httpuv` bridge. The default uses live
#'   mode automatically when a GigaTIME OME-TIFF overlay can be served.
#' @param dynamic_tiles Whether the H&E base image should also use live dynamic
#'   tiles. The default keeps the base image on static Deep Zoom tiles when
#'   possible and uses live tiles only for the GigaTIME channels.
#' @param transport Live viewer transport.
#' @param wait Whether to run the live viewer event loop before returning.
#' @param ... Additional arguments passed to [wsi_viewer()].
#'
#' @return `wsi_read_cellphenotyper_project()` returns a
#'   `wsi_cellphenotyper_project` object. `wsi_viewer_cellphenotyper()` returns
#'   a live viewer session when GigaTIME channels are overlaid, otherwise the
#'   HTML viewer path invisibly.
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
      stardist_preview = wsi_cellphenotyper_find_preview(manifest),
      gigatime_panel = wsi_cellphenotyper_find_gigatime_panel(manifest),
      gigatime_probs = wsi_cellphenotyper_find_gigatime_probs(manifest),
      gigatime_channels = wsi_cellphenotyper_find_gigatime_channels(manifest),
      gigatime_metadata = wsi_cellphenotyper_find_gigatime_metadata(manifest),
      kodama_embedding = wsi_cellphenotyper_find_kodama_embedding(manifest, root),
      kodama_geojson = wsi_cellphenotyper_find_kodama_geojson(manifest),
      kodama_plots = wsi_cellphenotyper_find_kodama_plots(manifest, root),
      grandqc_geojson = wsi_cellphenotyper_find_grandqc_geojson(manifest, root)
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
                                      gigatime_overlay = TRUE,
                                      gigatime_colours = NULL,
                                      gigatime_opacity = 0.55,
                                      gigatime_visible = TRUE,
                                      live = NULL,
                                      dynamic_tiles = FALSE,
                                      transport = c("auto", "websocket", "polling"),
                                      wait = FALSE,
                                      ...) {
  if (!inherits(project, "wsi_cellphenotyper_project")) {
    project <- wsi_read_cellphenotyper_project(project)
  }
  mode <- match.arg(mode)
  backend <- match.arg(backend)
  transport <- match.arg(transport)
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
  dots <- list(...)
  if (!is.null(dots$layers)) {
    extra_layers <- dots$layers
    if (inherits(extra_layers, "wsi_viewer_layer") || is.data.frame(extra_layers) ||
        is.matrix(extra_layers) || inherits(extra_layers, "wsi_roi")) {
      extra_layers <- list(extra_layers)
    }
    layers <- c(layers, extra_layers)
    dots$layers <- NULL
  }
  overlay_possible <- isTRUE(gigatime_overlay) &&
    !is.na(project$files$gigatime_probs %||% NA_character_) &&
    nzchar(project$files$gigatime_probs %||% "") &&
    file.exists(project$files$gigatime_probs %||% "") &&
    wsi_has_vips() &&
    requireNamespace("httpuv", quietly = TRUE)
  use_live <- if (is.null(live)) overlay_possible else isTRUE(live)
  dots$project_images <- dots$project_images %||%
    wsi_cellphenotyper_project_images(project, include_gigatime_panel = !overlay_possible)
  dots$title <- dots$title %||% sprintf("CellPhenotyper: %s", basename(project$root))
  dots$cellphenotyper <- dots$cellphenotyper %||% wsi_cellphenotyper_viewer_config(project)

  if (isTRUE(gigatime_overlay) && !overlay_possible &&
      !is.na(project$files$gigatime_probs %||% NA_character_) &&
      nzchar(project$files$gigatime_probs %||% "") &&
      file.exists(project$files$gigatime_probs %||% "")) {
    wsi_warn(
      "GigaTIME OME-TIFF was found, but live mIHC overlay requires both libvips and the optional R package `httpuv`. Opening without the tiled GigaTIME overlay."
    )
  }

  if (isTRUE(use_live)) {
    channels <- tryCatch(
      wsi_cellphenotyper_gigatime_channel_sources(
        project,
        slide,
        colours = gigatime_colours,
        opacity = gigatime_opacity,
        visible = gigatime_visible
      ),
      error = function(err) {
        wsi_warn(paste0("Could not prepare GigaTIME channel overlays: ", conditionMessage(err)))
        NULL
      }
    )
    if (!is.null(channels)) {
      dots$channel_sources <- wsi_channel_sources_combine(dots$channel_sources %||% NULL, channels)
      dots$base_layer_name <- dots$base_layer_name %||% "H&E"
      dots$stain <- dots$stain %||% "none"
    }
    args <- c(
      list(
        slide = slide,
        output = output,
        open = open,
        overwrite = overwrite,
        mode = mode,
        dynamic_tiles = dynamic_tiles,
        transport = transport,
        wait = wait,
        layers = layers
      ),
      dots
    )
    return(do.call(wsi_viewer_live, args))
  }

  on.exit(wsi_close(slide), add = TRUE)
  args <- c(
    list(
      slide = slide,
      output = output,
      open = open,
      overwrite = overwrite,
      mode = mode,
      layers = layers
    ),
    dots
  )
  do.call(wsi_viewer, args)
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
  cat("  CellPhenotyper cells: ", format(x$cell_count %||% 0L, big.mark = ","), "\n", sep = "")
  if (!is.na(x$files$cell_table %||% NA_character_)) {
    cat("  cell table: ", x$files$cell_table, "\n", sep = "")
  }
  if (!is.na(x$files$gigatime_panel %||% NA_character_)) {
    cat("  GigaTIME panel: ", x$files$gigatime_panel, "\n", sep = "")
  }
  if (!is.na(x$files$gigatime_probs %||% NA_character_)) {
    cat("  GigaTIME OME-TIFF: ", x$files$gigatime_probs, "\n", sep = "")
  }
  kodama <- x$files$kodama_geojson %||% wsi_cellphenotyper_empty_kodama_geojson()
  if (is.data.frame(kodama) && nrow(kodama)) {
    cat("  KODAMA GeoJSON: ", nrow(kodama), " file", if (nrow(kodama) == 1L) "" else "s", "\n", sep = "")
  }
  if (!is.na(x$files$kodama_embedding %||% NA_character_)) {
    cat("  KODAMA embedding: ", x$files$kodama_embedding, "\n", sep = "")
  }
  kodama_plots <- x$files$kodama_plots %||% wsi_cellphenotyper_empty_kodama_plots()
  if (is.data.frame(kodama_plots) && nrow(kodama_plots)) {
    cat("  KODAMA plots: ", nrow(kodama_plots), " file", if (nrow(kodama_plots) == 1L) "" else "s", "\n", sep = "")
  }
  grandqc <- x$files$grandqc_geojson %||% wsi_cellphenotyper_empty_grandqc_geojson()
  if (is.data.frame(grandqc) && nrow(grandqc)) {
    cat("  GrandQC GeoJSON: ", nrow(grandqc), " file", if (nrow(grandqc) == 1L) "" else "s", "\n", sep = "")
  }
  invisible(x)
}
