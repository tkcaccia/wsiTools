wsi_spatial_tile_size_pixels <- function(slide, tile_size, units = c("px", "um")) {
  wsi_check_slide(slide)
  units <- match.arg(units)
  tile_size <- as.numeric(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  if (identical(units, "px")) {
    size <- as.integer(round(tile_size))
    return(c(width = size, height = size))
  }

  mpp <- tryCatch(wsi_mpp(slide), error = function(err) list(x = NA_real_, y = NA_real_))
  mpp_x <- as.numeric(mpp[["x"]] %||% NA_real_)
  mpp_y <- as.numeric(mpp[["y"]] %||% mpp_x)
  if (!is.finite(mpp_x) || mpp_x <= 0 || !is.finite(mpp_y) || mpp_y <= 0) {
    wsi_abort("Micron tile sizes require image scale metadata (`wsi_mpp(slide)`). Use `units = \"px\"` or provide an image with microns-per-pixel metadata.")
  }
  c(
    width = as.integer(max(1L, round(tile_size / mpp_x))),
    height = as.integer(max(1L, round(tile_size / mpp_y)))
  )
}

wsi_spatial_tile_grid <- function(linked, tile_size = 512, units = c("px", "um"),
                                  roi = NULL, level = 0, bounds = c("drop", "trim", "error"),
                                  format = c("png", "jpeg", "tiff"), prefix = NULL,
                                  selected_ids = NULL, selection_scope = c("all", "selected")) {
  if (!inherits(linked, "wsi_seurat_spatial") && !inherits(linked, "wsi_spatial_object")) {
    wsi_abort("`linked` must be returned by a wsiTools spatial-image linker.")
  }
  units <- match.arg(units)
  bounds <- match.arg(bounds)
  format <- match.arg(format)
  selection_scope <- match.arg(selection_scope)
  slide <- linked$slide
  wsi_check_slide(slide)
  spots <- linked$spots
  if (!is.data.frame(spots) || !nrow(spots) || !all(c("x", "y") %in% names(spots))) {
    return(wsi_empty_tile_preview())
  }
  source_ids <- as.character(spots$barcode %||% spots$id %||% spots$label %||% seq_len(nrow(spots)))
  if (identical(selection_scope, "selected") || !is.null(selected_ids)) {
    if (anyDuplicated(source_ids)) wsi_abort("Spot IDs must be unique within the selected tissue before tile export.")
    selected_ids <- wsi_review_ids(selected_ids, "selected spot IDs", source_ids)
    spots <- spots[source_ids %in% selected_ids, , drop = FALSE]
    if (!nrow(spots)) return(wsi_empty_tile_preview())
  }

  size_px <- wsi_spatial_tile_size_pixels(slide, tile_size = tile_size, units = units)
  spot_id <- as.character(spots$barcode %||% spots$id %||% spots$label %||% seq_len(nrow(spots)))
  spot_id[is.na(spot_id) | !nzchar(spot_id)] <- sprintf("spot_%05d", which(is.na(spot_id) | !nzchar(spot_id)))
  tile_id <- paste0("spot_", wsi_safe_id(spot_id, "spot"))
  stem <- tile_id
  if (!is.null(prefix)) {
    stem <- paste(wsi_safe_id(prefix, "spatial"), stem, sep = "_")
  }
  coords <- data.frame(
    tile_id = tile_id,
    x = as.numeric(spots$x),
    y = as.numeric(spots$y),
    width = as.integer(size_px[["width"]]),
    height = as.integer(size_px[["height"]]),
    level = as.integer(level),
    row = seq_len(nrow(spots)),
    col = 1L,
    output_file = paste0(stem, ".", wsi_format_extension(format)),
    spot_id = spot_id,
    spot_label = as.character(spots$label %||% spot_id),
    stringsAsFactors = FALSE
  )
  grid <- wsi_tile_grid_from_coords(
    slide,
    coords,
    tile_size = max(size_px),
    level = level,
    anchor = "center",
    bounds = bounds
  )
  extra_columns <- setdiff(c("spot_id", "spot_label"), names(grid))
  if (nrow(grid) && length(extra_columns)) {
    idx <- match(grid$tile_id, coords$tile_id)
    grid <- cbind(grid, coords[idx, extra_columns, drop = FALSE])
  }
  if (!nrow(grid) || is.null(roi)) {
    return(wsi_tile_preview(grid))
  }
  roi <- wsi_viewer_coerce_rois(roi)
  if (!nrow(roi)) {
    return(wsi_tile_preview(grid[0, , drop = FALSE]))
  }

  downsample <- if ("downsample" %in% names(grid)) as.numeric(grid$downsample) else rep(1, nrow(grid))
  keep <- rep(FALSE, nrow(grid))
  for (i in seq_len(nrow(roi))) {
    x0 <- as.numeric(grid$x)
    y0 <- as.numeric(grid$y)
    x1 <- x0 + as.numeric(grid$width) * downsample
    y1 <- y0 + as.numeric(grid$height) * downsample
    inside <- wsi_rectangular_footprints_in_roi(data.frame(x0 = x0, y0 = y0, x1 = x1, y1 = y1), roi, i)
    keep <- keep | inside
  }
  wsi_tile_preview(grid[keep, , drop = FALSE])
}

wsi_spatial_tile_payload_grid <- function(tiles, max_tiles = 100000L) {
  max_tiles <- as.integer(wsi_check_scalar_number(max_tiles, "max_tiles", allow_zero = FALSE))
  if (is.data.frame(tiles)) {
    grid <- as.data.frame(tiles, stringsAsFactors = FALSE)
  } else if (is.list(tiles) && length(tiles)) {
    rows <- lapply(seq_along(tiles), function(i) {
      tile <- tiles[[i]]
      if (!is.list(tile)) {
        wsi_abort(sprintf("Tile entry %d must be a JSON object.", i))
      }
      tile <- lapply(tile, function(value) {
        if (is.null(value)) NA else value
      })
      as.data.frame(tile, stringsAsFactors = FALSE)
    })
    all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
    rows <- lapply(rows, function(row) {
      for (name in setdiff(all_names, names(row))) {
        row[[name]] <- NA
      }
      row[all_names]
    })
    grid <- do.call(rbind, rows)
  } else {
    wsi_abort("Spatial tile export requires a non-empty `tiles` array.")
  }
  if (nrow(grid) > max_tiles) {
    wsi_abort(sprintf("Refusing to export %s tiles in one request; maximum is %s.", nrow(grid), max_tiles))
  }
  needed <- c("x", "y", "width", "height")
  if (!all(needed %in% names(grid))) {
    wsi_abort("Spatial tile export tiles must include `x`, `y`, `width`, and `height`.")
  }
  for (column in c("x", "y", "width", "height", "downsample")) {
    if (column %in% names(grid)) {
      grid[[column]] <- suppressWarnings(as.numeric(grid[[column]]))
    }
  }
  if (any(!is.finite(grid$x)) || any(!is.finite(grid$y)) ||
      any(!is.finite(grid$width)) || any(grid$width <= 0) ||
      any(!is.finite(grid$height)) || any(grid$height <= 0)) {
    wsi_abort("Spatial tile coordinates and sizes must be finite positive values.")
  }
  grid$width <- as.integer(round(grid$width))
  grid$height <- as.integer(round(grid$height))
  grid$x <- as.integer(round(grid$x))
  grid$y <- as.integer(round(grid$y))
  if (!"level" %in% names(grid)) {
    grid$level <- 0L
  }
  grid$level <- suppressWarnings(as.integer(grid$level))
  grid$level[is.na(grid$level)] <- 0L
  if (!"downsample" %in% names(grid)) {
    grid$downsample <- 1
  }
  grid$downsample[is.na(grid$downsample) | !is.finite(grid$downsample) | grid$downsample <= 0] <- 1
  if (!"tile_id" %in% names(grid)) {
    grid$tile_id <- sprintf("spot_tile_%05d", seq_len(nrow(grid)))
  }
  grid$tile_id <- wsi_safe_id(as.character(grid$tile_id), "spot_tile")
  if (!"output_file" %in% names(grid)) {
    grid$output_file <- paste0(grid$tile_id, ".png")
  }
  grid$output_file <- basename(as.character(grid$output_file))
  grid$output_file[is.na(grid$output_file) | !nzchar(grid$output_file)] <- paste0(grid$tile_id[is.na(grid$output_file) | !nzchar(grid$output_file)], ".png")
  if (!"row" %in% names(grid)) {
    grid$row <- seq_len(nrow(grid))
  }
  if (!"col" %in% names(grid)) {
    grid$col <- 1L
  }
  grid$row <- suppressWarnings(as.integer(grid$row))
  grid$col <- suppressWarnings(as.integer(grid$col))
  grid$row[is.na(grid$row)] <- seq_len(nrow(grid))[is.na(grid$row)]
  grid$col[is.na(grid$col)] <- 1L
  wsi_tile_preview(grid)
}

wsi_spatial_tile_index <- function(manifest) {
  if (!is.data.frame(manifest)) {
    manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)
  }
  n <- nrow(manifest)
  file <- if ("file" %in% names(manifest)) as.character(manifest$file) else rep(NA_character_, n)
  output_file <- if ("output_file" %in% names(manifest)) as.character(manifest$output_file) else rep(NA_character_, n)
  image_file <- basename(ifelse(!is.na(file) & nzchar(file), file, output_file))
  spot_id <- if ("spot_id" %in% names(manifest)) {
    as.character(manifest$spot_id)
  } else if ("tile_id" %in% names(manifest)) {
    as.character(manifest$tile_id)
  } else {
    sprintf("spot_%05d", seq_len(n))
  }
  spot_id[is.na(spot_id) | !nzchar(spot_id)] <- sprintf("spot_%05d", which(is.na(spot_id) | !nzchar(spot_id)))
  image_file[is.na(image_file) | !nzchar(image_file)] <- paste0(wsi_safe_id(spot_id[is.na(image_file) | !nzchar(image_file)], "spot"), ".png")
  out <- data.frame(
    spot_id = spot_id,
    image_file = image_file,
    image_path = file,
    tile_id = if ("tile_id" %in% names(manifest)) as.character(manifest$tile_id) else spot_id,
    stringsAsFactors = FALSE
  )
  optional <- c("spot_label", "project_image", "project_section", "roi_id", "x", "y", "width", "height", "level")
  for (column in intersect(optional, names(manifest))) {
    out[[column]] <- manifest[[column]]
  }
  out
}

wsi_write_spatial_tile_index_file <- function(manifest, file, overwrite = FALSE) {
  if (is.null(file)) {
    return(invisible(NULL))
  }
  index <- wsi_spatial_tile_index(manifest)
  wsi_write_tile_manifest_file(index, file, overwrite = overwrite)
  invisible(index)
}

wsi_spatial_tile_export_response <- function(slide, payload, state = NULL) {
  wsi_check_slide(slide)
  if (!is.list(payload)) {
    wsi_abort("Spatial tile export payload must be a JSON object.")
  }
  unknown <- setdiff(names(payload), c(
    "output_dir", "format", "overwrite", "manifest_file", "spot_index_file", "tiles",
    "source_name", "project", "selected_roi", "tile_size", "units",
    "selection_scope", "selected_ids", "review_selected_ids", "context_id",
    "source_signature", "tissue_id", "frame_id", "image_sha256"
  ))
  if (length(unknown)) {
    wsi_abort(sprintf(
      "Unsupported spatial tile export field%s: %s.",
      if (length(unknown) == 1L) "" else "s",
      paste(unknown, collapse = ", ")
    ))
  }
  output_dir <- payload$output_dir %||% NULL
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(trimws(output_dir))) {
    wsi_abort("Provide a single non-empty `output_dir` for spatial tile export.")
  }
  output_dir <- path.expand(trimws(output_dir))
  format <- as.character(payload$format %||% "png")
  if (identical(format, "jpg")) {
    format <- "jpeg"
  }
  format <- match.arg(format, c("png", "jpeg", "tiff"))
  overwrite <- isTRUE(payload$overwrite)
  grid <- wsi_spatial_tile_payload_grid(payload$tiles %||% NULL)
  scope <- match.arg(payload$selection_scope %||% "all", c("all", "selected"))
  ids <- payload$review_selected_ids %||% payload$selected_ids
  if (identical(scope, "selected") || !is.null(ids)) {
    context <- if (inherits(state, "wsi_viewer_state")) state$review_context else NULL
    if (!is.null(payload$review_selected_ids) || !is.null(payload$source_signature)) {
      if (is.null(context)) wsi_abort("No canonical review context is attached to this tile-export session.")
      grid <- wsi_spatial_tile_review_filter(grid, context, ids,
        tissue_id = payload$tissue_id, frame_id = payload$frame_id,
        source_signature = payload$source_signature, context_id = payload$context_id,
        image_sha256 = payload$image_sha256, slide = slide)
    } else {
      if (!"spot_id" %in% names(grid)) wsi_abort("Selected tile export requires explicit spot IDs.")
      ids <- wsi_review_ids(ids, "selected spot IDs")
      if (!length(ids)) wsi_abort("No observations are selected; no tiles were exported.")
      if (any(!ids %in% grid$spot_id)) wsi_abort("Selected spot IDs do not match the tile preview.")
      grid <- grid[grid$spot_id %in% ids, , drop = FALSE]
    }
  }
  if (!is.null(payload$selected_roi)) {
    roi <- wsi_viewer_coerce_rois(payload$selected_roi)
    if (!nrow(roi)) wsi_abort("The selected tile-export annotation has no polygon geometry.")
    rectangles <- data.frame(x0 = grid$x, y0 = grid$y,
      x1 = grid$x + grid$width * grid$downsample,
      y1 = grid$y + grid$height * grid$downsample)
    keep <- rep(FALSE, nrow(grid))
    for (i in seq_len(nrow(roi))) keep <- keep | wsi_rectangular_footprints_in_roi(rectangles, roi, i)
    grid <- grid[keep, , drop = FALSE]
  }
  if (!nrow(grid)) wsi_abort("No complete selected tile footprints remain inside the annotation; no tiles were exported.")
  ext <- wsi_format_extension(format)
  grid$output_file <- paste0(tools::file_path_sans_ext(basename(grid$output_file)), ".", ext)
  manifest <- wsi_export_tiles(slide, grid, output_dir = output_dir, format = format, overwrite = overwrite)
  manifest_file <- payload$manifest_file %||% file.path(output_dir, "spatial_tiles_manifest.csv")
  wsi_write_tile_manifest_file(manifest, manifest_file, overwrite = overwrite)
  spot_index_file <- payload$spot_index_file %||% file.path(output_dir, "spot_tile_index.csv")
  wsi_write_spatial_tile_index_file(manifest, spot_index_file, overwrite = overwrite)

  if (inherits(state, "wsi_viewer_state")) {
    state$tile_preview <- wsi_tile_preview(grid)
    wsi_viewer_state_record_event(
      state,
      "tiles_extracted",
      list(
        tile_count = nrow(manifest),
        output_dir = output_dir,
        manifest_file = manifest_file,
        spot_index_file = spot_index_file,
        format = format,
        source_name = payload$source_name %||% NA_character_
      )
    )
  }
  list(
    ok = TRUE,
    tile_count = nrow(manifest),
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    manifest_file = normalizePath(manifest_file, winslash = "/", mustWork = FALSE),
    spot_index_file = normalizePath(spot_index_file, winslash = "/", mustWork = FALSE),
    format = format
  )
}

#' Test complete rectangular footprints against a polygon annotation
#' @param rectangles Data frame with native-pixel edge columns x0, y0, x1, y1.
#' @param rois Polygon or multipolygon wsi_roi annotations, with holes retained.
#' @param index One annotation row.
#' @return Logical containment for each rectangle. Touching the boundary is
#'   allowed within a relative 1e-9 numerical tolerance; entering a hole or a
#'   concave boundary is excluded. This is not an area-coverage estimate.
#' @export
wsi_rectangular_footprints_in_roi <- function(rectangles, rois, index = 1L) {
  if (!is.data.frame(rectangles) || !all(c("x0", "y0", "x1", "y1") %in% names(rectangles)))
    wsi_abort("Rectangular footprints require native edge columns x0, y0, x1 and y1.")
  values <- as.matrix(rectangles[, c("x0", "y0", "x1", "y1"), drop = FALSE])
  if (!is.numeric(values) || any(!is.finite(values)) ||
      any(values[, "x1"] <= values[, "x0"]) || any(values[, "y1"] <= values[, "y0"]))
    wsi_abort("Rectangular footprints must have finite edges and positive area.")
  if (!inherits(rois, "wsi_roi") || length(index) != 1L || !is.numeric(index) ||
      !is.finite(index) || index != as.integer(index) || index < 1L || index > nrow(rois))
    wsi_abort("Choose one valid polygon annotation row for footprint containment.")
  type <- tolower(rois$geometry_type[[index]])
  polygons <- if (type == "polygon") list(rois$coordinates[[index]]) else if (type == "multipolygon") rois$coordinates[[index]] else list()
  if (!length(polygons)) wsi_abort("Footprint containment requires polygon or multipolygon annotations.")
  if (!nrow(rectangles)) return(logical())
  epsilon <- pmin(values[, "x1"] - values[, "x0"], values[, "y1"] - values[, "y0"]) * 1e-9
  left <- values[, "x0"] + epsilon; right <- values[, "x1"] - epsilon
  top <- values[, "y0"] + epsilon; bottom <- values[, "y1"] - epsilon
  inside <- wsi_points_in_roi(rois, index, (left + right) / 2, (top + bottom) / 2)
  for (corner in list(c(left, top), c(left, bottom), c(right, top), c(right, bottom))) {
    n <- nrow(rectangles)
    inside <- inside & wsi_points_in_roi(rois, index, corner[seq_len(n)], corner[n + seq_len(n)])
  }
  # Slab intersection catches boundaries wholly enclosed by the rectangle,
  # including small holes and notches which miss all four corners and centre.
  for (polygon in polygons) for (ring in polygon) {
    vertices <- wsi_ring_matrix(ring)
    if (nrow(vertices) < 3L) next
    if (!identical(unname(vertices[1L, ]), unname(vertices[nrow(vertices), ]))) vertices <- rbind(vertices, vertices[1L, ])
    for (j in seq_len(nrow(vertices) - 1L)) {
      point <- vertices[j, ]; delta <- vertices[j + 1L, ] - point
      interval <- function(value, change, lower, upper) {
        if (abs(change) < .Machine$double.eps) {
          good <- value >= lower & value <= upper
          return(list(lo = ifelse(good, -Inf, Inf), hi = ifelse(good, Inf, -Inf)))
        }
        a <- (lower - value) / change; b <- (upper - value) / change
        list(lo = pmin(a, b), hi = pmax(a, b))
      }
      tx <- interval(point[[1L]], delta[[1L]], left, right)
      ty <- interval(point[[2L]], delta[[2L]], top, bottom)
      crosses <- pmax(0, tx$lo, ty$lo) <= pmin(1, tx$hi, ty$hi)
      inside[crosses] <- FALSE
    }
  }
  inside
}

wsi_spatial_tile_review_filter <- function(grid, context, selected_ids, tissue_id,
                                           frame_id = NULL, source_signature = NULL,
                                           context_id = NULL, image_sha256 = NULL,
                                           slide = NULL) {
  context <- wsi_validate_review_context(context)
  if (!identical(source_signature, context$source_signature) ||
      (!is.null(context_id) && !identical(context_id, context$context_id)))
    wsi_abort("The tile preview has a stale or different review source signature.")
  tissue_id <- wsi_review_text(tissue_id, "tile-export tissue_id")
  index <- match(tissue_id, vapply(context$images, `[[`, character(1), "tissue_id"))
  if (is.na(index)) wsi_abort("Tile export references an unknown tissue.")
  image <- context$images[[index]]
  if ((!is.null(frame_id) && !identical(frame_id, image$frame_id)) ||
      (!is.null(image_sha256) && !identical(image_sha256, image$sha256)))
    wsi_abort("Tile export image hash/frame differs from the review context.")
  if (!is.null(slide)) {
    wsi_check_slide(slide)
    path <- slide$path
    if (!is.character(path) || !file.exists(path)) wsi_abort("The live tile-export source image cannot be verified.")
    con <- file(path, "rb")
    actual <- tryCatch(paste0(openssl::sha256(con)), finally = close(con))
    if (!identical(actual, image$sha256)) wsi_abort("The live tile-export source image hash does not match the selected tissue.")
  }
  ids <- wsi_review_select(context, ids = selected_ids)
  if (!length(ids)) wsi_abort("No observations are selected; no tiles were exported.")
  observations <- Filter(function(observation) observation$id %in% ids && identical(observation$tissue_id, tissue_id), context$observations)
  if (!length(observations)) wsi_abort("The selection contains no observations on the active tile-export tissue.")
  # A spot-centred legacy preview must identify its selected source observation;
  # free-standing work tiles use wsi_review_tile_plan() instead.
  key <- if ("observation_id" %in% names(grid)) "observation_id" else if ("spot_id" %in% names(grid)) "spot_id" else NULL
  if (is.null(key)) wsi_abort("Tile preview needs canonical observation_id or unambiguous selected spot_id values.")
  candidates <- if (identical(key, "observation_id")) vapply(observations, `[[`, character(1), "id") else vapply(observations, `[[`, character(1), "original_id")
  if (anyDuplicated(candidates)) wsi_abort("Repeated local IDs need canonical observation_id values before tile export.")
  keep <- as.character(grid[[key]]) %in% candidates
  grid <- grid[keep, , drop = FALSE]
  if (!nrow(grid)) wsi_abort("No tile preview rows match the explicit selected observations.")
  positions <- match(as.character(grid[[key]]), candidates)
  if (anyDuplicated(as.character(grid[[key]])))
    wsi_abort("Selected spot-centred preview contains duplicate observation IDs.")
  downsample <- grid$downsample %||% rep(1, nrow(grid))
  if (any(!is.finite(downsample)) || any(downsample <= 0))
    wsi_abort("Selected tile preview downsample must be finite and positive.")
  if (any((grid$level %||% rep(0L, nrow(grid))) != 0L) || any(downsample != 1))
    wsi_abort("Canonical selected tile export currently requires native level-0 pixels.")
  for (i in seq_len(nrow(grid))) {
    observation <- observations[[positions[[i]]]]
    if (abs(grid$x[[i]] + grid$width[[i]] * downsample[[i]] / 2 - observation$x) > 0.51 ||
        abs(grid$y[[i]] + grid$height[[i]] * downsample[[i]] / 2 - observation$y) > 0.51)
      wsi_abort("Tile preview coordinates no longer match the selected observation centres.")
    if (grid$x[[i]] < 0 || grid$y[[i]] < 0 ||
        grid$x[[i]] + grid$width[[i]] * downsample[[i]] > image$width ||
        grid$y[[i]] + grid$height[[i]] * downsample[[i]] > image$height)
      wsi_abort("Selected tile footprint leaves its declared image frame.")
  }
  grid$observation_id <- vapply(observations[positions], `[[`, character(1), "id")
  grid$tissue_id <- image$tissue_id; grid$frame_id <- image$frame_id; grid$image_sha256 <- image$sha256
  grid
}

#' Plan deduplicated image-frame tiles from an explicit review selection
#' @param context A canonical review context.
#' @param selected_ids Explicit canonical selected observation IDs.
#' @param tile_size Positive tile side length; pixels must be integer values.
#' @param units Pixels or microns. Microns require image calibration.
#' @param bounds Drop incomplete edge tiles, trim them, or raise an error.
#' @param context_px Optional non-negative context margin in native pixels.
#' @param max_tiles Maximum deduplicated work-tile count.
#' @return A JSON-compatible plan; this never reads pixels or submits work.
#' @export
wsi_review_tile_plan <- function(context, selected_ids, tile_size = 112,
                                 units = c("px", "um"), bounds = c("drop", "trim", "error"),
                                 context_px = 0, max_tiles = 10000L) {
  context <- wsi_validate_review_context(context)
  units <- match.arg(units); bounds <- match.arg(bounds)
  tile_size <- wsi_review_number(tile_size, "tile_size", TRUE, identical(units, "px"))
  context_px <- wsi_review_number(context_px, "context_px", integer = TRUE)
  if (context_px < 0) wsi_abort("Tile context margin must be non-negative.")
  max_tiles <- wsi_review_number(max_tiles, "max_tiles", TRUE, TRUE)
  selected_ids <- sort(wsi_review_select(context, ids = selected_ids))
  observations <- Filter(function(observation) observation$id %in% selected_ids, context$observations)
  tile_map <- new.env(parent = emptyenv())
  work_count <- 0L
  excluded <- character()
  for (observation in observations) {
    image <- context$images[[match(observation$tissue_id, vapply(context$images, `[[`, character(1), "tissue_id"))]]
    if (identical(units, "um")) {
      if (is.null(image$mpp_x) || is.null(image$mpp_y)) wsi_abort("Micron tile planning requires positive calibration for each selected image.")
      width <- round(tile_size / image$mpp_x); height <- round(tile_size / image$mpp_y)
    } else width <- height <- tile_size
    if (width < 1 || height < 1) wsi_abort("Requested physical tile size is smaller than one image pixel.")
    column <- floor(observation$x / width); row <- floor(observation$y / height)
    x <- column * width; y <- row * height
    incomplete <- x + width > image$width || y + height > image$height
    if (incomplete && identical(bounds, "error")) wsi_abort("A selected observation belongs to an incomplete edge tile.")
    if (incomplete && identical(bounds, "drop")) {
      excluded <- c(excluded, observation$id)
      next
    }
    work_width <- min(width, image$width - x); work_height <- min(height, image$height - y)
    identity <- list(tissue_id = image$tissue_id, frame_id = image$frame_id, image_sha256 = image$sha256,
      x = x, y = y, width = work_width, height = work_height,
      nominal_width_px = width, nominal_height_px = height, context_px = context_px)
    tile_id <- paste0("review_tile_", substr(wsi_review_digest(identity), 1L, 24L))
    if (exists(tile_id, tile_map, inherits = FALSE)) {
      tile <- get(tile_id, tile_map, inherits = FALSE)
      tile$observation_ids <- as.list(sort(c(unlist(tile$observation_ids), observation$id)))
    } else {
      if (work_count >= max_tiles) wsi_abort("Review tile plan exceeds max_tiles; no partial plan was returned.")
      work_count <- work_count + 1L
      tile <- c(list(tile_id = tile_id), identity,
        list(level = 0L, grid_col = column, grid_row = row,
          read_x = max(0, x - context_px), read_y = max(0, y - context_px),
          read_width = min(image$width, x + work_width + context_px) - max(0, x - context_px),
          read_height = min(image$height, y + work_height + context_px) - max(0, y - context_px),
          observation_ids = list(observation$id)))
    }
    assign(tile_id, tile, tile_map)
  }
  tiles <- as.list(tile_map, all.names = TRUE)
  if (length(tiles)) tiles <- unname(tiles[order(vapply(tiles, `[[`, character(1), "tissue_id"),
    vapply(tiles, `[[`, numeric(1), "grid_row"), vapply(tiles, `[[`, numeric(1), "grid_col"))])
  list(schema = "wsitools-review-tile-plan", schema_version = 1L,
    context_id = context$context_id, source_signature = context$source_signature,
    selected_ids = as.list(selected_ids), row_count = length(selected_ids), work_count = length(tiles),
    included_observation_count = length(selected_ids) - length(excluded),
    membership_rule = "observation_centres", bounds_policy = bounds,
    tile_size = tile_size, units = units, context_px = context_px,
    images = Filter(function(image) image$tissue_id %in% vapply(observations, `[[`, character(1), "tissue_id"), context$images),
    tiles = tiles, excluded_ids = as.list(sort(excluded)),
    interpretation = "Centres select work tiles; this is not spot footprint coverage or biological ground truth. No image processing, inference or job submission was performed.")
}
