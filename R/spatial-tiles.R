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
                                  format = c("png", "jpeg", "tiff"), prefix = NULL) {
  if (!inherits(linked, "wsi_seurat_spatial") && !inherits(linked, "wsi_spatial_object")) {
    wsi_abort("`linked` must be returned by a wsiTools spatial-image linker.")
  }
  units <- match.arg(units)
  bounds <- match.arg(bounds)
  format <- match.arg(format)
  slide <- linked$slide
  wsi_check_slide(slide)
  spots <- linked$spots
  if (!is.data.frame(spots) || !nrow(spots) || !all(c("x", "y") %in% names(spots))) {
    return(wsi_empty_tile_preview())
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
    inside <- wsi_points_in_roi(roi, i, x0, y0) &
      wsi_points_in_roi(roi, i, x1, y0) &
      wsi_points_in_roi(roi, i, x1, y1) &
      wsi_points_in_roi(roi, i, x0, y1)
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

wsi_spatial_tile_export_response <- function(slide, payload, state = NULL) {
  wsi_check_slide(slide)
  if (!is.list(payload)) {
    wsi_abort("Spatial tile export payload must be a JSON object.")
  }
  unknown <- setdiff(names(payload), c(
    "output_dir", "format", "overwrite", "manifest_file", "tiles",
    "source_name", "project", "selected_roi", "tile_size", "units"
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
  ext <- wsi_format_extension(format)
  grid$output_file <- paste0(tools::file_path_sans_ext(basename(grid$output_file)), ".", ext)
  manifest <- wsi_export_tiles(slide, grid, output_dir = output_dir, format = format, overwrite = overwrite)
  manifest_file <- payload$manifest_file %||% file.path(output_dir, "spatial_tiles_manifest.csv")
  wsi_write_tile_manifest_file(manifest, manifest_file, overwrite = overwrite)

  if (inherits(state, "wsi_viewer_state")) {
    state$tile_preview <- wsi_tile_preview(grid)
    wsi_viewer_state_record_event(
      state,
      "tiles_extracted",
      list(
        tile_count = nrow(manifest),
        output_dir = output_dir,
        manifest_file = manifest_file,
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
    format = format
  )
}
