wsi_otsu_threshold <- function(values) {
  values <- values[is.finite(values)]
  if (!length(values)) {
    return(0.5)
  }
  hist <- hist(values, breaks = seq(0, 1, length.out = 257), plot = FALSE)
  counts <- hist$counts
  mids <- hist$mids
  total <- sum(counts)
  sum_total <- sum(counts * mids)
  weight_bg <- cumsum(counts)
  weight_fg <- total - weight_bg
  sum_bg <- cumsum(counts * mids)
  mean_bg <- sum_bg / pmax(weight_bg, 1)
  mean_fg <- (sum_total - sum_bg) / pmax(weight_fg, 1)
  between <- weight_bg * weight_fg * (mean_bg - mean_fg)^2
  mids[which.max(between)]
}

wsi_tissue_rgb_metrics <- function(image) {
  arr <- wsi_artifact_array(image)
  rgb <- arr[, , 1:3, drop = FALSE]
  dims <- dim(rgb)
  hsv <- grDevices::rgb2hsv(
    r = as.vector(rgb[, , 1L]),
    g = as.vector(rgb[, , 2L]),
    b = as.vector(rgb[, , 3L])
  )
  list(
    saturation = matrix(hsv["s", ], nrow = dims[[1L]], ncol = dims[[2L]]),
    brightness = matrix(hsv["v", ], nrow = dims[[1L]], ncol = dims[[2L]]),
    width = dims[[2L]],
    height = dims[[1L]]
  )
}

wsi_tissue_scale <- function(scale) {
  if (!is.numeric(scale) || !length(scale) || anyNA(scale) || any(!is.finite(scale)) || any(scale <= 0)) {
    wsi_abort("`scale` must be one or two positive finite numbers.")
  }
  if (length(scale) == 1L) {
    scale <- rep(scale, 2L)
  }
  c(x = as.numeric(scale[[1L]]), y = as.numeric(scale[[2L]]))
}

wsi_tissue_origin <- function(origin) {
  if (!is.numeric(origin) || length(origin) < 2L || anyNA(origin[1:2]) || any(!is.finite(origin[1:2]))) {
    wsi_abort("`origin` must be a numeric x/y coordinate pair.")
  }
  c(x = as.numeric(origin[[1L]]), y = as.numeric(origin[[2L]]))
}

wsi_tissue_component_bboxes <- function(components, scale, origin) {
  if (!length(components)) {
    return(data.frame(
      component_id = integer(),
      thumbnail_col = integer(),
      thumbnail_row = integer(),
      thumbnail_width = integer(),
      thumbnail_height = integer(),
      thumbnail_area = integer(),
      x = numeric(),
      y = numeric(),
      width = numeric(),
      height = numeric(),
      xmin = numeric(),
      ymin = numeric(),
      xmax = numeric(),
      ymax = numeric(),
      area = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(seq_along(components), function(i) {
    component <- components[[i]]
    min_row <- min(component[, "row"])
    max_row <- max(component[, "row"])
    min_col <- min(component[, "col"])
    max_col <- max(component[, "col"])
    x <- origin[["x"]] + (min_col - 1L) * scale[["x"]]
    y <- origin[["y"]] + (min_row - 1L) * scale[["y"]]
    xmax <- origin[["x"]] + max_col * scale[["x"]]
    ymax <- origin[["y"]] + max_row * scale[["y"]]
    data.frame(
      component_id = i,
      thumbnail_col = min_col,
      thumbnail_row = min_row,
      thumbnail_width = max_col - min_col + 1L,
      thumbnail_height = max_row - min_row + 1L,
      thumbnail_area = nrow(component),
      x = x,
      y = y,
      width = xmax - x,
      height = ymax - y,
      xmin = x,
      ymin = y,
      xmax = xmax,
      ymax = ymax,
      area = nrow(component) * scale[["x"]] * scale[["y"]],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

wsi_tissue_union_bbox <- function(component_bboxes) {
  empty <- c(x = NA_real_, y = NA_real_, width = NA_real_, height = NA_real_,
             xmin = NA_real_, ymin = NA_real_, xmax = NA_real_, ymax = NA_real_)
  if (!is.data.frame(component_bboxes) || !nrow(component_bboxes)) {
    return(empty)
  }
  xmin <- min(component_bboxes$xmin, na.rm = TRUE)
  ymin <- min(component_bboxes$ymin, na.rm = TRUE)
  xmax <- max(component_bboxes$xmax, na.rm = TRUE)
  ymax <- max(component_bboxes$ymax, na.rm = TRUE)
  c(x = xmin, y = ymin, width = xmax - xmin, height = ymax - ymin,
    xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
}

wsi_tissue_detection <- function(image, method = c("simple", "otsu"),
                                 saturation_threshold = 0.05,
                                 brightness_threshold = 0.8,
                                 min_area = 1000,
                                 scale = c(1, 1),
                                 origin = c(0, 0),
                                 metadata = list()) {
  method <- match.arg(method)
  saturation_threshold <- wsi_check_scalar_number(saturation_threshold, "saturation_threshold")
  brightness_threshold <- wsi_check_scalar_number(brightness_threshold, "brightness_threshold")
  min_area <- as.integer(wsi_check_scalar_number(min_area, "min_area", allow_zero = TRUE))
  if (saturation_threshold > 1) {
    wsi_abort("`saturation_threshold` must be less than or equal to 1.")
  }
  if (brightness_threshold > 1) {
    wsi_abort("`brightness_threshold` must be less than or equal to 1.")
  }
  scale <- wsi_tissue_scale(scale)
  origin <- wsi_tissue_origin(origin)
  metrics <- wsi_tissue_rgb_metrics(image)

  if (method == "simple") {
    mask <- metrics$saturation >= saturation_threshold & metrics$brightness <= brightness_threshold
  } else {
    threshold <- wsi_otsu_threshold(as.vector(metrics$brightness))
    mask <- metrics$brightness <= threshold & metrics$saturation >= saturation_threshold
    brightness_threshold <- threshold
  }

  components <- wsi_mask_component_list(mask, connectivity = "8", min_area = max(1L, min_area))
  filtered <- matrix(FALSE, nrow = nrow(mask), ncol = ncol(mask))
  for (component in components) {
    filtered[cbind(component[, "row"], component[, "col"])] <- TRUE
  }
  mask <- filtered
  component_bboxes <- wsi_tissue_component_bboxes(components, scale = scale, origin = origin)
  tissue_pixels <- sum(mask, na.rm = TRUE)
  total_pixels <- length(mask)
  tissue_fraction <- tissue_pixels / total_pixels
  background_fraction <- 1 - tissue_fraction
  tissue_area <- tissue_pixels * scale[["x"]] * scale[["y"]]
  background_area <- (total_pixels - tissue_pixels) * scale[["x"]] * scale[["y"]]

  structure(
    c(
      list(
        mask = mask,
        tissue_fraction = tissue_fraction,
        tissue_percentage = tissue_fraction * 100,
        background_fraction = background_fraction,
        background_percentage = background_fraction * 100,
        tissue_pixel_count = as.integer(tissue_pixels),
        background_pixel_count = as.integer(total_pixels - tissue_pixels),
        tissue_area = tissue_area,
        background_area = background_area,
        tissue_bounding_box = wsi_tissue_union_bbox(component_bboxes),
        component_bboxes = component_bboxes,
        scale_x = scale[["x"]],
        scale_y = scale[["y"]],
        origin = origin,
        method = method,
        parameters = list(
          saturation_threshold = saturation_threshold,
          brightness_threshold = brightness_threshold,
          min_area = min_area
        )
      ),
      metadata
    ),
    class = "wsi_tissue_mask"
  )
}

#' Detect tissue/background in a low-resolution image
#'
#' Classifies pixels as tissue or background using HSV saturation and
#' brightness thresholds. The function is intended for thumbnails or small
#' regions, not for loading an entire whole-slide image into R memory.
#'
#' @param image RGB/RGBA array, raster, or magick image.
#' @param method Masking method. `"simple"` applies the supplied thresholds;
#'   `"otsu"` uses Otsu thresholding on brightness and still requires minimum
#'   saturation.
#' @param saturation_threshold Minimum HSV saturation for tissue.
#' @param brightness_threshold Maximum HSV value/brightness for tissue.
#' @param min_area Minimum connected tissue component size in thumbnail pixels.
#'   Smaller components are removed before summaries are computed.
#' @param scale One or two positive numbers mapping mask pixels to source image
#'   coordinates. Use slide-pixel scale for thumbnail-derived masks.
#' @param origin Source image x/y coordinate of the mask origin.
#'
#' @return A `wsi_tissue_mask` object containing the logical `mask`,
#'   `tissue_fraction`, `tissue_percentage`, `tissue_area`,
#'   `tissue_bounding_box`, and per-component `component_bboxes`.
#' @export
#' @examples
#' img <- array(1, dim = c(32, 32, 3))
#' img[10:20, 8:24, 1] <- 0.65
#' img[10:20, 8:24, 2] <- 0.25
#' img[10:20, 8:24, 3] <- 0.55
#' wsi_detect_tissue(img, min_area = 1)
wsi_detect_tissue <- function(image, method = c("simple", "otsu"),
                              saturation_threshold = 0.05,
                              brightness_threshold = 0.8,
                              min_area = 1000,
                              scale = c(1, 1),
                              origin = c(0, 0)) {
  wsi_tissue_detection(
    image = image,
    method = method,
    saturation_threshold = saturation_threshold,
    brightness_threshold = brightness_threshold,
    min_area = min_area,
    scale = scale,
    origin = origin
  )
}

#' Estimate a simple tissue mask
#'
#' Creates a low-resolution thumbnail and classifies tissue/background with
#' simple color thresholds. The slide is not loaded into memory; only the
#' requested thumbnail is read.
#'
#' @param slide A `wsi_slide` object.
#' @param level Reserved for future explicit level control.
#' @param thumbnail_width Width of the thumbnail used for masking.
#' @param method Masking method.
#' @param saturation_threshold Minimum HSV saturation for tissue.
#' @param brightness_threshold Maximum HSV value for tissue.
#' @param min_area Minimum connected tissue component size in thumbnail pixels.
#'   Smaller components are removed before summaries are computed.
#'
#' @return A `wsi_tissue_mask` object containing the logical `mask`,
#'   `tissue_fraction`, `tissue_percentage`, `tissue_area`,
#'   `tissue_bounding_box`, and per-component `component_bboxes`.
#' @export
wsi_tissue_mask <- function(slide, level = "auto", thumbnail_width = 2048,
                            method = c("simple", "otsu"),
                            saturation_threshold = 0.05,
                            brightness_threshold = 0.8,
                            min_area = 1000) {
  wsi_check_slide(slide)
  method <- match.arg(method)
  thumbnail_width <- as.integer(wsi_check_scalar_number(thumbnail_width, "thumbnail_width", allow_zero = FALSE))

  thumb <- wsi_thumbnail(slide, width = thumbnail_width, format = "array", level = level)
  dims <- dim(thumb)
  scale <- c(
    slide$dimensions[["width"]] / dims[[2L]],
    slide$dimensions[["height"]] / dims[[1L]]
  )
  wsi_tissue_detection(
    image = thumb,
    method = method,
    saturation_threshold = saturation_threshold,
    brightness_threshold = brightness_threshold,
    min_area = min_area,
    scale = scale,
    origin = c(0, 0),
    metadata = list(
      level = level,
      thumbnail_width = dims[[2L]],
      thumbnail_height = dims[[1L]],
      slide_width = slide$dimensions[["width"]],
      slide_height = slide$dimensions[["height"]]
    )
  )
}
