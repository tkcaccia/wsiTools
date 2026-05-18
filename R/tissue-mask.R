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

#' Estimate a simple tissue mask
#'
#' Creates a low-resolution thumbnail and classifies tissue with simple color
#' thresholds. This first version is intentionally conservative; advanced
#' tissue segmentation is a future enhancement.
#'
#' @param slide A `wsi_slide` object.
#' @param level Reserved for future explicit level control.
#' @param thumbnail_width Width of the thumbnail used for masking.
#' @param method Masking method.
#' @param saturation_threshold Minimum HSV saturation for tissue.
#' @param brightness_threshold Maximum HSV value for tissue.
#' @param min_area Reserved for future connected-component filtering.
#'
#' @return A `wsi_tissue_mask` object.
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
  rgb <- thumb[, , 1:3, drop = FALSE]
  dims <- dim(rgb)
  hsv <- grDevices::rgb2hsv(
    r = as.vector(rgb[, , 1L]),
    g = as.vector(rgb[, , 2L]),
    b = as.vector(rgb[, , 3L])
  )
  saturation <- matrix(hsv["s", ], nrow = dims[[1L]], ncol = dims[[2L]])
  value <- matrix(hsv["v", ], nrow = dims[[1L]], ncol = dims[[2L]])

  if (method == "simple") {
    mask <- saturation >= saturation_threshold & value <= brightness_threshold
  } else {
    gray <- value
    threshold <- wsi_otsu_threshold(as.vector(gray))
    mask <- gray <= threshold & saturation >= saturation_threshold
  }

  structure(
    list(
      mask = mask,
      level = level,
      thumbnail_width = dims[[2L]],
      thumbnail_height = dims[[1L]],
      scale_x = slide$dimensions[["width"]] / dims[[2L]],
      scale_y = slide$dimensions[["height"]] / dims[[1L]],
      method = method,
      parameters = list(
        saturation_threshold = saturation_threshold,
        brightness_threshold = brightness_threshold,
        min_area = min_area
      )
    ),
    class = "wsi_tissue_mask"
  )
}
