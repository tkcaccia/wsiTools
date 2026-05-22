\name{wsi_detect_pen_marks}
\alias{wsi_detect_pen_marks}
\title{Detect pen marks and ink-like artifacts in a small image}
\usage{
wsi_detect_pen_marks(
  image,
  tissue_mask = NULL,
  estimate_tissue = TRUE,
  channel_threshold = 120,
  blue_red_ratio = 1.4,
  blue_green_ratio = 1.2,
  green_red_ratio = 1.3,
  green_blue_ratio = 1.3,
  red_green_ratio = 1.3,
  red_blue_ratio = 1.3,
  black_brightness_threshold = 45,
  black_edge_threshold = 35,
  black_edge_fraction_threshold = 0.05,
  min_area = 5
)
}
\arguments{
\item{image}{RGB/RGBA array, raster, or magick image.}

\item{tissue_mask}{Optional logical/numeric matrix or \code{wsi_tissue_mask} with
the same height and width as \code{image}. When supplied, tissue affected
percentages are calculated against tissue pixels only.}

\item{estimate_tissue}{If \code{TRUE} and \code{tissue_mask} is \code{NULL}, estimate a
simple tissue mask from \code{image} using \code{\link[=wsi_detect_tissue]{wsi_detect_tissue()}}.}

\item{channel_threshold}{Minimum dominant channel intensity for coloured ink.
Values may be supplied in \code{0..1} or \code{0..255} units.}

\item{blue_red_ratio, blue_green_ratio}{Dominance ratios for blue pen pixels.}

\item{green_red_ratio, green_blue_ratio}{Dominance ratios for green marker
pixels.}

\item{red_green_ratio, red_blue_ratio}{Dominance ratios for red pen pixels.}

\item{black_brightness_threshold}{Maximum grayscale brightness for black ink
candidates, in \code{0..1} or \code{0..255} units.}

\item{black_edge_threshold}{Minimum local grayscale edge magnitude used to
decide whether a dark component is ink-like.}

\item{black_edge_fraction_threshold}{Minimum fraction of edge-rich pixels
inside a dark component before the whole dark component is classified as
black ink.}

\item{min_area}{Minimum connected pen/ink component size in pixels.}
}
\value{
A \code{wsi_pen_mark_mask} object containing the combined \code{mask}, colour
masks, total and tissue-aware affected percentages, and connected-component
bounding boxes.
}
\description{
Detects strongly colour-dominant blue, green, and red pen/marker pixels,
plus black ink-like dark components with visible edge content. The function
operates on the supplied image only, so it is suitable for slide thumbnails,
viewport captures, regions, or tiles without loading a whole-slide image into
memory.
}
\examples{
img <- array(1, dim = c(32, 32, 3))
img[, 12:15, 1] <- 0.05
img[, 12:15, 2] <- 0.10
img[, 12:15, 3] <- 0.90
wsi_detect_pen_marks(img, estimate_tissue = FALSE, min_area = 1)
}
