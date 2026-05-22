\name{wsi_detect_stain_quality}
\alias{wsi_detect_stain_quality}
\title{Detect poor or excessive staining in a small image}
\usage{
wsi_detect_stain_quality(
  image,
  tissue_mask = NULL,
  estimate_tissue = FALSE,
  low_saturation_threshold = 0.08,
  high_brightness_threshold = 0.88,
  low_od_threshold = 0.15,
  high_od_threshold = 2.2,
  low_brightness_threshold = 0.18,
  low_fraction_threshold = 0.2,
  over_fraction_threshold = 0.2,
  min_tissue_fraction = 0.05,
  min_area = 5,
  epsilon = 1/255
)
}
\arguments{
\item{image}{RGB/RGBA array, raster, or magick image.}

\item{tissue_mask}{Optional logical/numeric matrix or \code{wsi_tissue_mask} with
the same height and width as \code{image}. When supplied, all metrics and masks
are calculated inside tissue pixels only.}

\item{estimate_tissue}{If \code{TRUE} and \code{tissue_mask} is \code{NULL}, estimate a
simple tissue mask from \code{image} using \code{\link[=wsi_detect_tissue]{wsi_detect_tissue()}}. For very pale
poorly stained images, pass an external tissue mask when possible.}

\item{low_saturation_threshold}{Minimum acceptable HSV saturation.}

\item{high_brightness_threshold}{Brightness above which pixels are considered
weakly stained or washed out.}

\item{low_od_threshold}{Optical-density sum below which pixels are considered
weakly stained.}

\item{high_od_threshold}{Optical-density sum above which pixels are
considered over-stained.}

\item{low_brightness_threshold}{Brightness below which pixels contribute to
the over-stain mask.}

\item{low_fraction_threshold, over_fraction_threshold}{Fraction thresholds
used to flag a region as low-stain or over-stained.}

\item{min_tissue_fraction}{Minimum tissue/evaluable fraction required before
flags are assigned.}

\item{min_area}{Minimum connected component size in pixels for low-stain and
over-stain regions.}

\item{epsilon}{Lower RGB bound before optical-density transformation.}
}
\value{
A \code{wsi_stain_quality} object containing summary metrics, low-stain
and over-stain masks, region bounding boxes, and flags.
}
\description{
Computes basic stain-quality metrics from a thumbnail, tile, or small region.
The detector uses transparent colour statistics: HSV saturation and
brightness, RGB optical density, and rule-based low-stain / over-stain masks.
It is intended for QC screening and tile manifests; robust diagnostic stain
assessment still requires assay- and laboratory-specific validation.
}
\examples{
img <- array(0.75, dim = c(32, 32, 3))
wsi_detect_stain_quality(img, estimate_tissue = FALSE, min_area = 1)
}
