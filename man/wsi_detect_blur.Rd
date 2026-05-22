\name{wsi_detect_blur}
\alias{wsi_detect_blur}
\title{Detect blur or out-of-focus content in a small image}
\usage{
wsi_detect_blur(
  image,
  threshold = 0.001,
  tissue_mask = NULL,
  estimate_tissue = FALSE,
  min_tissue_fraction = 0.05
)
}
\arguments{
\item{image}{RGB/RGBA array, raster, or magick image.}

\item{threshold}{Laplacian-variance threshold below which the image is
flagged as blurry.}

\item{tissue_mask}{Optional logical/numeric matrix or \code{wsi_tissue_mask} with
the same height and width as \code{image}. When supplied, focus metrics are
calculated on tissue pixels only.}

\item{estimate_tissue}{If \code{TRUE} and \code{tissue_mask} is \code{NULL}, estimate a
simple tissue mask from \code{image} using \code{\link[=wsi_detect_tissue]{wsi_detect_tissue()}}.}

\item{min_tissue_fraction}{Minimum fraction of tissue pixels required before
the tile is considered evaluable.}
}
\value{
A one-row data frame with focus scores and blur flags.
}
\description{
Computes a dependency-free focus score from a tile, thumbnail, or small
region. The primary score is the variance of the Laplacian on grayscale
intensities; blurry or out-of-focus images have low high-frequency content
and therefore low Laplacian variance. Additional Sobel/Tenengrad and local
contrast scores are returned for inspection.
}
\examples{
sharp <- array(rep(matrix(rep(c(0, 1), 32 * 32 / 2), nrow = 32), 3),
  dim = c(32, 32, 3)
)
wsi_detect_blur(sharp)
}
