\name{wsi_detect_fold_candidates}
\alias{wsi_detect_fold_candidates}
\title{Detect tissue fold candidate regions in a small image}
\usage{
wsi_detect_fold_candidates(
  image,
  tissue_mask = NULL,
  estimate_tissue = FALSE,
  high_od_threshold = 1.2,
  saturation_threshold = 0.25,
  brightness_threshold = 0.45,
  edge_threshold = 0.05,
  min_edge_fraction = 0.03,
  min_area = 20,
  min_aspect_ratio = 1,
  fold_fraction_threshold = 0.01,
  epsilon = 1/255
)
}
\arguments{
\item{image}{RGB/RGBA array, raster, or magick image.}

\item{tissue_mask}{Optional logical/numeric matrix or \code{wsi_tissue_mask} with
the same height and width as \code{image}.}

\item{estimate_tissue}{If \code{TRUE} and \code{tissue_mask} is \code{NULL}, estimate a
simple tissue mask from \code{image} using \code{\link[=wsi_detect_tissue]{wsi_detect_tissue()}}.}

\item{high_od_threshold}{Minimum RGB optical-density sum for candidate
pixels.}

\item{saturation_threshold}{Minimum HSV saturation for candidate pixels.}

\item{brightness_threshold}{Maximum HSV brightness/value for candidate
pixels.}

\item{edge_threshold}{Local grayscale edge threshold used inside connected
components.}

\item{min_edge_fraction}{Minimum fraction of edge-rich pixels required inside
a connected component.}

\item{min_area}{Minimum connected component size in pixels.}

\item{min_aspect_ratio}{Minimum component bounding-box aspect ratio. Use
values greater than 1 to prefer elongated fold-like regions.}

\item{fold_fraction_threshold}{Tissue fold-candidate fraction threshold used
for the summary flag.}

\item{epsilon}{Lower RGB bound before optical-density transformation.}
}
\value{
A \code{wsi_fold_candidate_mask} object with the filtered \code{mask}, raw
candidate mask, tissue-aware fractions, and component summaries.
}
\description{
Detects tissue fold candidates using a conservative rule-based combination
of high optical density, high saturation, low brightness, and local edge
content. The result should be treated as a candidate mask for QC review, not
a definitive tissue-fold classifier; dense nuclei, necrosis, blood, or dark
tumour regions can sometimes satisfy similar rules.
}
\examples{
img <- array(0.8, dim = c(32, 32, 3))
img[10:22, 14:18, 1] <- 0.18
img[10:22, 14:18, 2] <- 0.03
img[10:22, 14:18, 3] <- 0.10
wsi_detect_fold_candidates(img, estimate_tissue = FALSE, min_area = 1)
}
