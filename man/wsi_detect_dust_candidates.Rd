\name{wsi_detect_dust_candidates}
\alias{wsi_detect_dust_candidates}
\title{Detect dust or debris candidate regions in a small image}
\usage{
wsi_detect_dust_candidates(
  image,
  tissue_mask = NULL,
  estimate_tissue = FALSE,
  brightness_threshold = 0.3,
  edge_threshold = 0.05,
  min_edge_fraction = 0.1,
  min_contrast = 0.08,
  min_area = 2,
  max_area = 200,
  max_diameter = 40,
  dust_fraction_threshold = 0.001,
  ring_width = 2
)
}
\arguments{
\item{image}{RGB/RGBA array, raster, or magick image.}

\item{tissue_mask}{Optional logical/numeric matrix or \code{wsi_tissue_mask} with
the same height and width as \code{image}. When supplied, the detector reports
whether each small object lies on tissue, background, or the tissue edge.}

\item{estimate_tissue}{If \code{TRUE} and \code{tissue_mask} is \code{NULL}, estimate a
simple tissue mask from \code{image} using \code{\link[=wsi_detect_tissue]{wsi_detect_tissue()}}.}

\item{brightness_threshold}{Maximum HSV brightness/value for dark candidate
pixels.}

\item{edge_threshold}{Local grayscale edge threshold used around candidate
components.}

\item{min_edge_fraction}{Minimum fraction of edge-rich pixels required in
the component boundary/ring zone.}

\item{min_contrast}{Minimum local contrast, computed as surrounding ring
brightness minus candidate-object brightness.}

\item{min_area, max_area}{Minimum and maximum connected component size in
pixels.}

\item{max_diameter}{Maximum component bounding-box width or height in
pixels.}

\item{dust_fraction_threshold}{Candidate fraction threshold used for the
summary flag.}

\item{ring_width}{Pixel radius used to inspect local background around each
small object.}
}
\value{
A \code{wsi_dust_candidate_mask} object with the filtered \code{mask}, raw
dark-object mask, tissue/background fractions, and per-component summaries.
}
\description{
Detects small, dark, sharply defined dust/debris candidates using
connected-component filtering, local contrast, and edge strength. The
function is intended for QC triage on thumbnails, tiles, or small regions;
it does not diagnose the physical cause of an object and can confuse dust
with dark cells, pigments, pen specks, or small tissue fragments.
}
\examples{
img <- array(0.82, dim = c(32, 32, 3))
img[10:12, 10:12, ] <- 0.04
wsi_detect_dust_candidates(img, estimate_tissue = FALSE, min_area = 1)
}
