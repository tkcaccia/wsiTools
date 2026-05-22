\name{wsi_detect_bubble_candidates}
\alias{wsi_detect_bubble_candidates}
\title{Detect air bubble candidate regions in a small image}
\usage{
wsi_detect_bubble_candidates(
  image,
  tissue_mask = NULL,
  estimate_tissue = FALSE,
  brightness_threshold = 0.85,
  saturation_threshold = 0.18,
  edge_threshold = 0.05,
  min_edge_fraction = 0.03,
  min_ring_contrast = 0.05,
  min_roundness = 0.55,
  min_circularity = 0.25,
  min_fill_fraction = 0.35,
  max_fill_fraction = 0.98,
  min_area = 20,
  bubble_fraction_threshold = 0.01,
  ring_width = 2,
  tissue_expansion = 2
)
}
\arguments{
\item{image}{RGB/RGBA array, raster, or magick image.}

\item{tissue_mask}{Optional logical/numeric matrix or \code{wsi_tissue_mask} with
the same height and width as \code{image}. If supplied, candidates are searched
inside this mask after optional dilation by \code{tissue_expansion}.}

\item{estimate_tissue}{If \code{TRUE} and \code{tissue_mask} is \code{NULL}, estimate a
simple tissue mask from \code{image} using \code{\link[=wsi_detect_tissue]{wsi_detect_tissue()}}. Because bubble
centres can look like background, externally supplied tissue masks are more
reliable when available.}

\item{brightness_threshold}{Minimum HSV brightness/value for bright bubble
centre pixels.}

\item{saturation_threshold}{Maximum HSV saturation for bright bubble centre
pixels.}

\item{edge_threshold}{Local grayscale edge threshold used to score bubble
rims.}

\item{min_edge_fraction}{Minimum fraction of edge-rich pixels required in
the component boundary/ring zone.}

\item{min_ring_contrast}{Minimum brightness difference between the candidate
centre and its surrounding ring.}

\item{min_roundness}{Minimum bounding-box roundness, computed as
\code{min(width, height) / max(width, height)}.}

\item{min_circularity}{Minimum approximate circularity from component area
and exposed pixel perimeter.}

\item{min_fill_fraction, max_fill_fraction}{Allowed component area fraction
inside the component bounding box. This helps reject long streaks and
rectangular bright regions.}

\item{min_area}{Minimum connected bright-centre component size in pixels.}

\item{bubble_fraction_threshold}{Candidate fraction threshold used for the
summary flag.}

\item{ring_width}{Pixel radius used to inspect the outside rim of each
candidate component.}

\item{tissue_expansion}{Pixel radius used to expand a supplied or estimated
tissue mask before searching for bright bubble centres.}
}
\value{
A \code{wsi_bubble_candidate_mask} object with the filtered centre \code{mask},
raw candidate mask, ring mask, tissue-aware fractions, and component
summaries.
}
\description{
Detects air bubble candidates using bright low-saturation centres plus
sharp edge/ring, roundness, circularity, and connected-component filters.
This is a transparent QC heuristic for candidate regions, not a definitive
bubble classifier; whitespace, coverslip glare, tissue tears, or unstained
holes can produce similar appearances.
}
\examples{
img <- array(0.65, dim = c(48, 48, 3))
yy <- row(img[, , 1])
xx <- col(img[, , 1])
d <- sqrt((xx - 24)^2 + (yy - 24)^2)
for (channel in seq_len(3)) {
  plane <- img[, , channel]
  plane[d <= 7] <- 0.96
  plane[d > 7 & d <= 9] <- 0.35
  img[, , channel] <- plane
}
wsi_detect_bubble_candidates(img, estimate_tissue = FALSE, min_area = 10)
}
