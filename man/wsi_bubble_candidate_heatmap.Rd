\name{wsi_bubble_candidate_heatmap}
\alias{wsi_bubble_candidate_heatmap}
\title{Build a tiled air bubble candidate heatmap for a slide}
\usage{
wsi_bubble_candidate_heatmap(
  slide,
  grid = NULL,
  tile_size = 512,
  overlap = 0,
  level = 0,
  region = NULL,
  include_partial = FALSE,
  tissue_mask = NULL,
  ...
)
}
\arguments{
\item{slide}{A \code{wsi_slide} object.}

\item{grid}{Optional tile grid. If \code{NULL}, one is created with
\code{\link[=wsi_tile_grid]{wsi_tile_grid()}}.}

\item{tile_size, overlap, level, region, include_partial}{Arguments used when
\code{grid = NULL}.}

\item{tissue_mask}{Optional \code{wsi_tissue_mask} used to restrict the bubble
search region around tissue pixels.}

\item{...}{Thresholds passed to \code{\link[=wsi_detect_bubble_candidates]{wsi_detect_bubble_candidates()}}.}
}
\value{
A \code{wsi_bubble_candidate_heatmap} object with tile metrics, a bubble
fraction heatmap, and a candidate tile mask.
}
\description{
Reads a slide tile grid one region at a time and computes
\code{\link[=wsi_detect_bubble_candidates]{wsi_detect_bubble_candidates()}} for each tile. The output is intended for
QC triage and visual review; it should not be interpreted as definitive
bubble segmentation.
}
\examples{
slide <- wsiTools:::wsi_mock_slide(width = 256, height = 256)
bubbles <- wsi_bubble_candidate_heatmap(slide, tile_size = 128)
}
