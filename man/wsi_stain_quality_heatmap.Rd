\name{wsi_stain_quality_heatmap}
\alias{wsi_stain_quality_heatmap}
\title{Build a tiled stain-quality heatmap for a slide}
\usage{
wsi_stain_quality_heatmap(
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

\item{tissue_mask}{Optional \code{wsi_tissue_mask} used to evaluate staining
inside tissue pixels only.}

\item{...}{Thresholds passed to \code{\link[=wsi_detect_stain_quality]{wsi_detect_stain_quality()}}.}
}
\value{
A \code{wsi_stain_quality_heatmap} object with tile metrics and matrix
views for staining score, low-stain tiles, and over-stained tiles.
}
\description{
Reads a slide tile grid one region at a time and computes
\code{\link[=wsi_detect_stain_quality]{wsi_detect_stain_quality()}} for each tile. The result is designed for
quality-control maps and tile manifests without loading the whole slide into
R memory.
}
\examples{
slide <- wsiTools:::wsi_mock_slide(width = 256, height = 256)
stain <- wsi_stain_quality_heatmap(slide, tile_size = 128)
}
