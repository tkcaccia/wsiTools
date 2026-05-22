\name{wsi_focus_heatmap}
\alias{wsi_focus_heatmap}
\title{Build a tiled focus heatmap for a slide}
\usage{
wsi_focus_heatmap(
  slide,
  grid = NULL,
  tile_size = 512,
  overlap = 0,
  level = 0,
  region = NULL,
  include_partial = FALSE,
  threshold = 0.001,
  tissue_mask = NULL,
  min_tissue_fraction = 0.05
)
}
\arguments{
\item{slide}{A \code{wsi_slide} object.}

\item{grid}{Optional tile grid. If \code{NULL}, one is created with
\code{\link[=wsi_tile_grid]{wsi_tile_grid()}}.}

\item{tile_size, overlap, level, region, include_partial}{Arguments used when
\code{grid = NULL}.}

\item{threshold}{Laplacian-variance threshold below which a tile is flagged
as blurry.}

\item{tissue_mask}{Optional \code{wsi_tissue_mask} or tile-sized mask input used
by \code{\link[=wsi_detect_blur]{wsi_detect_blur()}}. For slide-level heatmaps, pass a
\code{wsi_tissue_mask}; tile-sized tissue masks are sampled from it before
scoring each tile.}

\item{min_tissue_fraction}{Minimum tile tissue fraction required before a
tile is considered evaluable.}
}
\value{
A \code{wsi_focus_heatmap} object with \code{tiles}, \code{heatmap},
\code{blurry_tile_mask}, \code{slide_focus_score}, and
\code{blurry_tile_fraction}.
}
\description{
Reads the requested grid one tile at a time, computes
\code{\link[=wsi_detect_blur]{wsi_detect_blur()}} for each tile, and returns tile-level focus
metrics plus matrix views of the focus score and blurry-tile mask. This never
loads the whole slide into R memory.
}
\examples{
slide <- wsiTools:::wsi_mock_slide(width = 256, height = 256)
focus <- wsi_focus_heatmap(slide, tile_size = 128, threshold = 0.001)
}
