\name{wsi_seurat_coordinates}
\alias{wsi_seurat_coordinates}
\title{Extract Seurat spatial coordinates used by wsiTools}
\usage{
wsi_seurat_coordinates(
  seurat,
  image = NULL,
  image_name = NULL,
  spatial_dir = NULL,
  scalefactors_json = NULL,
  tissue_positions = NULL,
  coordinate_scale = c("auto", "none", "fullres", "hires", "lowres", "seurat_image",
    "custom"),
  scale_x = NULL,
  scale_y = NULL,
  coordinate_flip = c("none", "vertical", "horizontal"),
  coordinate_rotation = c(0, 90, 180, 270),
  coordinate_transform = "none",
  plot = FALSE,
  output = NULL
)
}
\arguments{
\item{seurat}{A Seurat or Seurat-like object.}

\item{image}{Optional microscopy image path or \code{wsi_slide}. When supplied,
output \code{x} and \code{y} are level-0 slide coordinates used by the viewer.}

\item{image_name}{Spatial image name inside the Seurat object. By default the
first spatial image is used when available.}

\item{spatial_dir}{Optional 10x Genomics \code{spatial/} directory.}

\item{scalefactors_json}{Optional 10x Genomics scalefactors JSON file.}

\item{tissue_positions}{Optional 10x Genomics tissue positions CSV file.}

\item{coordinate_scale, scale_x, scale_y, coordinate_flip, coordinate_rotation, coordinate_transform}{Coordinate
mapping arguments; see \code{\link[=wsi_link_seurat_image]{wsi_link_seurat_image()}}.}

\item{plot}{If \code{TRUE}, draw a quick base R coordinate plot.}

\item{output}{Optional CSV file path where the coordinates are written.}
}
\value{
A data frame with at least \code{id}, \code{barcode}, \code{x}, and \code{y}.
Attributes describe the coordinate source, coordinate space, and mapping when
known.
}
\description{
\code{wsi_seurat_coordinates()} returns the spot or cell coordinates stored in a
Seurat object. If \code{image} is supplied, the function applies the same
scaling, flipping, rotation, and slide-coordinate mapping used by
\code{\link[=wsi_link_seurat_image]{wsi_link_seurat_image()}} and the
interactive viewer.
}
\examples{
\dontrun{
coords <- wsi_seurat_coordinates(seurat_object)
head(coords)

coords_slide <- wsi_seurat_coordinates(
  seurat_object,
  image = "tissue_image.btf",
  plot = TRUE,
  output = "seurat_coordinates.csv"
)
}
}
