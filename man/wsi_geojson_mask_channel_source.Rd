\name{wsi_geojson_mask_channel_source}
\alias{wsi_geojson_mask_channel_source}
\alias{wsi_add_geojson_mask_overlay}
\title{Create a tiled mask overlay from dense GeoJSON annotations}
\usage{
wsi_geojson_mask_channel_source(
  geojson,
  slide,
  output_dir,
  name = "Cell annotation mask",
  id = "cell_annotation_mask",
  output_html = NULL,
  downsample = 4,
  label_by = c("class", "name", "roi_id", "index", "constant"),
  visible = TRUE,
  opacity = 0.45,
  tile_size = 254,
  rebuild = FALSE,
  overwrite = FALSE,
  display_min_zoom = 5,
  ...
)

wsi_add_geojson_mask_overlay(
  viewer,
  geojson,
  slide,
  output_dir,
  ...,
  service = TRUE
)
}
\arguments{
\item{geojson}{Dense cell annotation GeoJSON file.}

\item{slide}{A \code{wsi_slide}, image path, or slide-like object used to derive
slide dimensions and mask extent.}

\item{output_dir}{Directory where the OME-TIFF mask, legend CSV, and tile
pyramid are written.}

\item{name, id}{Display name and stable layer id for the mask overlay.}

\item{output_html}{Optional viewer HTML path. When supplied, tile URLs are made
relative to that HTML file for portable static viewers.}

\item{downsample}{Full-resolution pixels represented by one mask pixel.}

\item{label_by}{Annotation field used to define mask classes.}

\item{visible, opacity}{Initial overlay visibility and opacity.}

\item{tile_size}{Deep Zoom tile size for the generated mask overlay.}

\item{rebuild}{Rebuild existing mask/tiles.}

\item{overwrite}{Overwrite an existing mask file when rebuilding.}

\item{display_min_zoom}{Minimum viewer zoom ratio before drawing the mask
overlay. Dense cell masks default to \code{5} so opening a whole-slide overview
does not request thousands of mask tiles.}

\item{...}{Additional arguments passed to \code{wsi_geojson_to_mask_tiff()}.}

\item{viewer}{A live \code{wsi_viewer_session}.}

\item{service}{Whether to immediately pump the live viewer service after adding
the channel source.}
}
\value{
A list with \code{source}, \code{mask}, and \code{tiles}. The \code{source}
element is a \code{wsi_channel_source} that can be used in \code{channel_sources}
or added to a live viewer with \code{viewer$add_channel_source()}.
}
\description{
Converts dense cell-level GeoJSON polygons into a coloured pyramidal OME-TIFF
mask, creates Deep Zoom tiles, and returns a transparent mask overlay with a
class legend. This is intended for very large cell annotations where drawing
every polygon as vector GeoJSON would make the viewer slow.
}
\details{
Use vector GeoJSON for selected or editable tissue ROIs. Use this function for
dense cell segmentation or cell-type annotations. The viewer displays the mask
as image tiles and uses the legend to toggle cell types/classes.
}
\examples{
\dontrun{
slide <- wsi_open("sample.svs")
mask <- wsi_geojson_mask_channel_source(
  geojson = "cells.geojson",
  slide = slide,
  output_dir = "mask_overlay",
  label_by = "class"
)
viewer <- wsi_viewer_live(slide, tiled = TRUE, channel_sources = list(mask$source))
viewer$open()
}
}
