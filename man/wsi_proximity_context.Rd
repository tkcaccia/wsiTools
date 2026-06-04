\name{wsi_proximity_context}
\alias{wsi_proximity_context}
\title{Create live viewer proximity-analysis context}
\usage{
wsi_proximity_context(spatial = NULL, cellphenotyper_project = NULL)
}
\arguments{
\item{spatial}{A linked spatial object returned by \code{\link[=wsi_link_seurat_image]{wsi_link_seurat_image()}} or the Giotto/SpatialExperiment linkers.}

\item{cellphenotyper_project}{A CellPhenotyper project returned by \code{\link[=wsi_read_cellphenotyper_project]{wsi_read_cellphenotyper_project()}}.}
}
\value{
A list used internally by the live viewer proximity endpoint.
}
\description{
\code{wsi_proximity_context()} is an advanced helper used by \code{\link[=wsi_viewer_live]{wsi_viewer_live()}} when a generic live viewer needs access to Seurat, Giotto, SpatialExperiment, or CellPhenotyper coordinates for proximity analysis. High-level spatial and CellPhenotyper viewer helpers set this automatically.
}
