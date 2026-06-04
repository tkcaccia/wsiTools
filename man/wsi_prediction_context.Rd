\name{wsi_prediction_context}
\alias{wsi_prediction_context}
\title{Create live viewer prediction context}
\usage{
wsi_prediction_context(spatial = NULL, cellphenotyper_project = NULL)
}
\arguments{
\item{spatial}{A linked spatial object returned by \code{\link[=wsi_link_seurat_image]{wsi_link_seurat_image()}} or the Giotto/SpatialExperiment linkers.}

\item{cellphenotyper_project}{A CellPhenotyper project returned by \code{\link[=wsi_read_cellphenotyper_project]{wsi_read_cellphenotyper_project()}}.}
}
\value{
A list used internally by the live viewer prediction endpoint.
}
\description{
\code{wsi_prediction_context()} is a small advanced helper used by
\code{\link[=wsi_viewer_live]{wsi_viewer_live()}} to expose R-side feature
matrices to the browser \strong{Prediction} menu without sending raw expression
data to JavaScript. The ordinary Seurat, Giotto, SpatialExperiment, and
CellPhenotyper viewer helper functions set this automatically.
}
