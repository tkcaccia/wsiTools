\name{wsi_experiment_registered_tissue_viewport}
\alias{wsi_experiment_registered_tissue_viewport}
\title{Query registered tissue points in a cell-image viewport}
\usage{
wsi_experiment_registered_tissue_viewport(experiment, tissue_id, asset_id, bbox, limit = 2000L)
}
\arguments{
\item{experiment}{A validated CellPhenotyper experiment or its manifest path.}
\item{tissue_id}{The tissue identifier in the experiment.}
\item{asset_id}{The registered tissue asset identifier.}
\item{bbox}{Named native cell-image bounds: \code{xmin}, \code{ymin}, \code{xmax}, and \code{ymax}.}
\item{limit}{Maximum number of points to return.}
}
\description{Reads a bounded subset of registered 0.5-um KODAMA tissue points, mapped into the full 0.25-um cell-image frame. It does not load a full-slide feature matrix.}
