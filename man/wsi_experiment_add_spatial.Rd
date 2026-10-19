\name{wsi_experiment_add_spatial}
\alias{wsi_experiment_add_spatial}
\title{Add a spatial transcriptomics object to a CellPhenotyper experiment}
\usage{
wsi_experiment_add_spatial(experiment, object, tissue_id,
  dataset_id = "spatial", coordinate_mapping, cluster_columns = NULL,
  reduction_names = NULL, image_name = NULL, trusted = FALSE,
  max_observations = 10000L)
}
\arguments{
\item{experiment}{A CellPhenotyper experiment or local manifest path.}
\item{object}{An accessible trusted local RDS path. Loaded objects are not accepted.}
\item{tissue_id}{Exact existing tissue identity.}
\item{dataset_id}{New stable dataset ID, unique within its tissue. Use letters, digits, underscore, dot, colon or hyphen, beginning with a letter or digit.}
\item{coordinate_mapping}{Confirmed same-section mapping with exact image SHA-256, frame, native-pixel coordinates or an explicit source-pixel affine. See \code{wsi_review_attach_spatial()}.}
\item{cluster_columns}{Selected categorical metadata columns.}
\item{reduction_names}{Selected named reductions. NULL includes none.}
\item{image_name}{Explicit source image name for multi-image Seurat objects.}
\item{trusted}{Must be TRUE to deserialize the selected local RDS once.}
\item{max_observations}{Total bounded review-context observation limit.}
}
\value{A list with \code{experiment}, \code{context}, \code{backup_path}, \code{object_artifact}, and \code{bundle_artifact}.}
\description{
Copies original RDS bytes and an immutable, bounded metadata/reduction JSON bundle
inside the experiment directory. Adds their linked artifacts and atomically
replaces the index while retaining a byte-identical backup. Existing datasets
cannot be overwritten. Subsequent opening verifies RDS hashes without
deserializing the object. Expression matrices remain only in the original RDS.
The entire RDS is retained for a multi-image object; each artifact's row count
describes its selected tissue view, not the total observations in that RDS.
}
\details{
New assets use relative content-addressed paths below \code{spatial/objects} and
\code{spatial/review}. The old index is retained as
\code{.<index-name>.before-spatial-<old-sha256>.json}. A cooperative lock and
pre-publication digest check reject competing imports; stale locks are never
removed automatically. Failures can leave unreferenced immutable assets but leave
the previous index intact.

The returned review context binds to the new index digest. Callers must explicitly
migrate validated human checkpoint IDs, labels and history before switching a
live session. Old context objects are untouched. Relocated experiment trees can
be opened afresh; existing path-bound checkpoints require explicit reconstruction.
No inference, expression analysis, registration or processing jobs are started.
}
