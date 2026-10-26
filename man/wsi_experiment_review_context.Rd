\name{wsi_experiment_review_context}
\alias{wsi_experiment_review_context}
\alias{wsi_experiment_train_reviewed_classifier}
\title{Review Explicit Experiment Observations and Cached Reductions}
\usage{
wsi_experiment_review_context(experiment, tissue_ids = NULL,
  feature_set_ids = NULL, max_observations = 10000L, strict = TRUE)

wsi_experiment_train_reviewed_classifier(experiment, context, checkpoint,
  feature_set_id, train_ids, ...)
}
\arguments{
\item{experiment}{Typed experiment or local experiment manifest.}
\item{tissue_ids}{Optional explicit tissue subset.}
\item{feature_set_ids}{Optional typed feature subset. \code{NULL} includes all.}
\item{max_observations}{Total review observation limit. Rows are never sampled.}
\item{strict}{When false, an exceeded row limit returns image metadata, an empty
observation/reduction set and an explicit unavailable status. Unloaded artifacts
are not read or validated in that metadata-only context.}
\item{context}{Exact validated review context used for the checkpoint.}
\item{checkpoint}{Human review checkpoint matching the context source signature.}
\item{feature_set_id}{Compatible typed image feature set.}
\item{train_ids}{Explicit canonical observation IDs; an empty selection never means all.}
\item{...}{CPU fitting arguments for \code{wsi_experiment_train_classifier()}.}
}
\description{
Constructs bounded, JSON-compatible review metadata from an existing experiment.
Only identity and native coordinate columns are loaded from feature CSVs, not
their predictor matrices. Existing version-one manifests remain valid without
new artifacts. No encoder, clustering, network or GPU job is started.
}
\details{
Optional tissue artifacts with \code{role = "review_observations"} declare
\code{dataset_id}, \code{observation_unit}, \code{frame_id}, \code{row_count},
\code{id_column}, \code{x_column}, \code{y_column}, optional
\code{cluster_columns}, and optional native-pixel \code{footprint_width_px} and
\code{footprint_height_px}. Tile/token observations require both footprints.

Artifacts with \code{role = "observation_reduction"} declare
\code{reduction_id}, \code{dataset_id} or \code{source_feature_set_id},
\code{observation_unit}, \code{frame_id}, \code{row_count}, \code{id_column}, and
exactly two \code{dimension_columns}. The dataset ID for a typed feature set is
\code{experiment:<feature_set_id>}. Optional \code{method} and \code{label}
describe the already computed embedding. Both artifact roles require a local
\code{path}, a full \code{sha256} digest and \code{format = "csv"} or
\code{"csv.gz"}. Reduction rows must identify every observation in the declared
tissue dataset exactly once. Joins use IDs, never row order or discovered files.

Image coordinates use native pixel centres; reduction coordinates occupy their
own embedding space. A reduction is not a Virchow2 feature set. Cluster values
remain metadata, separate from human labels and classifier predictions.

Reviewed classifier fitting requires explicit training IDs, a matching
checkpoint and confirmed \code{reviewed} records for compatible typed tile/token or cell
observations. A cell feature descriptor and ordered predictor allowlist are
required. Spatial cluster labels are never automatically promoted to targets. The existing CPU classifier retains training-only preprocessing and
records the checkpoint revision, review digest and human-label provenance.
Training and same-section agreement do not establish biological validation.
}
\value{
\code{wsi_experiment_review_context()} returns a plain validated review context.
\code{wsi_experiment_train_reviewed_classifier()} returns a portable CPU
classifier with human-review provenance; it does not save or publish a model.
}
\seealso{\code{\link{wsi_read_experiment}}, \code{\link{wsi_review_context}},
\code{\link{wsi_review_checkpoint}}, \code{\link{wsi_review_attach_spatial}}}
