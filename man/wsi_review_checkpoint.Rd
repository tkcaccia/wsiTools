\name{wsi_review_checkpoint}
\alias{wsi_review_checkpoint}
\alias{wsi_validate_review_checkpoint}
\alias{wsi_write_review_checkpoint}
\alias{wsi_read_review_checkpoint}
\title{Validate and persist explicit human review checkpoints}
\usage{
wsi_review_checkpoint(context, selected_ids = character(),
  review_records = list(), revision = 0L,
  history = list(past = list(), future = list()))

wsi_validate_review_checkpoint(checkpoint, context)

wsi_write_review_checkpoint(checkpoint, context, path, overwrite = FALSE)

wsi_read_review_checkpoint(path, context)
}
\arguments{
\item{context}{The canonical context returned by \code{wsi_review_context()}.}
\item{selected_ids}{Explicit unique canonical IDs. Empty means no selection.}
\item{review_records}{Human records with canonical \code{id}, \code{status}
(reviewed, excluded or uncertain), \code{reviewer}, and UTC ISO-8601
\code{updated_at}. Reviewed records require a non-empty \code{label}.
Optional \code{action} is confirm, correct, exclude or uncertain and must agree
with the status.}
\item{revision}{A non-negative integer revision.}
\item{history}{Bounded snapshot arrays named \code{past} and \code{future}.
Each snapshot contains explicit \code{selected_ids} and \code{review_records}.}
\item{checkpoint}{A version 1 \code{wsitools-review-checkpoint} list.}
\item{path}{A local JSON checkpoint path.}
\item{overwrite}{Replace an existing checkpoint when true.}
}
\description{
Keeps human labels and decisions separate from source clusters and predictions.
Restoring a checkpoint requires exact context identity and source signature.}
\details{
Unknown or duplicate IDs, stale image/source signatures, invalid records and
invalid history snapshots are errors. Review records cannot include
\code{cluster_values}, \code{predictions} or \code{model_sources} fields.
This API does not train models or alter source objects.

History is limited to 100 snapshots per direction by default, controlled by
\code{wsiTools.review_max_history}. Saved and loaded checkpoint JSON is limited
to 16 MiB by default, controlled by \code{wsiTools.review_max_checkpoint_bytes}.
Writes use a temporary file in the destination directory and atomic rename.
Numeric data are serialized with full double precision for source integrity.}
\value{The constructor, validator and reader return a normalized checkpoint
list. The writer returns the saved absolute path.}
\seealso{\code{\link{wsi_review_context}}, \code{\link{wsi_review_tile_plan}}}
