\name{wsi_review_attach_spatial}
\alias{wsi_review_attach_spatial}
\alias{wsi_review_attach_seurat}
\alias{wsi_review_inspect_spatial}
\alias{wsi_review_context_from_linked}
\title{Attach Same-Section Spatial Metadata to Human Review}
\usage{
wsi_review_attach_spatial(context, object, tissue_id, dataset_id = "spatial",
  coordinates = NULL, coordinate_mapping = NULL, cluster_columns = NULL,
  reduction_names = NULL, reductions = NULL, image_name = NULL,
  observation_unit = "spot", trusted = FALSE, footprint_width_px = NULL,
  footprint_height_px = NULL, max_observations = 10000L)

wsi_review_attach_seurat(context, seurat, tissue_id, ...)

wsi_review_inspect_spatial(object, trusted = FALSE, coordinates = NULL,
  image_name = NULL)

wsi_review_context_from_linked(linked, tissue_id = "section-1", frame_id = NULL,
  image_sha256 = NULL, context_id = NULL, max_observations = 10000L,
  confirmed = FALSE)
}
\arguments{
\item{context}{Validated review context.}
\item{object,seurat}{Loaded Seurat/compatible list object, or a trusted local RDS path.}
\item{tissue_id}{Exact same-section target tissue identity.}
\item{dataset_id}{Stable dataset ID, distinct within the target tissue.}
\item{coordinates}{Optional explicit ID/X/Y table. Otherwise existing Seurat
coordinate helpers or a list-like object's coordinate table are used without
automatic coordinate scaling.}
\item{coordinate_mapping}{A list containing \code{confirmed = TRUE},
\code{image_sha256}, \code{frame_id}, and \code{coordinate_space} equal to
\code{"native_pixels"} or \code{"source_pixels"}. Source pixels also require an
invertible finite 3-by-3 \code{affine} whose final row is \code{c(0, 0, 1)}.
The mapping may be stored as \code{wsiTools_review_mapping} in a list-like object.
Native pixels require an identity affine.}
\item{cluster_columns}{Explicit categorical metadata columns. \code{NULL} selects
conventional available cluster fields, including \code{seurat_clusters}.}
\item{reduction_names}{Explicit Seurat reduction names; \code{NULL} includes none.}
\item{reductions}{Explicitly named matrices/data frames with stable row names, or
lists containing \code{cell.embeddings}. Only their first two dimensions are
included in the bounded plot configuration.}
\item{image_name}{Explicit spatial image name when several are present.}
\item{observation_unit}{\code{"spot"} or \code{"cell"}; never tile/token.}
\item{trusted}{Explicit permission to deserialize a trusted local RDS file.}
\item{footprint_width_px,footprint_height_px}{Optional native-image footprint
dimensions, supplied together. No radius or physical scale is guessed.}
\item{max_observations}{Total bounded observation limit, without sampling.}
\item{linked}{Spatial image object made by an existing wsiTools linker.}
\item{frame_id}{Explicit native image frame or an identity derived from image bytes.}
\item{image_sha256}{Optional expected image digest; current bytes are checked.}
\item{context_id}{Optional stable context identity.}
\item{confirmed}{Explicit confirmation of the existing linked coordinate mapping.
Without it, only image metadata and an unavailable status are returned. A
\code{linked$coordinate_mapping$review_confirmed} flag is also accepted.}
\item{...}{Arguments passed to \code{wsi_review_attach_spatial()}.}
}
\description{
Attaches normalized same-section coordinates, selected cluster metadata and
cached reductions to the exact H\&E image frame. Coordinate tables that happen
to resemble pixels are never automatically confirmed. No registration,
expression export, cross-section matching, model execution or automatic
conversion of transcriptomic clusters into expert labels is performed.
}
\details{
All attached observation IDs are namespaced by context/experiment, tissue,
dataset and original ID. Metadata and reduction rows join by stable IDs.
Extra expression assays remain inside the source object and are not serialized.
The returned context retains bounded normalized data, source-reference hashes
and mapping metadata, so reconstruction does not require serializing a full
Seurat object. Read only trusted RDS files. Their default size limit is
\code{getOption("wsiTools.review_max_rds_bytes", 256 * 1024^2)}.
This limits file bytes, not decompressed R memory; large sources should first be
exported as bounded metadata and reductions.

Inspection reports coordinate source/count and available field/reduction names
without declaring a mapping valid. Multiple spatial images require an explicit
choice. Unknown coordinate space, wrong image hashes/frames, duplicate IDs,
invalid coordinates and singular transforms fail clearly.

The linked-image helper reads physical calibration from source image metadata,
not from an RNA-spacing estimate. Unavailable image calibration remains explicit.
Existing automatically selected display scaling is not expert registration
evidence; review requires separate confirmation. Human review records remain
independent of imported cluster values.
}
\value{A plain validated review context, or an inspection metadata list.}
\seealso{\code{\link{wsi_review_context}}, \code{\link{wsi_seurat_coordinates}},
\code{\link{wsi_experiment_review_context}}}
