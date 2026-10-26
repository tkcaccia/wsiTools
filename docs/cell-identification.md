# Cell identification in an experiment

Open the experiment index through **Open Project**, then choose **Cell
identification** in the workspace dock. Opening the file reads existing outputs;
it never downloads weights, starts an encoder or submits processing jobs.

For a full-slide project with a verified dual-resolution registration, choose
**Analysis → Registered tissue**. The panel lists only attached 0.5 µm/px
outputs, then displays them on the 0.25 µm/px cell image using the exact
pixel-edge affine saved with the two converted images. Virchow2 tile positions
are feature observations, KODAMA shows a two-dimensional embedding of the
visible tiles. Accepted Bora GeoJSON opens as ordinary editable tissue
annotations, with prediction provenance retained; these are separate from
ground truth and cell classes. The registered viewport panel retains read-only
**Predicted Bora** layers for legacy registered assets. The panel shows
source hashes and the X/Y scale; moving the image requests only a bounded
viewport page. If too many Bora polygons or vertices intersect the view, zoom
in to inspect a smaller region rather than accepting an incomplete boundary.
The full 0.5 µm image and complete cell/tissue tables are never sent to the
browser by this control. The current registered-asset contract has no
RGB-Virchow2 raster role; seeing tile positions or KODAMA points is not an RGB
feature image. A future RGB derivative must declare its own verified tissue
frame, image hash and tiled display source before it can appear here.

For tissue analysis, choose **Open KODAMA map** in **Analysis** or the experiment guide.
When a checked KODAMA map is registered, this opens its embedding and colours
native tissue tile footprints with the same cluster palette. Select a cluster
or embedding region to find the corresponding tissue area. Tissue clusters
and predicted cell types remain separate views. Saved projects retain the
chosen reduction, cluster field and review history. If the analysis did not
complete or no map was exported, the action explains why it is unavailable.

The compact panel leaves the tissue visible. Class totals and the selected-cell
inspector appear first. **Expand** provides more room; the table, model/filter
settings and CPU classifier controls open only when needed. Use **Select on
image** to draw around displayed cells (Shift adds, Alt subtracts), then inspect
and label them. **Focus selected cell** returns to its image location. The
optional review queue shows 50 cells per page and can restrict itself to selected
cells or the current image view.

Choose a **Backbone**, **Taxonomy** and model, then a class. The catalog can
include PanNuke, CoNSeP, Lizard, NuCLS main/super, Ocelot, MIDOG and PanopTILs
outputs. A head with no registered output displays its explicit unavailable
reason. Class names and order come from that exact model; labels from different
taxonomies are never voted together or silently translated.

The inspected CellViT++ archive provides 13 replaceable MLP heads: seven for
SAM (CoNSeP, Lizard, NuCLS main, NuCLS super, Ocelot, MIDOG and PanopTILs)
and six for HIPT (the same set except MIDOG). Each detector also supplies its
native PanNuke type head. These are 15 model outputs across two incompatible
representations, rather than 15 heads usable on any one feature table. An
unavailable HIPT MIDOG head stays unavailable. Research Virchow2 detector
variants need their own compatible checkpoints and intermediate encoder layers;
ordinary tissue Virchow2 tokens cannot be substituted for them.

Use **Colour** to inspect predicted cell type, model score, abstention/low-score
review candidates, separate tissue context or StarDist disagreement. The score
threshold only filters review candidates. It does not calibrate probabilities
or change source predictions. Native PanNuke scores can be pixel-type vote
fractions with no exported probability vector; MLP heads declare their own
uncalibrated probability vectors. Neither is clinical confidence. Tissue context
is separate soft evidence. Segmentation agreement is agreement between models,
not an independently validated reference; adjusted boundaries still require review.

Search or filter the paged cell table. Select individual cells or **Select
matching cells for review**, enter a class and reviewer, then **Confirm class**,
**Correct class**, **Uncertain** or **Exclude**. Decisions apply only to selected
cells on the current tissue. Original predictions remain unchanged. **Save
Project** retains decisions, selection, undo/redo and display choices; reopen
that session against its unchanged experiment and source files. **Download cell
evidence CSV** exports human decisions in columns separate from model outputs.

Class suggestions follow the selected model's exact vocabulary. **Use selected
prediction as class** fills the field only when all selected cells share one
available prediction; it records no decision. Inspect the cells and explicitly
confirm or correct the proposed class.

Review synchronization retains one in-flight checkpoint and the newest complete
pending checkpoint. Each contains the complete decision history; intermediate
unsent snapshots can be replaced without discarding decisions. CPU fitting
captures the selected IDs and reviewed checkpoint when clicked and reports
synchronization, fitting, completion or an error in a separate persistent status.
Changing display settings cannot erase that result. Failed synchronization also
prevents a save or fit from claiming success.

**Reviewed cell classifier** uses only explicit confirmed/corrected cells from
the selected feature set. At least two classes with two cells each are needed.
Uncertain/excluded observations, raw detector predictions and RNA cluster labels
are never training targets. Training-only preprocessing and the complete ordered
feature descriptor accompany the saved CPU fastPLS model. Use **Apply to current
tissue** to replay it without fitting. Different backbones, checkpoint hashes,
feature definitions or feature orders are rejected even when widths match.
Keep held-out patients separate before claiming classification accuracy.

The fitting panel shows selected human-label counts and explains why fitting is
disabled until every selected cell belongs to the exact feature set and at least
two classes have two observations each. Selecting RNA spots, uncertain cells or
excluded cells prevents fitting. A selection count above the table reports the
cells selected on the current tissue.

To guide review with spatial transcriptomics, select same-section RNA spots in
**Review workspace**, return to the cell panel, set a radius in microns and
choose **Select cells near selected RNA spots**. This produces a candidate cell
selection in the confirmed image frame. It does not assign a spot cluster label
to nearby cells. Multiple cells can occupy a spatial spot; adjudicate cell types
explicitly.

## Typed input and bounds

`tissue.cell_classification` references a SHA-256 checked local JSON artifact
with schema `cellphenotyper-cell-classification`, version 1. Its image identity,
frame, dimensions and per-axis MPP must match the experiment. It contains a
model catalog and immutable cells keyed by `cell_id`; each head has its own
prediction/status and explicit score semantics. Unavailable/abstained outputs
remain distinct from biological classes named “unknown” or “other”.

Existing projects can register a differently named frame only through an explicit
`cell_classification.registration`: source/target frame IDs, an exact 3×3 identity
`source_to_image_affine`, and `identity_verified` evidence for SHA-256, dimensions,
per-axis MPP, native pixel convention/space and zero origin. Both image records
must independently match this evidence. The registered feature set retains
`source_frame_id` and the identical registration while using the project frame
for display. Original classification JSON, source frame IDs and cell IDs remain
unchanged. Missing evidence and nonidentity transforms are rejected.

StarDist geometry review can be registered separately as
`cell_classification.segmentation_review`: checked `path`/`sha256`,
`source_frame_id` and `coordinate_convention`. The consumer reads the actual
`cellphenotyper.segmentation-review/v1` report. Its source image hash, dimensions,
per-axis MPP, native space and origin must match the classification image. A
checked `feature_manifest` must match the classification's recorded manifest
SHA-256 and exact feature descriptor. QC input cells must match
`manifest.source.normalized_segmentation.sha256` when declared, or the original
`source.cells_sha256` for legacy exact-source reports. A raw/normalized mismatch
is rejected. Cells join one-to-one through canonical IDs or unique original
source IDs, with contradictory identities rejected; centroids are never a join
key. Only QC fields are added to the runtime view. Original classification JSON,
predictions and cell IDs are preserved. Missing QC is displayed/exported as
**unavailable**, separately from a completed review with no flagged disagreement.

`cell_classification.tissue_context` can register a separate checked
`cellphenotyper-cell-tissue-context` version 1 report. It must bind the exact
classification, feature-manifest, normalized segmentation and image hashes and
the complete canonical cell-ID population. Numeric mask labels cannot stand in
for canonical cell IDs. Context joins preserve original predictions and feature
columns. The table reports the dominant overlapping tissue region, covered
fraction, boundary crossings and overlapping tissue labels; hover over the
context column for each region's overlap and signed centroid distance in microns
(positive inside). CSV exports retain every region and its distances. Uncovered
and overlapping fractions remain explicit. This context describes a tissue
model's regions; it never assigns a cell type or supplies a training target.

The nested `feature_set` must be registered exactly once in `tissue.feature_sets`.
Nested paths resolve relative to the classification JSON; experiment paths
resolve relative to the index. `kind="cellvit-cell"`, `observation_unit="cell"`
and an ordered predictor allowlist are mandatory. `descriptor` records backbone,
checkpoint SHA-256, runtime version, feature definition, feature count and image
coordinate frame. Actual normalized model MPP, patch size/overlap/token size,
runtime wheel SHA-256 and normalization/resampling/token provenance are required
in descriptor sampling/preprocessing. Source image MPP is recorded separately;
different normalized model sampling is incompatible even with the same weights. Its complete contents are part of the replay signature.
Identity, labels, predictions, confidence and split metadata are not predictors.
Legacy CellPhenotyper numeric-table fitting now also requires explicit ordered
`project$feature_columns` (or `attr(project$cells,"feature_columns")`); IDs and
QC fields are never selected automatically.

Default classification display limits are 10,000 cells, 64 MiB JSON and 64 model
entries. Existing experiment matrix/review limits also apply. Oversized data
requires a bounded derivative or a deliberately reviewed limit increase; no
cells are silently sampled. Display, API and synthetic contract tests do not
establish model accuracy or complete-slide throughput.

## Whole-slide cell indexes

Large cell populations use a checked SQLite index to keep full review records
and feature vectors out of the browser. The experiment registers
`cell_classification.index={path,sha256,format:"sqlite"}`. Optional R packages
DBI and RSQLite provide read-only queries. The index records immutable source
classification, feature-manifest, feature-table and image identity; predictor
values remain on the R side.

Opening an indexed experiment exposes its model catalog and complete class
totals. The image requests compact coordinates and selected model evidence
only for the current visible rectangle, in pages of 5,000. Panning or zooming
replaces that viewport cache; another tissue, source hash or model cannot reuse
it. Progress reports the visible cell count, and an interrupted stream never
claims that the viewport is complete. Every cell intersecting that rectangle
is returned; zoom does not sample cells or substitute a density grid. The
selected model's ordered palette gives each class a distinct colour.

At a scale bar of 1 mm or more, the cell overlay hides automatically and no
hidden viewport coordinates are fetched. Zooming in requests the visible cells
again. This applies per pane and preserves the manual Show cells setting.
Without calibrated MPP the viewer cannot infer that physical threshold and
keeps the layer available. Cells can overlap; zoom in to inspect them
individually. Class buttons show the same colours.

Full review records remain bounded to 500 per request, with 50 table rows per
page. Queue, viewport and exact-ID queries cannot replace the complete image
display. Image review selections are limited to 500 cells at a time. The compact
viewport cache is regenerated from checked sources on reopen; saved projects
retain display preferences and human decisions without duplicating that cache.

Version 3 CellViT indexes may point to versioned gzip-compressed float32
matrices in source-hash-checked partition archives. The browser receives no
1,280-dimensional feature vectors. A bounded reviewed-cell selection invokes
the packaged Python 3 decoder on the R side; it verifies the index, partition
manifest, matrix bytes, row hashes, canonical IDs and global coordinates before
returning only the selected ordered vectors for CPU PLS training. Python 3 is
required for this feature path. The archived source files remain immutable.

Use exact cell-ID lookup or the optional filtered queue to inspect individual
records. Queue batches use a cursor and the current filters. Selecting cells
requests a bounded review context that retains prior reviewed cells and their
history. If the configured review bound would be exceeded, the request stops
with an explanation; it never silently removes previous decisions. Exported
cell-evidence CSVs contain the loaded subset, as the button label states.

Saved classifiers retain checked references and feature-signature digests in
the project registry. Fresh R sessions restore their dropdown entries without
automatically deserializing or running a model. Applying a classifier explicitly
checks its file identity and exact feature representation.

## Fast morphology profile

Checked HoVer-NeXt completed outputs can use the same review panel with a
separate `cell-morphology` feature kind. Its 11 ordered size, shape and RGB
measurements have their own immutable source, checkpoint and sampling descriptor.
They can train a separate reviewed CPU classifier. They are incompatible with
CellViT embeddings, so no CellViT pretrained MLP head is applied to them.

HoVer-NeXt native classifications declare uncalibrated class-vote evidence.
If the upstream export omitted class scores, the label remains visible while
the score says **unavailable** and the score overlay is grey. A missing score is
never represented as zero. Opening or reviewing these completed outputs does
not start the upstream CUDA inference program.

Current Virchow2 pooled exports can explicitly declare
`grid.representative_policy="logical-centre-clipped-to-image"` with
`grid.edge_policy="white-padded"`. Display representatives are validated against
that exact rule; full logical footprints and clipped valid bounds are retained.
Padded observations cannot acquire ROI training labels from unsupported pixels.
Older grids without this policy retain strict regular-centre validation.
