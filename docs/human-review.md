# Review spatial clusters alongside H&E

Open a CellPhenotyper experiment through **Open Project**, then click **Review
workspace** in the bottom-right viewer dock. Escape closes a workspace and
returns focus to its button. This workflow uses existing observations and cached
image features. It does not start feature extraction or processing jobs.

1. Choose **Current tissue** or explicitly **All tissues**, the observation unit,
   the dataset and the cluster field. Check a cluster to select its observations.
   Equally named clusters from different objects stay separate unless you
   explicitly choose all datasets.
2. Choose a declared KODAMA or other reduction in **Linked plot**. Drag a lasso in
   the plot, or click **Lasso on tissue** and drag on H&E. Shift adds and Alt
   subtracts. **Select inside annotation** respects polygon holes and multiple
   parts. Selections join by canonical experiment/tissue/dataset/observation IDs,
   including when different tissues reuse the same barcodes.
3. Examine the tissue and thumbnail previews. Enter a class and reviewer, then
   **Confirm class**, **Correct class**, **Exclude**, or mark **Uncertain**.
   These actions create separate human review records. RNA clusters and model
   predictions are preserved. Undo and redo also survive saving.
4. Set the work tile size and explicit units. Microns require image calibration.
   **Preview work tiles from selection** creates one image-anchored grid for the
   current tissue. Several selected spots can share one tile. Edge tiles are
   dropped, trimmed, or rejected according to the selected policy.
5. **Export work tiles from current-tissue selection** downloads a JSON plan with
   exact coordinates, image hashes and source observation IDs. A changed tissue,
   selection or tile setting cancels an in-flight export. Repeat for another
   tissue. Empty selection produces no work. This is a processing request artifact;
   it does not submit CellPhenotyper or Bora work or export pixel patches.
6. Use the viewer's **Save Project** to retain metadata, selection, review history
   and the chosen reduction/scope. Reopen that file through **Open Project**. Keep
   the original experiment, images and artifact tables accessible. Changed images,
   calibration, or observation metadata require a new validated context.

## Add a spatial transcriptomics object to the CellPhenotyper project

Expand **Add spatial transcriptomics to this project**. Choose a trusted local `.rds`, or
paste its path in a browser. For an object containing multiple spatial images,
inspect first to list its available image names. Choose the exact Seurat image
for the active tissue and inspect again. Select metadata,
including the cluster fields and reductions to attach, then confirm that the coordinates are native full-resolution pixels of the same
H&E section. If coordinates use another source pixel frame, provide an explicit
invertible 3×3 affine. The application never guesses scale or orientation here.
Leave **Save the original object and selected display data in this
CellPhenotyper project** checked, then click **Add to CellPhenotyper project**.
The experiment index now links an untouched RDS copy and a small JSON file with
selected coordinates, clusters and reductions. Reopening the experiment index
restores that data without automatically deserializing the RDS. The RDS retains
the complete original object even when only one Seurat image/FOV is selected.
Its artifact row count describes the selected tissue view. External backing files
referenced from inside an RDS are not discovered or copied; keep those files
available for subsequent RNA analysis. The saved display bundle opens independently.

A previous-index backup stays beside the index, and the new assets have
immutable names under `spatial/objects` and `spatial/review`. Existing tissues,
features, settings and source files are preserved. The import fails if another
writer changed the index or the selected image coordinates lose invalid rows.
For a multi-image object, repeat the import for each matching tissue with its
explicit Seurat image name. Identical RDS bytes share one stored copy. Dataset
IDs are unique within a tissue; equal IDs on different tissues are allowed.

**Saved spatial objects** lists stored datasets for the active tissue. Choose a
dataset before selecting clusters; multiple objects can reuse the same cluster
names. Inspect the resulting cyan points over H&E before using selections.

Uncheck project storage to attach display data to the current viewer only.
Use **Save Project** separately to retain human decisions, selections and undo
history. After adding an object, save a fresh viewer project: older viewer files
retain their previous experiment-index identity. Keep the full experiment tree
together when moving it.

Only metadata, coordinates and named reductions enter the review context;
expression matrices are not embedded. Interactive contexts are bounded to 10,000
observations by default and local RDS files to 256 MiB. For larger data, prepare a
bounded metadata export or use the R APIs with an explicit limit. No silent
sampling occurs.

## Declare existing KODAMA results

The experiment adapter accepts hashed `review_observations` and
`observation_reduction` CSV/CSV.GZ artifacts with an explicit dataset, image frame,
observation unit, ID column and two reduction dimensions. It validates exact ID
joins independently of row order. It does not infer a KODAMA run from arbitrary
RData files. Existing legacy KODAMA workflows remain available.

```r
context <- wsi_experiment_review_context(experiment)
ids <- wsi_review_select(context, cluster_field = "seurat_clusters",
                        cluster_values = "3", tissue_ids = "section-1",
                        dataset_ids = "spatial")
plan <- wsi_review_tile_plan(context, ids, tile_size = 112, units = "um")
```

`wsi_experiment_train_reviewed_classifier()` can train a cached-feature PLS
classifier from explicitly reviewed **tile** observations. Spot/cell labels are
not automatically converted into tile training targets. Choose reviewed IDs
explicitly; spatial cluster membership alone is not expert supervision.

The small integration fixture tests selection and persistence with synthetic RNA
and a declared synthetic KODAMA-shaped embedding over real image/cache references.
It does not establish biological validity, classifier accuracy, KODAMA fitting, or
HPC/GPU lifecycle correctness.

To add an object from R, use `wsi_experiment_add_spatial()` with a trusted local
RDS path, the selected tissue, dataset ID, cluster/reduction names and a confirmed
image mapping. It returns the updated experiment, review context, object/bundle
artifacts and previous-index backup path. Imports currently use spot observations.
