# CellPhenotyper experiments and cached Virchow2 analysis

An `experiment.cellphenotyper.json` file with schema
`cellphenotyper-experiment`, version 1, describes one or more calibrated tissue
images and their existing feature caches. Opening an experiment never downloads
a model, extracts embeddings, starts a GPU task, or reinterprets tile/token rows
as cells.

```r
experiment <- wsi_read_experiment("experiment.cellphenotyper.json")
viewer <- wsi_viewer_experiment(experiment, open = TRUE)
features <- wsi_experiment_features(experiment, "virchow2-pooled")
```

The image path, SHA-256, dimensions, MPP, native frame and coordinate convention
are explicit. Feature sets name every predictor in order, their ID/x/y columns,
row count, CSV/CSV.gz format, hash, encoder revision/checkpoint, preprocessing
and native-pixel grid. A grid origin is its top-left **edge**; feature coordinates
are pixel centres. Pooled tile features require 2560 dimensions and
`CLS+mean_patch`; spatial patch tokens require 1280 dimensions and
`spatial_patch_tokens`. Neither representation can be silently substituted for
the other. An optional original-image affine is provenance; supplied feature
coordinates must already refer to the declared analysis image.

Relative paths resolve against the manifest directory. Image/feature digests are
checked on reading, and feature digests again when consumed. Predictor columns
exclude IDs/coordinates. Observation IDs are namespaced by experiment, tissue,
feature set and original ID. Multiple tissues must have the same ordered
feature, encoder and preprocessing contract before their rows can be combined.
The reader does not infer these properties from filenames.

`wsi_experiment_project(experiment, output = "case.wsiproject")` saves the source
manifest identity and all image references. Reopen with `wsi_read_project()` then
`wsi_read_experiment(project)`. A changed manifest requires explicit reopening;
pixel data and feature caches are not copied into the project. Hashed
`tissue_annotations` artifacts in the same native image frame are imported with
namespaced IDs and tissue scope. These imported annotations retain source
provenance and are not automatically an independent reference standard.

## Full-slide cells and tissue annotations

The experiment may register an indexed CellViT++ classification table and its
lossless feature partitions. The viewer reads visible cells and selected review
rows in bounded pages; it does not load every cell vector into the browser.
Choose **Cell identification** to switch the registered taxonomy or classifier.
Accepted Bora GeoJSON opens as ordinary editable tissue annotations with its
prediction provenance preserved. Tissue groups and cell types remain separate.
See [Cell identification](cell-identification.md) for model and review details.

An optional `image.display` reference can point to a native-pixel pyramid with
the same source hash, frame and dimensions, plus an explicit `vips`, `openslide`
or `auto` backend. The viewer checks its hash and dimensions before opening it;
annotation coordinates still use the analyzed image's unchanged native frame.

## Training and reuse

```r
# labels is an id/label table or a named character vector.
model <- wsi_experiment_train_classifier(
  experiment, "virchow2-pooled", labels,
  train_ids = names(labels), ncomp = 2, max_features = 64
)
wsi_write_experiment_classifier(model, "classifier.rds")
model <- wsi_read_experiment_classifier("classifier.rds")
predictions <- wsi_experiment_predict_classifier(model, experiment, tissue_ids = "tissue_B")
```

The CPU SIMPLS-LDA route fits missing-value means, variance selection, centring,
and scales on the explicitly selected training rows only. Other rows never fit
those parameters. The saved compact fastPLS model can predict without refitting.
Labels/classes, training IDs/tissues, optional groups, seed, effective requested
component count, feature contract and package version accompany the model.
Read RDS artifacts only from trusted local sources.

Viewer training accepts only observation footprints wholly within a selected
polygon annotation. Corners/centre and intersections between polygon boundaries
and the rectangular open interior exclude partly covered tiles, holes and
concave intrusions. Conflicting classes raise an error. Exclusion counts and
IDs are retained. Multi-image annotations must identify their tissue/image or
project index. This is a conservative containment rule, not an estimated area
coverage fraction. Saved classifier and prediction derivatives are placed under
`derivatives/wsitools/classifiers/`. Applying a saved classifier through the live
endpoint uses `action = "experiment_apply_classifier"` and `model_path`; it
does not require new training annotations.

Training, predictions and display colours do not constitute independent
biological validation. Group held-out specimens/patients explicitly; do not tune
features or components on those held-out observations. Raw LDA scores and
appearance probabilities are uncalibrated and must not be labelled clinical
confidence. Missing classes and domain shifts require separate assessment.

## Optional Bora appearance bridge

`wsi_experiment_appearance()` invokes an already-installed Python Bora module;
there is no automatic installation. Select its interpreter with
`options(wsiTools.bora_python = "/path/to/python")` or
`WSITOOLS_BORA_PYTHON`. Developers can also set `wsiTools.bora_pythonpath`.

The interpreter must already provide `bora.appearance_bridge`. If Bora is a
source checkout, point `wsiTools.bora_pythonpath` at that checkout's `src`
directory. Optional CPU bridge tests use `BORA_TEST_PYTHON` and
`BORA_TEST_PYTHONPATH`; they skip when the interpreter is not supplied.

Local mode requires one `active_tissue_id` and cannot include other tissues.
Shared mode applies a common fitted recipe across the selected compatible
tissues. Only rows explicitly labelled `train` are sent as training labels;
query rows remain blank. Supply `recipe_path` to replay a frozen recipe without
labels or refitting. The result includes bounded native-coordinate RGB points,
class probabilities, the frozen recipe and CSV artifact paths. The method is
`discriminative-appearance-v1`, not Bora's full RGB9+19 spatial-token procedure.
Pooled feature colours have tile resolution. Interpolating their display does
not create finer-resolution evidence.

## Resource limits

Default feature reads permit at most 10,000 rows and 256 MiB of numeric matrix
storage, set by `wsiTools.experiment_max_rows` and
`wsiTools.experiment_max_matrix_bytes`. The CSV reader reads at most the declared
row count plus one validation row. Character parsing adds transient memory.
Viewer appearance returns at most 10,000 observations
(`wsiTools.experiment_max_viewer_rows`); imported annotation files are bounded by
64 MiB (`wsiTools.experiment_max_annotation_bytes`). Exceeding a limit raises an
informative error; it never silently samples or loads the whole WSI. Select a
bounded tissue subset or deliberately raise a documented limit.

Synthetic contract tests exercise schema/hash/order errors, tile/token identity,
training-only preprocessing, saved-model replay, annotation footprint exclusions
and optional installed Bora CPU replay. They are software validation only.
