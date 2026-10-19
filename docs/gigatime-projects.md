# GigaTIME markers in experiment projects

An existing GigaTIME result can be registered in the CellPhenotyper experiment
index and opened through **Open Project**. Registration and viewing do not run
GigaTIME, download a model or start processing.

Click **GigaTIME markers**, then choose a marker and segmentation source.
**Marker image (mIHC layer)** shows the registered TIFF channel over H&E. You
can combine channels and change colour, opacity and contrast using the existing
channel controls in Layers. **Per-cell scores** shows the selected marker at
segmented-object centres. Changing the selected marker updates an enabled cell
view; only one cell marker is shown at a time. Image channels can be combined.
The table shows the first 50 objects; click an object ID to select it on tissue.
**Export this marker’s cell scores** exports every registered row for the
selected tissue, marker and segmentation. The UI never substitutes another
mask or tissue when that combination is absent.

Values are **uncalibrated virtual marker scores predicted from H&E**. They are
not measured protein abundance or calibrated probabilities. The viewer retains
the exported numeric values, including integer-storage scales, and uses a
per-dataset minimum/maximum colour range for cell display. Equal colours in
independently scaled cell views do not establish equal absolute scores. Nucleus
masks remain labelled as nuclei; no cell outline or cell type is inferred from
a centroid or a marker threshold.

Use **Save Project** to retain display settings. Reopening the scientific index
restores registered sources; opening the saved viewer project also restores its
layer settings. The source image, marker TIFF, label mask, CSV and index must
remain accessible. Move their directory tree together for relative paths.

## Register outputs explicitly

Use CellPhenotyper's `bin/export_experiment.py --spec` with the three documented
artifact roles: `gigatime_marker_image`, `cell_segmentation_mask`, and
`gigatime_cell_scores`. The producer guide `docs/EXPERIMENT_PROJECTS.md` defines
all fields and an example. The marker image can be registered without cell
scores; missing segmentation never creates synthetic cells. Experiments do not
need Virchow2 features to display registered GigaTIME output.

Each marker declares its actual zero-based TIFF page, and each score column is
explicitly mapped to a marker. The score record links the exact marker image
and label mask within the same tissue. All assets are hash checked and bound
to the native H&E frame. Native full-image integer cell/nucleus masks are
supported. Grid/tile observations cannot be relabelled as cells. Raw zero-based
pixel-index centroids require the declared half-pixel and scale transform;
coordinates already expressed as native H&E centres require identity. No crop,
scale, axis reversal or cross-section registration is guessed.

This interface supports scalar marker pages in TIFF/OME-TIFF and CSV/CSV.GZ
scores. Zarr must first be exported to a supported TIFF using the existing
producer export route. It does not discover arbitrary output folders or infer
historical image/mask provenance from matching filenames. Explicit registration
is an assertion of the correct source association; checksums alone cannot prove
which historical segmentation was used for quantification.

Tiled image viewing requires libvips (`vips` and `vipsheader`). Default bounded
cell display limits are 10,000 rows per score table, 64 MiB per CSV and 250,000
rendered marker items. Exceeding a limit reports an error; rows are not silently
sampled. Larger cell tables need a deliberately bounded derivative or reviewed
limits; indexed loading remains future work.
