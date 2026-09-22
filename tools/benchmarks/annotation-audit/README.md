# Annotation audit benchmarks

These standalone experiments support the [9 September 2026 audit](../../../docs/audits/tissue-annotation-performance-2026-09-09.md). They do not change the installed package or viewer behavior. Real annotation files are inputs only; no annotation data are included in this directory.

## Run

From the repository root, install the isolated, pinned JavaScript dependencies:

```sh
npm install --prefix tools/benchmarks/annotation-audit --ignore-scripts --no-audit --no-fund
```

Run benchmarks sequentially to avoid competition between their CPU workloads. Example with local data:

```sh
node --expose-gc tools/benchmarks/annotation-audit/indexes.mjs \
  Data/VisiumHD/Colorectal/Visium_HD_Human_Colon_Cancer_290325.geojson \
  /tmp/tissue-indexes.json

node --expose-gc tools/benchmarks/annotation-audit/indexes.mjs \
  Data/VisiumHD/Colorectal/cell_segmentations.geojson \
  /tmp/cells-indexes.json

node --expose-gc tools/benchmarks/annotation-audit/clipping.mjs \
  Data/VisiumHD/Colorectal/Visium_HD_Human_Colon_Cancer_290325.geojson \
  /tmp/tissue-clipping.json

node --expose-gc tools/benchmarks/annotation-audit/tiles.mjs \
  Data/VisiumHD/Colorectal/Visium_HD_Human_Colon_Cancer_290325.geojson \
  /tmp/tissue-tiles.json

Rscript tools/benchmarks/annotation-audit/formats.R \
  Data/VisiumHD/Colorectal/Visium_HD_Human_Colon_Cancer_290325.geojson \
  /tmp/tissue-formats.json

Rscript tools/benchmarks/annotation-audit/formats.R \
  Data/VisiumHD/Colorectal/cell_segmentations.geojson \
  /tmp/cells-formats.json

node tools/benchmarks/annotation-audit/render.mjs \
  Data/VisiumHD/Colorectal/Visium_HD_Human_Colon_Cancer_290325.geojson \
  /tmp/tissue-render.json
```

R format tests require `sf`, `jsonlite`, the installed `wsiTools`, and GDAL with FlatGeobuf support. Rendering requires locally installed Chrome. It launches an isolated headless browser against a temporary loopback server and closes both afterward. It writes screenshots to `/tmp/wsitools-annotation-audit-*.png`. Localhost and browser-launch permissions may be required. Replace paths for another OS. The R script removes only the temporary directory it creates for converted files.

For dependencies installed elsewhere, set `ANNOTATION_AUDIT_DEPS` to that directory containing `package.json` and `node_modules`. This audit used `/tmp/wsitools-annotation-audit-deps` to keep dependencies out of the repository.

## Interpretation

- `indexes.mjs`: full parse/stringify/clone, packed Float64 geometry, spatial queries with correctness checks, worker handoff. Query timing excludes index construction. Transfer-only timing excludes packing and any copy needed to retain the sender's buffer. WASM scan returns a resident view whereas some other methods allocate arrays.
- `clipping.mjs`: current `geometryClip()` against Clipper2/WASM, one polygon at a time. Current JS operations are bounded to 15 seconds; a slow completed operation is run once. Precision and output representation differ. Area checks do not prove topological equivalence. Worker startup/copying and the rest of the viewer are excluded.
- `tiles.mjs`: planar WSI data adapted to unmodified geojson-vt for this experiment only. `getTileRaw()` timings are cached reference lookups, not draw timings. MVT is measured on the generated quantized tiles. The JS FlatGeobuf writer is unindexed and strips most metadata; its numbers are not spatial-query or complete metadata round-trip results. Heap deltas across multiple tiling configurations include GC/retention effects and should not be treated as isolated per-index memory.
- `formats.R`: actual GDAL-indexed FlatGeobuf. Local warm file I/O, not HTTP Range performance. sf objects and wsi ROI objects have different representations. R `object.size()` can count shared references repeatedly and is not process RSS.
- `render.mjs`: actual tissue polygons, fill only, DPR 1, no WSI/labels/interaction/R sync. Canvas, SVG and WebGL use different flush/layout mechanisms. Zero-millisecond timer results do not mean zero GPU time. Two-animation-frame measurements are not FPS. Screenshots/nonblank checks do not prove exact topology or antialiasing equivalence.

The checked-in JSON files are one-machine observations, with min/median/max and input/source hashes where available. Do not interpret component ratios as a measured improvement to the complete viewer. Do not overwrite them when reproducing; use a separate results path.
