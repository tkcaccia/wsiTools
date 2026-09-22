# Tissue annotation performance audit

Date: 9 September 2026. Scope: the current local wsiTools working tree, including the recent editing-performance and synchronization modules. This is an audit and experimental comparison, not an implemented performance release.

## Recommendation

Keep exact vector geometry as the authoritative annotation representation. Separate it from the geometry used to draw the screen. Use an indexed binary baseline, a small mutable edit store, worker-based geometry processing, and caches that update only affected features and tiles. Keep the existing browser interface and OpenSeadragon image viewer in both browser and Tauri launches.

The first priority is **not changing GeoJSON to another delivery format**. It is removing expensive whole-polygon operations and collection-wide work from each edit. In the measured colorectal tissue file, one feature has 117,669 vertices. A small union with that feature took 14.86 seconds in the current JavaScript geometry path. A Clipper2/WASM prototype took 43 ms including input conversion and packed output. This is a component experiment, with different clipping implementations and precision settings, not a claim that the complete viewer is now hundreds of times faster.

The recommended order is:

1. Replace the expensive clipping path after correctness tests; retain geometry inside a worker between edits.
2. Introduce explicit changed-feature transactions for redraw, undo, R synchronization and persistence.
3. Index features and polygon parts; stop rebuilding annotation lists and scanning collections for routine interactions.
4. Cache exact annotation rendering independently of pointer movement, selection and labels.
5. Add indexed binary storage for large imports and reopen, followed by viewport render tiles and GPU meshes where measured benefits justify them.

**Do not force mask conversion, silently simplify saved boundaries, rebuild a complete tile archive after every stroke, or replace the entire viewer UI.** An imported mask can have its own tiled raster editor, but that is a different data model from exact vector editing.

## Evidence and Limits

Benchmarks used the user's real colorectal annotations, not generated rectangles. Inputs were read without modification. All temporary binary conversions were removed after the measurements. The production package, installed application and open viewer were not changed by this audit.

| Dataset | File size | Polygon features | Polygon vertices | Largest feature |
|---|---:|---:|---:|---:|
| Colorectal tissue, `Visium_HD_Human_Colon_Cancer_290325.geojson` | 13.27 MB | 359 | 825,698 | 117,669 vertices |
| Colorectal cells, `cell_segmentations.geojson` | 97.84 MB | 220,704 | 3,778,248 | 51 vertices |

The tissue file has 382 features in total; 23 non-polygon features were excluded from polygon rendering, clipping and JavaScript index tests but included in R file-format tests. This distinction matters: a few enormous tissue polygons require a different optimization from hundreds of thousands of small cells.

Machine: Apple M3, 8 logical CPUs, 8 GiB RAM, arm64 macOS Darwin 23.5. Node 26.3.0; R 4.6.0; sf 1.1.2; GEOS 3.13.0; GDAL 3.8.5. The rendering experiment used Chrome 152.0.7977.76 with ANGLE/Metal on the M3. Exact dependency versions, file hashes and relevant runtime source hashes are in the JSON results.

Most values below are medians of three runs. Index queries use 100 seeded viewport queries, repeated in five batches. Rendering uses 12 measured frames after two warm-up frames. These are warm local component measurements on a machine with other applications open, not cold-disk production latency, a controlled cross-OS trial or a full Tauri interaction test. Small sub-millisecond differences are particularly sensitive to timing noise.

The isolated Canvas, SVG and WebGL pages were run, checked for nonblank output, and their screenshots inspected. They render the actual tissue boundaries, but do not include the WSI image, labels, brush interactions, R synchronization or multiple panes. No production behavior was changed, so this audit does not claim to have verified a repaired viewer.

## Current Implementation

### Loading and Representation

- [R/roi.R](../../R/roi.R), `wsi_roi_from_geojson()` around line 277: recursively collects all coordinates to calculate bounds, creates one data frame per feature, then binds the rows. Coordinates, geometry, properties and original features are all retained through the ROI representation. R can share these references initially; it would be incorrect to count them as independent physical copies without measuring allocations.
- [R/roi.R](../../R/roi.R), `wsi_read_geojson()` around line 339: parses the complete JSON into nested R lists.
- [launch-viewer.R](../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R), `desktop_geojson_persistent_cache_file()`, `desktop_geojson_browser_copy()` and `desktop_start_geojson_import_job()` around lines 735-808: already provide fingerprinted RDS caches, a static browser copy/link and background R import. This is useful existing work, not a missing cache. However, the browser and R still separately ingest the geometry, and reopening the RDS restores a large nested object rather than reading only a viewport.
- [R/viewer.R](../../R/viewer.R), `denseGeojsonStaticLayer()` around line 3233: the static tissue route fetches and parses the full file, then builds its layer. Work is yielded between batches of features, but one very large feature can still consume a long task. `denseStaticRawGroups()` around line 3227 caches another coordinate layout using point objects.
- [R/viewer-session.R](../../R/viewer-session.R), dense GeoJSON response around lines 5895-6040: already supports viewport queries and a native bounding-box index. The static tissue route bypasses that partial geometry delivery by returning a full-file URL. The native index in [src/native-geometry.cpp](../../src/native-geometry.cpp), around line 78, is an adaptive uniform grid with a separate list for oversized boxes, not an R-tree.

**Consequence:** the initial bottleneck includes representation conversion and duplication, not just disk speed. A faster parser alone cannot remove the later conversion, indexing, path creation, metadata and synchronization costs.

### Display and Selection

- [inst/viewer/performance.js](../../inst/viewer/performance.js), around lines 54-114, already caches geometry and `Path2D`. Separate interaction scheduling exists around line 238, and per-pane scheduling around line 175. Those improvements should be retained.
- Its `wsiDrawRois()` still traverses the ROI collection, applies visibility checks and draws visible geometry. Large visible tissue polygons remain costly even with a cached path.
- [R/viewer.R](../../R/viewer.R), `roiAt()`, `layerObjectAt()` and `layerPolygonHit()` around lines 3083 and 3508-3513, traverse candidate collections before geometry tests. Bounding-box rejection exists; there is no persistent browser R-tree in these paths.
- `buildRoiList()` and its renderer variants clear and recreate list contents. `currentRoiListEntries()` also computes/sorts collection metadata. Selection should update a few rows, not require a collection rebuild.
- `draw()` around line 4773 schedules or draws many unrelated overlays, the navigator, status, scales and multi-view content. Label placement uses collision testing against already placed labels. Pointer-only changes should not enter that whole pipeline.

**Consequence:** an R-tree helps dense cell collections, but does little to accelerate filling or clipping one 100,000-vertex tissue polygon. Rendering, selection and editing need different indexes and caches.

### Brush, Wand and Caps Lock

- [inst/viewer/performance.js](../../inst/viewer/performance.js), `wsiAnnotationRecords()` around line 354, scans ROI and imported-layer records and filters them by bounds. `wsiFinishBrush()` around line 499 sends nearby candidates, not blindly every annotation. It also checks slide identity and source versions before accepting results.
- [inst/viewer/geometry-worker.js](../../inst/viewer/geometry-worker.js), `geometryClip()` around line 4, converts complete polygons to scaled integer point objects, constructs a map of original vertices, runs JavaScript Clipper with strict/simple and collinear-preserving options, and reconstructs nested output coordinates.
- `geometryStroke()` constructs circles at sampled points plus connecting polygons, then unions them. This introduces substantial intermediate geometry before the actual tissue edit.
- `geometryEdit()` around line 90 and `geometryClaim()` around line 139 union the full selected feature with the stroke. Caps Lock then intersects and differences each overlapping neighbor, performing two Boolean operations on a neighbor to establish and apply a change.
- The worker retains sources, but removes those outside the current active candidate list. Revisiting an area can retransmit previously used large features. The wand claim route also sends candidate geometry again.
- After the worker returns, the browser rebuilds changed geometry, materializes imported layer records, records undo, rebuilds the list, updates controls, schedules synchronization and redraws. Worker time is not the full perceived edit latency.

**Consequence:** moving work off the main thread was necessary, but the current worker still does too much work per stroke. Caps Lock multiplies the cost because both the selected region and its neighbors change.

### Undo, R Sync and Saving

- [inst/viewer/performance.js](../../inst/viewer/performance.js), `wsiSnapshotRoi()` and `wsiAnnotationSnapshot()` around lines 570-592, reuse unchanged geometry snapshots but still visit all ROIs and copy trajectory state. This is better than repeatedly deep-copying every vertex, but is not a changed-feature undo log.
- [inst/viewer/sync.js](../../inst/viewer/sync.js), `wsiSyncCapture()` around line 22 and `wsiSyncMessage()` around line 56, already implement revisioned patches. Routine annotation events still construct a complete feature collection representation and compare before/after maps across it. Changed patches also include the full feature ID order. Other event types can request full snapshots.
- [R/viewer-sync.R](../../R/viewer-sync.R) applies patches, but still performs collection-level assembly. Optimizing only WebSocket transport leaves this cost in place.
- [R/viewer.R](../../R/viewer.R), `annotationExportFeatures()` and `annotationExportPayload()` around lines 3959-3968, create full export structures and pretty-printed JSON on the UI thread. Payload construction precedes location selection in the export route.

**Consequence:** sending a patch is not enough if discovering that patch requires rebuilding or comparing the whole collection. Export should be a streamed snapshot operation after pending edits are committed, not a large synchronous UI operation.

There is also a correctness risk to test during this redesign: imported layer items, promoted editable ROIs and export/sync records have different lifecycles. A single canonical feature store should make it impossible for an annotation to remain visible but be omitted from a save, reappear from a stale source, or be updated under another slide's ID. This audit identifies that architectural risk; it does not assert that every route currently loses data.

## Measured Alternatives

### Parsing, Copying and Packed Coordinates

| Operation | Tissue | Cells |
|---|---:|---:|
| JavaScript JSON parse | 67 ms | 619 ms |
| JavaScript JSON stringify, compact | 28 ms | 220 ms |
| Structured clone of parsed object | 175 ms | 1,604 ms |
| Send nested polygon objects to a Node worker and receive acknowledgement | 171 ms | 1,760 ms |
| Pack coordinates and offsets | 12 ms | 57 ms |
| Copy coordinate buffer to worker and acknowledge | 4.0 ms | 17.2 ms |
| Transfer ownership of already prepared buffer and acknowledge | 0.055 ms | 0.143 ms |
| Packed Float64 geometry and Uint32 offsets | 13.22 MB | 63.10 MB |

The transfer measurement excludes packing and the copy needed when the sender wants to retain ownership. It is not an end-to-end zero-copy promise. These worker measurements use Node's V8 workers, not Safari/WebKit. Transfer detaches the sender's ArrayBuffer; transferring an index's own storage would invalidate that index unless ownership is deliberately moved. [Transferable objects](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Transferable_objects).

Approximate V8 heap increases while retaining source text and parsed objects were 71.7 MB for tissue and 380.1 MB for cells. These are not directly equivalent to the packed geometry sizes, which exclude properties, indexes, GPU data and retained text. They nevertheless show why storing multiple nested coordinate layouts is expensive. Peak process RSS in the results includes several benchmark alternatives and temporary allocations and must not be presented as the viewer's normal memory requirement.

### Spatial Queries

Average time for a 2,048 x 2,048 slide-pixel bounding-box query over the cell annotations:

| Method | Query time | Initial build |
|---|---:|---:|
| JavaScript object-array linear scan | 1.971 ms | None beyond boxes |
| Packed TypedArray linear scan | 0.349 ms | Pack boxes |
| Existing WASM linear scan, boxes already resident | 0.427 ms | WASM initialization and upload |
| RBush | 0.132 ms | 143 ms |
| Flatbush | 0.0195 ms | 47.6 ms |

The queried regions returned about 1,623 candidate features on average. For 512-pixel windows returning about 113 candidates, Flatbush averaged 0.0044 ms and RBush 0.017 ms. Results were checked against the same brute-force feature IDs. The packed Flatbush index occupied 8.48 MB for 220,704 boxes.

At a full-dataset query, all features must be returned; the advantage narrows or reverses. Output allocation differs too: the existing WASM benchmark returns a view over resident memory, whereas several alternatives allocate result arrays. With only 359 tissue features, all bounding-box approaches were already very cheap. **An index is a major dense-cell optimization, not the explanation for the 15-second tissue edit.**

Use Flatbush for immutable imported features plus RBush for changed/new features and a tombstone set masking replaced/deleted baseline records. RBush supports individual updates; Flatbush is a compact static index. Rebuild the baseline occasionally off-thread rather than on every change. [RBush](https://github.com/mourner/rbush), [Flatbush](https://github.com/mourner/flatbush).

### Polygon Editing

Test: a 160 x 160 pixel square touching a real feature boundary. Current `geometryClip()` was executed in an isolated worker in the same JavaScript realm as its inputs. The prototype used Clipper2/WASM 0.4.0.

| Feature size | Operation | Current JS geometry path | WASM prototype, import + operation + packed output |
|---|---|---:|---:|
| 117,669 vertices | Union | 14,857 ms, one completed run | 43.3 ms |
| 117,669 vertices | Difference | Exceeded 15,000 ms cutoff | 42.4 ms |
| 1,994 vertices | Union | 7.50 ms | 0.50 ms |
| 1,994 vertices | Difference | 9.02 ms | 0.46 ms |
| 339 vertices | Union | 2.38 ms | 0.19 ms |
| 339 vertices | Difference | 2.17 ms | 0.16 ms |

The comparison changes both the implementation and the representation: older JavaScript Clipper versus C++ Clipper2 compiled to WASM; 1/4096 pixel versus 0.0001 pixel precision; nested object output versus packed arrays. It does **not** isolate the benefit of the WASM runtime alone. The largest completed union differed by about 0.22 square pixels in total area across a region of roughly 94.7 million square pixels. Smaller completed tests also passed the stated relative area tolerance. Area agreement does not prove equivalent holes, connectivity, boundary location or validity. The timed-out difference has no current-JS reference result.

This evidence justifies a prototype integration behind a feature flag, followed by topology tests. It does not justify removing geometry safeguards or advertising the kernel ratio as viewer speed. Test a native build of the same algorithm before assuming Rust/Tauri IPC improves it further. [Clipper2/WASM](https://github.com/ErikSom/Clipper2-WASM), [Clipper2 documentation](https://www.angusj.com/clipper2/Docs/Overview.htm).

### geojson-vt, Raw Tiles and MVT

The tested **geojson-vt 5.0.2 does expose `getTileRaw()`**. It returns cached flat typed coordinate arrays; `getTile()` constructs nested coordinate pairs. Raw arrays belong to the index and are read-only to callers. Dynamic drilling creates finer tiles on demand, but the package does not provide a general transactional feature-edit API or application-wide byte-budgeted cache manager. [geojson-vt API](https://github.com/mapbox/geojson-vt).

| Tissue test | No simplification | Tolerance 3 |
|---|---:|---:|
| Initial tile index build | 147 ms | 131 ms |
| Root tile vertex count | 825,698 | 14,968 |
| Root cached legacy `getTile()` conversion | 22.52 ms | 0.158 ms |
| Representative z=3 tile vertices | 26,783 | 1,753 |
| z=3 MVT bytes | 53,976 | 4,152 |
| z=3 equivalent tile JSON bytes | 301,631 | 20,444 |
| z=3 MVT encode / decode to ring objects | 3.40 / 3.90 ms | 0.60 / 0.61 ms |

Repeated raw cached lookup was below 0.001 ms in these batches. That measures obtaining a reference, **not rendering, triangulation, encoding, copying or a cold tile build**. A first finer tile lookup was around 1.3 ms. Visiting a region increased retained tile count from 33 to 185; this is not a bounded-memory guarantee.

Default simplification discarded substantial detail in this example. It is not appropriate as a silent fix given the user's repeated request for unsimplified tissue boundaries. Keep full canonical geometry and make any optional display approximation explicitly controlled and independently validated.

The standard library projects longitude/latitude into a normalized Mercator plane. The test used a padded inverse-Mercator adapter solely to benchmark the unmodified package with WSI data; the source files were not changed. Production should use an explicit Cartesian adapter or a maintained planar tiler rather than feeding pixel values into longitude/latitude APIs. [Projection implementation](https://github.com/mapbox/geojson-vt/blob/v5.0.2/src/convert.js).

MVT encodes integer tile-local coordinates, so it can carry a WSI render grid with a custom decoder/manifest. It is not the canonical editing format: quantization, clipped edges and tile fragments must not replace original geometry. At world span S and extent E, quantization spacing is S/(2^z E) slide pixels. Setting simplification tolerance to zero does not remove this rounding. The `z` values here are tiler levels, not objective magnifications. [MVT specification](https://mapbox.github.io/vector-tile-spec/).

### FlatGeobuf and R Caches

Indexed FlatGeobuf was generated with GDAL through sf. Coordinates were kept in the original Cartesian pixel frame with no geographic CRS. Measurements include full R feature construction on reading, not merely bytes from disk.

| Operation | Tissue, all 382 features | Cells |
|---|---:|---:|
| Read original GeoJSON through sf | 801 ms | 4,687 ms |
| Write indexed FlatGeobuf after reading | 62 ms | 1,406 ms |
| Read full FlatGeobuf through sf | 36 ms | 688 ms |
| Query central 2,048-pixel window | 4 ms | 7 ms |
| Features / vertices returned by query | 4 / 51,314 | 1,901 / 35,913 |
| FlatGeobuf file size | 13.42 MB | 83.99 MB |

Separately, the existing tissue import took 828 ms in jsonlite plus 3,621 ms in `wsi_roi_from_geojson()`. Saving its normalized R object to compressed RDS took 3,003 ms, and reading it back took 1,528 ms. sf geometry and a complete wsi ROI object have different metadata layouts, so these are not interchangeable end-to-end timings. Metadata round-trip preservation must be validated before a cache replacement.

The JavaScript FlatGeobuf writer used in another microbenchmark produces an **unindexed** file; those results measure binary serialization only and must not be cited as spatial-query results. The GDAL measurements above are indexed. Local warm file reads do not establish HTTP Range or cold network performance.

FlatGeobuf is a good immutable baseline for exact coordinates and spatial access, but does not support general in-place feature modification. It returns whole matching features: the small tissue window above still retrieved 51,314 vertices. Large tissue polygons therefore need render fragments or a finer part/segment index in addition to a feature index. Keep unknown original GeoJSON properties in a metadata sidecar if the chosen writer does not preserve them exactly. [FlatGeobuf](https://flatgeobuf.org/), [GDAL FlatGeobuf driver](https://gdal.org/en/stable/drivers/vector/flatgeobuf.html).

### Canvas, SVG and WebGL

Actual 359 tissue polygon features, 825,698 vertices, 1,024 x 768 canvas, DPR 1, transparent fill only, no geometric simplification:

| Renderer | Initial preparation | Repeated frame work measured |
|---|---:|---:|
| Recreate Canvas paths every draw | 2.7 ms setup, path construction included in frames | 71.7 ms median submit/readback |
| Cached Canvas `Path2D` | 51.1 ms | 19.9 ms median submit/readback |
| SVG with cached path elements | 108.9 ms | 0.2 ms transform/layout; about 33.7 ms including two animation frames |
| WebGL with cached triangulated mesh | 635.3 ms, including 363.8 ms triangulation/expansion | Below timer resolution median submit/flush; about 33.5 ms including two animation frames |

Canvas flush used one-pixel readback, SVG used layout and animation frames, and WebGL used `finish()`. These are not equivalent hardware timing fences. **A recorded zero does not mean zero GPU work, and the two-frame values are not FPS measurements.**

The cached Canvas and rebuilt Canvas screenshots had identical nonblank pixel counts. SVG and WebGL were visually inspected as similar tissue shapes, but differed in coverage; the simple earcut-based WebGL prototype is not an exact topology/rasterization reference. Full fills with holes, outlines, adjacent boundaries and overlaps need more testing. [Earcut](https://github.com/mapbox/earcut).

This experiment argues against blanket claims that SVG is always slow: a few hundred cached paths can transform well. It does not establish that 220,704 interactive SVG nodes would perform well. Canvas remains a practical fallback; WebGL offers a promising steady-state path if triangulation is cached and moved off the UI thread.

## Comparison of Practical Options

Ratings describe the proposed integration, not universal properties. Low/medium/high complexity is relative to the present architecture.

| Approach | Rendering / loading | Edit latency | Memory | Complexity | Browser + Tauri |
|---|---|---|---|---|---|
| geojson-vt dynamic tiling, `getTileRaw()` | Fast cached delivery; avoids pair-object expansion; cold tiling still costs | Whole-index rebuild is poor; use immutable baseline plus edited-feature overlay | Full input/index and generated tile cache can grow | Medium-high: planar adapter, IDs, invalidation | JS works in both; package projection must be adapted |
| MVT/PBF | Compact transport; decode/mesh work remains | Do not edit quantized fragments; regenerate dirty tiles | Smaller transfer/cache, plus decoded geometry/meshes | High if adding complete pipeline | Both with custom WSI tile transform |
| MVT + PMTiles | Good immutable distribution/reopen with range access | Poor primary edit store; use separate delta store, compact archive later | Bounded directory/tile cache possible | High: archive build, Range support, revisions | Browser HTTP Range and Tauri local/range route |
| FlatGeobuf | Fast exact indexed baseline reads | Not an in-place editor; overlay changed records | Partial reads avoid full geometry residency; conversion has a peak | Medium with GDAL already available, otherwise higher | Browser reader and R/native reader; serve binary correctly |
| RBush | Fast candidate lookup; does not draw polygons | Cheap insert/remove after edit | Object-tree overhead; less compact than packed index | Low-medium | Both JS runtimes |
| Flatbush | Very fast packed baseline queries | Rebuild for baseline changes; combine with RBush delta | Compact contiguous index | Low-medium | Both; buffer can be transferred |
| Native R-tree or existing native grid | Fast R-side spatial queries; avoids shipping irrelevant data | Fast candidate lookup; geometry operations still required | Native packed structures can be efficient | Medium; existing grid already helps | Via common R/HTTP service in both |
| WSI grid postings, feature/part IDs per tile | Cheap viewport discovery; reuse image pyramid addressing | Update postings only for moved/changed parts | References duplicate across intersected tiles; huge bboxes need special handling | Medium | Both, no geographic assumptions |
| Clipped render fragments per WSI tile | Avoids drawing an entire giant polygon for a small viewport | Invalidate intersecting edit tiles, not whole document | Fragment duplication/gutters; bounded cache required | High for holes, seams and outline ownership | Both Canvas/GPU consumers |
| Changed-feature / dirty-tile transactions | Biggest systemic redraw reduction | Local work for add/edit/delete, atomic neighbor updates | Small deltas; bounded history | Medium-high but central to maintainability | Shared architecture across both |
| Web Workers | Removes parse/clip/tessellation stalls from UI | Does not itself shorten slow algorithms; queue and cancellation needed | Can double memory if sending object graphs | Low-medium; already partly implemented | Both; feature-detect worker canvas support |
| Packed TypedArrays and transferables | Avoid repeated object allocation; quick handoff | Changed ranges/owned buffers instead of full clones | Much more predictable; still metadata and GPU copies | Medium | Both; detached-buffer ownership is explicit |
| WASM geometry kernel | Helps clipping/tessellation, not automatically I/O or draw calls | Strong prototype results for Clipper2; validate topology | Linear memory + input/output staging; free native allocations | Medium-high; build/test/licensing | Both; no native installer dependency |
| Rust/Tauri-side geometry service | Useful for local file indexing, mmap and compute | IPC can offset kernel savings; benchmark same algorithm | Efficient native store, but browser still needs render data | High: packaging, IPC, cross-platform parity | Directly Tauri only; shared service/WASM needed for R browser |
| Simplification and precomputed LODs | Reduces drawn vertices substantially | Never edit/export reduced geometry | Several levels cost storage | Medium-high with topology preservation | Both; optional, not default for tissue fidelity |
| Cached Canvas paths / raster render tiles | Good incremental fallback; tiles avoid repeated full path fill | Redraw affected cache entries only | Raster textures depend on viewport/DPR/cache budget | Medium; Path2D already implemented | Broad support in both |
| SVG paths | Good cached transforms for small/moderate feature counts | Convenient selected-boundary interaction; huge `d` updates expensive | DOM and path parsing grow with count/size | Low for selected objects, poor fit for millions of nodes | Both; retain for handles/selected overlays if useful |
| WebGL mesh/border batches | Low repeated CPU submission; exact high-detail view possible | Retessellate changed features/chunks only | GPU buffers, indices, antialias/stroke data | High; picking, holes, seams, device recovery | Both with Canvas fallback |
| Visible/adjacent tile cache | Fast revisit/pan after warm-up | Must invalidate edited areas immediately | Byte-bounded LRU; reserve edit working memory | Medium | Both; no preload-everything policy |
| Binary baseline plus journal | Fast reopen, avoids repeated JSON parsing | Append bounded transactions; export separately | Bounded hot state and periodic checkpoints | Medium-high | R-backed file journal/SQLite; browser storage fallback |

## Coordinate and Data Contracts

All registration, hit testing, editing, analysis and export must use **original slide pixels**, not canvas pixels, geographic coordinates or the current zoom level.

Use an explicit coordinate-frame record: image fingerprint, slide width/height, origin, pixel units, axis convention and any import transform. Preserve negative or outside-image source coordinates if present; clipping to the tissue image must be an explicit edit policy, not an import accident. Display flips/rotation are viewport transforms, not edits to the saved geometry unless deliberately applied.

Use Float64 canonical coordinates. For GPU delivery, subtract a tile/viewport origin before converting to Float32. Preserve stable string feature IDs and polygon/ring offsets; dictionary-code class labels separately from geometry. Avoid sending UUIDs as lossy floating-point numeric IDs. Retain holes, MultiPolygon membership, class colors, locks, arbitrary source properties and separate objects sharing a class name.

At high zoom, load exact source geometry for visible regions. At low zoom, preserve full-detail tissue appearance by caching its rendering, rather than simplifying it implicitly. Optional LOD should be based on an explicit screen-error budget, not a hard feature-count cutoff. Scientific calculations and export must always use the canonical geometry. Rasterizing a temporary display tile does not mean converting the annotation into a mask or changing its editable source.

## Proposed Architecture

### 1. One Canonical Feature Store

Each annotation is identified by `(image_id, annotation_id)` and a revision. Metadata is separate from packed geometry. The view, annotation list, hit testing, R analyses and export refer to this same record rather than maintaining independent layer/ROI copies.

Start with packed in-memory geometry for moderate files. For larger datasets, create an immutable indexed baseline, preferably FlatGeobuf if an existing binary reader/writer can preserve the required data; otherwise a documented chunked binary geometry cache. Preserve the original file unchanged. Do not introduce a private format without a schema version, round-trip tests and migration policy.

A sparse edit store contains replacements, additions and tombstones. The logical document is the baseline plus this store. Flatbush indexes the baseline, RBush indexes changed records, and filtering removes stale baseline versions. Compact off-thread when the delta becomes large; swap the new baseline atomically.

For a very large individual polygon, also index its parts or edge blocks. A feature-level bbox alone cannot isolate a small edit to a 117,669-vertex region.

### 2. Separate Rendering From Editing

Keep microscopy image tiles in the current OpenSeadragon pipeline, including original-resolution close-zoom tiles. Annotation rendering has its own cache and invalidation rules.

Use three independently scheduled visual layers:

- Completed annotations, cached by feature revision and render-tile identity.
- Selection, labels and editing handles, updated without rebuilding completed geometry.
- Pointer and unfinished brush/wand preview, updated immediately and independently of R.

Begin with existing Path2D caching plus offscreen Canvas render tiles. Add cached GPU meshes when the steady-state benefit outweighs preparation and implementation costs. Rendering workers can return packed mesh buffers or ImageBitmaps; OffscreenCanvas availability must be checked in the specific WebView. [OffscreenCanvas](https://developer.mozilla.org/en-US/docs/Web/API/OffscreenCanvas).

A render tile contains references back to feature IDs and revisions. It is disposable. Do not use clipped render fragments as the geometry for saving, cell assignment or future edits. Avoid artificial outlines along clipping boundaries; use gutters and an explicit distinction between original boundary edges and tile-cut edges.

### 3. Atomic Local Edit Transactions

A brush operation should be a command containing slide ID, target ID, base revision, stroke samples/radius and editing policy. The geometry worker owns cached inputs. Small commands cross the thread boundary; repeated full polygon graphs do not.

For Caps Lock, compute the actual allowed claim after protecting locked regions. In one transaction, expand the selected feature and subtract the same claim from affected unlocked neighbors. Store every changed neighbor and deletion in the undo record. Selection or zoom changes must not change the operation's target after it is submitted.

Prefer a clipping result with changed/no-change information over separately computing intersection and difference, but benchmark this: difference can be more expensive than a cheap rejection for a non-intersecting bbox candidate. Add ring/segment rejection or prepared geometry before expensive clipping where appropriate.

Generate a clean swept brush region using stroke buffering or incremental local union, rather than blindly unioning many overlapping circles. Preserve the requested physical brush footprint and cap style. For giant features, introduce local edge-block processing only with robust reconstruction at edit-window boundaries; naively clipping and stitching rings risks holes and seams.

While computation finishes, show the local preview immediately. Do not claim an edit has been saved or synchronized until its revision is acknowledged. Queue dependent strokes or coalesce compatible unsent strokes; never silently discard a stroke because another one is running.

### 4. Dirty Features and Dirty Tiles

On commit, update only changed feature records, their bounds/index entries, their list rows, affected statistics and render cache entries.

For a local brush/Caps Lock claim, the changed display area is the claim's spatial support plus stroke/antialias gutter. It is usually much smaller than the target polygon's full bounding box. Prefer this exact dirty region. For deletion, whole-feature transforms or class/color changes, invalidate the union of old/new affected feature extents or all render references to that feature. Update every cached level that covers the change, including overview tiles.

Tile keys need source/image fingerprints, coordinate-frame ID, schema/render version, tile address and relevant revisions. Never key solely by a basename, layer label or reused session route. Accept asynchronous tile/worker results only when their identity and revision still match. Keep existing visible tiles until valid replacements arrive; do not blank all annotations while loading one viewport.

Each multi-view pane has its own viewport, pending requests and render scheduling. Geometry and immutable render resources may be shared for duplicate images, but pointer movement or zoom in pane A should not redraw unrelated pane B. A feature edit invalidates all panes showing that feature, including duplicate views of the same tissue.

### 5. Exact R Sync and Analysis

Create the synchronization patch directly from the transaction: changed IDs, geometry revisions, metadata updates and deletions. Do not rediscover it by comparing two complete feature collections. Send ordering only when order actually changes. Keep viewport and selection messages small.

Use acknowledgement, replay and occasional full recovery snapshots. Coalesce obsolete viewport updates while preserving edit order. R stores the same slide/feature/revision identifiers, applies only changed records, and invalidates association/proximity/prediction results only when relevant inputs changed. Analyses must declare the annotation revision they used. This avoids fast display with silently stale R results.

Browser and Tauri must use the same protocol and geometry semantics. Native acceleration can be behind that protocol. A Rust service may help local indexing or persistence, but a native-only command cannot serve an ordinary R-launched browser without a shared local service or equivalent WASM implementation. Avoid passing huge JSON strings through Tauri IPC; binary responses are available when appropriate. [Tauri Rust commands](https://v2.tauri.app/develop/calling-rust/).

### 6. Efficient Persistence

Save an append-only transaction log or a SQLite-backed edit store alongside the binary baseline. Each transaction includes its slide and base revision, affected IDs, replacement geometry/metadata, tombstones and enough inverse information for undo. Small strokes may be recorded too, but do not rely only on replaying operations through a future, potentially different clipping version.

For the R-backed desktop/browser workflow, use the local service for persistence. For an offline browser, IndexedDB or OPFS can hold a recoverable cache where supported, but browser storage is not a substitute for an explicit exported project. Record durable-save acknowledgement separately from visual commit and R synchronization.

Checkpoint in the background, with a temporary file and atomic manifest swap. Handle crash recovery and one-writer coordination; SQLite WAL has writer/checkpoint constraints, so its presence alone does not solve application concurrency. [SQLite WAL](https://sqlite.org/wal.html).

For GeoJSON export, first request the save destination from the user gesture, wait for pending edits, pin a consistent revision, and stream features from baseline plus edits. Serialize incrementally in a worker or R/native service. Do not pretty-print or build one enormous string on the UI thread. Preserve GeoJSON interoperability and existing export choices.

PMTiles is an optional **published rendering cache**, not the working annotation document. Its read-only archive design suits distribution and range access; edits should live elsewhere until archive regeneration/compaction. A pixel-grid manifest/custom renderer is still required because common PMTiles map consumers assume geographic tiling. The current `wsi_http_file_response()` in [R/dynamic-tiles.R](../../R/dynamic-tiles.R), around line 1747, reads a whole file and has no Range argument. A dedicated verified 206/Range route would be needed before using this helper for partial archive delivery. [PMTiles concepts](https://docs.protomaps.com/pmtiles/).

## Quick Wins and Implementation Order

| Priority | Change | Expected benefit and validation |
|---|---|---|
| 1 | Instrument parse, normalize, path creation, worker queue/compute, main-thread commit, redraw, sync and save separately | Establish which phase the user's real stroke is waiting for; include p50/p95 and vertex/candidate counts |
| 2 | Integrate the faster Boolean kernel behind a tested adapter | Largest measured editing opportunity; compare exact geometry semantics, locks, holes, overlap removal and undo before enabling |
| 3 | Keep worker geometry resident using a byte-budgeted cache and revision IDs | Avoid repeated graph cloning on brush/wand operations and revisits |
| 4 | Pass changed IDs directly into undo, list updates, render invalidation and sync | Avoid collection-wide work after a local change; undo must restore selected and reduced neighboring regions atomically |
| 5 | Virtualize annotation list; update selection/class rows in place | Remove DOM recreation and collection sorting from pointer interactions |
| 6 | Add browser Flatbush/RBush candidate indexes and cached bounds/areas | Faster dense selection; small tissue-file benefit alone is limited, as measurements show |
| 7 | Cache completed annotation display separately from labels, cursor and active stroke | Reduce redraw work without simplifying source boundaries; retain existing Path2D optimization |
| 8 | Binary normalized cache with manifest and partial reads | Faster large-file reopen, less repeated JSON/R list conversion; retain metadata fidelity |
| 9 | Stream exports and journal autosaves | Save cost scales with edits until explicit full export, without blocking the UI |
| 10 | Add planar render tiles/GPU mesh cache if frame measurements still miss targets | Improve large visible regions and multi-view; do not make this a prerequisite for fixing editing |

Avoid introducing all formats at once. A practical first implementation is **resident worker + faster clipping + changed-feature transactions + targeted redraw**. Next add **Flatbush/RBush and an indexed binary baseline**. MVT/PMTiles is valuable later for distribution or truly out-of-core viewing, but is not the quickest route to responsive Caps Lock editing.

For a more ambitious later design, a vector edit-expression graph can make a claim immediately representable as `target union stroke` and `neighbor minus stroke`, with lazy materialization. This is closer to a vector paint engine. It avoids eagerly rebuilding giant outlines but complicates selection, label ownership, export, analysis and long edit chains. Bound and checkpoint the expression depth. Prefer the measured simpler kernel/transaction improvements first.

## Acceptance Tests Before Release

1. Use real colorectal tissue, dense colorectal cells and the user's breast tissue annotations. Include a small-file control, a giant single polygon with holes, and large multi-slide collections.
2. Separate cold import, warm reopen, first image tile, first complete annotation display and first editable selection. Respect the user's requirement that the viewer not present an empty workspace before the image is ready; use the launcher's progress state while initializing.
3. Brush add/subtract, Caps Lock claims against several neighbors, wand add/subtract, create, delete, undo/redo and repeat strokes. Compare boundary displacement, area, holes, components and pairwise overlap, not just screenshots or total area.
4. Verify no lost annotations, stale layer copies or precision changes after save/reopen and GeoJSON round trip. Keep all classes, IDs, colors and unknown properties. Check imported overlapping annotations follow an explicit policy rather than silently changing on import.
5. Measure input-to-visible-feedback, pointer-up-to-committed-geometry, worker time, main-thread commit, R acknowledgement, peak RSS/JS heap/GPU cache estimates, bytes copied and cache hit rates. Use at least 30 representative edit trials for latency percentiles.
6. Pan/zoom repeatedly in one and four panes, including duplicate views of the same image. Check original-resolution image tiles, annotation alignment, stable boundaries, and no blank overlay during replacement.
7. Test point selection and spot/cell association at exactly the exported geometry revision. Run proximity and gene display after edits and after changing slides.
8. Test ordinary Chrome/Firefox/Safari browser launching and Tauri WebView on macOS, Windows and Linux. Check OffscreenCanvas/worker/WASM fallbacks and memory-constrained machines. Do not infer WebKit performance from Node or Chrome measurements.
9. Disconnect R, reconnect, refresh, interrupt an export and crash during a checkpoint. Confirm revision recovery, overwrite confirmation, durable-save reporting and complete export of committed edits.

Suggested targets, not achieved claims: cursor/tool feedback below 50 ms p95; cached navigation work within a 16-33 ms frame budget; common local edits below 200 ms p95 after geometry is resident; no multi-second main-thread task during parsing/commit/export; bounded caches that do not grow with every visited tile. Report giant-polygon and dense-cell results separately.

## Reproducibility and Unmeasured Work

Scripts and pinned dependency versions: [annotation-audit benchmark directory](../../tools/benchmarks/annotation-audit/README.md). Raw results: [results](../../tools/benchmarks/annotation-audit/results/). Inputs remain outside the artifacts; source hashes identify them.

Benchmarked: JavaScript parsing/copying, worker transfer, RBush/Flatbush/current WASM bbox scanning, current JS clipping versus Clipper2/WASM, geojson-vt raw/legacy tile lookup and simplification, MVT encoding/decoding, FlatGeobuf local reads/writes, current R normalization/RDS costs, and browser Canvas/SVG/WebGL fill rendering.

Not benchmarked here: complete PMTiles archive creation/range delivery; native Rust/C++ service versus the same WASM kernel; a production dirty-tile editor; robust local polygon stitching; full-cell SVG rendering; browser WebKit end-to-end editing; multi-view R/Tauri latency; cold disk/network performance; full property/topology equivalence of binary formats and GPU triangulation. These remain explicit validation tasks, not assumed speedups.
