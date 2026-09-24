# wsiTools 0.1.24

- Smoothed magic-wand boundaries and removed newly created tiny interior-hole
  artifacts after both additive and subtractive wand edits. Holes already
  present in the annotation and newly created medium or large holes remain.

- Dense vector annotations now remain visible at every zoom. Distant views use
  a bounded spatial sample with lightweight bounds, while closer views request
  progressively detailed local polygons without changing the stored geometry.
- wsiTools Desktop 0.1.9 includes the all-zoom dense-annotation overview in
  both browser- and Tauri-launched live viewers.

- Fixed the New ROI category control so selecting a category deselects the
  current annotation and arms a separate Brush, Draw, or Magic Wand ROI while
  preserving the active tool.
- Linux select controls now keep dark option and selected-option backgrounds,
  avoiding unreadable white text on a light native dropdown.
- wsiTools Desktop 0.1.8 opens Linux live viewers in an installed Chrome or
  Chromium app window using supported browser hardware acceleration. It no
  longer passes unsafe WebGPU, forced Vulkan, or GPU-blocklist override flags,
  and it verifies that Chrome remains alive before closing the WebKitGTK
  loading window. OpenSeadragon WebGL is the stable GPU-backed default;
  experimental WebGPU composition is opt-in.
- The Linux dependency plan now includes Ubuntu canberra GTK modules, documents
  `.deb`, AppImage, source, R-only, macOS and Windows installation routes, and
  explains WebKitGTK crash, black-screen and GTK-module diagnostics.
- Added `wsi_desktop_dependency_plan()`,
  `wsi_install_desktop_dependencies()` and `wsi_has_webkitgtk()` so Linux users
  can distinguish the required Tauri WebKitGTK 4.1 starter runtime from
  optional image backends and the optional Chrome/Chromium WebGPU route.
- The optional WebGPU base compositor now consumes OpenSeadragon
  `tile-loaded` image data and initializes before the tiled viewer. It no
  longer depends on the unsupported WebGL `tile-drawn` event, so loaded tiles
  can actually reach the GPU compositor.
- Magic Wand edits are now confined to an adjustable local screen-space reach
  (256 px by default) around the click. Wand contours are simplified to at most
  768 vertices, Boolean edits run in the geometry worker, and undo retains at
  most 10 actions within a 64-MiB estimated geometry budget.
- Hold Ctrl and double-click to select a tissue annotation without painting or
  running the Wand. Brush/Wand stays active; this also works in multi-view panes.
- Magic Wand selections now fill internal holes while retaining the outer
  boundary. This applies to vector and editable-mask annotations, including
  Alt/Command subtraction; existing class and locked-region protection remains.
- Local tissue edits now clip only intersecting polygon components and holes.
  Unchanged rings use reference patches between the geometry worker and viewer;
  unchanged component paths, hit-test indexes and undo snapshots are reused.
  Original GeoJSON detail is preserved at every zoom.
- Fixed an intermittent tile-worker startup race by publishing its readiness
  descriptor only after the complete file has been written.
- Added a bundled Clipper2 WebAssembly annotation-editing kernel with a
  JavaScript fallback. Repeated edits reuse worker-resident geometry and use
  transferable coordinate buffers. Original slide-pixel geometry and holes
  are retained; this does not convert tissue annotations into masks.
- Brush and priority-Wand edits now use changed-feature undo transactions,
  targeted annotation-list updates and explicit R synchronization patches.
  Caps Lock edits restore the selected region and trimmed neighbours together.
- Added editable TIFF/OME-TIFF tissue annotation masks. Supply
  `annotation_masks` to a live viewer, or select a TIFF mask as the tissue
  annotation in wsiTools Desktop. The source mask remains lazily tiled while
  Brush and Magic Wand write only sparse raster paint/erase tiles, avoiding
  polygon conversion and repeated geometry Boolean operations.
- TIFF mask legends are read from `<mask>_labels.csv`, `<mask>.labels.csv`, or
  an explicit data frame/CSV. Black mask background is transparent and legend
  classes remain individually selectable.
- The desktop starter now keeps image-preparation progress in the starter and
  opens the separate viewer only after R returns a display-ready image URL. It
  no longer exposes an empty viewer progress window.
- Desktop and live tiled viewers now wait for a decoded first-image preview
  before exposing the viewer URL. The preview remains visible until
  full-resolution tiles replace it. CZI projects prepare only the first scene
  by default, keeping later scenes lazy rather than delaying startup for every
  scene.
- Cached full-detail annotation paths and subtraction holes, with a separate
  lightweight canvas for the cursor and unfinished drawing. Moving the brush
  no longer rebuilds completed annotation paths, labels, and the navigator.
- Brush strokes now use incremental boundary-hit queries during dragging and
  polygon clipping in a browser worker on release. Original coordinates of
  untouched vertices are retained; new intersections use subpixel precision.
  Obsolete results are rejected if the active tissue or geometry changed.
- Added Caps Lock priority editing for Brush and Magic Wand. With Caps Lock
  on, the selected tissue annotation claims the added area and overlapping
  unlocked annotations are trimmed so regions never overlap; locked regions
  remain protected. The complete multi-annotation edit is restored by one
  undo action.
- Caps Lock priority edits now include nearby annotations that are still held
  in the optimized imported-tissue layer, not only already materialized ROIs.
  Only bounding-box candidates near the stroke are sent to the geometry
  worker, the selected and neighboring geometries commit atomically, and mask
  edit cache invalidation is limited to once per animation frame.
- Added revision-acknowledged annotation patches and compact viewport/selection
  synchronization, with full snapshots for initialization and recovery. Undo
  snapshots share unchanged geometry instead of cloning every polygon.
- Fixed zero-time R session polling so getters do not enter an indefinite
  `httpuv` service loop. Multi-view camera callbacks now invalidate individual
  panes, and brush commits avoid duplicate panel rebuilding.
- Fixed bodyless HTTP responses so CORS preflight requests cannot be gzip-
  encoded into invalid 204 responses, and cached tiles return bodyless 304
  responses. This removes intermittent HTTP parsing failures on reused browser
  connections to the R bridge.

- Prevented accidental Brush-to-Wand switches. Magic Wand now uses the
  deliberate `Shift+W` shortcut instead of bare `W`, and the Brush/Wand toolbar
  controls stop pointer propagation before changing tools.

- Imported tissue annotations can now be edited directly with Brush and Magic
  Wand. Selecting an optimized annotation promotes only that region to an
  editable ROI; Brush extends it (or subtracts with `Alt`/`Command`) and Wand
  refines its connected boundary while preserving its identity and category.
  `Alt` + Wand now removes the connected wand region from the selected
  annotation (`Command` is also accepted on macOS), with undo and live-R
  history support.

- Enclosed Brush and Magic Wand subtraction now uses a local geometry update
  instead of rasterizing the full annotation. This substantially reduces edit
  latency on large imported regions while retaining their detailed outer
  boundaries.

- Fixed a JavaScript call-stack overflow when Brush or Wand promoted and edited
  tissue annotations containing tens of thousands of vertices. Annotation
  bounds are now scanned incrementally instead of expanding every coordinate
  into `Math.min()` and `Math.max()` arguments.

- Tissue annotation boundaries now retain every GeoJSON vertex at all zoom
  levels instead of changing to simplified polygons, boxes, or centroids.
  Viewport culling remains active, and dense cell segmentations retain their
  separate close-zoom safeguards.

- Added live trajectory gene-correlation analysis. **Run profile** now matches
  spots/cells along the selected trajectory to the attached spatial object,
  ranks expression features by Spearman or Pearson correlation with trajectory
  position, displays an interactive results table, exports CSV, and exposes the
  complete result through `viewer$get_trajectory_correlations()`.

- Added a compact annotation toolbar above the main menus with Pan, Brush, and
  Magic Wand controls. The wand creates new editable ROIs or refines the
  selected tissue annotation from connected tissue colours, with adjustable
  tolerance, undo, live synchronization, and independent multi-view panes.

- Added the initial optional Tauri Rust/WGPU renderer foundation: a versioned
  live tile manifest, native camera and visible-tile selection, WGPU capability
  diagnostics, and focused native tile math tests. The production viewer stays
  OpenSeadragon while native controls are brought to feature parity.

- Added a shared OpenSeadragon 5 renderer path for direct R and Tauri launches:
  automatic WebGL with Canvas fallback, explicit GPU/CPU overrides, bounded
  per-frame tile composition, and maximum-level tile diagnostics to verify
  full-resolution pixels at close zoom.
- Fixed merged Seurat projects so each spatial image uses its own
  `GetTissueCoordinates()` table while RNA/SCT assays, reductions, clusters,
  metadata, and live gene values are matched and reordered by exact cell ID.
- Updated the desktop project wizard to list only real Seurat image keys and
  their coordinate counts, preventing global `orig.ident` values from being
  mistaken for spatial images.
- Added native coordinate-to-annotation association and spatial-object export
  that preserves registered image coordinates and annotation assignments.
- Improved large-image tile caching, dense annotation viewport loading,
  performance reporting, and CZI tile handling.
- Added a real desktop viewer startup window, native save dialogs with
  overwrite confirmation, and clearer launcher error reporting.

# wsiTools 0.1.23

- Restored optional StarDist and Mesmer selected-ROI segmentation workflows.
- Added mask import support in the interactive viewer.
- Improved live viewer documentation, including setup, synchronization, and troubleshooting notes.
- Added installed copy-paste scripts for common live viewer workflows under `inst/examples/`.
- Made backend-opening and command-line backend failures more actionable by reporting what failed, which backend was tried, how to check availability, and copyable setup/fix commands.
- Added `wsi_start()` as a read-only first-run checklist for package, backend, live-viewer, and next-command guidance.
- Added `wsi_open_viewer()` as a one-command entry point that opens an image with automatic backend, static/live, tiled/thumbnail, and browser-launch choices.
- Made R synchronization the default for Seurat, Giotto, SpatialExperiment, and their Seurat/SpatialExperiment multi-section project viewers; pass `live = FALSE` for static HTML output.
- Added live viewer TIFF export controls for the visible viewport and selected annotation bounding box. The export is written by R/backends through the live bridge, while annotation geometry remains exportable as GeoJSON or project state.
- Added SVG screenshot export alongside PNG screenshots for report, presentation, and publication-ready viewer captures.
- Added an explicit viewer preference save action and broader browser-local preference restore for brush size, annotation labels, preferred tool, display toggles, screenshot format, trajectory width, and related annotation settings.
- Removed the duplicate Project-menu "Open panel" action; the Project panel is reopened from the View menu to reduce toolbar duplication.
- Added an explicit selected-annotation relabel action so existing ROIs can be swapped from one pathology label/class to another without changing the class used for the next annotation.
- Improved History panel visibility controls so the View-menu History button, close button, and double-click minimize behavior match the Project and Annotations side panels.
- Added a troubleshooting Logs panel that retains toast messages, warnings, and browser errors, with copy/download controls and live R access through `viewer$get_logs()` / `<name>_logs`.
- Improved top-toolbar dropdown behavior so annotation, brush, trajectory, measurement, and export menus automatically collapse after the selected action is completed.
- Improved multi-view drag/drop so project images with sections resolve to a displayable section and browser-readable files dropped directly onto a pane are added to the project and opened in that pane.
- Fixed viewer screenshot export to composite the OpenSeadragon tissue canvas/tiles before drawing visible ROIs, trajectories, annotations, and overlays, with a preview fallback instead of silently saving a white image.
- Reorganized the Help dialog so Keyboard Shortcuts and Full Guide open as separate help pages instead of appearing together in one long panel.
