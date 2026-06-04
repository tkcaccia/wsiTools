# wsiTools 0.1.23

- Restored optional StarDist and Mesmer selected-ROI segmentation workflows.
- Added mask import support in the interactive viewer.
- Improved live viewer documentation, including setup, synchronization, and troubleshooting notes.
- Added installed copy-paste scripts for common live viewer workflows under `inst/examples/`.
- Made backend-opening and command-line backend failures more actionable by reporting what failed, which backend was tried, how to check availability, and copyable setup/fix commands.
- Added `wsi_start()` as a read-only first-run checklist for package, backend, live-viewer, and next-command guidance.
- Added `wsi_open_viewer()` as a one-command entry point that opens an image with automatic backend, static/live, tiled/thumbnail, and browser-launch choices.
