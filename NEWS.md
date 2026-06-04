# wsiTools 0.1.23

- Restored optional StarDist and Mesmer selected-ROI segmentation workflows.
- Added mask import support in the interactive viewer.
- Improved live viewer documentation, including setup, synchronization, and troubleshooting notes.
- Added installed copy-paste scripts for common live viewer workflows under `inst/examples/`.
- Made backend-opening and command-line backend failures more actionable by reporting what failed, which backend was tried, how to check availability, and copyable setup/fix commands.
- Added `wsi_start()` as a read-only first-run checklist for package, backend, live-viewer, and next-command guidance.
- Added `wsi_open_viewer()` as a one-command entry point that opens an image with automatic backend, static/live, tiled/thumbnail, and browser-launch choices.
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
