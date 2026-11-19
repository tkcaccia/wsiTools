---
name: wsi-tissue-visualizer
description: Build and review a focused, read-only whole-slide tissue viewer or figure from a slide and optional level-0 GeoJSON using wsiTools. Use for pathology visualization deliverables; do not use for annotation editing or scoring.
---

# wsiTools tissue visualizer

Use the bundled builder to make a static, full-resolution viewer. It calls the installed `wsiTools` R package to create Deep Zoom tiles, then places the tiles in a compact read-only interface. The output can be hosted as static files or inspected locally over HTTP. Keep the original slide and GeoJSON untouched.

## Build

Requires R with `wsiTools` 0.1.28 or newer, libvips, and a browser. From this skill directory:

```bash
Rscript scripts/build_viewer.R --slide /absolute/path/slide.svs --geojson /absolute/path/regions.geojson --out /absolute/path/tissue-viewer --title "Tissue review"
```

Omit `--geojson` for an image-only viewer. Use `--tile-format png` when lossless image tiles are required; JPEG at quality 92 is the default. The builder refuses to replace an existing output unless `--force` is supplied, and then only replaces a viewer made by this builder. It removes wsiTools' tile-cache metadata from the publishable bundle because that file records the source slide's absolute path.

To inspect locally, serve the *parent* of the output directory, for example `python3 -m http.server 8000 --directory /absolute/path`, then open `http://localhost:8000/tissue-viewer/`. Static hosts must publish `index.html`, `viewer.js`, `viewer.css`, `manifest.json`, and the whole `tiles/` directory together. The interface uses the OpenSeadragon 5.0.1 browser script, as the current wsiTools viewer does; pin or self-host that dependency if the destination requires offline use.

Before publishing, run `python3 scripts/verify_bundle.py /absolute/path/tissue-viewer`. This checks the DZI dimensions, top-resolution tiles, overlay feature count, and expected files.

For a standalone PNG, use `Rscript scripts/render_figure.R --slide /absolute/path/slide.svs --geojson /absolute/path/regions.geojson --out /absolute/path/figure.png`. This makes an overview up to 2400 pixels on its long side. For a close view, add `--x X --y Y --width W --height H` in level-0 pixels and `--pixels 3000`. The command needs the optional `magick` R package, reads only the needed region or thumbnail through wsiTools, draws exact polygons with a restrained fill, and adds a scale bar when microns-per-pixel metadata exists. Give the figure a caption identifying the classes and whether the overlay is a model prediction or annotation.

## Fidelity and review

- GeoJSON coordinates must be **level-0 slide pixels**, as in `wsi_viewer_roi()`. If coordinates use another system, determine and apply an explicit registration transform before building; do not guess from the rendered image.
- The builder copies GeoJSON bytes without changing polygons. If an overview needs simplification, provide a separate, labeled display layer and retain access to exact geometry. Never describe display filtering as a change in scored results.
- Check the result in a browser at fit, medium zoom, and close zoom. Compare at least two distinctive tissue landmarks against polygon edges; check a narrow viewport, controls, legend, tile loading, and browser console. Correct registration or visible rendering failures before delivery.
- For a still PNG, prefer `render_figure.R` with an intentional crop and pixel size. Browser screenshots and the screenshot action in `wsi_viewer_roi()` are useful when the exact interactive viewport or richer layers are required. Do not upscale a thumbnail and call it full resolution.
- Large GeoJSON still downloads and parses as one file in this compact viewer. For dense annotation sets, use wsiTools' live viewport-based dense annotation source or prepare a tiled/vector LOD pipeline; do not silently drop features to make a page load.

The compact viewer is visualization-only: it has pan, zoom, fit, class visibility, overlay opacity, outlines, and a physical scale bar when slide metadata contains microns per pixel. It does not expose wsiTools' annotation, analysis, or scoring controls. For those tasks, use `wsi_viewer_roi()` or `wsi_viewer_live()` directly.

Source APIs: `R/viewer.R` (`wsi_viewer()`, `wsi_viewer_roi()`), `R/roi.R` (`wsi_read_geojson()`), and `docs/architecture.md` in the wsiTools repository.
