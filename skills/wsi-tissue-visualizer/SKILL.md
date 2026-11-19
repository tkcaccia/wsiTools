---
name: wsi-tissue-visualizer
description: Build and review a focused, read-only whole-slide tissue viewer or figure from a slide and optional level-0 GeoJSON using wsiTools. Use for pathology visualization deliverables; do not use for annotation editing or scoring.
---

# wsiTools tissue visualizer

Use the bundled builder to make a static, read-only viewer. The default uses wsiTools to create local Deep Zoom tiles. Source-only mode uses a bounded overview plus browser range reads from an approved remote TIFF/COG, so the publishable bundle contains no full-slide image tiles. Keep the original slide, analysis mask, metrics, and exact scored geometry untouched.

## Build

Requires R with `wsiTools` 0.1.28 or newer, libvips, and a browser. From this skill directory:

```bash
Rscript scripts/build_viewer.R --slide /absolute/path/slide.svs --geojson /absolute/path/regions.geojson --out /absolute/path/tissue-viewer --title "Tissue review"
```

Omit `--geojson` for an image-only viewer. Use `--tile-format png` when lossless image tiles are required; JPEG at quality 92 is the default. The builder refuses to replace an existing output unless `--force` is supplied, and then only replaces a viewer made by this builder. It removes wsiTools' tile-cache metadata from the publishable bundle because that file records the source slide's absolute path.

To inspect locally, serve the *parent* of the output directory, for example `python3 -m http.server 8000 --directory /absolute/path`, then open `http://localhost:8000/tissue-viewer/`. Static hosts must publish `index.html`, `viewer.js`, `viewer.css`, `manifest.json`, and the whole `tiles/` directory together. The interface uses the OpenSeadragon 5.0.1 browser script, as the current wsiTools viewer does; pin or self-host that dependency if the destination requires offline use.

Before publishing, run `python3 scripts/verify_bundle.py /absolute/path/tissue-viewer`. This checks the DZI dimensions, top-resolution tiles, overlay feature count, and expected files.

## Source-only publication

Use this mode only when the original TIFF is publicly range readable and its publication has been approved. The local `--slide` file must be byte-identical to the remote source. Create a source identity JSON file with this shape (replace every example value):

```json
{
  "schema_version": 1,
  "remote_tiff_url": "https://example.org/approved-slide.tif",
  "source_sha256": "64 lowercase hex characters from sha256sum of the local TIFF",
  "width": 100000, "height": 80000, "mpp_x": 0.25, "mpp_y": 0.25,
  "coordinate_system": "level0_pixel_xy",
  "registration": {"annotation_to_source_affine": [1, 0, 0, 0, 1, 0]},
  "publish_origin": "https://example.org",
  "publication_approved": true, "preview_approved": true,
  "annotation_approved": true, "mask_approved": true,
  "landmarks": [
    {"name": "landmark A", "annotation_xy": [1000, 1500], "source_xy": [1000, 1500]},
    {"name": "landmark B", "annotation_xy": [5000, 4000], "source_xy": [5000, 4000]}
  ]
}
```

The builder verifies the **local** SHA256, dimensions, two bounded HTTP 206 ranges against the local TIFF, response length, TIFF marker, and CORS for `publish_origin`. It records `remote_sha256_verified: false`: it does not hash the whole remote TIFF in a browser. Do not use a URL with embedded credentials or query tokens. The current builder requires an explicit identity registration; transform annotations to source level-0 pixels before running it.

```bash
Rscript scripts/build_viewer.R \
  --slide /absolute/path/approved-slide.tif \
  --remote-tiff-url https://example.org/approved-slide.tif \
  --source-identity-manifest /absolute/path/source-identity.json \
  --geojson /absolute/path/exact.geojson \
  --out /absolute/path/tissue-viewer \
  --qa-report /absolute/path/tissue-qa \
  --preview-max-px 1600 --rgb-resample bilinear \
  --label-resample nearest --show-tile-progress

python3 scripts/verify_bundle.py /absolute/path/tissue-viewer \
  --qa-report /absolute/path/tissue-qa
```

Source-only mode requires R packages `magick` and `digest`, Python Playwright, and Chrome or Chromium for mandatory browser QA. Its QA report contains fit, medium, close, and narrow screenshots plus SHA256 hashes, two declared coordinate landmark checks, responsive-control checks, and source-detail/fallback counters. Review the screenshots to judge biological registration; the automated landmark check verifies the declared coordinate transform, not tissue semantics. The builder refuses to install a bundle if QA fails.

Optional `--display-tissue-mask FILE.png --fragment-navigation` adds navigation to disconnected tissue components. `FILE.png.json` must record `coordinate_system: "preview_pixel_xy"`, `source_sha256`, `source_width`, `source_height`, `width`, `height`, and `mask_sha256`; the PNG must match the generated overview dimensions. Mask pixels are sampled with nearest-neighbor only. This display mask is separate from any analysis or scored mask.

Optional `--contours display-smoothed --max-displacement-um N` makes a separate Chaikin-smoothed contour layer. The exact GeoJSON remains downloadable byte-for-byte. The builder rejects smoothing that changes polygon validity, ring/component counts, class identity, or exceeds the Hausdorff displacement bound in microns. The user can switch between exact and display contours. Do not use the smoothed layer for scoring.

Source-only hosting must publish every file in the output directory and retain access to the approved TIFF URL with CORS and byte ranges. The viewer uses the same pinned OpenSeadragon browser script as local mode and a bundled GeoTIFF.js browser library. Avoid publishing a source-only bundle under an origin different from `publish_origin` without rerunning the source check and QA.

For a standalone PNG, use `Rscript scripts/render_figure.R --slide /absolute/path/slide.svs --geojson /absolute/path/regions.geojson --out /absolute/path/figure.png`. This makes an overview up to 2400 pixels on its long side. For a close view, add `--x X --y Y --width W --height H` in level-0 pixels and `--pixels 3000`. The command needs the optional `magick` R package, reads only the needed region or thumbnail through wsiTools, draws exact polygons with a restrained fill, and adds a scale bar when microns-per-pixel metadata exists. Give the figure a caption identifying the classes and whether the overlay is a model prediction or annotation.

## Fidelity and review

- GeoJSON coordinates must be **level-0 slide pixels**, as in `wsi_viewer_roi()`. If coordinates use another system, determine and apply an explicit registration transform before building; do not guess from the rendered image.
- The builder copies GeoJSON bytes without changing polygons. If an overview needs simplification, provide a separate, labeled display layer and retain access to exact geometry. Never describe display filtering as a change in scored results.
- Check the result in a browser at fit, medium zoom, and close zoom. Compare at least two distinctive tissue landmarks against polygon edges; check a narrow viewport, controls, legend, tile loading, and browser console. Correct registration or visible rendering failures before delivery.
- For a still PNG, prefer `render_figure.R` with an intentional crop and pixel size. Browser screenshots and the screenshot action in `wsi_viewer_roi()` are useful when the exact interactive viewport or richer layers are required. Do not upscale a thumbnail and call it full resolution.
- Large GeoJSON still downloads and parses as one file in this compact viewer. For dense annotation sets, use wsiTools' live viewport-based dense annotation source or prepare a tiled/vector LOD pipeline; do not silently drop features to make a page load.

The compact viewer is visualization-only: it has pan, zoom, fit, class visibility, overlay opacity, outlines, and a physical scale bar when slide metadata contains microns per pixel. It does not expose wsiTools' annotation, analysis, or scoring controls. For those tasks, use `wsi_viewer_roi()` or `wsi_viewer_live()` directly.

Source APIs: `R/viewer.R` (`wsi_viewer()`, `wsi_viewer_roi()`), `R/roi.R` (`wsi_read_geojson()`), and `docs/architecture.md` in the wsiTools repository.
