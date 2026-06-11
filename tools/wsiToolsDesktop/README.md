# wsiTools Desktop

`wsiTools Desktop` is an optional Tauri launcher for users who do not want to
work directly in the R console. It keeps the existing R package architecture:
R still opens the image, starts the live `httpuv` bridge, serves dynamic tiles
when needed, and receives viewer edits. Tauri only provides the desktop window,
file picker, process management, and embedded viewer frame.

## What Happens

1. The user clicks **Select image**.
2. The Tauri app starts `Rscript`.
3. R runs `wsi_open_viewer(..., live = "yes", tiled = "yes")`.
4. R prints the generated live viewer URL.
5. Tauri opens that viewer inside the app window.
6. The R process stays alive so annotations, measurements, tiles, and other
   live events remain synchronized with R.

This does not load whole-slide images into R memory. The viewer still uses
precomputed tiles or the dynamic tile server, depending on the image and
available backends.

## Requirements

- R installed and available as `Rscript`.
- The R package installed:

```r
remotes::install_github("tkcaccia/wsiTools", upgrade = "never")
```

- Optional but strongly recommended R packages:

```r
install.packages(c("httpuv", "magick"))
```

- Optional runtime backends depending on image format:
  - libvips for fast tiled viewing and conversion.
  - OpenSlide for many pathology WSI formats.
  - native CZI or Bio-Formats for CZI files.

Check from R:

```r
library(wsiTools)
wsi_backends()
wsi_diagnose(live_test = FALSE)
```

If R is not on `PATH`, set:

```sh
export WSITOOLS_RSCRIPT="/full/path/to/Rscript"
```

On Windows PowerShell:

```powershell
$env:WSITOOLS_RSCRIPT = "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
```

## Development

Install Node.js, Rust, and the Tauri prerequisites for your operating system.
Then run:

```sh
cd tools/wsiToolsDesktop
npm install
npm run dev
```

The desktop UI is bundled with Vite. If clicking buttons does nothing, restart
`npm run dev` so Tauri reloads the bundled frontend rather than an older static
window.

Build installers:

```sh
npm run build
```

## Design Notes

The app deliberately does not reimplement image reading in Rust. All WSI logic
stays in R/wsiTools:

- `wsi_open_viewer()` selects the appropriate viewer route.
- `wsi_viewer_live()` starts live synchronization.
- `httpuv` handles browser-to-R events.
- OpenSeadragon still receives tiled image sources.
- OpenSlide, libvips, native CZI, and Bio-Formats remain optional runtime
  capabilities.

This keeps the desktop wrapper small and avoids duplicating backend logic.
