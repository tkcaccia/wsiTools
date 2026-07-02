# Desktop Downloads

This page is for users who want to launch wsiTools through a desktop
application instead of starting from R code.

The desktop app is a lightweight Tauri launcher. It opens a project or image
selection window, starts `Rscript` in the background, and displays the live
wsiTools viewer. The R package still performs image access, live
synchronization, dynamic tiles, annotations, and analysis.

## Download Prebuilt Installers

Prebuilt desktop installers are attached to the GitHub release:

[Download wsiTools Desktop 0.1.1](https://github.com/tkcaccia/wsiTools/releases/tag/desktop-v0.1.1)

| Platform | Asset | Notes |
| --- | --- | --- |
| macOS Apple Silicon | `wsiTools-Desktop_0.1.1_macos_aarch64.dmg` | For Apple Silicon Macs. Unsigned builds may need right-click -> Open. |
| Windows x64 | `wsiTools-Desktop_0.1.1_windows_x64-setup.exe` | NSIS installer for Windows 10/11 x64. |
| Ubuntu/Debian x64 | `wsiTools-Desktop_0.1.1_linux_amd64.deb` | Install with `sudo apt install ./file.deb`. |
| Fedora/RHEL-style x86_64 | `wsiTools-Desktop_0.1.1_linux_x86_64.rpm` | Install with your RPM package manager. |
| All platforms | `SHA256SUMS.txt` | Optional checksum verification. |

## Required Runtime Dependencies

The desktop app does not bundle R or the optional WSI backends. Install these
before using the app:

1. Install R from CRAN: <https://cran.r-project.org/>
2. Install the wsiTools R package:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE
)

library(wsiTools)
wsi_backends()
wsi_diagnose(live_test = FALSE)
```

3. Install optional backends only if your workflow needs them:

| Tool | Needed for |
| --- | --- |
| libvips | fast conversion, pyramids, Deep Zoom tiles, OME-TIFF export |
| OpenSlide | SVS, NDPI, SCN, MRXS, and other WSI formats supported by OpenSlide |
| native CZI / Bio-Formats | CZI and broad microscopy format support |
| ImageMagick | fallback previews for ordinary image formats |
| StarDist / Mesmer | optional cell segmentation workflows |

See the [backend setup guide](backends.md) for installation commands and
platform-specific notes.

## First Launch Checklist

After installing the desktop app:

1. Open the app.
2. The app checks for `Rscript` using `WSITOOLS_RSCRIPT`, the environment
   `PATH`, `R_HOME`, and common R installation folders.
3. If R is found, choose **Create new project** or **Open project**.
4. Add one or more microscopy images.
5. Click **Run R** to start the live viewer.
6. Keep the R process running while the viewer is open.

If the app cannot find R, set the `WSITOOLS_RSCRIPT` environment variable to
the full path of `Rscript`, or use the **Download R** button shown by the app.

macOS/Linux:

```sh
export WSITOOLS_RSCRIPT="/path/to/Rscript"
```

Windows PowerShell:

```powershell
$env:WSITOOLS_RSCRIPT = "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
```

## Source Builds

If you want to compile the desktop app yourself, use the
[Tauri build guide](tauri-build.md). Building from source requires Node.js,
Rust, and platform-specific desktop build tools.

## Troubleshooting

If the desktop app opens but the viewer is blank:

- Confirm that `Rscript` is available.
- Confirm that `library(wsiTools)` works in R.
- Run `wsi_backends()` and `wsi_diagnose(live_test = FALSE)`.
- Check whether optional backends such as OpenSlide or libvips are installed
  for the image format you are opening.
- See [troubleshooting](troubleshooting.md) for common errors.
