# Compile the Tauri Desktop App

wsiTools includes an optional Tauri desktop launcher in
`tools/wsiToolsDesktop`. The desktop app is intended for users who prefer a file
picker and desktop window instead of typing R commands.

The desktop app does not replace the R package. It starts `Rscript`, asks
wsiTools to open a live tiled viewer, and embeds the resulting
`http://127.0.0.1:<port>` viewer inside the Tauri window. R still owns image
reading, annotation synchronization, dynamic tiles, and optional backends.

## Repository Layout

```text
tools/wsiToolsDesktop/
  package.json              # npm scripts and JavaScript dependencies
  src/                      # Vite frontend
  src-tauri/                # Rust/Tauri application
  src-tauri/tauri.conf.json # bundle configuration
```

The most important commands are:

```sh
cd tools/wsiToolsDesktop
npm install
npm run dev
npm run build
```

`npm run dev` opens the development app. `npm run build` creates platform
installers/bundles under `tools/wsiToolsDesktop/src-tauri/target/release/bundle`.

## Before Building

Install and test the R package first:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("tkcaccia/wsiTools", upgrade = "never", build_vignettes = FALSE)

library(wsiTools)
wsi_backends()
wsi_diagnose(live_test = FALSE)
```

The desktop app needs `Rscript`. If it is not on `PATH`, set
`WSITOOLS_RSCRIPT` before launching or building the app.

macOS/Linux:

```sh
export WSITOOLS_RSCRIPT="/Library/Frameworks/R.framework/Resources/bin/Rscript"
```

Windows PowerShell:

```powershell
$env:WSITOOLS_RSCRIPT = "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
```

Optional WSI backends such as OpenSlide, libvips, native CZI, Bio-Formats,
StarDist, and Mesmer remain runtime capabilities. They are not bundled into the
Tauri app.

## macOS Build

### 1. Install system tools

Install Xcode command line tools:

```sh
xcode-select --install
```

Install Homebrew if needed, then install Node.js:

```sh
brew install node
```

Install Rust:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source "$HOME/.cargo/env"
```

Check:

```sh
node --version
npm --version
cargo --version
rustc --version
Rscript --version
```

### 2. Build the app

```sh
cd /path/to/wsiTools/tools/wsiToolsDesktop
npm install
npm run build
```

The macOS app bundle and disk image are written under:

```text
tools/wsiToolsDesktop/src-tauri/target/release/bundle/
```

Typical outputs include `.app` and `.dmg` files.

### 3. Run in development mode

```sh
cd /path/to/wsiTools/tools/wsiToolsDesktop
npm run dev
```

If the app opens but cannot start R, set `WSITOOLS_RSCRIPT` to the full
`Rscript` path. GUI apps on macOS often do not inherit the same `PATH` as your
terminal.

### macOS notes

- Unsigned builds may be blocked by Gatekeeper on other Macs.
- For local testing, right-click the app and choose **Open** if macOS blocks it.
- For distribution to other users, Apple Developer signing and notarization are
  recommended.
- Large Rust/Tauri builds can use several GB of disk space under
  `src-tauri/target`.

## Windows Build

### 1. Install prerequisites

Install:

- R for Windows.
- Rtools matching your R version, so wsiTools can compile from source.
- Node.js LTS.
- Rust through `rustup`.
- Microsoft C++ Build Tools / Visual Studio Build Tools with the Desktop C++
  workload.
- Microsoft Edge WebView2 Runtime, usually already present on Windows 10/11.

Check from PowerShell:

```powershell
Rscript --version
node --version
npm --version
cargo --version
rustc --version
```

If `Rscript` is not found:

```powershell
$env:WSITOOLS_RSCRIPT = "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
```

### 2. Install wsiTools

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE,
  INSTALL_opts = "--no-multiarch"
)

library(wsiTools)
wsi_backends()
wsi_diagnose(live_test = FALSE)
```

If installation fails because of `00LOCK-wsiTools`, close all R sessions and
delete the `00LOCK-wsiTools` folder from your R library directory.

### 3. Build the app

```powershell
cd C:\path\to\wsiTools\tools\wsiToolsDesktop
npm install
npm run build
```

The Windows installers are written under:

```text
tools\wsiToolsDesktop\src-tauri\target\release\bundle\
```

Typical outputs include `.msi` and `.exe` installers.

### Windows notes

- Do not use spaces or non-ASCII characters in the repository path while
  debugging build problems.
- If `npm run build` cannot find Rust, restart PowerShell after installing
  Rust or run the Rust environment script suggested by `rustup`.
- If the app starts but cannot open the viewer, check the app log and confirm
  that `Rscript` can run `library(wsiTools)` from PowerShell.
- Code signing is recommended before distributing installers broadly.

## Linux Build

These commands are for Ubuntu/Debian. Tauri v2 uses WebKitGTK 4.1 on Linux.

### 1. Install system packages

```sh
sudo apt update
sudo apt install -y \
  libwebkit2gtk-4.1-dev \
  build-essential \
  curl \
  wget \
  file \
  libxdo-dev \
  libssl-dev \
  libayatana-appindicator3-dev \
  librsvg2-dev
```

Install Node.js and Rust. One simple route is:

```sh
sudo apt install -y nodejs npm
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source "$HOME/.cargo/env"
```

Check:

```sh
Rscript --version
node --version
npm --version
cargo --version
rustc --version
pkg-config --modversion webkit2gtk-4.1
```

### 2. Build the app

```sh
cd /path/to/wsiTools/tools/wsiToolsDesktop
npm install
npm run build
```

The Linux bundles are written under:

```text
tools/wsiToolsDesktop/src-tauri/target/release/bundle/
```

Typical outputs include `.deb`, `.rpm`, and/or AppImage-style artifacts,
depending on the platform and Tauri configuration.

### Linux notes

- If `webkit2gtk-4.1` is missing, the Rust build will fail before the app is
  created.
- GUI apps may not inherit shell startup files. If R is not found, set
  `WSITOOLS_RSCRIPT` in the environment used to launch the app.
- On remote desktops, the Tauri app must be launched inside the graphical
  session, not only through a headless SSH shell.

## Cleaning Build Space

Tauri/Rust build folders can become large. To clean compiled artifacts:

```sh
cd tools/wsiToolsDesktop/src-tauri
cargo clean
```

To remove JavaScript dependencies:

```sh
cd tools/wsiToolsDesktop
rm -rf node_modules
```

On Windows PowerShell:

```powershell
cd tools\wsiToolsDesktop\src-tauri
cargo clean

cd ..
Remove-Item -Recurse -Force node_modules
```

## Troubleshooting

### `npm: command not found`

Install Node.js and restart the terminal.

macOS:

```sh
brew install node
```

Ubuntu:

```sh
sudo apt install nodejs npm
```

Windows: install Node.js LTS from the official installer.

### `cargo metadata` or `cargo: command not found`

Install Rust with `rustup`, then restart the terminal or run:

```sh
source "$HOME/.cargo/env"
```

### `No space left on device`

Free disk space or clean old Rust build artifacts:

```sh
cd tools/wsiToolsDesktop/src-tauri
cargo clean
```

### The app opens but the viewer is white

Check the app log and confirm that R can open a live viewer outside Tauri:

```r
library(wsiTools)
viewer <- wsi_open_viewer("sample.svs", live = "yes", tiled = "yes")
```

Also check:

```r
wsi_backends()
wsi_diagnose(live_test = FALSE)
```

### The app cannot find R

Set `WSITOOLS_RSCRIPT` to the full `Rscript` path before launching the app.

### The selected image preview changes but the main viewer shows an old image

Stop the running R process from the app, close the app, and restart it. During
development, run:

```sh
npm run dev
```

again so Tauri reloads the current frontend and Rust code.

## Distribution Checklist

Before sharing the desktop app with another user:

1. Build on the same operating system you want to distribute to.
2. Install and test wsiTools in R on that machine.
3. Run `wsi_backends()` and confirm required image backends are available.
4. Test opening a small image and one real WSI.
5. Confirm the app log can be copied.
6. Sign/notarize the app or installer if distributing outside your own machine.

## References

- Tauri v2 prerequisites: <https://v2.tauri.app/start/prerequisites/>
- Tauri distribution documentation: <https://v2.tauri.app/distribute/>
- wsiTools desktop overview: [Desktop App](desktop-app.md)
