//! Configure WebKitGTK before GTK or any worker threads are initialized.
//!
//! DMA-BUF presentation can fail before the first window is painted on Linux
//! (KMS CREATE_DUMB / GBM permission errors; WebKit bug 254807). Use WebKit's
//! compatible presentation path by default, including on non-NVIDIA systems.
//! GPU vendor detection is insufficient: this also depends on the compositor,
//! driver, device permissions and sandbox. This does not disable WebGL or change
//! the external Chromium viewer's hardware acceleration policy.

use std::{ffi::{CStr, OsStr, OsString}, path::{Path, PathBuf}};

const DMABUF_SETTING: &str = "WEBKIT_DISABLE_DMABUF_RENDERER";
const EGL_FILES: &str = "__EGL_VENDOR_LIBRARY_FILENAMES";
const EGL_DIRS: &str = "__EGL_VENDOR_LIBRARY_DIRS";

fn egl_override(existing_files: Option<&OsStr>, existing_dirs: Option<&OsStr>,
                wayland: bool, vendor: Option<&str>, manifest: Option<&Path>) -> Option<OsString> {
    if existing_files.is_some() || existing_dirs.is_some() || wayland || vendor != Some("NVIDIA Corporation") {
        return None;
    }
    manifest.map(|path| path.as_os_str().to_owned())
}

fn x11_vendor() -> Option<String> {
    if std::env::var_os("DISPLAY").is_none() { return None; }
    // Load at runtime so Wayland/software-only installations do not acquire
    // new mandatory GLX link dependencies. Close only the display opened here.
    unsafe {
        let x11 = libloading::Library::new("libX11.so.6").ok()?;
        let glx = libloading::Library::new("libGLX.so.0").ok()?;
        let open: libloading::Symbol<unsafe extern "C" fn(*const std::os::raw::c_char) -> *mut std::ffi::c_void> = x11.get(b"XOpenDisplay\0").ok()?;
        let close: libloading::Symbol<unsafe extern "C" fn(*mut std::ffi::c_void) -> i32> = x11.get(b"XCloseDisplay\0").ok()?;
        let screen: libloading::Symbol<unsafe extern "C" fn(*mut std::ffi::c_void) -> i32> = x11.get(b"XDefaultScreen\0").ok()?;
        let query: libloading::Symbol<unsafe extern "C" fn(*mut std::ffi::c_void, i32, i32) -> *const std::os::raw::c_char> = glx.get(b"glXQueryServerString\0").ok()?;
        let display = open(std::ptr::null());
        if display.is_null() { return None; }
        let raw = query(display, screen(display), 1); // GLX_VENDOR
        let vendor = if raw.is_null() { None } else { Some(CStr::from_ptr(raw).to_string_lossy().into_owned()) };
        close(display);
        vendor
    }
}

fn nvidia_manifest() -> Option<PathBuf> {
    for directory in ["/usr/share/glvnd/egl_vendor.d", "/etc/glvnd/egl_vendor.d"] {
        let Ok(entries) = std::fs::read_dir(directory) else { continue; };
        let mut paths: Vec<_> = entries.flatten().map(|entry| entry.path()).collect();
        paths.sort();
        for path in paths {
            if path.extension() != Some(OsStr::new("json")) { continue; }
            let Some(value) = std::fs::read(&path).ok().and_then(|bytes| serde_json::from_slice::<serde_json::Value>(&bytes).ok()) else { continue; };
            let Some(library) = value.pointer("/ICD/library_path").and_then(|value| value.as_str()) else { continue; };
            let name = Path::new(library).file_name().and_then(|name| name.to_str()).unwrap_or("");
            if name.starts_with("libEGL_nvidia.so") && unsafe { libloading::Library::new(library) }.is_ok() {
                return Some(path);
            }
        }
    }
    None
}

fn default_dmabuf_setting(existing: Option<&OsStr>) -> Option<&'static str> {
    // Preserve even empty/non-Unicode explicit settings rather than guessing
    // what the user or system administrator intended.
    existing.is_none().then_some("1")
}

pub fn configure_before_webkit() {
    let files = std::env::var_os(EGL_FILES);
    let dirs = std::env::var_os(EGL_DIRS);
    let wayland = std::env::var_os("WAYLAND_DISPLAY").is_some() ||
        std::env::var("XDG_SESSION_TYPE").is_ok_and(|value| value == "wayland");
    if files.is_none() && dirs.is_none() && !wayland {
        let vendor = x11_vendor();
        if vendor.as_deref() == Some("NVIDIA Corporation") {
            let manifest = nvidia_manifest();
            if let Some(value) = egl_override(None, None, false, vendor.as_deref(), manifest.as_deref()) {
                // GLVND otherwise probes Mesa too, which can emit DRI2 errors
                // on a proprietary NVIDIA X screen even when NVIDIA works.
                std::env::set_var(EGL_FILES, value);
            }
        }
    }
    let existing = std::env::var_os(DMABUF_SETTING);
    if let Some(value) = default_dmabuf_setting(existing.as_deref()) {
        // Called once on the main thread, before Tauri starts GTK or workers.
        std::env::set_var(DMABUF_SETTING, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn egl_selection_requires_nvidia_x11_and_a_usable_manifest() {
        let path = Path::new("/vendor/nvidia.json");
        assert_eq!(egl_override(None, None, false, Some("NVIDIA Corporation"), Some(path)), Some(path.as_os_str().to_owned()));
        assert_eq!(egl_override(None, None, false, Some("Mesa"), Some(path)), None);
        assert_eq!(egl_override(None, None, false, None, Some(path)), None);
        assert_eq!(egl_override(None, None, true, Some("NVIDIA Corporation"), Some(path)), None);
        assert_eq!(egl_override(None, None, false, Some("NVIDIA Corporation"), None), None);
    }

    #[test]
    fn egl_selection_preserves_explicit_settings_including_empty_values() {
        for value in ["", "/custom/vendor.json"] {
            assert_eq!(egl_override(Some(OsStr::new(value)), None, false, Some("NVIDIA Corporation"), Some(Path::new("nvidia.json"))), None);
            assert_eq!(egl_override(None, Some(OsStr::new(value)), false, Some("NVIDIA Corporation"), Some(Path::new("nvidia.json"))), None);
        }
    }

    #[test]
    fn clean_install_uses_compatible_presentation() {
        assert_eq!(default_dmabuf_setting(None), Some("1"));
    }

    #[test]
    fn explicit_webkit_settings_are_preserved() {
        for value in ["0", "1", "", "custom"] {
            assert_eq!(default_dmabuf_setting(Some(OsStr::new(value))), None);
        }
    }

    #[test]
    fn non_unicode_setting_is_preserved() {
        use std::os::unix::ffi::OsStrExt;
        assert_eq!(default_dmabuf_setting(Some(OsStr::from_bytes(b"\xff"))), None);
    }

    #[test]
    fn startup_configures_fresh_process_and_preserves_overrides() {
        // Separate processes exercise the real environment without racing other
        // tests that may start threads or modify environment variables.
        for value in [None, Some("0"), Some("1"), Some("")] {
            let mut child = std::process::Command::new(std::env::current_exe().unwrap());
            child
                .args(["linux_webkit_startup_child_probe", "--nocapture"])
                .env("WSITOOLS_TEST_WEBKIT_EXPECTED", value.unwrap_or("1"))
                .env_remove(DMABUF_SETTING);
            if let Some(value) = value {
                child.env(DMABUF_SETTING, value);
            }
            let result = child.output().unwrap();
            assert!(result.status.success(), "{}", String::from_utf8_lossy(&result.stderr));
        }
    }

    #[test]
    fn linux_webkit_startup_child_probe() {
        let Some(expected) = std::env::var_os("WSITOOLS_TEST_WEBKIT_EXPECTED") else {
            return;
        };
        configure_before_webkit();
        assert_eq!(std::env::var_os(DMABUF_SETTING), Some(expected.clone()));
        configure_before_webkit();
        assert_eq!(std::env::var_os(DMABUF_SETTING), Some(expected));
    }
}
