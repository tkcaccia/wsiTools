use base64::{engine::general_purpose, Engine as _};
use serde::{Deserialize, Serialize};
use std::{
    env, fs,
    io::{BufRead, BufReader, Read, Write},
    net::TcpStream,
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    sync::{mpsc, Arc, Mutex},
    thread,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};
use tauri::{path::BaseDirectory, AppHandle, Emitter, Manager, State};
use tauri_plugin_dialog::{DialogExt, MessageDialogButtons, MessageDialogKind};

#[cfg(feature = "native-wgpu")]
mod native_renderer;

const VIEWER_WINDOW_LABEL: &str = "viewer-main";

#[derive(Default)]
struct RViewerState {
    child: Mutex<Option<Child>>,
    native_child: Mutex<Option<Child>>,
    logs: Arc<Mutex<Vec<String>>>,
}

impl Drop for RViewerState {
    fn drop(&mut self) {
        if let Ok(mut guard) = self.child.lock() {
            if let Some(mut child) = guard.take() {
                let _ = child.kill();
                let _ = child.wait();
            }
        }
        if let Ok(mut guard) = self.native_child.lock() {
            if let Some(mut child) = guard.take() {
                let _ = child.kill();
                let _ = child.wait();
            }
        }
    }
}

#[derive(Clone, Debug, Default, Serialize)]
struct ViewerLaunch {
    viewer_url: String,
    html_file: String,
    sync_url: String,
    log_file: String,
    r_pid: u32,
    viewer_engine: String,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct RRuntimeStatus {
    available: bool,
    rscript: String,
    source: String,
    message: String,
    download_url: String,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct NewProjectItem {
    image: String,
    cell_annotation: Option<String>,
    tissue_annotation: Option<String>,
    spatial_data: Option<String>,
    spatial_sample_id: Option<String>,
}

#[derive(Clone, Debug)]
struct ResolvedProjectItem {
    image: PathBuf,
    cell_annotation: Option<PathBuf>,
    tissue_annotation: Option<PathBuf>,
    spatial_data: Option<PathBuf>,
    spatial_sample_id: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SpatialTissueInfo {
    tissue_id: String,
    n_spots: Option<u64>,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SpatialInspection {
    object_type: String,
    file_path: String,
    tissues: Vec<SpatialTissueInfo>,
    message: String,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct SaveFilter {
    name: String,
    extensions: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SavePathSelection {
    path: String,
    overwrite_confirmed: bool,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct NativeRendererStatus {
    available: bool,
    backend: String,
    adapter: String,
    device_type: String,
    message: String,
}

#[tauri::command]
fn native_renderer_diagnostics() -> NativeRendererStatus {
    #[cfg(feature = "native-wgpu")]
    {
        return match native_renderer::probe_wgpu() {
            Ok(status) => NativeRendererStatus {
                available: status.available,
                backend: status.backend,
                adapter: status.adapter,
                device_type: status.device_type,
                message: status.message,
            },
            Err(error) => NativeRendererStatus {
                // A surface-less adapter probe can fail on macOS before winit
                // creates the native window. The child renderer performs the
                // authoritative surface-compatible probe at launch and reports
                // its result through the startup status handshake.
                available: true,
                backend: "launch-probe-deferred".to_string(),
                adapter: String::new(),
                device_type: String::new(),
                message: format!(
                    "Native WGPU is compiled; adapter verification is deferred until its window opens ({error})"
                ),
            },
        };
    }
    #[cfg(not(feature = "native-wgpu"))]
    {
        NativeRendererStatus {
            available: false,
            backend: "not-built".to_string(),
            adapter: String::new(),
            device_type: String::new(),
            message: "This desktop build does not include the optional native WGPU renderer. Rebuild wsiTools Desktop with `--features native-wgpu`.".to_string(),
        }
    }
}

fn confirm_file_replacement(app: &AppHandle, window: &tauri::WebviewWindow, path: &Path) -> bool {
    if !path.exists() {
        return true;
    }
    let name = path
        .file_name()
        .and_then(|value| value.to_str())
        .unwrap_or("the selected file");
    app.dialog()
        .message(format!(
            "A file named \"{name}\" already exists. Do you want to replace it?"
        ))
        .parent(window)
        .title("Replace existing file?")
        .kind(MessageDialogKind::Warning)
        .buttons(MessageDialogButtons::OkCancelCustom(
            "Replace".to_string(),
            "Cancel".to_string(),
        ))
        .blocking_show()
}

fn push_log(logs: &Arc<Mutex<Vec<String>>>, line: impl Into<String>) {
    let mut guard = logs.lock().unwrap();
    guard.push(line.into());
    if guard.len() > 1000 {
        let extra = guard.len() - 1000;
        guard.drain(0..extra);
    }
}

fn emit_viewer_progress(app: &AppHandle, value: &str) {
    let _ = app.emit_to(VIEWER_WINDOW_LABEL, "viewer-progress", value.to_string());
}

fn r_string(value: &str) -> String {
    format!("\"{}\"", value.replace('\\', "\\\\").replace('"', "\\\""))
}

fn stop_existing_child(state: &RViewerState) {
    if let Some(mut child) = state.child.lock().unwrap().take() {
        let _ = child.kill();
        let _ = child.wait();
    }
}

fn stop_existing_native_child(state: &RViewerState) {
    if let Some(mut child) = state.native_child.lock().unwrap().take() {
        let _ = child.kill();
        let _ = child.wait();
    }
}

#[cfg(feature = "native-wgpu")]
fn native_startup_status(path: &Path) -> Option<(String, String)> {
    let value = fs::read(path).ok()?;
    let value = serde_json::from_slice::<serde_json::Value>(&value).ok()?;
    let state = value.get("state")?.as_str()?.trim().to_string();
    let message = value
        .get("message")
        .and_then(|item| item.as_str())
        .unwrap_or("")
        .trim()
        .to_string();
    Some((state, message))
}

#[cfg(feature = "native-wgpu")]
fn native_startup_status_path(app: &AppHandle) -> Result<PathBuf, String> {
    let directory = session_dir(app)?;
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();
    Ok(directory.join(format!("native-wgpu-startup-{stamp}.json")))
}

#[cfg(feature = "native-wgpu")]
fn wait_for_native_startup(
    child: &mut Child,
    status_path: &Path,
    logs: &Arc<Mutex<Vec<String>>>,
) -> Result<(), String> {
    let deadline = Instant::now() + Duration::from_secs(8);
    loop {
        if let Some(status) = child
            .try_wait()
            .map_err(|error| format!("Could not inspect native WGPU viewer process: {error}"))?
        {
            let detail = native_startup_status(status_path)
                .map(|(_, message)| message)
                .filter(|message| !message.is_empty())
                .unwrap_or_else(|| format!("The native process exited with status {status}."));
            return Err(format!("Native Rust/WGPU viewer failed during startup: {detail}"));
        }
        if let Some((state, message)) = native_startup_status(status_path) {
            if state == "ready" {
                push_log(logs, format!("Native Rust/WGPU startup ready: {message}"));
                return Ok(());
            }
            if state == "error" {
                let _ = child.kill();
                let _ = child.wait();
                return Err(format!("Native Rust/WGPU viewer failed during startup: {message}"));
            }
        }
        if Instant::now() >= deadline {
            push_log(
                logs,
                "Native Rust/WGPU process is still starting; it remains connected to the live R session.",
            );
            return Ok(());
        }
        thread::sleep(Duration::from_millis(40));
    }
}

fn pid_file(session_dir: &PathBuf) -> PathBuf {
    session_dir.join("current-r-viewer.pid")
}

fn remove_pid_file(session_dir: &PathBuf) {
    let _ = fs::remove_file(pid_file(session_dir));
}

fn write_pid_file(session_dir: &PathBuf, pid: u32) {
    let _ = fs::write(pid_file(session_dir), pid.to_string());
}

fn kill_process_id(pid: u32) -> bool {
    if pid == 0 {
        return false;
    }
    #[cfg(target_family = "unix")]
    {
        Command::new("kill")
            .arg("-TERM")
            .arg(pid.to_string())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map(|status| status.success())
            .unwrap_or(false)
    }
    #[cfg(target_family = "windows")]
    {
        Command::new("taskkill")
            .args(["/PID", &pid.to_string(), "/T", "/F"])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map(|status| status.success())
            .unwrap_or(false)
    }
}

fn stop_previous_session_pid(session_dir: &PathBuf, logs: &Arc<Mutex<Vec<String>>>) {
    let path = pid_file(session_dir);
    let Ok(text) = fs::read_to_string(&path) else {
        return;
    };
    let Ok(pid) = text.trim().parse::<u32>() else {
        remove_pid_file(session_dir);
        return;
    };
    if kill_process_id(pid) {
        push_log(
            logs,
            format!("Stopped stale R viewer process from previous desktop session: pid {pid}."),
        );
    } else {
        push_log(
            logs,
            format!("Previous R viewer pid {pid} was not running or could not be stopped."),
        );
    }
    remove_pid_file(session_dir);
}

fn clear_legacy_viewer_artifacts(session_dir: &PathBuf, logs: &Arc<Mutex<Vec<String>>>) {
    let legacy_html = session_dir.join("wsiTools_desktop_live_viewer.html");
    let legacy_tiles = session_dir.join("wsiTools_desktop_live_viewer_tiles");
    let mut cleared = Vec::new();

    if legacy_html.exists() && fs::remove_file(&legacy_html).is_ok() {
        cleared.push("shared viewer HTML");
    }
    if legacy_tiles.exists() && fs::remove_dir_all(&legacy_tiles).is_ok() {
        cleared.push("shared legacy tile cache");
    }
    if !cleared.is_empty() {
        push_log(
            logs,
            format!(
                "Cleared {} before opening the new project.",
                cleared.join(" and ")
            ),
        );
    }
}

fn command_works(path: &str) -> bool {
    Command::new(path)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|status| status.success())
        .unwrap_or(false)
}

fn executable_names() -> Vec<&'static str> {
    #[cfg(target_family = "windows")]
    {
        vec!["Rscript.exe", "Rscript"]
    }
    #[cfg(not(target_family = "windows"))]
    {
        vec!["Rscript"]
    }
}

fn backend_path_candidates() -> Vec<PathBuf> {
    let mut out = Vec::new();
    #[cfg(target_os = "macos")]
    {
        out.extend([
            PathBuf::from("/opt/homebrew/bin"),
            PathBuf::from("/opt/homebrew/sbin"),
            PathBuf::from("/usr/local/bin"),
            PathBuf::from("/usr/local/sbin"),
            PathBuf::from("/opt/local/bin"),
            PathBuf::from("/usr/bin"),
            PathBuf::from("/bin"),
            PathBuf::from("/usr/sbin"),
            PathBuf::from("/sbin"),
        ]);
    }
    #[cfg(all(target_family = "unix", not(target_os = "macos")))]
    {
        out.extend([
            PathBuf::from("/usr/local/bin"),
            PathBuf::from("/usr/bin"),
            PathBuf::from("/bin"),
            PathBuf::from("/usr/local/sbin"),
            PathBuf::from("/usr/sbin"),
            PathBuf::from("/sbin"),
            PathBuf::from("/opt/conda/bin"),
            PathBuf::from("/opt/homebrew/bin"),
        ]);
    }
    #[cfg(target_family = "windows")]
    {
        out.extend([
            PathBuf::from(r"C:\Program Files\libvips\bin"),
            PathBuf::from(r"C:\Program Files\openslide-win64\bin"),
            PathBuf::from(r"C:\Program Files\Git\cmd"),
            PathBuf::from(r"C:\rtools44\x86_64-w64-mingw32.static.posix\bin"),
            PathBuf::from(r"C:\rtools44\usr\bin"),
        ]);
        for key in ["CONDA_PREFIX", "MAMBA_ROOT_PREFIX"] {
            if let Ok(root) = env::var(key) {
                let root = PathBuf::from(root);
                out.extend([
                    root.join("Library").join("bin"),
                    root.join("Scripts"),
                    root.join("condabin"),
                ]);
            }
        }
        if let Ok(userprofile) = env::var("USERPROFILE") {
            let userprofile = PathBuf::from(userprofile);
            for root in [
                userprofile.join("miniconda3"),
                userprofile.join("anaconda3"),
                userprofile.join("miniforge3"),
                userprofile.join("mambaforge"),
                userprofile.join("micromamba"),
            ] {
                out.extend([
                    root.join("Library").join("bin"),
                    root.join("Scripts"),
                    root.join("condabin"),
                ]);
            }
        }
    }
    out
}

fn augmented_path_env() -> String {
    let mut entries = Vec::<PathBuf>::new();
    for candidate in backend_path_candidates() {
        if candidate.exists() && !entries.iter().any(|entry| entry == &candidate) {
            entries.push(candidate);
        }
    }
    if let Some(paths) = env::var_os("PATH") {
        for path in env::split_paths(&paths) {
            if !entries.iter().any(|entry| entry == &path) {
                entries.push(path);
            }
        }
    }
    env::join_paths(entries)
        .map(|value| value.to_string_lossy().to_string())
        .unwrap_or_else(|_| env::var("PATH").unwrap_or_default())
}

fn viewer_renderer_env() -> String {
    match env::var("WSITOOLS_VIEWER_RENDERER")
        .unwrap_or_else(|_| "auto".to_string())
        .trim()
        .to_ascii_lowercase()
        .as_str()
    {
        "gpu" => "gpu".to_string(),
        "cpu" => "cpu".to_string(),
        _ => "auto".to_string(),
    }
}

fn viewer_tile_compositor_env(selected_engine: &str) -> String {
    let _ = selected_engine;
    // The browser viewer owns the complete UI. It may use WebGPU when available,
    // and remains fully functional through the OpenSeadragon fallback otherwise.
    "auto".to_string()
}

fn viewer_engine(value: &str) -> String {
    let _ = value;
    // Desktop has one supported viewer path. The browser preserves the full
    // application UI and falls back to OpenSeadragon when WebGPU is absent.
    "browser".to_string()
}

fn path_candidates_from_env() -> Vec<String> {
    let mut out = Vec::new();
    let path_value = augmented_path_env();
    if !path_value.is_empty() {
        let paths = std::ffi::OsString::from(path_value);
        for dir in env::split_paths(&paths) {
            for name in executable_names() {
                out.push(dir.join(name).to_string_lossy().to_string());
            }
        }
    }
    out
}

fn r_home_candidates() -> Vec<String> {
    let mut out = Vec::new();
    if let Ok(base) = env::var("R_HOME") {
        let root = PathBuf::from(base);
        for name in executable_names() {
            out.push(root.join("bin").join(name).to_string_lossy().to_string());
            out.push(
                root.join("bin")
                    .join("x64")
                    .join(name)
                    .to_string_lossy()
                    .to_string(),
            );
        }
    }
    out
}

fn windows_r_candidates() -> Vec<String> {
    let mut out = Vec::new();
    for key in ["ProgramFiles", "ProgramFiles(x86)"] {
        if let Ok(base) = env::var(key) {
            let r_dir = PathBuf::from(base).join("R");
            if let Ok(entries) = fs::read_dir(r_dir) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    out.push(
                        path.join("bin")
                            .join("Rscript.exe")
                            .to_string_lossy()
                            .to_string(),
                    );
                    out.push(
                        path.join("bin")
                            .join("x64")
                            .join("Rscript.exe")
                            .to_string_lossy()
                            .to_string(),
                    );
                }
            }
        }
    }
    out
}

fn rscript_download_url() -> &'static str {
    "https://cran.r-project.org/"
}

fn locate_rscript_detail() -> Result<(String, String), String> {
    if let Ok(path) = env::var("WSITOOLS_RSCRIPT") {
        if command_works(&path) {
            return Ok((path, "WSITOOLS_RSCRIPT".to_string()));
        }
    }

    let mut candidates = Vec::new();
    candidates.extend(path_candidates_from_env());
    candidates.extend(r_home_candidates());
    candidates.extend([
        "/usr/local/bin/Rscript".to_string(),
        "/opt/homebrew/bin/Rscript".to_string(),
        "/Library/Frameworks/R.framework/Resources/bin/Rscript".to_string(),
        "/usr/bin/Rscript".to_string(),
    ]);
    candidates.extend(windows_r_candidates());

    candidates
        .into_iter()
        .find(|path| command_works(path))
        .map(|path| {
            let source = if path_candidates_from_env().iter().any(|candidate| candidate == &path) {
                "PATH"
            } else if r_home_candidates().iter().any(|candidate| candidate == &path) {
                "R_HOME"
            } else {
                "standard install location"
            };
            (path, source.to_string())
        })
        .ok_or_else(|| {
            format!(
                "Could not find Rscript. Install R from {}, add Rscript to PATH, or set WSITOOLS_RSCRIPT to the full Rscript path.",
                rscript_download_url()
            )
        })
}

fn locate_rscript() -> Result<String, String> {
    locate_rscript_detail().map(|(path, _source)| path)
}

fn launch_script_path(app: &AppHandle) -> Result<PathBuf, String> {
    let dev_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("resources")
        .join("launch-viewer.R");
    if dev_path.exists() {
        return Ok(dev_path);
    }
    app.path()
        .resolve("resources/launch-viewer.R", BaseDirectory::Resource)
        .map_err(|err| format!("Could not resolve bundled R launcher script: {err}"))
}

fn session_dir(app: &AppHandle) -> Result<PathBuf, String> {
    let dir = app
        .path()
        .app_data_dir()
        .map_err(|err| format!("Could not resolve app data directory: {err}"))?
        .join("sessions");
    fs::create_dir_all(&dir).map_err(|err| {
        format!(
            "Could not create session directory {}: {err}",
            dir.display()
        )
    })?;
    Ok(dir)
}

#[tauri::command]
fn check_r() -> RRuntimeStatus {
    match locate_rscript_detail() {
        Ok((rscript, source)) => RRuntimeStatus {
            available: true,
            message: format!("R ready: {rscript}"),
            rscript,
            source,
            download_url: rscript_download_url().to_string(),
        },
        Err(error) => RRuntimeStatus {
            available: false,
            rscript: String::new(),
            source: "not found".to_string(),
            message: error,
            download_url: rscript_download_url().to_string(),
        },
    }
}

#[tauri::command]
fn open_r_download_page() -> Result<(), String> {
    let url = rscript_download_url();
    #[cfg(target_os = "macos")]
    let status = Command::new("open").arg(url).status();

    #[cfg(target_os = "windows")]
    let status = Command::new("cmd").args(["/C", "start", "", url]).status();

    #[cfg(all(target_family = "unix", not(target_os = "macos")))]
    let status = Command::new("xdg-open").arg(url).status();

    status
        .map_err(|err| format!("Could not open R download page: {err}"))?
        .success()
        .then_some(())
        .ok_or_else(|| format!("Could not open R download page automatically. Visit {url}"))
}

#[tauri::command]
fn viewer_logs(state: State<'_, Arc<RViewerState>>) -> Result<Vec<String>, String> {
    Ok(state.logs.lock().unwrap().clone())
}

#[tauri::command]
fn stop_r_viewer(app: AppHandle, state: State<'_, Arc<RViewerState>>) -> Result<(), String> {
    if let Ok(dir) = session_dir(&app) {
        stop_previous_session_pid(&dir, &state.logs);
        remove_pid_file(&dir);
    }
    stop_existing_child(&state);
    stop_existing_native_child(&state);
    if let Some(window) = app.get_webview_window(VIEWER_WINDOW_LABEL) {
        let _ = window.close();
    }
    push_log(&state.logs, "Stopped R viewer process.");
    Ok(())
}

#[tauri::command]
fn save_launcher_log(app: AppHandle, log_text: String) -> Result<String, String> {
    let dir = app
        .path()
        .download_dir()
        .or_else(|_| app.path().app_data_dir())
        .map_err(|err| format!("Could not resolve a log output directory: {err}"))?;
    fs::create_dir_all(&dir).map_err(|err| {
        format!(
            "Could not create log output directory {}: {err}",
            dir.display()
        )
    })?;
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_secs())
        .unwrap_or(0);
    let file = dir.join(format!("wsiTools-desktop-log-{stamp}.txt"));
    fs::write(&file, log_text)
        .map_err(|err| format!("Could not write launcher log {}: {err}", file.display()))?;
    Ok(file.to_string_lossy().to_string())
}

#[tauri::command]
async fn save_viewer_file(
    app: AppHandle,
    window: tauri::WebviewWindow,
    file_name: String,
    data_base64: String,
    filters: Vec<SaveFilter>,
) -> Result<Option<String>, String> {
    let confirmation_app = app.clone();
    let confirmation_window = window.clone();
    let mut dialog = app
        .dialog()
        .file()
        .set_parent(&window)
        .set_title("Save wsiTools file")
        .set_file_name(file_name);
    for filter in filters {
        let extensions: Vec<&str> = filter
            .extensions
            .iter()
            .map(|x| x.trim().trim_start_matches('.'))
            .filter(|x| !x.is_empty())
            .collect();
        if !extensions.is_empty() {
            dialog = dialog.add_filter(filter.name, &extensions);
        }
    }
    tauri::async_runtime::spawn_blocking(move || {
        let Some(path) = dialog.blocking_save_file() else {
            return Ok(None);
        };
        let path = path
            .into_path()
            .map_err(|err| format!("Could not resolve selected save path: {err}"))?;
        if !confirm_file_replacement(&confirmation_app, &confirmation_window, &path) {
            return Ok(None);
        }
        let bytes = general_purpose::STANDARD
            .decode(data_base64.as_bytes())
            .map_err(|err| format!("Could not decode viewer file data: {err}"))?;
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).map_err(|err| {
                format!(
                    "Could not create selected output directory {}: {err}",
                    parent.display()
                )
            })?;
        }
        fs::write(&path, bytes)
            .map_err(|err| format!("Could not write viewer file {}: {err}", path.display()))?;
        Ok(Some(path.to_string_lossy().to_string()))
    })
    .await
    .map_err(|err| format!("Viewer save task failed: {err}"))?
}

#[tauri::command]
async fn choose_viewer_save_path(
    app: AppHandle,
    window: tauri::WebviewWindow,
    file_name: String,
    filters: Vec<SaveFilter>,
) -> Result<Option<SavePathSelection>, String> {
    let confirmation_app = app.clone();
    let confirmation_window = window.clone();
    let mut dialog = app
        .dialog()
        .file()
        .set_parent(&window)
        .set_title("Save spatial object")
        .set_file_name(file_name);
    for filter in filters {
        let extensions: Vec<&str> = filter
            .extensions
            .iter()
            .map(|x| x.trim().trim_start_matches('.'))
            .filter(|x| !x.is_empty())
            .collect();
        if !extensions.is_empty() {
            dialog = dialog.add_filter(filter.name, &extensions);
        }
    }
    tauri::async_runtime::spawn_blocking(move || {
        let Some(path) = dialog.blocking_save_file() else {
            return Ok(None);
        };
        let path = path
            .into_path()
            .map_err(|err| format!("Could not resolve selected save path: {err}"))?;
        let existed = path.exists();
        if !confirm_file_replacement(&confirmation_app, &confirmation_window, &path) {
            return Ok(None);
        }
        Ok(Some(SavePathSelection {
            path: path.to_string_lossy().to_string(),
            overwrite_confirmed: existed,
        }))
    })
    .await
    .map_err(|err| format!("Viewer save path task failed: {err}"))?
}

fn recent_log_tail(logs: &Arc<Mutex<Vec<String>>>) -> String {
    let guard = logs.lock().unwrap();
    let mut tail = guard.iter().rev().take(24).cloned().collect::<Vec<_>>();
    tail.reverse();
    tail.join("\n")
}

fn viewer_launch_ready(launch: &ViewerLaunch, require_live_sync_url: bool) -> bool {
    !launch.viewer_url.is_empty() && (!require_live_sync_url || !launch.sync_url.is_empty())
}

fn wait_for_viewer_url(
    rx: &mpsc::Receiver<()>,
    launch: &Arc<Mutex<ViewerLaunch>>,
    state: &Arc<RViewerState>,
    require_live_sync_url: bool,
) -> Result<(), String> {
    let start = Instant::now();
    loop {
        match rx.recv_timeout(Duration::from_millis(250)) {
            Ok(()) => {
                let ready = {
                    let launch = launch.lock().unwrap();
                    viewer_launch_ready(&launch, require_live_sync_url)
                };
                if ready {
                    return Ok(());
                }
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {
                let status = {
                    let mut guard = state.child.lock().unwrap();
                    let status = if let Some(child) = guard.as_mut() {
                        child.try_wait().map_err(|err| {
                            format!("Could not check R viewer process status: {err}")
                        })?
                    } else {
                        None
                    };
                    if status.is_some() {
                        let _ = guard.take();
                    }
                    status
                };
                if let Some(status) = status {
                    let tail = recent_log_tail(&state.logs);
                    return Err(format!(
                        "R exited before returning the {} (status: {status}).{}{}",
                        if require_live_sync_url {
                            "live synchronization URL required by the Native Rust/WGPU viewer"
                        } else {
                            "viewer URL"
                        },
                        if tail.is_empty() {
                            ""
                        } else {
                            "\n\nRecent log:\n"
                        },
                        tail
                    ));
                }
                if start.elapsed() > Duration::from_secs(180) {
                    stop_existing_child(state);
                    return Err(
                        if require_live_sync_url {
                            "Timed out waiting for R to return the live synchronization URL required by the Native Rust/WGPU viewer. Open the log panel for details."
                        } else {
                            "Timed out waiting for R to start the live viewer. Open the log panel for details."
                        }
                            .to_string(),
                    );
                }
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                let tail = recent_log_tail(&state.logs);
                return Err(format!(
                    "R viewer output stream closed before the {} was returned.{}{}",
                    if require_live_sync_url {
                        "live synchronization URL required by the Native Rust/WGPU viewer"
                    } else {
                        "viewer URL"
                    },
                    if tail.is_empty() {
                        ""
                    } else {
                        "\n\nRecent log:\n"
                    },
                    tail
                ));
            }
        }
    }
}

fn parse_http_local_url(url: &str) -> Option<(String, u16, String)> {
    let raw = url.trim();
    let rest = raw.strip_prefix("http://")?;
    let (host_port, path) = match rest.split_once('/') {
        Some((host_port, path)) => (host_port, format!("/{path}")),
        None => (rest, "/".to_string()),
    };
    let (host, port) = match host_port.rsplit_once(':') {
        Some((host, port)) => (host.to_string(), port.parse::<u16>().ok()?),
        None => (host_port.to_string(), 80),
    };
    if !matches!(host.as_str(), "127.0.0.1" | "localhost") {
        return None;
    }
    Some((host, port, path))
}

fn viewer_url_ready(url: &str) -> bool {
    let Some((host, port, path)) = parse_http_local_url(url) else {
        return true;
    };
    let Ok(mut stream) = TcpStream::connect((host.as_str(), port)) else {
        return false;
    };
    let _ = stream.set_read_timeout(Some(Duration::from_secs(2)));
    let _ = stream.set_write_timeout(Some(Duration::from_secs(2)));
    let request = format!(
        "GET {path} HTTP/1.1\r\nHost: {host}:{port}\r\nConnection: close\r\nCache-Control: no-store\r\n\r\n"
    );
    if stream.write_all(request.as_bytes()).is_err() {
        return false;
    }
    let mut buf = [0_u8; 256];
    let Ok(n) = stream.read(&mut buf) else {
        return false;
    };
    let head = String::from_utf8_lossy(&buf[..n]);
    head.starts_with("HTTP/1.1 2")
        || head.starts_with("HTTP/1.1 3")
        || head.starts_with("HTTP/1.0 2")
        || head.starts_with("HTTP/1.0 3")
}

fn wait_for_viewer_http_ready(url: &str, logs: &Arc<Mutex<Vec<String>>>) -> Result<(), String> {
    if parse_http_local_url(url).is_none() {
        return Ok(());
    }
    let start = Instant::now();
    while start.elapsed() < Duration::from_secs(20) {
        if viewer_url_ready(url) {
            push_log(logs, format!("Viewer HTTP URL is ready: {url}"));
            return Ok(());
        }
        thread::sleep(Duration::from_millis(200));
    }
    push_log(
        logs,
        format!("Viewer HTTP URL did not answer before opening the WebView: {url}"),
    );
    Err(format!(
        "R returned a viewer URL, but the local viewer server did not answer: {url}. Check whether firewall/security software is blocking localhost or whether R exited after creating the viewer."
    ))
}

fn validate_file(path: &str, label: &str) -> Result<PathBuf, String> {
    if path.trim().is_empty() {
        return Err(format!("Choose {label} first."));
    }
    let file = PathBuf::from(path);
    if !file.exists() {
        return Err(format!("{label} file not found: {}", file.display()));
    }
    Ok(file)
}

fn optional_file(path: Option<String>, label: &str) -> Result<Option<PathBuf>, String> {
    match path {
        Some(value) if !value.trim().is_empty() => Ok(Some(validate_file(&value, label)?)),
        _ => Ok(None),
    }
}

fn optional_text(value: Option<String>) -> Option<String> {
    value
        .map(|text| text.trim().to_string())
        .filter(|text| !text.is_empty())
}

fn inspect_spatial_object_target(file_path: String) -> Result<SpatialInspection, String> {
    let file = validate_file(&file_path, "Spatial transcriptomics data")?;
    let rscript = locate_rscript()?;
    let inspector = r#"
args <- commandArgs(trailingOnly = TRUE)
path <- args[[1L]]
clean_value <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "unknown"
  gsub("[\r\n\t]", " ", x)
}
first_col <- function(df, choices) {
  if (!is.data.frame(df)) return(NULL)
  lower <- tolower(names(df))
  for (choice in tolower(choices)) {
    hit <- which(lower == choice)
    if (length(hit)) return(names(df)[[hit[[1L]]]])
  }
  NULL
}
load_object <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "rds")) return(readRDS(path))
  env <- new.env(parent = emptyenv())
  loaded <- load(path, envir = env)
  if (!length(loaded)) stop("No object was found in the selected file.", call. = FALSE)
  env[[loaded[[1L]]]]
}
emit <- function(key, value) {
  cat(sprintf("WSITOOLS_%s=%s\n", key, value))
}
emit_tissues <- function(ids) {
  ids <- clean_value(ids)
  ids <- ids[nzchar(ids)]
  if (!length(ids)) ids <- "default"
  tab <- sort(table(ids), decreasing = TRUE)
  for (id in names(tab)) {
    cat(sprintf("WSITOOLS_TISSUE=%s\t%s\n", id, as.integer(tab[[id]])))
  }
}
obj <- load_object(path)
classes <- class(obj)
object_type <- if (inherits(obj, "Seurat")) {
  "Seurat"
} else if (inherits(obj, "SpatialExperiment")) {
  "SpatialExperiment"
} else if (inherits(obj, "SingleCellExperiment")) {
  "SingleCellExperiment"
} else if (inherits(obj, "SummarizedExperiment")) {
  "SummarizedExperiment"
} else if (any(grepl("giotto", classes, ignore.case = TRUE))) {
  "Giotto"
} else {
  classes[[1L]]
}
tissues <- character()
tissues_emitted <- FALSE
if (inherits(obj, "Seurat")) {
  images <- tryCatch(names(methods::slot(obj, "images")), error = function(e) character())
  if (length(images)) {
    for (image_id in images) {
      image_obj <- tryCatch(methods::slot(obj, "images")[[image_id]], error = function(e) NULL)
      coordinates <- tryCatch(methods::slot(image_obj, "coordinates"), error = function(e) NULL)
      n_coordinates <- if (is.data.frame(coordinates) || is.matrix(coordinates)) nrow(coordinates) else NA_integer_
      cat(sprintf(
        "WSITOOLS_TISSUE=%s\t%s\n",
        clean_value(image_id),
        if (is.finite(n_coordinates)) as.integer(n_coordinates) else ""
      ))
    }
    tissues_emitted <- TRUE
  } else {
    meta <- tryCatch(methods::slot(obj, "meta.data"), error = function(e) NULL)
    if (is.data.frame(meta)) {
      col <- first_col(meta, c("sample_id", "sample", "section", "image_id", "orig.ident", "slide_id", "tissue", "library_id"))
      if (!is.null(col)) tissues <- meta[[col]]
    }
  }
} else if (inherits(obj, "SpatialExperiment") ||
           inherits(obj, "SingleCellExperiment") ||
           inherits(obj, "SummarizedExperiment")) {
  col_data <- tryCatch(as.data.frame(SummarizedExperiment::colData(obj)), error = function(e) NULL)
  if (is.data.frame(col_data)) {
    col <- first_col(col_data, c("sample_id", "sample", "section", "image_id", "orig.ident", "slide_id", "tissue", "library_id"))
    if (!is.null(col)) tissues <- col_data[[col]]
  }
}
if (!length(tissues)) tissues <- "default"
emit("OBJECT_TYPE", object_type)
emit("MESSAGE", sprintf("Detected %s in %s", object_type, basename(path)))
if (!tissues_emitted) emit_tissues(tissues)
"#;
    let output = Command::new(&rscript)
        .arg("--vanilla")
        .arg("-e")
        .arg(inspector)
        .arg(file.to_string_lossy().to_string())
        .env("PATH", augmented_path_env())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|err| format!("Could not inspect spatial object with Rscript: {err}"))?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !output.status.success() {
        return Err(format!(
            "Could not inspect spatial transcriptomics object with R.\n\n{}{}",
            stdout, stderr
        ));
    }

    let mut object_type = "unknown".to_string();
    let mut message = String::new();
    let mut tissues = Vec::new();
    for line in stdout.lines() {
        if let Some(value) = line.strip_prefix("WSITOOLS_OBJECT_TYPE=") {
            object_type = value.trim().to_string();
        } else if let Some(value) = line.strip_prefix("WSITOOLS_MESSAGE=") {
            message = value.trim().to_string();
        } else if let Some(value) = line.strip_prefix("WSITOOLS_TISSUE=") {
            let mut parts = value.splitn(2, '\t');
            let tissue_id = parts.next().unwrap_or("default").trim().to_string();
            let n_spots = parts
                .next()
                .and_then(|text| text.trim().parse::<u64>().ok());
            if !tissue_id.is_empty() {
                tissues.push(SpatialTissueInfo { tissue_id, n_spots });
            }
        }
    }
    if tissues.is_empty() {
        tissues.push(SpatialTissueInfo {
            tissue_id: "default".to_string(),
            n_spots: None,
        });
    }
    if message.is_empty() {
        message = format!("Detected {object_type} spatial object.");
    }
    Ok(SpatialInspection {
        object_type,
        file_path: file.to_string_lossy().to_string(),
        tissues,
        message,
    })
}

fn launch_r_new_project_target(
    app: AppHandle,
    state: Arc<RViewerState>,
    project_items: Vec<NewProjectItem>,
    selected_engine: String,
) -> Result<ViewerLaunch, String> {
    if project_items.is_empty() {
        return Err("Select at least one image before running R.".to_string());
    }
    let items = project_items
        .into_iter()
        .map(|item| {
            Ok(ResolvedProjectItem {
                image: validate_file(&item.image, "Image")?,
                cell_annotation: optional_file(item.cell_annotation, "Cell annotation")?,
                tissue_annotation: optional_file(item.tissue_annotation, "Tissue annotation")?,
                spatial_data: optional_file(item.spatial_data, "Spatial transcriptomics data")?,
                spatial_sample_id: optional_text(item.spatial_sample_id),
            })
        })
        .collect::<Result<Vec<_>, String>>()?;

    state.logs.lock().unwrap().clear();

    let rscript = locate_rscript()?;
    let script = launch_script_path(&app)?;
    let session_dir = session_dir(&app)?;

    push_log(&state.logs, "Preparing a clean desktop viewer session.");
    stop_previous_session_pid(&session_dir, &state.logs);
    stop_existing_child(&state);
    clear_legacy_viewer_artifacts(&session_dir, &state.logs);

    push_log(&state.logs, format!("Rscript: {rscript}"));
    push_log(
        &state.logs,
        format!("Backend PATH for R: {}", augmented_path_env()),
    );
    let renderer = viewer_renderer_env();
    let selected_engine = viewer_engine(&selected_engine);
    let tile_compositor = viewer_tile_compositor_env(&selected_engine);
    push_log(
        &state.logs,
        format!("Desktop viewer engine: {selected_engine}"),
    );
    push_log(&state.logs, format!("Viewer renderer: {renderer}"));
    push_log(&state.logs, format!("Tile compositor: {tile_compositor}"));
    push_log(&state.logs, "Mode: new-project");
    for (index, item) in items.iter().enumerate() {
        push_log(
            &state.logs,
            format!("Image {}: {}", index + 1, item.image.display()),
        );
        if let Some(path) = &item.cell_annotation {
            push_log(
                &state.logs,
                format!("  Cell annotation: {}", path.display()),
            );
        }
        if let Some(path) = &item.tissue_annotation {
            push_log(
                &state.logs,
                format!("  Tissue annotation: {}", path.display()),
            );
        }
        if let Some(path) = &item.spatial_data {
            push_log(
                &state.logs,
                format!("  Spatial transcriptomics data: {}", path.display()),
            );
        }
        if let Some(sample_id) = &item.spatial_sample_id {
            push_log(
                &state.logs,
                format!("  Spatial tissue/sample ID: {sample_id}"),
            );
        }
    }
    push_log(&state.logs, "R code sent to R:");
    push_log(&state.logs, "library(wsiTools)");
    push_log(&state.logs, "project_images <- data.frame(");
    let image_r = items
        .iter()
        .map(|item| r_string(&item.image.to_string_lossy()))
        .collect::<Vec<_>>()
        .join(", ");
    let cell_r = items
        .iter()
        .map(|item| {
            item.cell_annotation
                .as_ref()
                .map(|path| r_string(&path.to_string_lossy()))
                .unwrap_or_else(|| r_string(""))
        })
        .collect::<Vec<_>>()
        .join(", ");
    let tissue_r = items
        .iter()
        .map(|item| {
            item.tissue_annotation
                .as_ref()
                .map(|path| r_string(&path.to_string_lossy()))
                .unwrap_or_else(|| r_string(""))
        })
        .collect::<Vec<_>>()
        .join(", ");
    let spatial_r = items
        .iter()
        .map(|item| {
            item.spatial_data
                .as_ref()
                .map(|path| r_string(&path.to_string_lossy()))
                .unwrap_or_else(|| r_string(""))
        })
        .collect::<Vec<_>>()
        .join(", ");
    let sample_r = items
        .iter()
        .map(|item| {
            item.spatial_sample_id
                .as_ref()
                .map(|sample_id| r_string(sample_id))
                .unwrap_or_else(|| r_string(""))
        })
        .collect::<Vec<_>>()
        .join(", ");
    push_log(&state.logs, format!("  image = c({image_r}),"));
    push_log(&state.logs, format!("  cell_annotation = c({cell_r}),"));
    push_log(&state.logs, format!("  tissue_annotation = c({tissue_r}),"));
    push_log(&state.logs, format!("  spatial_data = c({spatial_r}),"));
    push_log(&state.logs, format!("  spatial_sample_id = c({sample_r}),"));
    push_log(&state.logs, "  stringsAsFactors = FALSE");
    push_log(&state.logs, ")");
    push_log(&state.logs, "viewer <- wsi_open_viewer(");
    push_log(&state.logs, "  project_images$image,");
    push_log(&state.logs, "  live = \"yes\",");
    push_log(&state.logs, "  tiled = \"yes\",");
    push_log(&state.logs, format!("  renderer = \"{renderer}\","));
    push_log(
        &state.logs,
        "  dynamic_tiles = \"auto\"  # dynamic on first open; prebuilt when cache exists,",
    );
    push_log(&state.logs, "  open = FALSE,");
    push_log(&state.logs, "  wait = FALSE");
    push_log(&state.logs, ")");

    let mut args = vec![
        script.to_string_lossy().to_string(),
        "--mode".to_string(),
        "new-project".to_string(),
    ];
    for item in &items {
        args.push("--image".to_string());
        args.push(item.image.to_string_lossy().to_string());
        if let Some(path) = &item.cell_annotation {
            args.push("--cell".to_string());
            args.push(path.to_string_lossy().to_string());
        }
        if let Some(path) = &item.tissue_annotation {
            args.push("--tissue".to_string());
            args.push(path.to_string_lossy().to_string());
        }
        if let Some(path) = &item.spatial_data {
            args.push("--spatial".to_string());
            args.push(path.to_string_lossy().to_string());
        }
        if let Some(sample_id) = &item.spatial_sample_id {
            args.push("--sample".to_string());
            args.push(sample_id.to_string());
        }
    }

    let mut child = Command::new(&rscript)
        .args(args)
        .env("PATH", augmented_path_env())
        .env("WSITOOLS_VIEWER_RENDERER", &renderer)
        .env("WSITOOLS_TILE_COMPOSITOR", &tile_compositor)
        .env("WSITOOLS_DESKTOP_SESSION_DIR", &session_dir)
        .env("WSITOOLS_DESKTOP_SOURCE_DIR", env!("CARGO_MANIFEST_DIR"))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|err| format!("Could not start Rscript: {err}"))?;

    let pid = child.id();
    write_pid_file(&session_dir, pid);
    let stdout = child.stdout.take();
    let stderr = child.stderr.take();
    *state.child.lock().unwrap() = Some(child);

    let launch = Arc::new(Mutex::new(ViewerLaunch {
        r_pid: pid,
        viewer_engine: selected_engine.clone(),
        ..ViewerLaunch::default()
    }));
    let (tx, rx) = mpsc::channel::<()>();

    if let Some(stdout) = stdout {
        let logs = state.logs.clone();
        let launch_state = launch.clone();
        let tx = tx.clone();
        let progress_app = app.clone();
        thread::spawn(move || {
            let reader = BufReader::new(stdout);
            for line in reader.lines().map_while(Result::ok) {
                if let Some(value) = line.strip_prefix("WSITOOLS_VIEWER_URL=") {
                    launch_state.lock().unwrap().viewer_url = value.trim().to_string();
                    let _ = tx.send(());
                } else if let Some(value) = line.strip_prefix("WSITOOLS_VIEWER_HTTP_URL=") {
                    launch_state.lock().unwrap().viewer_url = value.trim().to_string();
                } else if let Some(value) = line.strip_prefix("WSITOOLS_VIEWER_FILE=") {
                    launch_state.lock().unwrap().html_file = value.trim().to_string();
                } else if let Some(value) = line.strip_prefix("WSITOOLS_SYNC_URL=") {
                    launch_state.lock().unwrap().sync_url = value.trim().to_string();
                    let _ = tx.send(());
                } else if let Some(value) = line.strip_prefix("WSITOOLS_LOG_FILE=") {
                    launch_state.lock().unwrap().log_file = value.trim().to_string();
                } else if let Some(value) = line.strip_prefix("WSITOOLS_STAGE=") {
                    emit_viewer_progress(&progress_app, value.trim());
                }
                push_log(&logs, line);
            }
        });
    }

    if let Some(stderr) = stderr {
        let logs = state.logs.clone();
        thread::spawn(move || {
            let reader = BufReader::new(stderr);
            for line in reader.lines().map_while(Result::ok) {
                push_log(&logs, line);
            }
        });
    }

    let require_live_sync_url = selected_engine == "native";
    wait_for_viewer_url(&rx, &launch, &state, require_live_sync_url)?;

    let result = launch.lock().unwrap().clone();
    if result.viewer_url.is_empty() {
        stop_existing_child(&state);
        return Err("R started but did not return a viewer URL.".to_string());
    }
    let ready_url = if require_live_sync_url {
        &result.sync_url
    } else {
        &result.viewer_url
    };
    wait_for_viewer_http_ready(ready_url, &state.logs)?;
    Ok(result)
}

fn launch_r_target(
    app: AppHandle,
    state: Arc<RViewerState>,
    target_path: String,
    target_mode: &str,
    selected_engine: String,
) -> Result<ViewerLaunch, String> {
    let mode = match target_mode {
        "project" => "project",
        _ => "image",
    };
    if target_path.trim().is_empty() {
        return Err(if mode == "project" {
            "Choose a wsiTools project folder first.".to_string()
        } else {
            "Choose an image file first.".to_string()
        });
    }
    let target = PathBuf::from(&target_path);
    if mode == "project" {
        if !target.is_dir() {
            return Err(format!("Project folder not found: {}", target.display()));
        }
        let manifest = target.join("project.json");
        if !manifest.exists() {
            return Err(format!(
                "This folder is not a wsiTools project because project.json was not found: {}",
                manifest.display()
            ));
        }
    } else if !target.exists() {
        return Err(format!("Image file not found: {}", target.display()));
    }

    state.logs.lock().unwrap().clear();

    let rscript = locate_rscript()?;
    let script = launch_script_path(&app)?;
    let session_dir = session_dir(&app)?;

    push_log(&state.logs, "Preparing a clean desktop viewer session.");
    stop_previous_session_pid(&session_dir, &state.logs);
    stop_existing_child(&state);
    clear_legacy_viewer_artifacts(&session_dir, &state.logs);

    push_log(&state.logs, format!("Rscript: {rscript}"));
    push_log(
        &state.logs,
        format!("Backend PATH for R: {}", augmented_path_env()),
    );
    let renderer = viewer_renderer_env();
    let selected_engine = viewer_engine(&selected_engine);
    let tile_compositor = viewer_tile_compositor_env(&selected_engine);
    push_log(
        &state.logs,
        format!("Desktop viewer engine: {selected_engine}"),
    );
    push_log(&state.logs, format!("Viewer renderer: {renderer}"));
    push_log(&state.logs, format!("Tile compositor: {tile_compositor}"));
    push_log(&state.logs, format!("Mode: {mode}"));
    push_log(&state.logs, format!("Target: {}", target.display()));
    push_log(&state.logs, "R code sent to R:");
    push_log(&state.logs, "library(wsiTools)");
    if mode == "project" {
        push_log(
            &state.logs,
            format!(
                "project <- wsi_read_project({})",
                r_string(&target.to_string_lossy())
            ),
        );
        push_log(&state.logs, "viewer <- wsi_open_viewer(");
        push_log(&state.logs, "  project$slide_path,");
    } else {
        push_log(&state.logs, "viewer <- wsi_open_viewer(");
        push_log(
            &state.logs,
            format!("  {},", r_string(&target.to_string_lossy())),
        );
    }
    push_log(&state.logs, "  live = \"yes\",");
    push_log(&state.logs, "  tiled = \"yes\",");
    push_log(&state.logs, format!("  renderer = \"{renderer}\","));
    push_log(&state.logs, "  dynamic_tiles = FALSE,");
    push_log(&state.logs, "  open = FALSE,");
    push_log(&state.logs, "  wait = FALSE");
    push_log(&state.logs, ")");
    if mode == "project" {
        push_log(
            &state.logs,
            "restore_project_state(viewer, project, service = FALSE)",
        );
    }

    let mut child = Command::new(&rscript)
        .arg(script)
        .arg("--mode")
        .arg(mode)
        .arg(target)
        .env("PATH", augmented_path_env())
        .env("WSITOOLS_VIEWER_RENDERER", &renderer)
        .env("WSITOOLS_TILE_COMPOSITOR", &tile_compositor)
        .env("WSITOOLS_DESKTOP_SESSION_DIR", &session_dir)
        .env("WSITOOLS_DESKTOP_SOURCE_DIR", env!("CARGO_MANIFEST_DIR"))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|err| format!("Could not start Rscript: {err}"))?;

    let pid = child.id();
    write_pid_file(&session_dir, pid);
    let stdout = child.stdout.take();
    let stderr = child.stderr.take();
    *state.child.lock().unwrap() = Some(child);

    let launch = Arc::new(Mutex::new(ViewerLaunch {
        r_pid: pid,
        viewer_engine: selected_engine.clone(),
        ..ViewerLaunch::default()
    }));
    let (tx, rx) = mpsc::channel::<()>();

    if let Some(stdout) = stdout {
        let logs = state.logs.clone();
        let launch_state = launch.clone();
        let tx = tx.clone();
        let progress_app = app.clone();
        thread::spawn(move || {
            let reader = BufReader::new(stdout);
            for line in reader.lines().map_while(Result::ok) {
                if let Some(value) = line.strip_prefix("WSITOOLS_VIEWER_URL=") {
                    launch_state.lock().unwrap().viewer_url = value.trim().to_string();
                    let _ = tx.send(());
                } else if let Some(value) = line.strip_prefix("WSITOOLS_VIEWER_HTTP_URL=") {
                    launch_state.lock().unwrap().viewer_url = value.trim().to_string();
                } else if let Some(value) = line.strip_prefix("WSITOOLS_VIEWER_FILE=") {
                    launch_state.lock().unwrap().html_file = value.trim().to_string();
                } else if let Some(value) = line.strip_prefix("WSITOOLS_SYNC_URL=") {
                    launch_state.lock().unwrap().sync_url = value.trim().to_string();
                    let _ = tx.send(());
                } else if let Some(value) = line.strip_prefix("WSITOOLS_LOG_FILE=") {
                    launch_state.lock().unwrap().log_file = value.trim().to_string();
                } else if let Some(value) = line.strip_prefix("WSITOOLS_STAGE=") {
                    emit_viewer_progress(&progress_app, value.trim());
                }
                push_log(&logs, line);
            }
        });
    }

    if let Some(stderr) = stderr {
        let logs = state.logs.clone();
        thread::spawn(move || {
            let reader = BufReader::new(stderr);
            for line in reader.lines().map_while(Result::ok) {
                push_log(&logs, line);
            }
        });
    }

    let require_live_sync_url = selected_engine == "native";
    wait_for_viewer_url(&rx, &launch, &state, require_live_sync_url)?;

    let result = launch.lock().unwrap().clone();
    if result.viewer_url.is_empty() {
        stop_existing_child(&state);
        return Err("R started but did not return a viewer URL.".to_string());
    }
    let ready_url = if require_live_sync_url {
        &result.sync_url
    } else {
        &result.viewer_url
    };
    wait_for_viewer_http_ready(ready_url, &state.logs)?;
    Ok(result)
}

#[tauri::command]
async fn start_r_viewer(
    app: AppHandle,
    state: State<'_, Arc<RViewerState>>,
    file_path: String,
    viewer_engine: String,
) -> Result<ViewerLaunch, String> {
    let state = state.inner().clone();
    tauri::async_runtime::spawn_blocking(move || {
        launch_r_target(app, state, file_path, "image", viewer_engine)
    })
    .await
    .map_err(|err| format!("R launcher task failed: {err}"))?
}

#[tauri::command]
async fn start_r_project(
    app: AppHandle,
    state: State<'_, Arc<RViewerState>>,
    project_path: String,
    viewer_engine: String,
) -> Result<ViewerLaunch, String> {
    let state = state.inner().clone();
    tauri::async_runtime::spawn_blocking(move || {
        launch_r_target(app, state, project_path, "project", viewer_engine)
    })
    .await
    .map_err(|err| format!("R launcher task failed: {err}"))?
}

#[tauri::command]
async fn start_r_new_project(
    app: AppHandle,
    state: State<'_, Arc<RViewerState>>,
    project_items: Vec<NewProjectItem>,
    viewer_engine: String,
) -> Result<ViewerLaunch, String> {
    let state = state.inner().clone();
    tauri::async_runtime::spawn_blocking(move || {
        launch_r_new_project_target(app, state, project_items, viewer_engine)
    })
    .await
    .map_err(|err| format!("R launcher task failed: {err}"))?
}

#[tauri::command]
async fn inspect_spatial_object(file_path: String) -> Result<SpatialInspection, String> {
    tauri::async_runtime::spawn_blocking(move || inspect_spatial_object_target(file_path))
        .await
        .map_err(|err| format!("Spatial object inspection task failed: {err}"))?
}

#[tauri::command]
fn open_viewer_loading_window(app: AppHandle) -> Result<(), String> {
    if let Some(window) = app.get_webview_window(VIEWER_WINDOW_LABEL) {
        let _ = window.close();
    }
    tauri::WebviewWindowBuilder::new(
        &app,
        VIEWER_WINDOW_LABEL,
        tauri::WebviewUrl::App("viewer-loading.html".into()),
    )
    .title("wsiTools Viewer")
    .inner_size(1500.0, 950.0)
    .min_inner_size(960.0, 700.0)
    .resizable(true)
    .build()
    .map_err(|err| format!("Could not open viewer loading window: {err}"))?;
    Ok(())
}

#[tauri::command]
fn close_viewer_window(app: AppHandle) -> Result<(), String> {
    if let Some(window) = app.get_webview_window(VIEWER_WINDOW_LABEL) {
        window
            .close()
            .map_err(|err| format!("Could not close viewer window: {err}"))?;
    }
    Ok(())
}

#[tauri::command]
fn open_viewer_window(app: AppHandle, url: String) -> Result<(), String> {
    if url.trim().is_empty() {
        return Err("Viewer URL was empty.".to_string());
    }
    let parsed = url
        .parse()
        .map_err(|err| format!("Viewer URL is not valid: {err}"))?;
    if let Some(window) = app.get_webview_window(VIEWER_WINDOW_LABEL) {
        window
            .navigate(parsed)
            .map_err(|err| format!("Could not navigate viewer window: {err}"))?;
        let _ = window.show();
        let _ = window.set_focus();
        return Ok(());
    }

    tauri::WebviewWindowBuilder::new(
        &app,
        VIEWER_WINDOW_LABEL,
        tauri::WebviewUrl::External(parsed),
    )
    .title("wsiTools Viewer")
    .inner_size(1500.0, 950.0)
    .min_inner_size(960.0, 700.0)
    .resizable(true)
    .build()
    .map_err(|err| format!("Could not open viewer window: {err}"))?;
    Ok(())
}

#[tauri::command]
fn open_native_viewer(
    app: AppHandle,
    url: String,
    state: State<'_, Arc<RViewerState>>,
) -> Result<(), String> {
    #[cfg(feature = "native-wgpu")]
    {
        let executable = env::current_exe()
            .map_err(|error| format!("Could not locate wsiTools Desktop executable: {error}"))?;
        stop_existing_native_child(&state);
        let status_path = native_startup_status_path(&app)?;
        let _ = fs::remove_file(&status_path);
        let mut child = Command::new(executable)
            .arg("--native-viewer-url")
            .arg(url)
            .arg("--native-viewer-status")
            .arg(&status_path)
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|error| format!("Could not start native WGPU viewer: {error}"))?;
        if let Some(stderr) = child.stderr.take() {
            let logs = state.logs.clone();
            thread::spawn(move || {
                let reader = BufReader::new(stderr);
                for line in reader.lines().map_while(Result::ok) {
                    push_log(&logs, format!("[Native WGPU] {line}"));
                }
            });
        }
        wait_for_native_startup(&mut child, &status_path, &state.logs)?;
        *state.native_child.lock().unwrap() = Some(child);
        return Ok(());
    }
    #[cfg(not(feature = "native-wgpu"))]
    Err("This desktop build does not include WGPU support.".to_string())
}

fn main() {
    #[cfg(feature = "native-wgpu")]
    if let Some(viewer_url) = std::env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .windows(2)
        .find_map(|args| (args[0] == "--native-viewer-url").then(|| args[1].clone()))
    {
        let native_args = std::env::args().skip(1).collect::<Vec<_>>();
        let startup_status = native_args
            .windows(2)
            .find_map(|args| (args[0] == "--native-viewer-status").then(|| PathBuf::from(&args[1])));
        if let Err(error) = native_renderer::run_native_viewer(viewer_url, startup_status) {
            eprintln!("wsiTools native WGPU viewer failed: {error}");
            std::process::exit(1);
        }
        return;
    }
    tauri::Builder::default()
        .plugin(tauri_plugin_dialog::init())
        .manage(Arc::new(RViewerState::default()))
        .invoke_handler(tauri::generate_handler![
            check_r,
            open_r_download_page,
            start_r_viewer,
            start_r_project,
            start_r_new_project,
            inspect_spatial_object,
            native_renderer_diagnostics,
            open_viewer_loading_window,
            open_viewer_window,
            open_native_viewer,
            close_viewer_window,
            stop_r_viewer,
            viewer_logs,
            save_launcher_log,
            save_viewer_file,
            choose_viewer_save_path
        ])
        .run(tauri::generate_context!())
        .expect("error while running wsiTools Desktop");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_launch_waits_for_the_live_sync_url() {
        let browser_only = ViewerLaunch {
            viewer_url: "file:///tmp/wsi-viewer.html".to_string(),
            ..ViewerLaunch::default()
        };
        assert!(viewer_launch_ready(&browser_only, false));
        assert!(!viewer_launch_ready(&browser_only, true));

        let native_ready = ViewerLaunch {
            sync_url: "http://127.0.0.1:8788/viewer-state".to_string(),
            ..browser_only
        };
        assert!(viewer_launch_ready(&native_ready, true));
    }
}
