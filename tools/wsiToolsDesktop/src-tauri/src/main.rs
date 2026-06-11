use serde::{Deserialize, Serialize};
use std::{
    env, fs,
    io::{BufRead, BufReader},
    path::PathBuf,
    process::{Child, Command, Stdio},
    sync::{mpsc, Arc, Mutex},
    thread,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};
use tauri::{path::BaseDirectory, AppHandle, Manager, State};

#[derive(Default)]
struct RViewerState {
    child: Mutex<Option<Child>>,
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
    }
}

#[derive(Clone, Debug, Default, Serialize)]
struct ViewerLaunch {
    viewer_url: String,
    html_file: String,
    sync_url: String,
    log_file: String,
    r_pid: u32,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct NewProjectItem {
    image: String,
    cell_annotation: Option<String>,
    tissue_annotation: Option<String>,
    spatial_data: Option<String>,
}

#[derive(Clone, Debug)]
struct ResolvedProjectItem {
    image: PathBuf,
    cell_annotation: Option<PathBuf>,
    tissue_annotation: Option<PathBuf>,
    spatial_data: Option<PathBuf>,
}

fn push_log(logs: &Arc<Mutex<Vec<String>>>, line: impl Into<String>) {
    let mut guard = logs.lock().unwrap();
    guard.push(line.into());
    if guard.len() > 1000 {
        let extra = guard.len() - 1000;
        guard.drain(0..extra);
    }
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

fn command_works(path: &str) -> bool {
    Command::new(path)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|status| status.success())
        .unwrap_or(false)
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

fn locate_rscript() -> Result<String, String> {
    if let Ok(path) = env::var("WSITOOLS_RSCRIPT") {
        if command_works(&path) {
            return Ok(path);
        }
    }

    let mut candidates = vec![
        "Rscript".to_string(),
        "Rscript.exe".to_string(),
        "/usr/local/bin/Rscript".to_string(),
        "/opt/homebrew/bin/Rscript".to_string(),
        "/Library/Frameworks/R.framework/Resources/bin/Rscript".to_string(),
    ];
    candidates.extend(windows_r_candidates());

    candidates
        .into_iter()
        .find(|path| command_works(path))
        .ok_or_else(|| {
            "Could not find Rscript. Install R, add Rscript to PATH, or set WSITOOLS_RSCRIPT to the full Rscript path.".to_string()
        })
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
fn check_r() -> Result<String, String> {
    let rscript = locate_rscript()?;
    Ok(format!("R ready: {rscript}"))
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

fn recent_log_tail(logs: &Arc<Mutex<Vec<String>>>) -> String {
    let guard = logs.lock().unwrap();
    let mut tail = guard.iter().rev().take(24).cloned().collect::<Vec<_>>();
    tail.reverse();
    tail.join("\n")
}

fn wait_for_viewer_url(rx: &mpsc::Receiver<()>, state: &Arc<RViewerState>) -> Result<(), String> {
    let start = Instant::now();
    loop {
        match rx.recv_timeout(Duration::from_millis(250)) {
            Ok(()) => return Ok(()),
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
                        "R exited before returning a viewer URL (status: {status}).{}{}",
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
                        "Timed out waiting for R to start the live viewer. Open the log panel for details."
                            .to_string(),
                    );
                }
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                let tail = recent_log_tail(&state.logs);
                return Err(format!(
                    "R viewer output stream closed before a viewer URL was returned.{}{}",
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

fn launch_r_new_project_target(
    app: AppHandle,
    state: Arc<RViewerState>,
    project_items: Vec<NewProjectItem>,
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

    push_log(&state.logs, format!("Rscript: {rscript}"));
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
    push_log(&state.logs, format!("  image = c({image_r}),"));
    push_log(&state.logs, format!("  cell_annotation = c({cell_r}),"));
    push_log(&state.logs, format!("  tissue_annotation = c({tissue_r}),"));
    push_log(&state.logs, format!("  spatial_data = c({spatial_r}),"));
    push_log(&state.logs, "  stringsAsFactors = FALSE");
    push_log(&state.logs, ")");
    push_log(&state.logs, "viewer <- wsi_open_viewer(");
    push_log(&state.logs, "  project_images$image,");
    push_log(&state.logs, "  live = \"yes\",");
    push_log(&state.logs, "  tiled = \"yes\",");
    push_log(&state.logs, "  dynamic_tiles = TRUE,");
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
    }

    let mut child = Command::new(&rscript)
        .args(args)
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
        ..ViewerLaunch::default()
    }));
    let (tx, rx) = mpsc::channel::<()>();

    if let Some(stdout) = stdout {
        let logs = state.logs.clone();
        let launch_state = launch.clone();
        let tx = tx.clone();
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
                } else if let Some(value) = line.strip_prefix("WSITOOLS_LOG_FILE=") {
                    launch_state.lock().unwrap().log_file = value.trim().to_string();
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

    wait_for_viewer_url(&rx, &state)?;

    let result = launch.lock().unwrap().clone();
    if result.viewer_url.is_empty() {
        stop_existing_child(&state);
        return Err("R started but did not return a viewer URL.".to_string());
    }
    Ok(result)
}

fn launch_r_target(
    app: AppHandle,
    state: Arc<RViewerState>,
    target_path: String,
    target_mode: &str,
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

    push_log(&state.logs, format!("Rscript: {rscript}"));
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
    push_log(&state.logs, "  dynamic_tiles = TRUE,");
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
        ..ViewerLaunch::default()
    }));
    let (tx, rx) = mpsc::channel::<()>();

    if let Some(stdout) = stdout {
        let logs = state.logs.clone();
        let launch_state = launch.clone();
        let tx = tx.clone();
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
                } else if let Some(value) = line.strip_prefix("WSITOOLS_LOG_FILE=") {
                    launch_state.lock().unwrap().log_file = value.trim().to_string();
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

    wait_for_viewer_url(&rx, &state)?;

    let result = launch.lock().unwrap().clone();
    if result.viewer_url.is_empty() {
        stop_existing_child(&state);
        return Err("R started but did not return a viewer URL.".to_string());
    }
    Ok(result)
}

#[tauri::command]
async fn start_r_viewer(
    app: AppHandle,
    state: State<'_, Arc<RViewerState>>,
    file_path: String,
) -> Result<ViewerLaunch, String> {
    let state = state.inner().clone();
    tauri::async_runtime::spawn_blocking(move || launch_r_target(app, state, file_path, "image"))
        .await
        .map_err(|err| format!("R launcher task failed: {err}"))?
}

#[tauri::command]
async fn start_r_project(
    app: AppHandle,
    state: State<'_, Arc<RViewerState>>,
    project_path: String,
) -> Result<ViewerLaunch, String> {
    let state = state.inner().clone();
    tauri::async_runtime::spawn_blocking(move || {
        launch_r_target(app, state, project_path, "project")
    })
    .await
    .map_err(|err| format!("R launcher task failed: {err}"))?
}

#[tauri::command]
async fn start_r_new_project(
    app: AppHandle,
    state: State<'_, Arc<RViewerState>>,
    project_items: Vec<NewProjectItem>,
) -> Result<ViewerLaunch, String> {
    let state = state.inner().clone();
    tauri::async_runtime::spawn_blocking(move || {
        launch_r_new_project_target(app, state, project_items)
    })
    .await
    .map_err(|err| format!("R launcher task failed: {err}"))?
}

#[tauri::command]
fn open_viewer_window(app: AppHandle, url: String) -> Result<(), String> {
    if url.trim().is_empty() {
        return Err("Viewer URL was empty.".to_string());
    }
    let parsed = url
        .parse()
        .map_err(|err| format!("Viewer URL is not valid: {err}"))?;
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_millis())
        .unwrap_or(0);
    tauri::WebviewWindowBuilder::new(
        &app,
        format!("viewer-{stamp}"),
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

fn main() {
    tauri::Builder::default()
        .plugin(tauri_plugin_dialog::init())
        .manage(Arc::new(RViewerState::default()))
        .invoke_handler(tauri::generate_handler![
            check_r,
            start_r_viewer,
            start_r_project,
            start_r_new_project,
            open_viewer_window,
            stop_r_viewer,
            viewer_logs,
            save_launcher_log
        ])
        .run(tauri::generate_context!())
        .expect("error while running wsiTools Desktop");
}
