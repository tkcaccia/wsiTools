import { invoke } from "@tauri-apps/api/core";
import { open } from "@tauri-apps/plugin-dialog";

const homeScreen = document.getElementById("homeScreen");
const appShell = document.getElementById("appShell");
const openProjectHome = document.getElementById("openProjectHome");
const createProjectHome = document.getElementById("createProjectHome");
const backHome = document.getElementById("backHome");
const workspaceSubtitle = document.getElementById("workspaceSubtitle");
const imageStep = document.getElementById("imageStep");
const associationStep = document.getElementById("associationStep");
const addImage = document.getElementById("addImage");
const nextAssociations = document.getElementById("nextAssociations");
const backToImages = document.getElementById("backToImages");
const runR = document.getElementById("runR");
const stopViewer = document.getElementById("stopViewer");
const imageList = document.getElementById("imageList");
const associationList = document.getElementById("associationList");
const runtimePanel = document.getElementById("runtimePanel");
const launchError = document.getElementById("launchError");
const logOutput = document.getElementById("logOutput");
const rStatus = document.getElementById("rStatus");
const homeRStatus = document.getElementById("homeRStatus");
const copyLog = document.getElementById("copyLog");
const saveLog = document.getElementById("saveLog");

let projectImages = [];
let currentMode = "home";
let logTimer = null;
let lastRustLogs = [];
let viewerWindowOpen = false;
const localLogs = [];

function timestamp() {
  return new Date().toLocaleTimeString();
}

function combinedLogText() {
  return [
    ...localLogs,
    ...lastRustLogs.map((line) => `[R] ${line}`)
  ].join("\n");
}

function renderLogs() {
  logOutput.textContent = combinedLogText() || "No log messages yet.";
}

function appendLog(message) {
  const lines = String(message).split(/\r?\n/);
  for (const line of lines) {
    localLogs.push(`[${timestamp()}] ${line}`);
  }
  if (localLogs.length > 500) {
    localLogs.splice(0, localLogs.length - 500);
  }
  renderLogs();
}

function setStatus(message, kind = "info") {
  for (const target of [rStatus, homeRStatus]) {
    if (!target) continue;
    target.textContent = message;
    target.dataset.kind = kind;
  }
  appendLog(`Status: ${message}`);
}

function setBusy(isBusy) {
  openProjectHome.disabled = isBusy;
  createProjectHome.disabled = isBusy;
  backHome.disabled = isBusy;
  addImage.disabled = isBusy;
  nextAssociations.disabled = isBusy || projectImages.length === 0;
  backToImages.disabled = isBusy;
  runR.disabled = isBusy || projectImages.length === 0;
  stopViewer.disabled = !viewerWindowOpen;
  for (const button of document.querySelectorAll("[data-action]")) {
    button.disabled = isBusy;
  }
}

function clearLaunchError() {
  if (!launchError) return;
  launchError.hidden = true;
  launchError.textContent = "";
}

function showLaunchError(error) {
  if (!launchError) return;
  launchError.textContent = String(error || "Viewer launch failed.");
  launchError.hidden = false;
}

function showHome() {
  currentMode = "home";
  clearLaunchError();
  homeScreen.hidden = false;
  appShell.hidden = true;
  runtimePanel.hidden = true;
  setBusy(false);
}

function showCreateProject() {
  currentMode = "create-images";
  clearLaunchError();
  homeScreen.hidden = true;
  appShell.hidden = false;
  imageStep.hidden = false;
  associationStep.hidden = true;
  runtimePanel.hidden = true;
  workspaceSubtitle.textContent = "Create a new project";
  renderProjectImages();
  setBusy(false);
}

function showAssociations() {
  currentMode = "create-associations";
  clearLaunchError();
  imageStep.hidden = true;
  associationStep.hidden = false;
  runtimePanel.hidden = true;
  workspaceSubtitle.textContent = "Associate data to each image";
  renderAssociations();
  setBusy(false);
}

function showOpenProjectLog() {
  currentMode = "open";
  clearLaunchError();
  homeScreen.hidden = true;
  appShell.hidden = false;
  imageStep.hidden = false;
  associationStep.hidden = true;
  runtimePanel.hidden = false;
  workspaceSubtitle.textContent = "Open saved project";
  renderProjectImages("Opening saved project...");
  setBusy(false);
}

function basename(path) {
  return String(path || "").split(/[\\/]/).filter(Boolean).pop() || String(path || "");
}

function itemId(path) {
  return `${Date.now()}_${Math.random().toString(36).slice(2)}_${basename(path)}`;
}

function resetProjectInputs() {
  projectImages = [];
  renderProjectImages();
  renderAssociations();
}

function normaliseSelectedFiles(selected) {
  return (Array.isArray(selected) ? selected : [selected]).filter(Boolean).map(String);
}

function addProjectImages(paths) {
  const known = new Set(projectImages.map((item) => item.image));
  let added = 0;
  for (const path of paths) {
    if (known.has(path)) continue;
    projectImages.push({
      id: itemId(path),
      image: path,
      cellAnnotation: "",
      tissueAnnotation: "",
      spatialData: ""
    });
    known.add(path);
    added += 1;
  }
  appendLog(`Added ${added} image${added === 1 ? "" : "s"} to the project.`);
  renderProjectImages();
  renderAssociations();
}

function removeProjectImage(id) {
  const before = projectImages.length;
  projectImages = projectImages.filter((item) => item.id !== id);
  if (projectImages.length !== before) {
    appendLog("Removed image from project.");
  }
  if (currentMode === "create-associations" && projectImages.length === 0) {
    showCreateProject();
    return;
  }
  renderProjectImages();
  renderAssociations();
}

function pathText(path) {
  return path ? path : "None";
}

function renderProjectImages(message = null) {
  nextAssociations.disabled = projectImages.length === 0;
  runR.disabled = projectImages.length === 0;
  if (message) {
    imageList.textContent = message;
    return;
  }
  if (!projectImages.length) {
    imageList.textContent = "No images selected.";
    return;
  }
  imageList.replaceChildren(...projectImages.map((item, index) => {
    const row = document.createElement("div");
    row.className = "imageRow";
    const text = document.createElement("div");
    text.className = "imageRowText";
    text.innerHTML = `<strong>${index + 1}. ${basename(item.image)}</strong><code></code>`;
    text.querySelector("code").textContent = item.image;
    const remove = document.createElement("button");
    remove.type = "button";
    remove.textContent = "Remove";
    remove.dataset.action = "remove-image";
    remove.dataset.id = item.id;
    row.append(text, remove);
    return row;
  }));
}

function renderAssociations() {
  runR.disabled = projectImages.length === 0;
  if (!projectImages.length) {
    associationList.textContent = "No images selected.";
    return;
  }
  associationList.replaceChildren(...projectImages.map((item, index) => {
    const card = document.createElement("section");
    card.className = "associationCard";
    card.dataset.id = item.id;
    card.innerHTML = `
      <div class="associationHeader">
        <div>
          <strong>${index + 1}. ${basename(item.image)}</strong>
          <code></code>
        </div>
        <button type="button" data-action="remove-image" data-id="${item.id}">Remove image</button>
      </div>
      <div class="associationActions">
        <button type="button" data-action="set-cell" data-id="${item.id}">Select cell annotation</button>
        <button type="button" data-action="clear-cell" data-id="${item.id}">Clear</button>
        <button type="button" data-action="set-tissue" data-id="${item.id}">Select tissue annotation</button>
        <button type="button" data-action="clear-tissue" data-id="${item.id}">Clear</button>
        <button type="button" data-action="set-spatial" data-id="${item.id}">Select spatial transcriptomics data</button>
        <button type="button" data-action="clear-spatial" data-id="${item.id}">Clear</button>
      </div>
      <dl class="associationFiles">
        <div><dt>Cell</dt><dd data-field="cell"></dd></div>
        <div><dt>Tissue</dt><dd data-field="tissue"></dd></div>
        <div><dt>Spatial</dt><dd data-field="spatial"></dd></div>
      </dl>
    `;
    card.querySelector(".associationHeader code").textContent = item.image;
    card.querySelector('[data-field="cell"]').textContent = pathText(item.cellAnnotation);
    card.querySelector('[data-field="tissue"]').textContent = pathText(item.tissueAnnotation);
    card.querySelector('[data-field="spatial"]').textContent = pathText(item.spatialData);
    return card;
  }));
}

function quoteRString(value) {
  return JSON.stringify(String(value));
}

function quoteRVector(values) {
  return `c(${values.map(quoteRString).join(", ")})`;
}

function rNewProjectLaunchCode() {
  const lines = [
    "library(wsiTools)",
    "project_images <- data.frame(",
    `  image = ${quoteRVector(projectImages.map((item) => item.image))},`,
    `  cell_annotation = ${quoteRVector(projectImages.map((item) => item.cellAnnotation || ""))},`,
    `  tissue_annotation = ${quoteRVector(projectImages.map((item) => item.tissueAnnotation || ""))},`,
    `  spatial_data = ${quoteRVector(projectImages.map((item) => item.spatialData || ""))},`,
    "  stringsAsFactors = FALSE",
    ")",
    "viewer <- wsi_open_viewer(",
    "  project_images$image,",
    '  live = "yes",',
    '  tiled = "yes",',
    "  dynamic_tiles = TRUE,",
    "  open = FALSE,",
    "  wait = FALSE",
    ")"
  ];
  return lines.join("\n");
}

function rProjectLaunchCode(projectPath) {
  return [
    "library(wsiTools)",
    `project <- wsi_read_project(${quoteRString(projectPath)})`,
    "viewer <- wsi_open_viewer(",
    "  project$slide_path,",
    '  live = "yes",',
    '  tiled = "yes",',
    "  dynamic_tiles = TRUE,",
    "  open = FALSE,",
    "  wait = FALSE",
    ")",
    "restore_project_state(viewer, project, service = FALSE)"
  ].join("\n");
}

async function refreshLogs() {
  try {
    lastRustLogs = await invoke("viewer_logs");
    renderLogs();
  } catch (error) {
    appendLog(`Could not fetch R/viewer logs: ${error}`);
  }
}

function startLogPolling() {
  if (logTimer) return;
  logTimer = window.setInterval(refreshLogs, 1500);
}

function stopLogPolling() {
  if (!logTimer) return;
  window.clearInterval(logTimer);
  logTimer = null;
}

async function checkR() {
  appendLog("Checking Rscript availability.");
  try {
    const info = await invoke("check_r");
    setStatus(info, "ok");
  } catch (error) {
    setStatus("R not found", "error");
    appendLog(error);
  }
}

async function chooseFile(options, label) {
  appendLog(`${label} clicked; opening native file picker.`);
  setStatus(`selecting ${label.toLowerCase()}`, "info");
  const selected = await open(options);
  if (!selected || (Array.isArray(selected) && selected.length === 0)) {
    appendLog(`${label} picker returned no file.`);
    setStatus("nothing selected", "info");
    return null;
  }
  return selected;
}

async function openViewerWindow(url) {
  await invoke("open_viewer_window", { url });
  viewerWindowOpen = true;
  stopViewer.disabled = false;
}

async function handleLaunch(launcher, codeLog, successPrefix) {
  clearLaunchError();
  setBusy(true);
  setStatus("sending code to R", "info");
  appendLog("R code sent to R:");
  appendLog(codeLog);
  startLogPolling();
  try {
    const launch = await launcher();
    appendLog(`${successPrefix}: ${launch.viewer_url}`);
    if (launch.sync_url) appendLog(`Live sync endpoint: ${launch.sync_url}`);
    await openViewerWindow(launch.viewer_url);
    setStatus("viewer window open", "ok");
    await refreshLogs();
  } catch (error) {
    setStatus("viewer failed", "error");
    appendLog(`Viewer launch failed: ${error}`);
    showLaunchError(error);
    stopLogPolling();
  } finally {
    setBusy(false);
  }
}

async function selectAssociation(id, kind) {
  const item = projectImages.find((entry) => entry.id === id);
  if (!item) return;
  const specs = {
    cell: {
      title: `Select cell annotation for ${basename(item.image)}`,
      filters: [
        { name: "Cell annotation", extensions: ["geojson", "json", "csv", "tsv", "tif", "tiff", "ome.tif", "ome.tiff", "png"] },
        { name: "All files", extensions: ["*"] }
      ],
      key: "cellAnnotation",
      status: "cell annotation selected"
    },
    tissue: {
      title: `Select tissue annotation for ${basename(item.image)}`,
      filters: [
        { name: "Tissue annotation", extensions: ["geojson", "json"] },
        { name: "All files", extensions: ["*"] }
      ],
      key: "tissueAnnotation",
      status: "tissue annotation selected"
    },
    spatial: {
      title: `Select spatial transcriptomics data for ${basename(item.image)}`,
      filters: [
        { name: "R spatial object", extensions: ["rds", "rda", "rdata", "RData"] },
        { name: "All files", extensions: ["*"] }
      ],
      key: "spatialData",
      status: "spatial data selected"
    }
  };
  const spec = specs[kind];
  const selected = await chooseFile({
    multiple: false,
    directory: false,
    title: spec.title,
    filters: spec.filters
  }, spec.title);
  if (!selected) return;
  item[spec.key] = Array.isArray(selected) ? selected[0] : selected;
  appendLog(`${spec.status}: ${item[spec.key]}`);
  renderAssociations();
  setStatus(spec.status, "ok");
}

createProjectHome.addEventListener("click", () => {
  resetProjectInputs();
  showCreateProject();
  setStatus("add microscopy images", "info");
  appendLog("Create new project selected.");
});

openProjectHome.addEventListener("click", async () => {
  appendLog("Open project clicked; opening native project folder picker.");
  setStatus("opening project picker", "info");
  try {
    const selected = await open({
      multiple: false,
      directory: true,
      title: "Open previously saved wsiTools project"
    });
    const projectPath = Array.isArray(selected) ? selected[0] : selected;
    if (!projectPath) {
      appendLog("Project picker returned no folder.");
      setStatus("no project selected", "info");
      return;
    }
    showOpenProjectLog();
    appendLog(`Selected saved project: ${projectPath}`);
    await handleLaunch(
      () => invoke("start_r_project", { projectPath }),
      rProjectLaunchCode(projectPath),
      "Project viewer opened"
    );
  } catch (error) {
    setStatus("project selection failed", "error");
    appendLog(`Project picker failed: ${error}`);
  }
});

backHome.addEventListener("click", async () => {
  appendLog("Home clicked.");
  try {
    await invoke("stop_r_viewer");
  } catch (error) {
    appendLog(`Could not stop R while returning home: ${error}`);
  }
  viewerWindowOpen = false;
  stopLogPolling();
  resetProjectInputs();
  showHome();
  await refreshLogs();
});

addImage.addEventListener("click", async () => {
  try {
    const selected = await chooseFile({
      multiple: true,
      directory: false,
      title: "Add new image",
      filters: [
        {
          name: "Whole-slide and microscopy images",
          extensions: [
            "svs", "ndpi", "scn", "mrxs", "tif", "tiff", "btf",
            "ome.tif", "ome.tiff", "czi", "vsi", "bif", "dcm",
            "png", "jpg", "jpeg"
          ]
        },
        { name: "All files", extensions: ["*"] }
      ]
    }, "Add new image");
    if (!selected) return;
    addProjectImages(normaliseSelectedFiles(selected));
    setStatus("image added", "ok");
  } catch (error) {
    setStatus("image selection failed", "error");
    appendLog(`Image selection failed: ${error}`);
  }
});

nextAssociations.addEventListener("click", () => {
  if (!projectImages.length) {
    setStatus("add at least one image", "error");
    return;
  }
  showAssociations();
  setStatus("associate data to images", "info");
});

backToImages.addEventListener("click", () => {
  showCreateProject();
  setStatus("add or remove images", "info");
});

imageList.addEventListener("click", (event) => {
  const button = event.target.closest("button[data-action]");
  if (!button) return;
  if (button.dataset.action === "remove-image") {
    removeProjectImage(button.dataset.id);
  }
});

associationList.addEventListener("click", async (event) => {
  const button = event.target.closest("button[data-action]");
  if (!button) return;
  const id = button.dataset.id;
  const item = projectImages.find((entry) => entry.id === id);
  if (!item) return;
  try {
    if (button.dataset.action === "remove-image") {
      removeProjectImage(id);
    } else if (button.dataset.action === "set-cell") {
      await selectAssociation(id, "cell");
    } else if (button.dataset.action === "set-tissue") {
      await selectAssociation(id, "tissue");
    } else if (button.dataset.action === "set-spatial") {
      await selectAssociation(id, "spatial");
    } else if (button.dataset.action === "clear-cell") {
      item.cellAnnotation = "";
      renderAssociations();
    } else if (button.dataset.action === "clear-tissue") {
      item.tissueAnnotation = "";
      renderAssociations();
    } else if (button.dataset.action === "clear-spatial") {
      item.spatialData = "";
      renderAssociations();
    }
  } catch (error) {
    setStatus("selection failed", "error");
    appendLog(`Selection failed: ${error}`);
  }
});

runR.addEventListener("click", async () => {
  if (!projectImages.length) {
    setStatus("add image(s) first", "error");
    appendLog("Run R clicked without image input.");
    return;
  }
  await handleLaunch(
    () => invoke("start_r_new_project", { projectItems: projectImages }),
    rNewProjectLaunchCode(),
    "Viewer opened"
  );
});

stopViewer.addEventListener("click", async () => {
  appendLog("Stop R clicked.");
  try {
    await invoke("stop_r_viewer");
    viewerWindowOpen = false;
    setStatus("R stopped", "info");
    stopViewer.disabled = true;
    stopLogPolling();
    await refreshLogs();
  } catch (error) {
    setStatus("stop failed", "error");
    appendLog(error);
  }
});

copyLog.addEventListener("click", async () => {
  const text = combinedLogText() || "No log messages yet.";
  try {
    await navigator.clipboard.writeText(text);
    setStatus("log copied", "ok");
  } catch (error) {
    setStatus("copy failed; saving instead", "error");
    appendLog(`Clipboard copy failed: ${error}`);
    try {
      const file = await invoke("save_launcher_log", { logText: combinedLogText() });
      appendLog(`Launcher log saved: ${file}`);
    } catch (saveError) {
      appendLog(`Could not save launcher log: ${saveError}`);
    }
  }
});

saveLog.addEventListener("click", async () => {
  try {
    const file = await invoke("save_launcher_log", { logText: combinedLogText() || "No log messages yet." });
    setStatus("log saved", "ok");
    appendLog(`Launcher log saved: ${file}`);
  } catch (error) {
    setStatus("log save failed", "error");
    appendLog(`Could not save launcher log: ${error}`);
  }
});

window.addEventListener("error", (event) => {
  appendLog(`JavaScript error: ${event.message}`);
});

window.addEventListener("unhandledrejection", (event) => {
  appendLog(`Unhandled promise rejection: ${event.reason}`);
});

window.addEventListener("beforeunload", () => {
  invoke("stop_r_viewer").catch(() => {});
});

appendLog("wsiTools Desktop JavaScript loaded.");
renderProjectImages();
renderAssociations();
checkR();
refreshLogs();
