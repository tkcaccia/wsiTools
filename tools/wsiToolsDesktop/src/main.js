import { invoke } from "@tauri-apps/api/core";
import { open } from "@tauri-apps/plugin-dialog";
import { LogicalSize } from "@tauri-apps/api/dpi";
import { getCurrentWindow } from "@tauri-apps/api/window";

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
const homeRHelp = document.getElementById("homeRHelp");
const downloadRHome = document.getElementById("downloadRHome");
const downloadRPanel = document.getElementById("downloadRPanel");
const copyLog = document.getElementById("copyLog");
const saveLog = document.getElementById("saveLog");

let projectImages = [];
let currentMode = "home";
let logTimer = null;
let lastRustLogs = [];
let viewerWindowOpen = false;
let rAvailable = false;
let rDownloadUrl = "https://cran.r-project.org/";
const localLogs = [];
const spatialInspections = new Map();
const launcherWindow = getCurrentWindow();
const launcherWindowSizes = {
  home: { width: 620, height: 380, minWidth: 520, minHeight: 320, maxHeight: 720 },
  images: { width: 760, height: 460, minWidth: 620, minHeight: 380, maxHeight: 760 },
  associations: { width: 860, height: 620, minWidth: 700, minHeight: 460, maxHeight: 860 },
  runtime: { width: 860, height: 720, minWidth: 700, minHeight: 520, maxHeight: 900 }
};
let launcherResizeTimer = null;

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

function activeLauncherLayout() {
  if (!homeScreen.hidden) return "home";
  if (!runtimePanel.hidden) return "runtime";
  if (!associationStep.hidden) return "associations";
  return "images";
}

function activeLauncherContent() {
  if (!homeScreen.hidden) return document.querySelector(".homeCard");
  return document.querySelector(".launcherPanel");
}

function launcherTargetSize(layout = activeLauncherLayout()) {
  const preset = launcherWindowSizes[layout] || launcherWindowSizes.home;
  const content = activeLauncherContent();
  const rect = content?.getBoundingClientRect();
  const padding = layout === "home" ? 44 : 48;
  const contentWidth = Math.ceil(rect?.width || preset.width - padding) + padding;
  const contentHeight = Math.ceil(content?.scrollHeight || rect?.height || preset.height - padding) + padding;
  return {
    width: Math.max(preset.minWidth, Math.min(1040, Math.max(preset.width, contentWidth))),
    height: Math.max(preset.minHeight, Math.min(preset.maxHeight, Math.max(preset.height, contentHeight))),
    minWidth: preset.minWidth,
    minHeight: preset.minHeight
  };
}

async function fitLauncherWindow(layout = activeLauncherLayout(), center = false) {
  if (!launcherWindow) return;
  const size = launcherTargetSize(layout);
  try {
    await launcherWindow.setMinSize(new LogicalSize(size.minWidth, size.minHeight));
    await launcherWindow.setSize(new LogicalSize(size.width, size.height));
    if (center) await launcherWindow.center();
  } catch (error) {
    appendLog(`Could not resize launcher window: ${error}`);
  }
}

function scheduleLauncherWindowFit(layout = activeLauncherLayout(), center = false) {
  window.clearTimeout(launcherResizeTimer);
  launcherResizeTimer = window.setTimeout(() => fitLauncherWindow(layout, center), 40);
}

function setBusy(isBusy) {
  openProjectHome.disabled = isBusy || !rAvailable;
  createProjectHome.disabled = isBusy || !rAvailable;
  backHome.disabled = isBusy;
  addImage.disabled = isBusy;
  nextAssociations.disabled = isBusy || projectImages.length === 0;
  backToImages.disabled = isBusy;
  runR.disabled = isBusy || !rAvailable || projectImages.length === 0;
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
  scheduleLauncherWindowFit("home", true);
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
  scheduleLauncherWindowFit("images");
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
  scheduleLauncherWindowFit("associations");
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
  scheduleLauncherWindowFit("runtime");
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
      spatialData: "",
      spatialSampleId: ""
    });
    known.add(path);
    added += 1;
  }
  appendLog(`Added ${added} image${added === 1 ? "" : "s"} to the project.`);
  renderProjectImages();
  renderAssociations();
}

function normaliseMatchToken(value) {
  return String(value || "").toLowerCase().replace(/[^a-z0-9]+/g, "");
}

function spatialInspectionFor(path) {
  return spatialInspections.get(path || "") || null;
}

async function inspectSpatialObject(path) {
  if (!path) return null;
  const cached = spatialInspectionFor(path);
  if (cached) return cached;
  appendLog(`Inspecting spatial transcriptomics object: ${path}`);
  const inspection = await invoke("inspect_spatial_object", { filePath: path });
  spatialInspections.set(path, inspection);
  const count = Array.isArray(inspection.tissues) ? inspection.tissues.length : 0;
  appendLog(`Spatial object detected: ${inspection.objectType || "unknown"} with ${count} tissue/sample ${count === 1 ? "entry" : "entries"}.`);
  return inspection;
}

function autoAssignSpatialTissues(spatialPath, inspection) {
  const tissues = Array.isArray(inspection?.tissues) ? inspection.tissues : [];
  if (!spatialPath || tissues.length === 0) return;
  const tissueIds = tissues.map((entry) => entry.tissueId).filter(Boolean);
  const used = new Set(projectImages.map((item) => item.spatialSampleId).filter(Boolean));
  for (const item of projectImages) {
    if (!item.spatialData) {
      item.spatialData = spatialPath;
    }
    if (item.spatialData !== spatialPath || item.spatialSampleId) {
      continue;
    }
    const imageToken = normaliseMatchToken(basename(item.image));
    let match = tissueIds.find((id) => {
      const token = normaliseMatchToken(id);
      return token && (imageToken.includes(token) || token.includes(imageToken));
    });
    if (!match && tissueIds.length === projectImages.length) {
      match = tissueIds.find((id) => !used.has(id));
    }
    if (match) {
      item.spatialSampleId = match;
      used.add(match);
    }
  }
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
  runR.disabled = !rAvailable || projectImages.length === 0;
  if (message) {
    imageList.textContent = message;
    scheduleLauncherWindowFit(activeLauncherLayout());
    return;
  }
  if (!projectImages.length) {
    imageList.textContent = "No images selected.";
    scheduleLauncherWindowFit(activeLauncherLayout());
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
  scheduleLauncherWindowFit(activeLauncherLayout());
}

function renderAssociations() {
  runR.disabled = projectImages.length === 0;
  if (!projectImages.length) {
    associationList.textContent = "No images selected.";
    scheduleLauncherWindowFit(activeLauncherLayout());
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
    const inspection = spatialInspectionFor(item.spatialData);
    const tissues = Array.isArray(inspection?.tissues) ? inspection.tissues : [];
    if (item.spatialData && tissues.length > 0) {
      const mapping = document.createElement("div");
      mapping.className = "spatialMapping";
      mapping.innerHTML = `
        <label>
          <span>Spatial tissue/sample for this image</span>
          <select data-action="set-spatial-sample" data-id="${item.id}"></select>
        </label>
        <small></small>
      `;
      const select = mapping.querySelector("select");
      const empty = document.createElement("option");
      empty.value = "";
      empty.textContent = "Choose tissue/sample in spatial object";
      select.append(empty);
      for (const entry of tissues) {
        const value = String(entry.tissueId || "");
        const option = document.createElement("option");
        option.value = value;
        option.textContent = `${value}${entry.nSpots == null ? "" : ` (${entry.nSpots} spots/cells)`}`;
        select.append(option);
      }
      select.value = item.spatialSampleId || "";
      mapping.querySelector("small").textContent =
        `${inspection.objectType || "Spatial object"}: map the microscopy image to the matching tissue saved inside the R object.`;
      card.append(mapping);
    } else if (item.spatialData && inspection?.message) {
      const mapping = document.createElement("div");
      mapping.className = "spatialMapping";
      mapping.textContent = inspection.message;
      card.append(mapping);
    }
    return card;
  }));
  scheduleLauncherWindowFit(activeLauncherLayout());
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
    `  spatial_sample_id = ${quoteRVector(projectImages.map((item) => item.spatialSampleId || ""))},`,
    "  stringsAsFactors = FALSE",
    ")",
    "viewer <- wsi_open_viewer(",
    "  project_images$image,",
    '  live = "yes",',
    '  tiled = "yes",',
    "  dynamic_tiles = FALSE,",
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
    "  dynamic_tiles = FALSE,",
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
  const info = await invoke("check_r");
  rAvailable = Boolean(info.available);
  rDownloadUrl = info.downloadUrl || rDownloadUrl;
  if (rAvailable) {
    setStatus(info.message, "ok");
    if (homeRHelp) homeRHelp.hidden = true;
    if (downloadRPanel) downloadRPanel.hidden = true;
    appendLog(`Rscript source: ${info.source || "unknown"}`);
  } else {
    setStatus("R not found", "error");
    appendLog(info.message);
    if (homeRHelp) homeRHelp.hidden = false;
    if (downloadRPanel) downloadRPanel.hidden = false;
  }
  setBusy(false);
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
  if (!rAvailable) {
    const message = `R is required before starting the viewer. Install R from ${rDownloadUrl}, restart wsiTools Desktop, and try again.`;
    setStatus("R not found", "error");
    appendLog(message);
    showLaunchError(message);
    return;
  }
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
  if (kind === "spatial") {
    item.spatialSampleId = "";
    const inspection = await inspectSpatialObject(item.spatialData);
    const tissues = Array.isArray(inspection?.tissues) ? inspection.tissues : [];
    if (tissues.length === 1) {
      item.spatialSampleId = tissues[0].tissueId || "";
    } else if (tissues.length > 1) {
      autoAssignSpatialTissues(item.spatialData, inspection);
    }
  }
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
      item.spatialSampleId = "";
      renderAssociations();
    }
  } catch (error) {
    setStatus("selection failed", "error");
    appendLog(`Selection failed: ${error}`);
  }
});

associationList.addEventListener("change", (event) => {
  const select = event.target.closest('select[data-action="set-spatial-sample"]');
  if (!select) return;
  const item = projectImages.find((entry) => entry.id === select.dataset.id);
  if (!item) return;
  item.spatialSampleId = select.value || "";
  appendLog(`Mapped ${basename(item.image)} to spatial tissue/sample: ${item.spatialSampleId || "not selected"}.`);
  setStatus("spatial tissue mapped", "ok");
});

runR.addEventListener("click", async () => {
  if (!projectImages.length) {
    setStatus("add image(s) first", "error");
    appendLog("Run R clicked without image input.");
    return;
  }
  for (const item of projectImages) {
    const inspection = spatialInspectionFor(item.spatialData);
    const tissues = Array.isArray(inspection?.tissues) ? inspection.tissues : [];
    if (item.spatialData && tissues.length > 1 && !item.spatialSampleId) {
      const message = `Choose the spatial tissue/sample for ${basename(item.image)} before running R.`;
      setStatus("spatial mapping needed", "error");
      appendLog(message);
      showLaunchError(message);
      return;
    }
  }
  await handleLaunch(
    () => invoke("start_r_new_project", { projectItems: projectImages }),
    rNewProjectLaunchCode(),
    "Viewer opened"
  );
});

async function openRDownloadPage() {
  appendLog(`Opening R download page: ${rDownloadUrl}`);
  try {
    await invoke("open_r_download_page");
    setStatus("R download page opened", "info");
  } catch (error) {
    setStatus("open browser failed", "error");
    appendLog(error);
    window.location.href = rDownloadUrl;
  }
}

downloadRHome.addEventListener("click", openRDownloadPage);
downloadRPanel.addEventListener("click", openRDownloadPage);

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
