/* A focused read-only companion to wsiTools' Deep Zoom and level-0 GeoJSON APIs. */
(() => {
  'use strict';
  const el = id => document.getElementById(id);
  const canvas = el('overlay');
  const ctx = canvas.getContext('2d');
  const status = el('status');
  const loading = el('loading');
  const state = { viewer: null, manifest: null, features: [], visible: new Map(), opacity: 0.18, outlines: true, queued: false };

  function setStatus(message, error = false) {
    status.textContent = message;
    status.classList.toggle('error', error);
    if (error) { loading.textContent = message; loading.hidden = false; }
  }

  function featureClass(feature) {
    const p = feature.properties || {};
    return String(p.classification?.name || p.class || p.assigned_group || 'annotation');
  }

  function boundsOf(node, out) {
    if (!Array.isArray(node)) return;
    if (node.length >= 2 && typeof node[0] === 'number' && typeof node[1] === 'number') {
      const x = node[0], y = node[1];
      if (Number.isFinite(x) && Number.isFinite(y)) {
        out[0] = Math.min(out[0], x); out[1] = Math.min(out[1], y);
        out[2] = Math.max(out[2], x); out[3] = Math.max(out[3], y);
      }
    } else for (const child of node) boundsOf(child, out);
  }

  function addLine(path, line, close) {
    if (!Array.isArray(line) || !line.length) return;
    let started = false;
    for (const pair of line) {
      if (!Array.isArray(pair) || !Number.isFinite(pair[0]) || !Number.isFinite(pair[1])) continue;
      if (!started) { path.moveTo(pair[0], pair[1]); started = true; }
      else path.lineTo(pair[0], pair[1]);
    }
    if (close && started) path.closePath();
  }

  function buildFeature(feature) {
    const geometry = feature?.geometry;
    if (!geometry || !geometry.coordinates) return null;
    const type = geometry.type;
    const coords = geometry.coordinates;
    const bounds = [Infinity, Infinity, -Infinity, -Infinity];
    boundsOf(coords, bounds);
    if (!Number.isFinite(bounds[0])) return null;
    const path = new Path2D();
    if (type === 'Polygon') for (const ring of coords) addLine(path, ring, true);
    else if (type === 'MultiPolygon') for (const polygon of coords) for (const ring of polygon) addLine(path, ring, true);
    else if (type === 'LineString') addLine(path, coords, false);
    else if (type === 'MultiLineString') for (const line of coords) addLine(path, line, false);
    else if (type !== 'Point' && type !== 'MultiPoint') return null;
    return { type, coords, path, bounds, className: featureClass(feature) };
  }

  function colorFor(className) {
    const classes = state.manifest?.overlay?.classes || [];
    const match = classes.find(item => item.name.toLowerCase() === className.toLowerCase());
    return match?.color || '#b54255';
  }

  function pointOnScreen(item, x, y) {
    return state.viewer.viewport.pixelFromPoint(item.imageToViewportCoordinates(x, y), true);
  }

  function scaleBar(scaleX) {
    const mpp = Number(state.manifest?.image?.mpp_x);
    if (!Number.isFinite(mpp) || mpp <= 0 || scaleX <= 0) { el('scale').hidden = true; return; }
    const targetUm = 105 * mpp / scaleX;
    const exponent = Math.floor(Math.log10(targetUm));
    const base = 10 ** exponent;
    const nice = [1, 2, 5, 10].map(n => n * base).reduce((best, n) => Math.abs(n - targetUm) < Math.abs(best - targetUm) ? n : best);
    const px = nice / mpp * scaleX;
    el('scale-line').style.width = `${px}px`;
    el('scale-label').textContent = nice >= 1000 ? `${+(nice / 1000).toPrecision(2)} mm` : `${+nice.toPrecision(2)} µm`;
    el('scale').hidden = false;
  }

  function render() {
    state.queued = false;
    const viewer = state.viewer;
    if (!viewer || !viewer.world || !viewer.world.getItemCount()) return;
    const rect = canvas.getBoundingClientRect();
    const cssW = Math.max(1, rect.width), cssH = Math.max(1, rect.height);
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    const pixelW = Math.round(cssW * dpr), pixelH = Math.round(cssH * dpr);
    if (canvas.width !== pixelW || canvas.height !== pixelH) { canvas.width = pixelW; canvas.height = pixelH; }
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    const item = viewer.world.getItemAt(0);
    const width = Number(state.manifest.image.width), height = Number(state.manifest.image.height);
    const p0 = pointOnScreen(item, 0, 0), px = pointOnScreen(item, width, 0), py = pointOnScreen(item, 0, height);
    const sx = (px.x - p0.x) / width, sy = (py.y - p0.y) / height;
    if (!Number.isFinite(sx) || !Number.isFinite(sy) || sx <= 0 || sy <= 0) return;
    scaleBar(sx);
    ctx.setTransform(dpr * sx, 0, 0, dpr * sy, dpr * p0.x, dpr * p0.y);
    ctx.lineJoin = 'round'; ctx.lineCap = 'round'; ctx.lineWidth = 1.4 / sx;
    for (const feature of state.features) {
      if (state.visible.get(feature.className.toLowerCase()) === false) continue;
      const b = feature.bounds;
      if (p0.x + b[2] * sx < 0 || p0.x + b[0] * sx > cssW || p0.y + b[3] * sy < 0 || p0.y + b[1] * sy > cssH) continue;
      ctx.strokeStyle = colorFor(feature.className);
      ctx.fillStyle = colorFor(feature.className);
      if (feature.type === 'Polygon' || feature.type === 'MultiPolygon') {
        ctx.globalAlpha = state.opacity;
        ctx.fill(feature.path, 'evenodd');
        if (state.outlines) { ctx.globalAlpha = 0.9; ctx.stroke(feature.path); }
      } else if (feature.type === 'Point' || feature.type === 'MultiPoint') {
        const points = feature.type === 'Point' ? [feature.coords] : feature.coords;
        ctx.globalAlpha = 0.9;
        for (const p of points) { ctx.beginPath(); ctx.arc(p[0], p[1], 3.5 / sx, 0, Math.PI * 2); ctx.fill(); }
      } else { ctx.globalAlpha = 0.9; ctx.stroke(feature.path); }
    }
    ctx.globalAlpha = 1;
  }

  function scheduleRender() {
    if (!state.queued) { state.queued = true; requestAnimationFrame(render); }
  }

  function makeLegend() {
    const legend = el('legend');
    const classes = state.manifest?.overlay?.classes || [];
    if (!classes.length) { legend.textContent = 'Image only'; return; }
    for (const item of classes) {
      const label = document.createElement('label'); label.className = 'legend-item';
      const check = document.createElement('input'); check.type = 'checkbox'; check.checked = true;
      check.setAttribute('aria-label', `Show ${item.name}`);
      check.addEventListener('change', () => { state.visible.set(item.name.toLowerCase(), check.checked); scheduleRender(); });
      const swatch = document.createElement('span'); swatch.className = 'swatch'; swatch.style.backgroundColor = item.color;
      const text = document.createElement('span'); text.textContent = item.name;
      label.append(check, swatch, text); legend.append(label);
    }
  }

  async function readJSON(url) {
    const response = await fetch(url);
    if (!response.ok) throw new Error(`${url}: HTTP ${response.status}`);
    return response.json();
  }

  async function init() {
    try {
      state.manifest = await readJSON('manifest.json');
      const manifest = state.manifest;
      if (manifest.mode === 'remote-tiff') return; // remote.js owns source-only bundles.
      if (manifest.schema_version !== 1 || !manifest.image?.dzi || !(manifest.image.width > 0) || !(manifest.image.height > 0)) throw new Error('Invalid viewer manifest');
      el('title').textContent = manifest.title || 'Tissue viewer';
      document.title = `${manifest.title || 'Tissue viewer'} · wsiTools`;
      if (manifest.overlay) {
        const data = await readJSON(manifest.overlay.url);
        if (data.type !== 'FeatureCollection' || !Array.isArray(data.features)) throw new Error('Overlay is not a GeoJSON FeatureCollection');
        state.features = data.features.map(buildFeature).filter(Boolean);
        state.opacity = Math.min(0.65, Math.max(0, Number(manifest.overlay.opacity) || 0.18));
        el('opacity').value = String(state.opacity);
        el('opacity-value').textContent = `${Math.round(state.opacity * 100)}%`;
      }
      makeLegend();
      if (!window.OpenSeadragon) throw new Error('OpenSeadragon did not load');
      state.viewer = OpenSeadragon({
        element: el('osd'), tileSources: manifest.image.dzi,
        showNavigationControl: false, showNavigator: false,
        minZoomImageRatio: 0.8, maxZoomPixelRatio: 8,
        animationTime: 0.3, blendTime: 0.12,
        gestureSettingsMouse: { clickToZoom: false, dblClickToZoom: true, scrollToZoom: true, dragToPan: true },
        gestureSettingsTouch: { pinchToZoom: true, dragToPan: true }
      });
      state.viewer.addHandler('open', () => {
        loading.hidden = true;
        setStatus(state.features.length ? `${state.features.length.toLocaleString()} exact regions · level-0 coordinates` : 'Full-resolution tissue image');
        scheduleRender();
      });
      state.viewer.addHandler('open-failed', event => setStatus(`Could not load tiles: ${event?.message || 'check the Deep Zoom files'}`, true));
      state.viewer.addHandler('tile-load-failed', () => setStatus('Some image tiles could not load. Check the tiles directory.', true));
      for (const name of ['animation', 'animation-finish', 'resize', 'update-viewport']) state.viewer.addHandler(name, scheduleRender);
      window.addEventListener('resize', scheduleRender);
      el('zoom-in').addEventListener('click', () => { state.viewer.viewport.zoomBy(1.5); state.viewer.viewport.applyConstraints(); });
      el('zoom-out').addEventListener('click', () => { state.viewer.viewport.zoomBy(1 / 1.5); state.viewer.viewport.applyConstraints(); });
      el('fit').addEventListener('click', () => state.viewer.viewport.goHome());
      el('opacity').addEventListener('input', event => { state.opacity = Number(event.target.value); el('opacity-value').textContent = `${Math.round(state.opacity * 100)}%`; scheduleRender(); });
      el('outlines').addEventListener('change', event => { state.outlines = event.target.checked; scheduleRender(); });
    } catch (error) { setStatus(error.message || String(error), true); }
  }
  init();
})();
