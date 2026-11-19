/* Source-only TIFF viewer. Exact vectors remain downloadable and untouched. */
(() => {
  'use strict';
  const $ = id => document.getElementById(id);
  const state = { ready: false, detailRequests: 0, detailSuccess: 0, detailFallbacks: 0,
    networkFailures: 0, features: [], fragments: [], viewer: null, manifest: null, opacity: .18, outlines: true };
  const canvas = $('overlay'), ctx = canvas.getContext('2d');
  const status = (message, error = false) => {
    $('status').textContent = message; $('status').classList.toggle('error', error);
    if (error) { $('loading').textContent = message; $('loading').hidden = false; }
  };
  const progress = () => {
    const node = $('tile-progress');
    if (!state.manifest?.show_tile_progress) { node.hidden = true; return; }
    node.hidden = state.detailRequests === 0;
    node.textContent = `${state.detailRequests} source tile${state.detailRequests === 1 ? '' : 's'} loading…`;
  };
  async function json(url) { const r = await fetch(url); if (!r.ok) throw Error(`${url}: HTTP ${r.status}`); return r.json(); }
  function pathFor(geometry) {
    if (!geometry || !geometry.coordinates) throw Error('Missing GeoJSON geometry');
    const path = new Path2D(), bounds = [Infinity, Infinity, -Infinity, -Infinity];
    const update = ([x, y]) => {
      if (!Number.isFinite(x) || !Number.isFinite(y)) throw Error('Invalid contour coordinate');
      bounds[0] = Math.min(bounds[0], x); bounds[1] = Math.min(bounds[1], y);
      bounds[2] = Math.max(bounds[2], x); bounds[3] = Math.max(bounds[3], y);
    };
    const line = (ring, close = true) => {
      ring.forEach(([x, y], i) => {
        update([x, y]);
        if (i) path.lineTo(x, y); else path.moveTo(x, y);
      }); if (close) path.closePath();
    };
    if (geometry.type === 'Polygon') geometry.coordinates.forEach(p => line(p));
    else if (geometry.type === 'MultiPolygon') geometry.coordinates.forEach(p => p.forEach(r => line(r)));
    else if (geometry.type === 'LineString') line(geometry.coordinates, false);
    else if (geometry.type === 'MultiLineString') geometry.coordinates.forEach(p => line(p, false));
    else if (geometry.type === 'Point') update(geometry.coordinates);
    else if (geometry.type === 'MultiPoint') geometry.coordinates.forEach(update);
    else throw Error(`Unsupported GeoJSON geometry: ${geometry.type}`);
    return { path, bounds, type: geometry.type, coords: geometry.coordinates };
  }
  function className(f) { const p = f.properties || {}; return String(p.classification?.name || p.class || p.assigned_group || 'annotation'); }
  function makeFeatures(data) {
    if (data.type !== 'FeatureCollection' || !Array.isArray(data.features)) throw Error('Invalid GeoJSON');
    return data.features.map(f => {
      const shape = pathFor(f.geometry);
      const name = className(f);
      const color = state.manifest.overlay.classes.find(x => x.name.toLowerCase() === name.toLowerCase())?.color || '#b54255';
      return { ...shape, color, name };
    });
  }
  let queued = false;
  function draw() {
    queued = false;
    const viewer = state.viewer;
    if (!viewer?.world?.getItemCount()) return;
    const rect = canvas.getBoundingClientRect(), dpr = Math.min(devicePixelRatio || 1, 2);
    canvas.width = Math.max(1, Math.round(rect.width * dpr)); canvas.height = Math.max(1, Math.round(rect.height * dpr));
    ctx.setTransform(1, 0, 0, 1, 0, 0); ctx.clearRect(0, 0, canvas.width, canvas.height);
    const item = viewer.world.getItemAt(0), w = state.manifest.image.width, h = state.manifest.image.height;
    const a = viewer.viewport.pixelFromPoint(item.imageToViewportCoordinates(0, 0), true);
    const b = viewer.viewport.pixelFromPoint(item.imageToViewportCoordinates(w, h), true);
    const sx = (b.x - a.x) / w, sy = (b.y - a.y) / h;
    if (!(sx > 0 && sy > 0)) return;
    const mpp = state.manifest.image.mpp_x;
    if (mpp > 0) {
      const target = 105 * mpp / sx, power = 10 ** Math.floor(Math.log10(target));
      const nice = [1, 2, 5, 10].map(n => n * power).reduce((best, n) => Math.abs(n - target) < Math.abs(best - target) ? n : best);
      $('scale-line').style.width = `${nice / mpp * sx}px`;
      $('scale-label').textContent = nice >= 1000 ? `${+(nice / 1000).toPrecision(2)} mm` : `${+nice.toPrecision(2)} µm`;
      $('scale').hidden = false;
    }
    ctx.setTransform(dpr * sx, 0, 0, dpr * sy, dpr * a.x, dpr * a.y);
    ctx.lineJoin = 'round'; ctx.lineCap = 'round'; ctx.lineWidth = 1.4 / sx;
    for (const f of state.features) {
      if (state.visible?.get(f.name.toLowerCase()) === false) continue;
      const q = f.bounds;
      if (a.x + q[2] * sx < 0 || a.x + q[0] * sx > rect.width || a.y + q[3] * sy < 0 || a.y + q[1] * sy > rect.height) continue;
      ctx.fillStyle = f.color; ctx.strokeStyle = f.color;
      if (f.type === 'Polygon' || f.type === 'MultiPolygon') {
        ctx.globalAlpha = state.opacity; ctx.fill(f.path, 'evenodd');
        if (state.outlines) { ctx.globalAlpha = .94; ctx.stroke(f.path); }
      } else if (f.type === 'Point' || f.type === 'MultiPoint') {
        ctx.globalAlpha = .94;
        const points = f.type === 'Point' ? [f.coords] : f.coords;
        for (const p of points) { ctx.beginPath(); ctx.arc(p[0], p[1], 3.5 / sx, 0, 2 * Math.PI); ctx.fill(); }
      } else { ctx.globalAlpha = .94; ctx.stroke(f.path); }
    }
    ctx.globalAlpha = 1;
    if (state.maskAlpha) {
      ctx.globalCompositeOperation = 'destination-in'; ctx.imageSmoothingEnabled = false;
      ctx.drawImage(state.maskAlpha, 0, 0, w, h);
      ctx.globalCompositeOperation = 'source-over';
    }
  }
  function schedule() { if (!queued) { queued = true; requestAnimationFrame(draw); } }
  function tileSource(manifest, preview, maskAlpha) {
    const w = manifest.image.width, h = manifest.image.height, tileSize = 512;
    const maxLevel = Math.ceil(Math.log2(Math.max(w, h)));
    let tiffPromise, countPromise;
    const images = new Map(), infos = new Map();
    const tiff = () => tiffPromise ||= GeoTIFF.fromUrl(manifest.image.url).catch(e => { tiffPromise = null; throw e; });
    const info = async index => {
      if (!infos.has(index)) infos.set(index, (async () => {
        const image = await (images.get(index) || tiff().then(t => t.getImage(index)));
        images.set(index, Promise.resolve(image));
        const ratioX = w / image.getWidth(), ratioY = h / image.getHeight();
        if (!(ratioX >= 1 && ratioY >= 1)) throw Error('Invalid TIFF pyramid dimensions');
        return { image, ratioX, ratioY, downsample: Math.max(ratioX, ratioY) };
      })().catch(e => { infos.delete(index); images.delete(index); throw e; }));
      return infos.get(index);
    };
    const choose = async factor => {
      countPromise ||= tiff().then(t => t.getImageCount()).catch(e => { countPromise = null; throw e; });
      const count = await countPromise; if (!count) throw Error('TIFF has no image levels');
      let index = Math.min(count - 1, Math.max(0, Math.floor(Math.log2(factor))));
      let selected = await info(index);
      while (selected.downsample > factor * 1.01 && index > 0) selected = await info(--index);
      while (index + 1 < count) {
        const next = await info(index + 1);
        if (next.downsample <= selected.downsample || next.downsample > factor * 1.01) break;
        selected = next; index++;
      }
      return selected;
    };
    const finish = (context, tile) => {
      const image = new Image();
      image.onload = () => { if (!context.userData?.aborted) context.finish(image, null, 'image'); };
      image.onerror = () => { if (!context.userData?.aborted) context.finish(null, 'Tile canvas could not render'); };
      image.src = tile.toDataURL('image/png');
    };
    const applyMask = (c, box, outW, outH) => {
      if (!maskAlpha) return;
      c.save(); c.globalCompositeOperation = 'destination-in'; c.imageSmoothingEnabled = false;
      c.drawImage(maskAlpha, box.x / w * maskAlpha.width, box.y / h * maskAlpha.height,
        box.w / w * maskAlpha.width, box.h / h * maskAlpha.height, 0, 0, outW, outH);
      c.restore();
    };
    const previewTile = (context, box, outW, outH) => {
      const tile = document.createElement('canvas'); tile.width = outW; tile.height = outH;
      const c = tile.getContext('2d'); c.imageSmoothingEnabled = true; c.imageSmoothingQuality = 'high';
      c.drawImage(preview, box.x / w * preview.naturalWidth, box.y / h * preview.naturalHeight,
        box.w / w * preview.naturalWidth, box.h / h * preview.naturalHeight, 0, 0, outW, outH);
      applyMask(c, box, outW, outH);
      finish(context, tile);
    };
    return {
      width: w, height: h, tileSize, tileOverlap: 0, minLevel: 0, maxLevel,
      getTileUrl(level, x, y) { return `source-tiff:${level}:${x}:${y}`; },
      getTilePostData(level, x, y) { return { level, x, y }; },
      hasTransparency() { return !!maskAlpha; },
      async downloadTileStart(context) {
        const { level, x, y } = context.postData;
        const factor = 2 ** (maxLevel - level), left = x * tileSize * factor, top = y * tileSize * factor;
        const box = { x: left, y: top, w: Math.max(0, Math.min(w - left, tileSize * factor)), h: Math.max(0, Math.min(h - top, tileSize * factor)) };
        const outW = Math.max(1, Math.ceil(box.w / factor)), outH = Math.max(1, Math.ceil(box.h / factor));
        if (factor > 32) { previewTile(context, box, outW, outH); return; }
        state.detailRequests++; progress();
        try {
          const selected = await choose(factor), image = selected.image;
          const window = [Math.floor(box.x / selected.ratioX), Math.floor(box.y / selected.ratioY),
            Math.min(image.getWidth(), Math.ceil((box.x + box.w) / selected.ratioX)),
            Math.min(image.getHeight(), Math.ceil((box.y + box.h) / selected.ratioY))];
          const rgb = await image.readRGB({ window, width: outW, height: outH, interleave: true,
            resampleMethod: manifest.image.rgb_resample });
          if (rgb.length !== outW * outH * 3) throw Error('Unexpected RGB tile length');
          const rgba = new Uint8ClampedArray(outW * outH * 4);
          for (let i = 0, j = 0; i < rgb.length; i += 3, j += 4) {
            rgba[j] = rgb[i]; rgba[j + 1] = rgb[i + 1]; rgba[j + 2] = rgb[i + 2]; rgba[j + 3] = 255;
          }
          const tile = document.createElement('canvas'); tile.width = outW; tile.height = outH;
          const c = tile.getContext('2d');
          c.putImageData(new ImageData(rgba, outW, outH), 0, 0);
          applyMask(c, box, outW, outH);
          state.detailSuccess++; finish(context, tile);
        } catch (e) {
          state.detailFallbacks++; state.networkFailures++;
          status(`Source detail fallback: ${e.message}`, true);
          previewTile(context, box, outW, outH);
        } finally { state.detailRequests--; progress(); }
      },
      downloadTileAbort(context) { if (context.userData) context.userData.aborted = true; }
    };
  }
  function loadImage(url) {
    return new Promise((resolve, reject) => {
      const image = new Image(); image.onload = () => resolve(image); image.onerror = () => reject(Error(`Could not load ${url}`)); image.src = url;
    });
  }
  function alphaMask(image) {
    const result = document.createElement('canvas'); result.width = image.naturalWidth; result.height = image.naturalHeight;
    const c = result.getContext('2d', { willReadFrequently: true });
    c.drawImage(image, 0, 0);
    const data = c.getImageData(0, 0, result.width, result.height);
    for (let i = 0; i < data.data.length; i += 4) {
      const keep = data.data[i + 3] >= 128 && Math.max(data.data[i], data.data[i + 1], data.data[i + 2]) >= 128;
      data.data[i] = data.data[i + 1] = data.data[i + 2] = 255;
      data.data[i + 3] = keep ? 255 : 0;
    }
    c.putImageData(data, 0, 0);
    return result;
  }
  function findFragments(mask, sourceW, sourceH) {
    const sample = document.createElement('canvas'); sample.width = 256; sample.height = 256;
    const c = sample.getContext('2d', { willReadFrequently: true }); c.imageSmoothingEnabled = false;
    c.drawImage(mask, 0, 0, 256, 256);
    const pixels = c.getImageData(0, 0, 256, 256).data, seen = new Uint8Array(256 * 256), fragments = [];
    for (let i = 0; i < seen.length; i++) {
      if (seen[i] || pixels[i * 4 + 3] < 128 || Math.max(pixels[i * 4], pixels[i * 4 + 1], pixels[i * 4 + 2]) < 128) continue;
      const queue = [i]; seen[i] = 1; let head = 0, minX = 256, minY = 256, maxX = 0, maxY = 0;
      while (head < queue.length) {
        const v = queue[head++], x = v % 256, y = Math.floor(v / 256);
        minX = Math.min(minX, x); maxX = Math.max(maxX, x); minY = Math.min(minY, y); maxY = Math.max(maxY, y);
        for (const n of [v - 1, v + 1, v - 256, v + 256]) {
          if (n < 0 || n >= seen.length || seen[n] || Math.abs(n % 256 - x) + Math.abs(Math.floor(n / 256) - y) !== 1) continue;
          const k = n * 4;
          if (pixels[k + 3] < 128 || Math.max(pixels[k], pixels[k + 1], pixels[k + 2]) < 128) continue;
          seen[n] = 1; queue.push(n);
        }
      }
      if (queue.length >= 5) fragments.push({ pixels: queue.length, x: minX / 256 * sourceW, y: minY / 256 * sourceH,
        w: (maxX - minX + 1) / 256 * sourceW, h: (maxY - minY + 1) / 256 * sourceH });
    }
    return fragments.sort((a, b) => b.pixels - a.pixels);
  }
  async function init() {
    try {
      const manifest = await json('manifest.json'); if (manifest.mode !== 'remote-tiff') return;
      state.manifest = manifest;
      if (!window.OpenSeadragon || !window.GeoTIFF) throw Error('Viewer libraries did not load');
      $('title').textContent = manifest.title; document.title = `${manifest.title} · wsiTools`;
      $('mode').textContent = 'Read-only · source TIFF detail';
      const preview = await loadImage(manifest.image.preview);
      if (manifest.overlay) {
        const exact = await json(manifest.overlay.exact_url);
        const display = manifest.overlay.display_url ? await json(manifest.overlay.display_url) : null;
        state.exactFeatures = makeFeatures(exact); state.displayFeatures = display ? makeFeatures(display) : null;
        state.features = manifest.overlay.contours === 'display-smoothed' && display ? state.displayFeatures : state.exactFeatures;
        state.opacity = Math.min(.65, Math.max(0, Number(manifest.overlay.opacity) || .18));
        $('opacity').value = state.opacity; $('opacity-value').textContent = `${Math.round(state.opacity * 100)}%`;
        state.visible = new Map();
        for (const item of manifest.overlay.classes) {
          const label = document.createElement('label'); label.className = 'legend-item';
          const check = document.createElement('input'); check.type = 'checkbox'; check.checked = true;
          check.setAttribute('aria-label', `Show ${item.name}`);
          check.addEventListener('change', () => { state.visible.set(item.name.toLowerCase(), check.checked); schedule(); });
          const swatch = document.createElement('span'); swatch.className = 'swatch'; swatch.style.backgroundColor = item.color;
          const text = document.createElement('span'); text.textContent = item.name;
          label.append(check, swatch, text); $('legend').append(label);
        }
        const downloads = $('downloads'); downloads.hidden = false;
        const exactLink = document.createElement('a'); exactLink.href = manifest.overlay.exact_url; exactLink.download = '';
        exactLink.textContent = 'Download exact GeoJSON'; downloads.append(exactLink);
        if (display) {
          $('contour-wrap').hidden = false; $('contours').value = 'display';
          $('contours').addEventListener('change', e => { state.features = e.target.value === 'display' ? state.displayFeatures : state.exactFeatures; schedule(); });
          const link = document.createElement('a'); link.href = manifest.overlay.display_url; link.download = '';
          link.textContent = 'Download display-only contours'; downloads.append(link);
        }
      } else $('legend').textContent = 'Image only';
      let maskAlpha = null;
      if (manifest.display_mask) {
        const mask = await loadImage(manifest.display_mask.url);
        maskAlpha = alphaMask(mask);
        state.maskAlpha = maskAlpha;
        if (manifest.display_mask.fragment_navigation) state.fragments = findFragments(mask, manifest.image.width, manifest.image.height);
        if (state.fragments.length) {
          $('fragment-wrap').hidden = false;
          state.fragments.forEach((f, i) => { const option = document.createElement('option'); option.value = String(i); option.textContent = `Fragment ${i + 1}`; $('fragment').append(option); });
        }
      }
      state.viewer = OpenSeadragon({ element: $('osd'), tileSources: tileSource(manifest, preview, maskAlpha),
        showNavigationControl: false, showNavigator: false, minZoomImageRatio: .8, maxZoomPixelRatio: 8,
        animationTime: .3, blendTime: .12,
        gestureSettingsMouse: { clickToZoom: false, dblClickToZoom: true, scrollToZoom: true, dragToPan: true },
        gestureSettingsTouch: { pinchToZoom: true, dragToPan: true } });
      state.viewer.addHandler('open', () => { $('loading').hidden = true; state.ready = true; status(manifest.overlay ? `${manifest.overlay.feature_count} regions · exact level-0 coordinates` : 'Source TIFF image'); schedule(); });
      state.viewer.addHandler('open-failed', e => status(`Could not open source: ${e.message}`, true));
      state.viewer.addHandler('tile-load-failed', () => { state.networkFailures++; status('A source tile could not load', true); });
      for (const name of ['animation', 'animation-finish', 'resize', 'update-viewport']) state.viewer.addHandler(name, schedule);
      addEventListener('resize', schedule);
      $('zoom-in').onclick = () => { state.viewer.viewport.zoomBy(1.5); state.viewer.viewport.applyConstraints(); };
      $('zoom-out').onclick = () => { state.viewer.viewport.zoomBy(1 / 1.5); state.viewer.viewport.applyConstraints(); };
      $('fit').onclick = () => state.viewer.viewport.goHome();
      $('opacity').oninput = e => { state.opacity = Number(e.target.value); $('opacity-value').textContent = `${Math.round(state.opacity * 100)}%`; schedule(); };
      $('outlines').onchange = e => { state.outlines = e.target.checked; schedule(); };
      $('fragment').onchange = e => {
        if (e.target.value === '') { state.viewer.viewport.goHome(); return; }
        const f = state.fragments[Number(e.target.value)], pad = .08;
        state.viewer.viewport.fitBounds(state.viewer.world.getItemAt(0).imageToViewportRectangle(
          Math.max(0, f.x - f.w * pad), Math.max(0, f.y - f.h * pad), f.w * (1 + 2 * pad), f.h * (1 + 2 * pad)));
      };
      window.__wsiViewerQA = {
        state: () => ({ ready: state.ready, detailRequests: state.detailRequests, detailSuccess: state.detailSuccess,
          detailFallbacks: state.detailFallbacks, networkFailures: state.networkFailures, fragmentCount: state.fragments.length,
          width: manifest.image.width, height: manifest.image.height }),
        focus: mode => {
          const viewport = state.viewer.viewport;
          viewport.goHome(true);
          if (mode !== 'fit') {
            const factor = { medium: 3, close: 12, narrow: 20 }[mode];
            if (!factor) throw Error(`Unknown QA mode ${mode}`);
            viewport.zoomBy(factor, null, true); viewport.applyConstraints(true);
          }
          schedule();
        },
        point: (x, y) => {
          const item = state.viewer.world.getItemAt(0);
          const p = state.viewer.viewport.pixelFromPoint(item.imageToViewportCoordinates(x, y), true);
          return [p.x, p.y];
        }
      };
    } catch (e) { status(e.message || String(e), true); }
  }
  init();
})();
