let wsiAnnotationControlDown = false;
let wsiAnnotationPickDown = null, wsiAnnotationLastClick = null;

function wsiAnnotationPickMode() {
  return wsiAnnotationControlDown && (mode === 'brush' || mode === 'wand') && !brushing;
}

function wsiAnnotationPickCursor(down) {
  const changed = wsiAnnotationControlDown !== down;
  wsiAnnotationControlDown = down;
  document.documentElement.classList.toggle('wsiAnnotationPick', wsiAnnotationPickMode());
  if (changed) wsiRequestInteraction();
}

function wsiAnnotationPickSurface(target) {
  if (typeof multiViewLayout !== 'undefined' && multiViewLayout > 1) {
    const index = multiViewPanes.findIndex(pane => pane && pane.overlay === target);
    return index >= 0 ? {target, pane:multiViewPanes[index], index} : null;
  }
  return target === canvas ? {target, pane:null, index:-1} : null;
}

function wsiPickTissueAnnotation(event, surface) {
  const {pane, index} = surface;
  if (pane) {
    if (pane.blank || !multiViewEnsureEditingContext(pane, index)) return;
    multiViewFocusPane(index, false);
    multiViewHoverPane = pane;
    multiViewLastCanvasPointer = multiViewCanvasPoint(event, pane);
  } else lastCanvasPointer = canvasPoint(event.clientX, event.clientY);
  lastPointer = pane ? multiViewPointerToSlide(event, pane) : pointerToSlide(event);
  const tissue = (item, layer={}) => isDrawable(item) && !isTrajectoryAreaRoi(item) &&
    !layerItemIsPoint(item) && !/cell|segmentation/i.test(String(layer.source_type || item.source_type || ''));
  const hit = showRois ? roiAt(lastPointer, tissue) : -1;
  if (hit >= 0) {
    selectAnnotation(hit, true);
    syncRoiSelection('ctrl_double_click');
  } else {
    const rec = layerObjectAt(lastPointer, tissue);
    if (!rec) return;
    selectLayerObject(rec.layerIndex, rec.itemIndex, true);
    scheduleViewerStateSync('layer_updated', {id:rec.layer.id, selection_only:true, reason:'ctrl_double_click'});
  }
  // Do not switch tools, alter the geometry, or create an undo entry.
  if (pane) saveActiveProjectAnnotations();
  draw();
}

function wsiStopAnnotationPick(event) {
  event.preventDefault();
  event.stopImmediatePropagation();
}

// Capture before brush/Wand mouse-down and OpenSeadragon navigation handlers.
// Count mouse releases too: macOS Control-click can suppress native dblclick.
window.addEventListener('pointerdown', event => {
  wsiAnnotationPickCursor(!!event.ctrlKey);
  if (wsiAnnotationPickMode() && wsiAnnotationPickSurface(event.target)) event.stopImmediatePropagation();
}, true);
window.addEventListener('mousedown', event => {
  const surface = wsiAnnotationPickSurface(event.target);
  if (!surface || !wsiAnnotationPickMode() || !event.ctrlKey) return;
  wsiStopAnnotationPick(event);
  if (event.button !== 0 && !(viewerIsMac() && event.button === 2)) return;
  wsiAnnotationPickDown = {...surface, x:event.clientX, y:event.clientY, mode};
}, true);
window.addEventListener('mouseup', event => {
  const down = wsiAnnotationPickDown;
  if (!down) return;
  wsiAnnotationPickDown = null;
  wsiStopAnnotationPick(event);
  const now = performance.now(), last = wsiAnnotationLastClick;
  if (!event.ctrlKey || mode !== down.mode || event.target !== down.target ||
      Math.hypot(event.clientX-down.x, event.clientY-down.y) > 6) {
    wsiAnnotationLastClick = null;
    return;
  }
  if (last && last.target === down.target && last.mode === mode &&
      (now-last.time < 500 || event.detail === 2) &&
      Math.hypot(event.clientX-last.x, event.clientY-last.y) <= 6) {
    wsiAnnotationLastClick = null;
    wsiPickTissueAnnotation(event, down);
  } else wsiAnnotationLastClick = {...down, time:now};
}, true);
['click','dblclick','contextmenu'].forEach(type => window.addEventListener(type, event => {
  if (event.ctrlKey && wsiAnnotationPickMode() && wsiAnnotationPickSurface(event.target)) wsiStopAnnotationPick(event);
}, true));
window.addEventListener('mousemove', event => wsiAnnotationPickCursor(!!event.ctrlKey), true);
window.addEventListener('keydown', event => wsiAnnotationPickCursor(!!event.ctrlKey), true);
window.addEventListener('keyup', event => {
  wsiAnnotationPickCursor(!!event.ctrlKey);
  if (!event.ctrlKey) wsiAnnotationLastClick = null;
}, true);
window.addEventListener('blur', () => {
  wsiAnnotationPickDown = wsiAnnotationLastClick = null;
  wsiAnnotationPickCursor(false);
});
const wsiAnnotationPickStyle = document.createElement('style');
wsiAnnotationPickStyle.textContent = '.wsiAnnotationPick canvas.brushing,.wsiAnnotationPick canvas.wand{cursor:default!important;}';
document.head.appendChild(wsiAnnotationPickStyle);
