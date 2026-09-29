const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

const context = vm.createContext({
  window: { addEventListener: () => {} },
  roiBounds: roi => roi.bbox,
  boundsOverlap: (a, b) => !!a && !!b && a.xmin <= b.xmax && a.xmax >= b.xmin &&
    a.ymin <= b.ymax && a.ymax >= b.ymin,
  unionBounds: (a, b) => a ? {
    xmin: Math.min(a.xmin, b.xmin), ymin: Math.min(a.ymin, b.ymin),
    xmax: Math.max(a.xmax, b.xmax), ymax: Math.max(a.ymax, b.ymax)
  } : b
});
vm.runInContext(fs.readFileSync('inst/viewer/performance.js', 'utf8'), context);
context.items = Array.from({ length: 500 }, (_, index) => ({
  id: index, bbox: { xmin: index * 20, ymin: 0, xmax: index * 20 + 10, ymax: 10 }
}));
context.view = { xmin: 400, ymin: 0, xmax: 450, ymax: 10 };
vm.runInContext('var visible = wsiVisibleRoiEntries(items, view)', context);
assert.deepEqual(Array.from(context.visible, entry => entry.index), [20, 21, 22]);
context.otherItems = Array.from({ length: 500 }, (_, index) => ({
  id: `other-${index}`, bbox: { xmin: index * 20, ymin: 20, xmax: index * 20 + 10, ymax: 30 }
}));
context.otherView = { xmin: 400, ymin: 20, xmax: 450, ymax: 30 };
vm.runInContext('wsiVisibleRoiEntries(otherItems, otherView)', context);
const firstIndex = vm.runInContext('wsiRoiViewportIndexes.get(items)', context);
const otherIndex = vm.runInContext('wsiRoiViewportIndexes.get(otherItems)', context);
context.items[21].bbox = { xmin: 9000, ymin: 0, xmax: 9010, ymax: 10 };
vm.runInContext('wsiInvalidateGeometry(items[21]); visible = wsiVisibleRoiEntries(items, view)', context);
assert.deepEqual(Array.from(context.visible, entry => entry.index), [20, 22]);
assert.equal(vm.runInContext('wsiRoiViewportIndexes.get(items)', context), firstIndex);
vm.runInContext('wsiVisibleRoiEntries(otherItems, otherView)', context);
assert.equal(vm.runInContext('wsiRoiViewportIndexes.get(otherItems)', context), otherIndex);
context.items.push({ id: 500, bbox: { xmin: 420, ymin: 0, xmax: 430, ymax: 10 } });
vm.runInContext('visible = wsiVisibleRoiEntries(items, view)', context);
assert.deepEqual(Array.from(context.visible, entry => entry.index), [20, 22, 500]);
console.log('ROI viewport index: visible-only candidates and edit/add invalidation passed.');
