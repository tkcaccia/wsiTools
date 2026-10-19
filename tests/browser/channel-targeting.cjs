// Execute the production main-view and multi-view channel routing functions.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const lines = fs.readFileSync(path.join(__dirname, '../../R/viewer.R'), 'utf8').split('\n');
const names = ['channelNormValue', 'channelBasename', 'channelPushIdentity', 'channelIdentitySetFrom',
  'activeChannelIdentitySet', 'channelExplicitTargets', 'channelBindingValues', 'channelFullPath',
  'channelTargetObjects', 'channelExplicitScopeMatch', 'channelUniqueBasenameMatch',
  'channelLegacyScopeMatch', 'channelSourceMatchesActive', 'multiViewSourceForPane',
  'multiViewChannelIdentitySet', 'multiViewChannelSourceMatchesPane'];
const context = vm.createContext({
  projectItems: [{id: 'a', path: '/study/a/image.png'}, {id: 'b', path: '/study/b/image.png'}],
  activeProjectIndex: 0, normaliseChannelSource: source => source,
  projectDisplaySource: (item, section) => section || item,
  activeProjectSection: () => null, projectSections: item => item.sections || []
});
for (const name of names) {
  const line = lines.find(value => value.trim().startsWith('"function ' + name + '('));
  assert.ok(line, name + ' is extracted from production code');
  vm.runInContext(JSON.parse(line.trim().replace(/,$/, '')), context);
}
let assertions = 0;
function matches(source, index, expected, message) {
  context.activeProjectIndex = index;
  assert.equal(context.channelSourceMatchesActive(source), expected, 'Main view: ' + message);
  assert.equal(context.multiViewChannelSourceMatchesPane(source, {entry: {item: context.projectItems[index]}}), expected,
    'Multi-view: ' + message);
  assertions += 2;
}
const a = {id: 'marker-a', metadata: {target_project_item_id: 'a', target_path: '/study/a/image.png'}};
matches(a, 0, true, 'explicit matching tissue ID');
matches(a, 1, false, 'duplicate image basename cannot override a different tissue ID');
matches({...a, metadata: {...a.metadata, target_path: '/study/b/image.png'}}, 1, false,
  'matching path cannot override contradictory explicit tissue ID');
matches({...a, metadata: {...a.metadata, project_item_id: 'b'}}, 1, false,
  'an additional identity cannot override a contradictory explicit tissue ID');
matches({id: 'case', metadata: {target_project_item_id: 'A'}}, 0, false,
  'scientific tissue IDs remain case sensitive');
matches({id: 'path-only', metadata: {target_path: '/study/a/image.png'}}, 0, true, 'explicit full path');
matches({id: 'path-only', metadata: {target_path: '/study/a/image.png'}}, 1, false,
  'duplicate basename cannot override a different explicit full path');
matches({id: 'legacy', path: '/study/a/image.png'}, 0, true, 'legacy exact source path');
matches({id: 'legacy', path: '/study/a/image.png'}, 1, false, 'legacy duplicate basename remains ambiguous');
matches({id: 'basename', metadata: {target_path: 'image.png'}}, 0, false, 'explicit ambiguous basename is rejected');
context.projectItems[1].path = '/study/b/second.png';
matches({id: 'basename', metadata: {target_path: 'image.png'}}, 0, true, 'unique legacy basename binding');
matches({id: 'legacy', path: 'second.png'}, 1, true, 'unique unbound legacy basename');
matches({id: 'windows', metadata: {target_path: '\\study\\a\\image.png'}}, 0, true, 'path separator normalization');
context.projectItems = [context.projectItems[0]];
matches({id: 'wrong', metadata: {target_project_item_id: 'b'}}, 0, false,
  'single-image viewer still honors contradictory explicit bindings');
matches({id: 'unbound-stain'}, 0, true, 'unbound single-image legacy channel remains supported');
console.log(assertions + ' main/multi-view channel targeting assertions passed.');
