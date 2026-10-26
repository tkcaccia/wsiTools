// CPU-only annotation-status contract: node tests/browser/experiment-bora-status.cjs
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const vm=require('node:vm');
const source=fs.readFileSync(path.resolve(__dirname,'../../inst/viewer/experiment-workbench.js'),'utf8');

function fixture(interpretation,status=null){
  const nodes={};
  class Element{
    constructor(tag){this.tagName=tag;this.children=[];this.style={};this.hidden=false;this.textContent='';}
    set id(value){this._id=value;nodes[value]=this;}get id(){return this._id;}
    append(...children){for(const child of children){child.parent=this;this.children.push(child);}}
    prepend(child){child.parent=this;this.children.unshift(child);}
    after(child){const siblings=this.parent.children;child.parent=this.parent;siblings.splice(siblings.indexOf(this)+1,0,child);}
    setAttribute(name,value){(this.attributes??={})[name]=value;}
  }
  const project=new Element('section');project.id='projectPanelBody';
  const annotations=new Element('section');annotations.id='roiPanelBody';
  const summary=new Element('div');summary.id='roiSummary';annotations.append(summary);
  const rois=[],imported=[];let builds=0;
  const context=vm.createContext({console,document:{createElement:tag=>new Element(tag)},setTimeout(){},
    cfg:{experiment:{viewer_evidence:{bora_interpretation:interpretation,bora_status:status,cell_coverage:[]}}},
    rois,el:id=>nodes[id]||null,tissueAnnotationLayerEntries:()=>imported,
    buildRoiList(){builds++;}});
  vm.runInContext(source,context);
  return {context,nodes,annotations,summary,rois,imported,get builds(){return builds;}};
}

const sourceText='Expert tissue boundaries have not been validated for this slide.';
const f=fixture(sourceText);f.context.wsiExperimentBindWorkbench();
const notice=f.nodes.experimentBoraAnnotationStatus;
assert.ok(notice,'The Annotations panel receives its own status');
assert.equal(f.annotations.children[1],notice,'The status appears directly below the annotation summary');
assert.equal(notice.hidden,false);
assert.equal(notice.textContent,'No Bora tissue boundaries are available in this project. Predictions are withheld pending review. '+sourceText);
assert.match(f.nodes.experimentEvidenceSummary.textContent,/Bora tissue predictions withheld/);
f.rois.push({id:'human-annotation'});f.context.buildRoiList();
assert.equal(notice.hidden,false,'A manual ROI cannot clear the Bora-withheld notice');
assert.match(f.nodes.experimentEvidenceSummary.textContent,/Bora tissue predictions withheld/);
f.rois.pop();f.imported.push({item:{id:'imported'}});f.context.buildRoiList();
assert.equal(notice.hidden,false,'An unrelated imported layer cannot clear the notice');
f.imported[0].item.properties={source_role:'tissue_annotations'};f.context.buildRoiList();
assert.equal(notice.hidden,true,'A qualified tissue annotation clears an unspecific notice');
assert.match(f.nodes.experimentEvidenceSummary.textContent,/Bora prediction · exploratory/);
f.imported.pop();f.context.buildRoiList();
assert.equal(notice.hidden,false,'Removing the last annotation restores the warning');
f.imported.push({review:true,layer:{metadata:{review_only:true,training_eligible:false,
  native_he_boundary_refinement_applied:true}},item:{id:'refined-review'}});
f.context.buildRoiList();
assert.equal(notice.hidden,false);
assert.equal(notice.textContent,
  'Bora predicted tissue boundaries are available for exploratory review after native H&E refinement. They are not ground truth or PLS training regions.');
assert.match(f.nodes.experimentEvidenceSummary.textContent,/Bora prediction · exploratory review/);
f.imported.pop();f.context.buildRoiList();
const withheld=fixture(sourceText,'withheld_pending_corrected_algorithm');withheld.context.wsiExperimentBindWorkbench();
withheld.rois.push({properties:{source_role:'tissue_annotations'}});withheld.context.buildRoiList();
assert.equal(withheld.nodes.experimentBoraAnnotationStatus.hidden,false,
  'The explicit withheld status remains visible even if unrelated ROI state claims tissue annotations');
assert.equal(f.builds,6,'The original annotation refresh remains active');

const unsafe='<img src=x onerror=alert(1)>';
const literal=fixture(unsafe);literal.context.wsiExperimentBindWorkbench();
assert.ok(literal.nodes.experimentBoraAnnotationStatus.textContent.endsWith(unsafe));
assert.equal(literal.nodes.experimentBoraAnnotationStatus.children.length,0,'Project interpretation is assigned as text, never HTML');
const absent=fixture(null);absent.context.wsiExperimentBindWorkbench();
assert.equal(absent.nodes.experimentBoraAnnotationStatus.hidden,true,
  'No Bora notice is shown without project evidence');
console.log('Bora annotation-pending status contracts passed.');
