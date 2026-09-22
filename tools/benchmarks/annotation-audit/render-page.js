import earcut from '/earcut.js';

const canvas=document.querySelector('canvas'),svg=document.querySelector('svg');
const data=await (await fetch('/annotations')).json();
const features=(Array.isArray(data)?data:data.features||[]).filter(f=>['Polygon','MultiPolygon'].includes(f.geometry?.type));
const groups=features.flatMap(f=>f.geometry.type==='Polygon'?[f.geometry.coordinates]:f.geometry.coordinates);
const bounds=[Infinity,Infinity,-Infinity,-Infinity];
for(const p of groups)for(const r of p)for(const xy of r){bounds[0]=Math.min(bounds[0],xy[0]);bounds[1]=Math.min(bounds[1],xy[1]);bounds[2]=Math.max(bounds[2],xy[0]);bounds[3]=Math.max(bounds[3],xy[1]);}
const paths=p=>{const path=new Path2D();for(const r of p){r.forEach((xy,i)=>i?path.lineTo(...xy):path.moveTo(...xy));path.closePath();}return path;};
const median=a=>[...a].sort((x,y)=>x-y)[Math.floor(a.length/2)];
const frame=()=>new Promise(resolve=>requestAnimationFrame(resolve));
const worldScale=Math.min(1024/(bounds[2]-bounds[0]),768/(bounds[3]-bounds[1]))*.94;
window.runRender=async function(mode) {
  const started=performance.now();let draw,readPixel,cleanup=()=>{};
  svg.style.display=mode==='svg'?'block':'none';canvas.style.display=mode==='svg'?'none':'block';
  const setup={mode,features:features.length,polygons:groups.length,vertices:groups.reduce((n,p)=>n+p.reduce((s,r)=>s+r.length,0),0)};
  if(mode==='canvas_cached'||mode==='canvas_rebuild') {
    const cx=canvas.getContext('2d');
    const cached=mode==='canvas_cached'?groups.map(paths):null;
    draw=(dx,scale)=>{cx.resetTransform();cx.clearRect(0,0,1024,768);cx.setTransform(scale,0,0,scale,20-bounds[0]*scale+dx,20-bounds[1]*scale);
      cx.fillStyle='rgba(200,50,80,.35)';for(let i=0;i<groups.length;i++)cx.fill(cached?cached[i]:paths(groups[i]),'evenodd');
      // Force queued raster work to complete; includes a small readback cost.
      cx.getImageData(0,0,1,1);
    };
    readPixel=()=>Array.from(cx.getImageData(0,0,1024,768).data).filter((v,i)=>i%4===3&&v>0).length;
  } else if(mode==='svg') {
    const ns='http://www.w3.org/2000/svg',group=document.createElementNS(ns,'g');svg.replaceChildren(group);
    for(const p of groups){const el=document.createElementNS(ns,'path');el.setAttribute('d',p.map(r=>'M'+r.map(xy=>xy.join(',')).join('L')+'Z').join(''));el.setAttribute('fill','rgba(200,50,80,.35)');el.setAttribute('fill-rule','evenodd');group.appendChild(el);}
    draw=(dx,scale)=>{group.setAttribute('transform',`translate(${20-bounds[0]*scale+dx},${20-bounds[1]*scale}) scale(${scale})`);group.getBoundingClientRect();};
    readPixel=()=>group.childNodes.length;
  } else {
    const gl=canvas.getContext('webgl2',{antialias:true,preserveDrawingBuffer:true,alpha:true});
    if(!gl)return {mode,unavailable:true};
    const info=gl.getExtension('WEBGL_debug_renderer_info');setup.gpu=info?gl.getParameter(info.UNMASKED_RENDERER_WEBGL):gl.getParameter(gl.RENDERER);
    const shader=(type,code)=>{const s=gl.createShader(type);gl.shaderSource(s,code);gl.compileShader(s);if(!gl.getShaderParameter(s,gl.COMPILE_STATUS))throw Error(gl.getShaderInfoLog(s));return s;};
    const program=gl.createProgram();gl.attachShader(program,shader(gl.VERTEX_SHADER,'#version 300 es\nin vec2 a;uniform vec3 t;void main(){vec2 q=a*t.z+t.xy;gl_Position=vec4(q.x/512.0-1.0,1.0-q.y/384.0,0,1);}'));
    gl.attachShader(program,shader(gl.FRAGMENT_SHADER,'#version 300 es\nprecision highp float;out vec4 color;void main(){color=vec4(0.7843,0.1961,0.3137,0.35);}'));
    gl.linkProgram(program);if(!gl.getProgramParameter(program,gl.LINK_STATUS))throw Error(gl.getProgramInfoLog(program));gl.useProgram(program);
    const t=performance.now(),chunks=[];let triangleVertices=0;
    for(const p of groups){const vertices=[],holes=[];let count=0;for(let ri=0;ri<p.length;ri++){if(ri)holes.push(count);for(const xy of p[ri]){vertices.push(xy[0]-bounds[0],xy[1]-bounds[1]);count++;}}
      const index=earcut(vertices,holes,2),flat=new Float32Array(index.length*2);for(let i=0;i<index.length;i++){flat[i*2]=vertices[index[i]*2];flat[i*2+1]=vertices[index[i]*2+1];}chunks.push(flat);triangleVertices+=index.length;}
    setup.triangulate_and_expand_ms=performance.now()-t;const joined=new Float32Array(triangleVertices*2);let offset=0;for(const c of chunks){joined.set(c,offset);offset+=c.length;}
    setup.vertex_buffer_bytes=joined.byteLength;const buffer=gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER,buffer);gl.bufferData(gl.ARRAY_BUFFER,joined,gl.STATIC_DRAW);
    const attr=gl.getAttribLocation(program,'a');gl.enableVertexAttribArray(attr);gl.vertexAttribPointer(attr,2,gl.FLOAT,false,0,0);
    const uniform=gl.getUniformLocation(program,'t');gl.enable(gl.BLEND);gl.blendFuncSeparate(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA,gl.ONE,gl.ONE_MINUS_SRC_ALPHA);
    draw=(dx,scale)=>{gl.clearColor(0,0,0,0);gl.clear(gl.COLOR_BUFFER_BIT);gl.uniform3f(uniform,20+dx,20,scale);gl.drawArrays(gl.TRIANGLES,0,triangleVertices);gl.finish();};
    readPixel=()=>{const bytes=new Uint8Array(1024*768*4);gl.readPixels(0,0,1024,768,gl.RGBA,gl.UNSIGNED_BYTE,bytes);let n=0;for(let i=3;i<bytes.length;i+=4)if(bytes[i])n++;return n;};
    cleanup=()=>{gl.deleteBuffer(buffer);gl.deleteProgram(program);};
  }
  setup.prepare_ms=performance.now()-started;
  const submit=[],present=[];
  for(let i=0;i<14;i++){await frame();const t=performance.now();draw(i%5,worldScale*(1+(i%3)*.002));submit.push(performance.now()-t);await frame();await frame();present.push(performance.now()-t);}
  const nonblank=readPixel();cleanup();
  return {...setup,submit_or_flush_median_ms:median(submit.slice(2)),submit_or_flush_max_ms:Math.max(...submit),two_raf_median_ms:median(present.slice(2)),nonblank};
};
window.auditReady=true;
