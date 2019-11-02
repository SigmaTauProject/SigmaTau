
import {div,svg,Div} from "/modules/Div.m.js";

import {cell} from "/modules/FRP/Cell.m.js";

import {Port,Wire} from "/modules/Ports.m.js";


export default
function makeLayout(ports) {
	return div("body",
		ports	.filter(p=>p.type=="hackEV")
			.map(p=>hackEV3DView(p)),
		ports	.filter(p=>p.type=="la")
			.map(p=>locationArray(p)),
		ports	.filter(p=>p.type=="wire")
			.map(p=>slider().escRef(s=>s.value.changes().forEach(v=>p.set(v)))),
	);
}



function slider() {
	let slider = new GUIItem(
		div("input",
			{ type:"range"
			, min:"-1"
			, max:"1"
			, step:"0.01"
			, value:"0"
			}
		)
	);
	let c = cell(slider.el.value);
	slider.el.addEventListener("input",e=>c.change(e.srcElement.value));
	slider.value = c.map(v=>Number(v));
	return slider;
}

function locationArray(laPort) {
	let svgContent;
	let svgGui = new GUIItem(
		svg("svg", 
			{	viewBox:"-1 -1 2 2",
				style:"width:100%;height:100%;position:absolute;top:0;left:0;z-index:-1;",
			},
			svg("g", (el=>svgContent=el), {transform:"scale(1,-1) scale(0.01)"},
			),
		),
	);
	let shipEls = [];
	setInterval(()=>{
			laPort.locations.forEach((l,i)=>{
				if (shipEls.length <= i) {
					let newShip = svg("polygon", {points:"-0.5,-0.5 0,0.5 0.5,-0.5 0,-0.25"});
					shipEls.push(newShip);
					svgContent.appendChild(newShip);
				}
				shipEls[i].setAttribute("transform",`translate(${l[0]},${l[1]})`);
			})
		},
		10,
	);
	return svgGui;
}

function hackEV3DView(hackEVPort) {
	const canvas = div("canvas");
	const gl = canvas.getContext("webgl");
	if (!gl) gl = canvas.getContext("experimental-webgl");
	const programInfo = twgl.createProgramInfo(gl,
		[`	attribute vec4 position;
			
			void main() {
				gl_Position = position;
			}
		`,
		`	precision mediump float;
			
			uniform vec2 resolution;
			uniform float time;
			
			void main() {
				vec2 uv = gl_FragCoord.xy / resolution;
				float color = 0.0;
				// lifted from glslsandbox.com
				color += sin( uv.x * cos( time / 3.0 ) * 60.0 ) + cos( uv.y * cos( time / 2.80 ) * 10.0 );
				color += sin( uv.y * sin( time / 2.0 ) * 40.0 ) + cos( uv.x * sin( time / 1.70 ) * 40.0 );
				color += sin( uv.x * sin( time / 1.0 ) * 10.0 ) + sin( uv.y * sin( time / 3.50 ) * 80.0 );
				color *= sin( time / 10.0 ) * 0.5;
				
				gl_FragColor = vec4( vec3( color * 0.5, sin( color + time / 2.5 ) * 0.75, color ), 1.0 );
			}
		`]
	);
	
	const arrays = {
		position: [-1, -1, 0, 1, -1, 0, -1, 1, 0, -1, 1, 0, 1, -1, 0, 1, 1, 0],
	};
	const bufferInfo = twgl.createBufferInfoFromArrays(gl, arrays);
	
	function render(time) {
		twgl.resizeCanvasToDisplaySize(gl.canvas);
		gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
		
		const uniforms = {
			time: time * 0.001,
			resolution: [gl.canvas.width, gl.canvas.height],
		};
		
		gl.useProgram(programInfo.program);
		twgl.setBuffersAndAttributes(gl, programInfo, bufferInfo);
		twgl.setUniforms(programInfo, uniforms);
		twgl.drawBufferInfo(gl, gl.TRIANGLES, bufferInfo);
		
		requestAnimationFrame(render);
	}
	requestAnimationFrame(render);
	
	return canvas;
}


class GUIItem {
	constructor(el) {
		this.el = el;
	}
}
Object.prototype.escRef = function(f) {
	f(this);
	return this;
}


