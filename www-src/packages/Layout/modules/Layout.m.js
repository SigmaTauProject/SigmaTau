
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
		[`	
uniform mat4 worldViewProjection;
attribute vec4 position;
void main() {
	gl_Position = worldViewProjection * position;
}
		`,
		`
void main() {
	gl_FragColor = vec4(0.7,0.7,0.7,1);
}
		`]
	);
	
	gl.clearColor(0,0,0,1);
	
	
	const arrays = {
		position:	[1	, 0	, 0
			, -1	, 1	, 0
			, -1	, -0.3	, 0
			, -1.4	, 0	, 1
			],
		indices:	[ 0	, 2	, 1
			, 0	, 3	, 2
			, 0	, 1	, 3
			, 1	, 2	, 3
			],
	};
	const bufferInfo = twgl.createBufferInfoFromArrays(gl, arrays);
	const tex = twgl.createTexture(gl, {
		min: gl.NEAREST,
		mag: gl.NEAREST,
		src: [
			255, 255, 255, 255,
			192, 192, 192, 255,
			192, 192, 192, 255,
			255, 255, 255, 255,
		],
	});
	const uniforms = {
	};
	function render(time) {
		time *= 0.001;
		gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
		gl.enable(gl.DEPTH_TEST);
		gl.enable(gl.CULL_FACE);
		gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
		const fov = 90 * Math.PI / 180;
		const aspect = gl.canvas.clientWidth / gl.canvas.clientHeight;
		const zNear = 0.1;
		const zFar = 1000;
		////const projection = mat4.perspective(mat4.create(), fov, aspect, zNear, zFar);
		const projection = mat4.mul(mat4.create(), mat4.perspective(mat4.create(), fov, aspect, zNear, zFar), new Float32Array([0,0,-1,0, 1,0,0,0, 0,1,0,0, 0,0,0,1]));
		const eye = vec3.fromValues(1, -6, 4);
		const target = vec3.fromValues(0, 0, 0);
		const up = vec3.fromValues(0, 0, 1);
		////const view = mat4.lookAt(mat4.create(), eye, target, up);
		////const view = mat4.rotateY(mat4.create(), mat4.fromTranslation(mat4.create(),vec3.fromValues(6,0,-1)), Math.PI);
		////const view = mat4.translate(mat4.create(), mat4.fromYRotation(mat4.create(), -Math.PI/16), vec3.fromValues(6,0,-2));
		const view = mat4.mul	( mat4.create()
			, mat4.fromYRotation(mat4.create(), -Math.PI/6)
			, mat4.fromTranslation(mat4.create(), vec3.fromValues(6,0,-4))
			);
		gl.useProgram(programInfo.program);
		for (let entity of hackEVPort.entities) {
			////const world = mat4.fromZRotation(mat4.create(),time);
			const world = mat4.fromTranslation(mat4.create(),vec3.fromValues(entity.pos[0],entity.pos[1],0));
			const viewProjection = mat4.mul(mat4.create(), mat4.mul(mat4.create(), projection, mat4.fromZRotation(mat4.create(),time)),view);
			uniforms.worldViewProjection = mat4.mul(mat4.create(), viewProjection, world);
			twgl.setBuffersAndAttributes(gl, programInfo, bufferInfo);
			twgl.setUniforms(programInfo, uniforms);
			gl.drawElements(gl.TRIANGLES, bufferInfo.numElements, gl.UNSIGNED_SHORT, 0);
		}
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
Object.defineProperty(Object.prototype, "escRef", {value:
	function (f) {
		f(this);
		return this;
	}
});


