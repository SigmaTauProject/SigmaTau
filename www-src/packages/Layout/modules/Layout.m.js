
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
uniform mat4 u_worldViewProjection;
attribute vec4 position;
void main() {
	gl_Position = u_worldViewProjection * position;
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
		const fov = 30 * Math.PI / 180;
		const aspect = gl.canvas.clientWidth / gl.canvas.clientHeight;
		const zNear = 0.5;
		const zFar = 10;
		const projection = mat4.perspective(mat4.create(), fov, aspect, zNear, zFar);
		const eye = vec3.fromValues(1, -6, 4);
		const target = vec3.fromValues(0, 0, 0);
		const up = vec3.fromValues(0, 0, 1);
		const world = mat4.fromZRotation(mat4.create(),time);
		////const view = mat4.lookAt(mat4.create(), eye, target, up);
		////const view = mat4.rotateY(mat4.create(), mat4.fromTranslation(mat4.create(),vec3.fromValues(6,0,-1)), Math.PI);
		////const view = mat4.translate(mat4.create(), mat4.fromYRotation(mat4.create(), -Math.PI/16), vec3.fromValues(6,0,-2));
		const view = mat4.mul	( mat4.create()
			, mat4.fromYRotation(mat4.create(), -Math.PI/6)
			, mat4.fromTranslation(mat4.create(), vec3.fromValues(6,0,-4))
			);
		const viewProjection = mat4.mul(mat4.create(), mat4.mul(mat4.create(), projection, new Float32Array([0,0,-1,0, 1,0,0,0, 0,1,0,0, 0,0,0,1])), view);
		uniforms.u_worldViewProjection = mat4.mul(mat4.create(), viewProjection, world);
		gl.useProgram(programInfo.program);
		twgl.setBuffersAndAttributes(gl, programInfo, bufferInfo);
		twgl.setUniforms(programInfo, uniforms);
		gl.drawElements(gl.TRIANGLES, bufferInfo.numElements, gl.UNSIGNED_SHORT, 0);
		requestAnimationFrame(render);
	}
	requestAnimationFrame(render);
	
	////const shipMesh = {
	////	position:	[ 1	, 0	, 0
	////		, -1	, 0	, 0.5
	////		, -1	, 0.75	, 0
	////		, 1	, 0	, 0
	////		, -1	, -0.5	, 0
	////		,-1	,0	,0.5
	////		],
	////};
	////const bufferInfo = twgl.createBufferInfoFromArrays(gl, shipMesh);
	////
	////var mat = mat4.identity(mat4.create());
	////mat4.scale(mat,mat,[0.5,0.5,0.5]);
	////
	////var viewMat = mat4.identity(mat4.create());
	////mat4.translate(mat,mat,[-2,0,0]);
	////mat4.rotate(mat,mat,Math.PI,[0,0,1]);
	////
	////var projMat = mat4.identity(mat4.create());
	////mat4.perspective(projMat, 90, 1, 1, 1000);
	////
	////var worldViewMat = mat4.create();
	////
	////function render(time) {
	////	twgl.resizeCanvasToDisplaySize(gl.canvas);
	////	gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
	////	
	////	mat4.mul(worldViewMat, projMat,viewMat,mat);
	////	console.log(worldViewMat);
	////	
	////	const uniforms = {
	////		transformMatrix: worldViewMat,
	////	};
	////	
	////	gl.useProgram(programInfo.program);
	////	twgl.setBuffersAndAttributes(gl, programInfo, bufferInfo);
	////	twgl.setUniforms(programInfo, uniforms);
	////	twgl.drawBufferInfo(gl, gl.TRIANGLES, bufferInfo);
	////	
	////	requestAnimationFrame(render);
	////}
	////requestAnimationFrame(render);
	
	////var boxVertices = 
	////[ // X, Y, Z           R, G, B
	////	// Top
	////	-1.0, 1.0, -1.0,   0.5, 0.5, 0.5,
	////	-1.0, 1.0, 1.0,    0.5, 0.5, 0.5,
	////	1.0, 1.0, 1.0,     0.5, 0.5, 0.5,
	////	1.0, 1.0, -1.0,    0.5, 0.5, 0.5,
////
	////	// Left
	////	-1.0, 1.0, 1.0,    0.75, 0.25, 0.5,
	////	-1.0, -1.0, 1.0,   0.75, 0.25, 0.5,
	////	-1.0, -1.0, -1.0,  0.75, 0.25, 0.5,
	////	-1.0, 1.0, -1.0,   0.75, 0.25, 0.5,
////
	////	// Right
	////	1.0, 1.0, 1.0,    0.25, 0.25, 0.75,
	////	1.0, -1.0, 1.0,   0.25, 0.25, 0.75,
	////	1.0, -1.0, -1.0,  0.25, 0.25, 0.75,
	////	1.0, 1.0, -1.0,   0.25, 0.25, 0.75,
////
	////	// Front
	////	1.0, 1.0, 1.0,    1.0, 0.0, 0.15,
	////	1.0, -1.0, 1.0,    1.0, 0.0, 0.15,
	////	-1.0, -1.0, 1.0,    1.0, 0.0, 0.15,
	////	-1.0, 1.0, 1.0,    1.0, 0.0, 0.15,
////
	////	// Back
	////	1.0, 1.0, -1.0,    0.0, 1.0, 0.15,
	////	1.0, -1.0, -1.0,    0.0, 1.0, 0.15,
	////	-1.0, -1.0, -1.0,    0.0, 1.0, 0.15,
	////	-1.0, 1.0, -1.0,    0.0, 1.0, 0.15,
////
	////	// Bottom
	////	-1.0, -1.0, -1.0,   0.5, 0.5, 1.0,
	////	-1.0, -1.0, 1.0,    0.5, 0.5, 1.0,
	////	1.0, -1.0, 1.0,     0.5, 0.5, 1.0,
	////	1.0, -1.0, -1.0,    0.5, 0.5, 1.0,
	////];
////
	////var boxIndices =
	////[
	////	// Top
	////	0, 1, 2,
	////	0, 2, 3,
	////	
	////	// Left
	////	5, 4, 6,
	////	6, 4, 7,
	////	
	////	// Right
	////	8, 9, 10,
	////	8, 10, 11,
	////	
	////	// Front
	////	13, 12, 14,
	////	15, 14, 12,
	////	
	////	// Back
	////	16, 17, 18,
	////	16, 18, 19,
	////	
	////	// Bottom
	////	21, 20, 22,
	////	22, 20, 23
	////];
	////
	////const bufferInfo = twgl.createBufferInfoFromArrays(gl, {position:boxVertices, indices:boxIndices});
	////
	////var worldMat = mat4.create();
	////var viewMat = mat4.create();
	////var projMat = mat4.create();
	////mat4.identity(worldMat);
	////mat4.lookAt(viewMat, [0, 0, -8], [0, 0, 0], [0, 1, 0]);
	////mat4.perspective(projMat, glMatrix.toRadian(45), canvas.clientWidth / canvas.clientHeight, 0.1, 1000.0);
////
	////var xRotationMat = mat4.create();
	////var yRotationMat = mat4.create();
////
	//////
	////// Main render loop
	//////
	////var identityMat = mat4.create();
	////mat4.identity(identityMat);
	////var angle = 0;
	////var loop = function () {
	////	angle = performance.now() / 1000 / 6 * 2 * Math.PI;
	////	mat4.rotate(yRotationMat, identityMat, angle, [0, 1, 0]);
	////	mat4.rotate(xRotationMat, identityMat, angle / 4, [1, 0, 0]);
	////	mat4.mul(worldMat, yRotationMat, xRotationMat);
////
	////	gl.clearColor(0.75, 0.85, 0.8, 1.0);
	////	gl.clear(gl.DEPTH_BUFFER_BIT | gl.COLOR_BUFFER_BIT);
	////	
	////	const uniforms = {
	////		worldMat: worldMat,
	////		viewMat: viewMat,
	////		projMat: projMat,
	////	};
	////	
	////	gl.useProgram(programInfo.program);
	////	twgl.setBuffersAndAttributes(gl, programInfo, bufferInfo);
	////	twgl.setUniforms(programInfo, uniforms);
	////	twgl.drawBufferInfo(gl, gl.TRIANGLES, bufferInfo);
	////	
	////	requestAnimationFrame(loop);
	////};
	////requestAnimationFrame(loop);
	
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


