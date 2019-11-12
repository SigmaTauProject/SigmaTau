
import {div,svg,Div} from "/modules/Div.m.js";

import {cell} from "/modules/FRP/Cell.m.js";

import {Port,Wire} from "/modules/Ports.m.js";


export default
function makeLayout(ports) {
	return div("body",
		ports	.filter(p=>p.type=="hackEV")
			.map(p=>hackEV3DView(p)),
		ports	.filter(p=>p.type=="radarArc")
			.map(p=>radarView(p)),
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

function radarView(radarArcPort) {
	let canvas = div("canvas", {style:"width:1200px;height:800px;"});
	let ctx = canvas.getContext("2d");
	
	function render(time) {
		canvas.width = canvas.clientWidth; canvas.height = canvas.clientHeight;
		ctx.fillStyle = "#000";
		ctx.fillRect(0, 0, canvas.width, canvas.height);
		for (let ping of radarArcPort.pings) {
			ctx.beginPath();
			ctx.arc(canvas.width/2+ping[1], canvas.height/2-ping[0], 2, 0, 2 * Math.PI, false);
			ctx.fillStyle = '#8f0';
			ctx.fill();
		}
		requestAnimationFrame(render);
	}
	requestAnimationFrame(render);
	return canvas;
}

function hackEV3DView(hackEVPort) {
	const canvas = div("canvas", {style:"width:1200px;height:800px;"});
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
	gl.enable(gl.DEPTH_TEST);
	gl.enable(gl.CULL_FACE);
	const projection = mat4.mul
		( mat4.create()
		, mat4.perspective	( mat4.create()
			, 90 * Math.PI / 180// FOV
			////, gl.canvas.clientWidth / gl.canvas.clientHeight// aspect
			, 1// aspect
			, 0.1// near
			, 1000// far
			)
		, new Float32Array([0,0,-1,0, 1,0,0,0, 0,1,0,0, 0,0,0,1])
		);
	
	const arrays = {
		position:	[1.4	, 0	, 0
			, -0.7	, 1	, 0
			, -0.7	, -0.7	, 0
			, -1	, 0	, 1
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
	function render(time) {
		time *= 0.001;
		{
			canvas.width = canvas.clientWidth; canvas.height = canvas.clientHeight;
			gl.viewport(0, 0, canvas.width, canvas.height);
		}
		gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
		
		const viewingSize = (canvas.clientWidth + canvas.clientHeight) / 2;
		const view = mat4.mul	( mat4.create()
			, mat4.fromScaling(mat4.create(), [1,viewingSize/canvas.clientWidth,viewingSize/canvas.clientHeight])
			, mat4.mul	( mat4.create()
				, mat4.fromYRotation(mat4.create(), -Math.PI/2)
				, mat4.fromTranslation(mat4.create(), vec3.fromValues(0,0,-64))
				)
			);
		gl.useProgram(programInfo.program);
		for (let entity of hackEVPort.entities) {
			////const world = mat4.fromZRotation(mat4.create(),time);
			const world = mat4.mul	( mat4.create()
				, mat4.fromTranslation(mat4.create(),vec3.fromValues(entity.pos[0],entity.pos[1],0))
				, mat4.fromQuat(mat4.create(), [entity.ori[1],entity.ori[2],entity.ori[3],entity.ori[0]])
				);
			const viewProjection = mat4.mul(mat4.create(), projection, view);
			twgl.setUniforms(programInfo, {
				worldViewProjection : mat4.mul(mat4.create(), viewProjection, world)
			});
			twgl.setBuffersAndAttributes(gl, programInfo, bufferInfo);
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


