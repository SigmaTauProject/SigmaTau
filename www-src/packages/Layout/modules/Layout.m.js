
import {div,svg,Div} from "/modules/Div.m.js";

import {cell} from "/modules/FRP/Cell.m.js";

import {Port,Wire} from "/modules/Ports.m.js";

import {PerspectiveCamera} from "./Camera.m.js";


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
	var renderer = new THREE.WebGLRenderer();
	var canvas = renderer.domElement;
	canvas.style = "width:1200px;height:1200px";
	renderer.setClearColor("#808080");
	
	var scene = new THREE.Scene();
	var camera = new THREE.PerspectiveCamera(75, 1, 0.1, 1000);
	camera.position.z = 5;
	
	////var geo = new THREE.SphereGeometry( 1,10,10 );
	////var mat = new THREE.MeshLambertMaterial({color: 0xF7F7F7});
	////var cube = new THREE.Mesh( geo, mat );
	////scene.add( cube );
	
	var loader = new THREE.OBJLoader()
	loader.load("models/ship.obj", (object)=>{
		scene.add(object);
	});
	
	var directionalLight = new THREE.DirectionalLight( 0xff0000, 0.5 );
	directionalLight.position.set(5,3,10);
	scene.add( directionalLight );
	
	function render() {
		renderer.setSize(canvas.clientWidth, canvas.clientHeight);
		camera.aspect = canvas.clientWidth / canvas.clientHeight;
		camera.updateProjectionMatrix();
		
		renderer.render(scene, camera);
		
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


