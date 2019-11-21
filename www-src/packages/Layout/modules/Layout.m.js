
import {div,svg,Div} from "/modules/Div.m.js";

import {cell} from "/modules/FRP/Cell.m.js";

import {Port,Wire} from "/modules/Ports.m.js";

import {PerspectiveCamera} from "./Camera.m.js";


export default
function makeLayout(bridge) {
	let el = div("body");
	bridge.ports.forEach((ports)=>{
		while (el.firstChild) el.removeChild(el.firstChild);
		[
			...ports	.filter(p=>p.type=="hackEV")
				.map(p=>hackEV3DView(p)),
			...ports	.filter(p=>p.type=="radarArc")
				.map(p=>radarView(p)),
			...ports	.filter(p=>p.type=="la")
				.map(p=>locationArray(p)),
			...ports	.filter(p=>p.type=="wire")
				.map(p=>slider(p)),
			...ports	.filter(p=>p.type=="wire")
				.map(p=>slider(p,{min:"0"})),
		].forEach(c=>el.appendChild(c));
	});
	return el;
}



function slider(wirePort, config={}) {
	let slider = div(	"input",
		{ type	: "range"
		, min	: config.min	|| "-1"
		, max	: config.max	|| "1"
		, step	: config.step	|| "0.01"
		, value	: config.value	|| "0"
		}
	);
	
	slider.addEventListener("input",e=>wirePort.set(e.srcElement.value));
	return slider;
}

function locationArray(laPort) {
	let svgContent;
	let svgEl = svg("svg", 
		{	viewBox:"-1 -1 2 2",
			style:"width:100%;height:100%;position:absolute;top:0;left:0;z-index:-1;",
		},
		svg("g", (el=>svgContent=el), {transform:"scale(1,-1) scale(0.01)"},
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
	return svgEl;
}

function radarView(radarArcPort) {
	let canvas = div("canvas", {style:"width:1200px;height:800px;"});
	let ctx = canvas.getContext("2d");
	
	radarArcPort.pings.forEach(render);
	function render(pings) {
		canvas.width = canvas.clientWidth; canvas.height = canvas.clientHeight;
		ctx.fillStyle = "#000";
		ctx.fillRect(0, 0, canvas.width, canvas.height);
		for (let ping of pings) {
			ctx.beginPath();
			ctx.arc(canvas.width/2+ping[1], canvas.height/2-ping[0], 2, 0, 2 * Math.PI, false);
			ctx.fillStyle = '#8f0';
			ctx.fill();
		}
	}
	return canvas;
}

function hackEV3DView(hackEVPort) {
	var renderer = new THREE.WebGLRenderer({antialias:true});
	var canvas = renderer.domElement;
	var loader = new THREE.OBJLoader();
	canvas.style = "width:1200px;height:1200px";
	renderer.setClearColor("#000000");
	
	var outerScene = new THREE.Scene();
	var scene = new THREE.Group();
	outerScene.add(scene);
	scene.matrixAutoUpdate = false;
	scene.matrix.fromArray(new Float32Array([0,0,-1,0, 1,0,0,0, 0,1,0,0, 0,0,0,1]))
	////var camera = new PerspectiveCamera(75, 1, 0.1, 1000);
	////scene.add(camera);
	////camera.position.z = 2*2;
	////camera.position.y = 0.5*2;
	////camera.position.x = -2*2;
	////camera.rotation.y = 0.8;
	////var camera = new THREE.PerspectiveCamera(75, 1, 0.1, 1000);
	////scene.add(camera);
	////camera.position.z = 2*8;
	////camera.position.y = 0.5*8;
	////camera.position.x = -2*8;
	////camera.rotation.y = 0.8;
	var camera = new THREE.PerspectiveCamera(75, 1, 0.1, 1000);
	camera.position.z = 2*8;
	camera.position.y = 2*8;
	camera.position.x = 0.5*8;
	camera.rotation.x = -0.8;
	
	
	////var axesHelper = new THREE.AxesHelper( 5 );
	////scene.add( axesHelper );
	
	var directionalLight = new THREE.DirectionalLight( 0xff0000, 0.5 );
	directionalLight.position.set(5,3,10);
	scene.add( directionalLight );
	var pointLight = new THREE.PointLight( 0x00ff00, 0.25, 100 );
	pointLight.position.set(-5,-3,-10);
	scene.add( pointLight );
	
	var shipTemplate;
	var loaded = false;
	var ships = [];
	
	loader.load("models/ship.obj", (object)=>{
		shipTemplate = object;
		loaded = true;
	});
	
	hackEVPort.entities.forEach(render);
	
	function render(entities) {
		if (!loaded) return;
		
		renderer.setSize(canvas.clientWidth, canvas.clientHeight);
		camera.aspect = canvas.clientWidth / canvas.clientHeight;
		camera.updateProjectionMatrix();
		
		entities.forEach((entity, index)=>{
			while (ships.length <= index) {
				let s = shipTemplate.clone();
				scene.add(s);
				ships.push(s);
			}
			let ship = ships[index];
			ship.position.fromArray(entity.pos);
			////ship.position.multiplyScalar(0.2);
			ship.quaternion.fromArray([entity.ori[1],entity.ori[2],entity.ori[3],entity.ori[0]]);
		});
		
		renderer.render(outerScene, camera);
	}
	
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


