
import Main	from "./main.mjs"	;

import vec from "./glVec2/vec.mjs";
window.vec = vec;
import findKey from "./loose/findKey.mjs";
Object.prototype.findKey = function(value){return findKey(this,value);}

window.onload = function () {
	////class ServerCom {
	////	constructor() {
	////		this.socket = io();
	////	}
	////	on(...args) {
	////		this.socket.on(...args);
	////	}
	////	send(...args) {
	////		this.socket.emit(...args);
	////	}
	////}
	
	new Main();////new Socket());
}

Math.degrees = function(rad) {
	return rad*(180/Math.PI);
}
	
Math.radians = function(deg) {
	return deg * (Math.PI/180);
}

window.log = console.log;

