
import {div,Div} from "/modules/Div.m.js";

import {cell} from "/modules/FRP/Cell.m.js";

import {Port,Wire} from "/modules/Ports.m.js";


export default
function makeLayout(ports) {
	return div("body",
		ports	.filter(p=>p.type=="wire")
			.map(p=>slider().escRef(s=>s.value.changes().forEach(v=>p.set(v)))));
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


class GUIItem {
	constructor(el) {
		this.el = el;
	}
}
Object.prototype.escRef = function(f) {
	f(this);
	return this;
}


