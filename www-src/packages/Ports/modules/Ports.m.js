
import {div,Div} from "/modules/Div.m.js";

import {cell} from "/modules/FRP/Cell.m.js";

export
class Port {
	constructor(send, id,type) {
		this.send = send;
		this.id = id;
		this.type = type;
	}
}

export
class Wire extends Port {
	constructor(send, id) {
		super(send, id,"wire");
		this.value = 0;
	}
	set(value) {
		console.log(this.id,value);
		this.value = 0;
	}
	adjust(amount) {
		console.log(this.id,amount);
		this.value += amount;
	}
}

export
function portBuilder(send) {
	let nextID = 0;
	let ports = [];
	let ob = {};
	ob.done = () => ports;
	ob.wire = () => {ports.push(new Wire(send,nextID++)); return ob;};
	return ob;
}

