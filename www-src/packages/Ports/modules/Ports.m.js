
import {div,Div} from "/modules/Div.m.js";

import {cell} from "/modules/FRP/Cell.m.js";

export
class Port {
	constructor(id,type) {
		this.id = id;
		this.type = type
	}
}

export
class Wire extends Port {
	constructor(id) {
		super(id,"wire");
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

