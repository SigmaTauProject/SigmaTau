import * as C from "./Cell.m.js";

export {Cell};

class Cell extends C.Cell {
	constructor (...args) {
		super(...args);
		this._root.addNode(scope=>this.initial=scope[this.nodeIdentifier]);
	}
	grab() {
		return this.initial;
	}
	map(f) {
		throw Error("Map not currently allowed on Hybrid Cell");
	}
	forEach(f) {
		throw Error("ForEach not currently allowed on Hybrid Cell");
	}
}


