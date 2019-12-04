import {Push} from "../Core/Push.m.js";
import {newRoot,newDeadRoot} from "../Core/PushRoot.m.js";

export {RootCell,Cell,cell,constant,};
export {makeCell,};

class Cell extends Push {
	constructor (initial, ...args) {
		super(...args);
		this.initial = initial;
	}
	map(f) {
		let n = makeCell(f(this.initial),this._root);
		this._root.addNode((scope)=>{scope[n.nodeIdentifier]=f(scope[this.nodeIdentifier])});
		return n;
	}
	forEach(f) {
		f(this.initial);
		this._root.addNode((scope)=>{f(scope[this.nodeIdentifier])});
		return this;
	}
}
class RootCell extends Cell {
	constructor(initial) {
		let nodeId = Symbol();
		super(initial,newRoot(nodeId),nodeId);
	}
	change(value) {
		this._root.send(value);
	}
}
class ConstantCell extends Cell {
	get value() {
		return this.initial;
	}
	map(f) {
		return makeConstantCell(f(this.initial),this._root);
	}
	forEach(f) {
		f(this.initial);
		return this;
	}
}

function cell(initial) {
	return new RootCell(initial);
}
function constant(value) {
	return makeConstantCell(value);
}

function makeCell(initial,root=null,nnid=null) {
	if (nnid==null)
		nnid = Symbol();
	if (root==null)
		root = newRoot(nnid);
	return new Cell(initial,root,nnid);
}
function makeConstantCell(value,root=null,nnid=null) {
	if (nnid==null)
		nnid = Symbol();
	if (root==null)
		root = newDeadRoot();
	return new ConstantCell(value,root,nnid);
}


