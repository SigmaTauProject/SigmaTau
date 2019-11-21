import {Cell,makeCell} from "../Base/Cell.m.js";
import {EnumCell} from "./ConstantCell.m.js";
import {partialRoot,joinRoots} from "../Core/PushRoot.m.js";

EnumCell.prototype.switch = function() {
	if (this.values.all(v=>v instanceof Cell))
		return this.switchC();
	if (this.values.all(v=>v instanceof Stream))
		return this.switchS();
	console.assert(false);
}
EnumCell.prototype.switchC = function() {
	let nnid = Symbol();
	let current = this.initial;
	
	this._root.addNode(scope=>{
		current = scope[this.nodeIdentifier];
		scope[nnid] = scope[this.nodeIdentifier].grab();
	});
	
	let valueRoots = this.values.map(v=>{
		let vRoot = partialRoot(v._root);
		v._root.addNode(scope=>{
			if (v==current) {
				scope[nnid] = scope[v.nodeIdentifier];
				vRoot.sendScope(scope);
			}
		});
		return vRoot;
	});
	
	let n = makeCell(this.initial.initial,joinRoots(nnid,[this._root,...valueRoots]),nnid);
	
	return n;
}
EnumCell.prototype.switchS = function() {
	console.assert(false,"Unimplemented");
}




