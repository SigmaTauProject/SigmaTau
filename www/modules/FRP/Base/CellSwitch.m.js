import {Cell,makeCell} from "./Cell.m.js";
import * as HC from "./HybridCell.m.js";

Cell.prototype.impureSwitch = function() {
	if (this.initial instanceof Cell)
		return this.impureSwitchC();
	else //if (this.initial instanceof Stream)
		return this.impureSwitchS();
}
Cell.prototype.impureSwitchC = function() {
	let n = makeCell(this.initial.initial,null,this.nodeIdentifier);
	
	let lastRoot = this.initial._root;
	let currentNID = this.initial.nodeIdentifier;
	this.initial._root.addNode(callback);
	
	this.changes().forEach((value)=>{
		console.assert(value instanceof HC.Cell, "Must use `.caching()` on cells within a impureSwitch.");
		lastRoot.unsafeRemoveNode(callback);
		currentNID = value.nodeIdentifier;
		lastRoot = value._root;
		value._root.addNode(callback);
		n._root.send(value.grab());
	});
	return n;
	
	function callback(scope) {
		n._root.send(scope[currentNID]);
		return scope;
	}
}
Cell.prototype.impureSwitchS = function() {
	console.assert(false,"Unimplemented");
}




