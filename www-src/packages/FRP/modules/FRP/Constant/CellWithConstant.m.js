import {Cell} from "../Base/Cell.m.js";
import {ConstantChangingCell,makeConstantChangingCell} from "./ConstantCell.m.js";

Cell.prototype.mapTo = function(value) {
	let n = makeConstantChangingCell(value,this._root);
	this._root.addNode((scope)=>{
		scope[n.nodeIdentifier]=n.value;
	});
	return n;
}



