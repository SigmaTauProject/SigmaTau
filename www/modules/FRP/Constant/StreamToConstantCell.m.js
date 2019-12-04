import {NeverStream,ConstantStream,EnumStream} from "./ConstantStream.m.js";
import {makeConstantCell,makeConstantChangingCell,makeEnumCell,} from "./ConstantCell.m.js";


NeverStream.prototype.hold = function(initial) {
	return makeConstantCell(initial,this._root,this.nodeIdentifier);
}

ConstantStream.prototype.hold = function(initial) {
	if (initial == this.value)
		return makeConstantChangingCell(initial,this._root,this.nodeIdentifier);
	else
		return EnumStream.prototype.hold.bind(this)(initial);
}

EnumStream.prototype.hold = function(initial) {
	let values = this.values;
	if (values.indexOf(initial)==-1)
		values = [initial,...values]; // Not pushing due to mutation.
	return makeEnumCell(initial,values,this._root,this.nodeIdentifier);
}

