import {ConstantCell,ConstantChangingCell,EnumCell} from "./ConstantCell.m.js";
import {makeNeverStream,makeConstantStream,makeEnumStream,} from "./ConstantStream.m.js";
import {} from "../Base/StreamWithCell.m.js";

ConstantCell.prototype.changes = function() {
	return makeNeverStream(this._root,this.nodeIdentifier);
}
ConstantChangingCell.prototype.changes = function() {
	return makeConstantStream(this.value,this._root,this.nodeIdentifier);
}
EnumCell.prototype.changes = function() {
	return makeEnumStream(this.values,this._root,this.nodeIdentifier);
}




