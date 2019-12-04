import {ConstantCell,ConstantChangingCell,EnumCell,} from "./ConstantCell.m.js";
import * as PC from "./ConstantPullCell.m.js";

EnumCell.prototype.cache = function() {
	let value = this.initial;
	this.forEach(v=>value=v);
	return new PC.enumCell(()=>value,this.values);
}
ConstantChangingCell.prototype.cache = function() {
	return new PC.constantCell(this.value);
}
ConstantCell.prototype.cache = function() {
	return new PC.constantCell(this.value);
}



