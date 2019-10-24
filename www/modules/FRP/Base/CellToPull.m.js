import {Cell} from "./Cell.m.js";
import * as PC from "./PullCell.m.js";

Cell.prototype.cache = function() {
		let value = this.initial;
		this.forEach(v=>value=v);
		return new PC.cell(()=>value);
}
Cell.prototype.caching = function() {
	return new HC.Cell(this.initial,this._root,this.nodeIdentifier);
}



