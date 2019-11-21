import {Cell,RootCell} from "./Cell.m.js";
import * as HC from "./HybridCell.m.js";

Cell.prototype.caching = function() {
	return new HC.Cell(this.initial,this._root,this.nodeIdentifier);
}
RootCell.prototype.caching = function() {
	return new HC.RootCell(this.initial,this._root,this.nodeIdentifier);
}



