import {Stream} from "./Stream.m.js";
import {Cell} from "./Cell.m.js";
////import {Cell as PullCell} from "./PullCell.m.js";
import {} from "./CellToStream.m.js";
import {compareRoots,same,overlapping,discrete} from "../Core/PushRoot.m.js";

Stream.prototype.snapshot = function(c,f=(s,c)=>c) {
	if (compareRoots(this._root)==same)
		return c.changes();
	if (c instanceof Cell)
		c = c.cache();
	return this.map(()=>c.grab());
}


