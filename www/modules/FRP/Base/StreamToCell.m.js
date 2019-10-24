import {Stream} from "./Stream.m.js";
import {makeCell} from "./Cell.m.js";


Stream.prototype.hold = function(initial) {
	return makeCell(initial,this._root,this.nodeIdentifier);
}
Stream.prototype.accum = function(initial,f=(previous,func)=>func(previous)) {
	return this.scan(initial,f).hold(initial);
}

