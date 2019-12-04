import {Cell,makeCell} from "./Cell.m.js";
import {} from "./Stream.m.js";
import {} from "./StreamScanFilter.m.js";

Cell.prototype.reduce = function() {
	return this.changes().scanFilter(this.initial,(x,y)=>typeof x=="object"?(!x.deepEquals(y)):x!=y).hold(this.initial);
}


