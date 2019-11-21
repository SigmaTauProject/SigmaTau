import {Cell} from "../Base/Cell.m.js";
import * as PC from "../Base/PullCell.m.js";
import * as HC from "../Base/HybridCell.m.js";

/**	Takes a value to be treated as a cell.
	If value is a cell it will simply be returned, else it will be made into a constant.
*/
export
function asCell(value) {
	if (value instanceof Cell || a instanceof PC.Cell || a instanceof HC.Cell)
		return value;
	return Cell.constant(value);
}
 
