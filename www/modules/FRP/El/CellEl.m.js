import {} from "../Base/Cell.m.js";
import {} from "../Base/CellToStream.m.js";

/**	Creates an changing el from a cell in an el.
	The reference to the returned element must be only held within the DOM (a parent element).
	The returned element must never be taken out of the dom (it must always be in a parent element).
*/
export
function cellEl(elCell) {
	let last = elCell.initial;
	elCell.changes().forEach(el=>{last.parentElement.replaceChild(el,last);last=el;});
	return elCell.initial;
}
 
