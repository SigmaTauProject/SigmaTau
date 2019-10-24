export {Cell,cell,constant};

class Cell {
	constructor (callback) {
		this.grab = callback;
	}
	map(f) {
		return new Cell(()=>f(callback()));
	}
}

function cell(callback) {
	return new Cell(callback);
}
function constant(value) {
	return new Cell(()=>value);
}
Cell.cell = cell;
Cell.constant = constant;




