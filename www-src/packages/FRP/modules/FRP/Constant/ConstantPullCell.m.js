import {Cell} from "../Base/PullCell.m.js";

export {EnumCell,ConstantCell,enumCell,constantCell};

class EnumCell extends Cell {
	constructor (callback, values) {
		super(callback);
		this._callback = callback;
		this.values = values;
	}
	map(f) {
		return new EnumCell(()=>f(callback()),this.values.map(f));
	}
	grab() {
		let v = this._callback();
		if (this.values.indexOf(v)!=-1)
			return v;
		else
			console.assert(false);
	}
}
class ConstantCell extends EnumCell {
	constructor(value) {
		super(()=>console.assert(false,"Why is this being called?!"),[value]);
	}
	get value() {
		return this.values[0];
	}
	map(f) {
		return new ConstantCell(f(this.value));
	}
	grab() {
		return this.value;
	}
}

function enumCell(callback,values) {
	return new Cell(callback,values);
}
EnumCell.cell = enumCell;
Cell.enum = enumCell;

function constantCell(value) {
	return new Cell(()=>value);
}
ConstantCell.cell = constantCell;
Cell.constant = constantCell;




