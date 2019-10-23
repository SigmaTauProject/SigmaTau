

export default class IdArray {
	constructor() {
		this.array	= {}	;
		this.nextId	= 0	;
	}
	push(item) {
		this.array[this.nextId++] = item;
	}
	remove(item) {
		const key = Object.keys(this.array).find(key => this.array[key] === value);
		delete this.array[key];
	}
	each(callback) {
		var newArray = {};
		for (var key of Object.keys(this.array)) {
			newArray[key] = callback(this.array[key],key);
		}
		return newArray;
	}
}


