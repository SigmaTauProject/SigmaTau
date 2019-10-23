
export function newPush(callback) {
	return new Push(callback);
}

export class Push {
	constructor(callback) {
		this._callback = callback;
	}
}

export function map(f, ph) {
	return newPush(v=>ph._callback(f(v)));
}

export function send(ph, v) {
	ph._callback(v);
}

