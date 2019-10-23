


export function newPushArray(addCallback) {
	return new PushArray(addCallback);
}

export class PushArray {
	constructor(addCallback) {
		this._addCallback = addCallback;
	}
}

export function map(f, pha) {
	return newPushArray(v=>cpha._addCallback(f(v)));
}

export function add(pha, v) {
	pha._addCallback(v);
}

