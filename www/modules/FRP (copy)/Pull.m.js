import {newPush} from "./Push.m.js";


export function newPull(callback) {
	return new Pull(callback);
}
export function cache() {
	let c;
	return [newPush(v=>c=v),newPull(()=>c)];
}

export class Pull {
	constructor(callback) {
		this._callback = callback;
	}
}

export function map(f, pl) {
	return newPush(v=>pl._callback(f(v));
}

export function grab(pl) {
	pl._callback();
}


