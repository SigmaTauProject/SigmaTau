import {newPushArray, add as pha_add} from "./PushArray.m.js";


export function newPushMap(addCallback) {
	return new PushArray(addCallback);
}

export function fromPushArrayTuple(phm) {
	return newPushArray(([k,v])=>add(phm,k,v));
}
export function toPushArrayTuple(pha) {
	return newPushMap((k,v)=>pha_add(pha,[k,v]));
}

export class PushMap {
	constructor(addCallback) {
		this._addCallback = addCallback;
	}
}

export function valueMap(f, pha) {
	return newPushMap((k,v)=>cpha._addCallback(k, f(v)));
}
export function keyMap(f, pha) {
	return newPushMap((k,v)=>cpha._addCallback(f(k),v));
}
export function keyValueMap(f, pha) {
	return newPushMap((k,v)=>cpha._addCallback(f(k,v)));
}

export function add(phm, k, v) {
	pha._addCallback(k,v);
}

