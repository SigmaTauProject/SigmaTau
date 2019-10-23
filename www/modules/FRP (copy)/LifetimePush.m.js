import * as Ph from "./Push.m.js";
import * as L from "./Lifetime.m.js";


export function newLifetimePush(lifetime,push) {
	return new LifetimePush(lifetime,push);
}
export function makeLifetimePush(killCallback,sendCallback) {
	return new LifetimePush(L.newLifetime(killCallback),Ph.newPush(sendCallback));
}

export class LifetimePush {
	constructor(lifetime,push) {
		this._lifetime = lifetime;
		this._push = push;
	}
}

export function map(f, lph) {
	return newLifetimePush(v=>lph._push._callback(f(v)),lph._lifetime);
}

export function send(lph, v) {
	Ph.send(lph._push, v);
}
export function kill(lph) {
	L.kill(ph._lifetime);
}

export function lifetime(lph) {
	return lph._lifetime;
}
export function push(lph) {
	return lph._push;
}

