import {newPush} from "./Push.m.js";
import {newLifetime} from "./Lifetime.m.js";

export function newListenerPush(...listeners) {
	return new ListenerPush(...listeners);
}

export class ListenerPush {
	constructor(...listeners) {
		this._listeners = listeners;
	}
}

export function map(f, lp) {
	return newPush(v=>send(lp, f(v)));
}

export function send(lp, v) {
	lp._listeners.forEach(l=>l(v));
}
export function listen(lp, ...listeners) {
	lp._listeners.push(...listeners);
	return newLifetime(()=>{listeners.forEach(l=>lp._listeners.remove(l);});
}

export function push(lp) {
	return newPush(v=>send(lp,v));
}

