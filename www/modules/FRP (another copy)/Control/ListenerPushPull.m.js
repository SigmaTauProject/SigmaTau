import * as LP from "./ListenerPush.m.js";
import * as Pl from "./Pull.m.js";


export function newListenerPushPull(listenerPush,pull) {
	return new PushPull(listenerPush,pull);
}
export function makeLifetimePush(grabCallback, ...listeners) {
	return new LifetimePush(LP.newListenerPush(...listeners),Pl.newPull(grabCallback));
}

export class Push {
	constructor(listenerPush,pull) {
		this._listenerPush = push;
		this._pull = pull;
	}
}

export function send(lpp, v) {
	LP.send(pp._listenerPush);
}
export function grab(lpp) {
	pp._pull._callback();
}
export function listen(lpp, ...listeners) {
	return LP.listen(lpp._listenerPush, ...listeners);
}

export function listenerPush(lpp) {
	return pp._listenerPush;
}
export function pull(lpp) {
	return pp._pull;
}
export function push(lpp) {
	return LP.push(pp._listenerPush);
}


