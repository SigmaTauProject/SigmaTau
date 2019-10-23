import * as Ph from "./Push.m.js";
import * as Pl from "./Pull.m.js";


export function newPushPull(push,pull) {
	return new PushPull(push,pull);
}
export function makeLifetimePush(sendCallback,grabCallback) {
	return new LifetimePush(Ph.newPush(sendCallback),Pl.newPull(grabCallback));
}

export class Push {
	constructor(push,pull) {
		this._push = push;
		this._pull = pull;
	}
}

export function send(pp, v) {
	Ph.send(pp._push, v);
}
export function grab(pp) {
	Pl.grab(pp._pull);
}

export function push(pp) {
	return pp._push;
}
export function pull(pp) {
	return pp._pull;
}


