import {newPush} from "../Data/Push.m.js";

export function pushMap(f, ph) {
	return newPush(v=>ph._send_callback(f(v)));
}

export function send(ph, v) {
	ph._send_callback(v);
}


