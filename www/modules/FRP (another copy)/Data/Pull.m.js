import {newPush} from "./Push.m.js";


export function newPull(grab_callback) {
	return {_grab_callback: grab_callback};
}
export function cache() {
	let c;
	return [newPush(v=>c=v),newPull(()=>c)];
}
export function pull(p) {
	if (isDefined(p._pull)
		return p._pull;
	else
		return newPull(p._grab_callback)
}

