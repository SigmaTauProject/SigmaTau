import {newPush} from "./Push.m.js";


export function newPull(grab_callback) {
	return {grab_callback: grab_callback};
}
export function cache() {
	let c;
	return [newPush(v=>c=v),newPull(()=>c)];
}
export function pull(p) {
	if (isDefined(p.pull))
		return p.pull;
	else
		return newPull(p.grab_callback)
}

