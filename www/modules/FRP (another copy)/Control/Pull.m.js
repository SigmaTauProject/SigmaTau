import {newPull} from "../Data/Pull.m.js";

export function pullMap(f, pl) {
	return newPull(()=>f(pl._grab_callback(v)));
}

export function grab(pl) {
	pl._grab_callback();
}


