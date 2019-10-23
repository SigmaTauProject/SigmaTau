import {newCaller} from "../Data/Caller.m.js";
import {newLifetime} from "../Data/Lifetime.m.js";

export function listen(lp, ...listeners) {
	lp._listeners.push(...listeners);
	return newLifetime(()=>{listeners.forEach(l=>lp._listeners.remove(l);});
}

