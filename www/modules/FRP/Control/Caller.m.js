import {newLifetime} from "../Data/Lifetime.m.js";

export function listen(lp, ...listeners) {
	lp.listeners.push(...listeners);
	return newLifetime(()=>{listeners.forEach(l=>lp.listeners.remove(l);});
}

