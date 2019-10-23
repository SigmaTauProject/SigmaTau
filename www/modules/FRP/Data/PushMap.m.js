import {newPushSet} from "./PushSet.m.js";
import {add} from "../Control/PushSet.m.js";


export function newPushMap(insert_callback) {
	return {insert_callback:insert_callback};
}

export function fromPushArrayTuple(phm) {
	return newPushSet(([k,v])=>insert(phm,k,v));
}
export function toPushArrayTuple(pha) {
	return newPushMap((k,v)=>add(pha,[k,v]));
}




