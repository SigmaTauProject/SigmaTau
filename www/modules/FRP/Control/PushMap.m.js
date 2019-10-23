
export function valueMap(f, phm) {
	return {insert_callback : (k,v)=>phm.insert_callback(k, f(v))};
}
export function keyMap(f, phm) {
	return {insert_callback : (k,v)=>phm.insert_callback(f(k),v))};
}
export function keyValueMap(f, phm) {
	return {insert_callback : (k,v)=>phm.insert_callback(f(k,v))};
}

export function insert(phm, k, v) {
	pha.insert_callback(k,v);
}

