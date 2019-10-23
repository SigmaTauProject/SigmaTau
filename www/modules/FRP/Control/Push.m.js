

export function pushMap(f, ph) {
	return {send_callback : v=>ph.send_callback(f(v)));
}

export function send(ph, v) {
	ph.send_callback(v);
}


