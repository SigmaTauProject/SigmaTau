

export function pullMap(f, pl) {
	return {grab_callback : ()=>f(pl.grab_callback())};
}

export function grab(pl) {
	pl.grab_callback();
}


