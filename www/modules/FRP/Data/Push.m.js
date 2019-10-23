
export function newPush(send_callback) {
	return {send_callback: send_callback};
}

export function push(p) {
	if (isDefined(p.push))
		return p.push;
	else
		return newPush(p.send_callback)
}


