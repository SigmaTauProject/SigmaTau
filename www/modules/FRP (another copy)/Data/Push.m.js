
export function newPush(send_callback) {
	return {_send_callback: send_callback};
}

export function push(p) {
	if (isDefined(p._push)
		return p._push;
	else
		return newPush(p._send_callback)
}


