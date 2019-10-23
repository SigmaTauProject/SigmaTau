

export function newPushPull(push,pull) {
	return	{ push	: push
		, send_callback	: push.send_callback
		, pull	: pull
		, grab_callback	: pull.grab_callback
		};
}
export function makeLifetimePush(send_callback, grab_callback) {
	return	{ send_callback	: send_callback
		, grab_callback	: grab_callback
		};
}

