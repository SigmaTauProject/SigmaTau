

export function newPushPullCaller(push,pull,caller) {
	return	{ push	: push
		, send_callback	: push.send_callback
		, pull	: pull
		, grab_callback	: pull.grab_callback
		, caller	: caller
		, listeners	: caller.listeners
		};
}
export function makeLifetimePush(send_callback, grab_callback, ...listeners) {
	return	{ send_callback	: send_callback
		, grab_callback	: grab_callback
		, listeners	: listeners
		};
}





