

export function newPushCaller(...listeners) {
	return	{ listeners	: listeners
		, send_callback	: v=>this._listeners.forEach(l=>l(v));
		};
}

export function pushCaller(x) {
	if (isDefined(c.pushCaller))
		return c.pushCaller;
	else if (isDefined(c.push)&&isDefined(c.caller))
		return	{ push	: c.push
			, send_callback	: c.push.send_callback
			, caller	: c.caller
			, listeners	: c.caller.listeners
			};
	else if (isDefined(c.push)&&isDefined(c.listeners))
		return	{ push	: c.push
			, send_callback	: c.push.send_callback
			, listeners	: c.listeners
			};
	else if (isDefined(c.send_callback)&&isDefined(c.caller))
		return	{ send_callback	: c.send_callback
			, caller	: c.caller
			, listeners	: c.caller.listeners
			};
	}
	else
		return	{ send_callback	: c.send_callback
			, listeners	: c.listeners
			};
}

