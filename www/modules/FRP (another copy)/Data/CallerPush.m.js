

export function newCallerPush(...listeners) {
	return	{ _listeners	: listeners
		, _send_callback	: v=>this._listeners.forEach(l=>l(v));
		};
}


