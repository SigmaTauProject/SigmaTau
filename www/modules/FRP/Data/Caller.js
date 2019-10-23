

export function newCaller(...listeners) {
	return	{ listeners	: listeners
		};
}

export function caller(c) {
	if (isDefined(c.caller))
		return c.caller;
	else
		return newCaller(c.listeners)
}

