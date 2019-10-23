

export function newCaller(...listeners) {
	return	{ _listeners	: listeners
		};
}

export function caller(c) {
	if (isDefined(c._caller)
		return c._caller;
	else
		return newCaller(c._listeners)
}

