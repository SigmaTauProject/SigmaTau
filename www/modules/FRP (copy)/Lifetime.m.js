


function newLifetime(callback) {
	return new Lifetime(callback);
}

export class Lifetime {
	constructor(callback) {
		this._callback = callback;
	}
}

export function kill(lt) {
	lt._callback();
}


