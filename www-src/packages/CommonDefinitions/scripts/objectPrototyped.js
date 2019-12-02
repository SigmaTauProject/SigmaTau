"use strict";

Object.defineProperty(Object.prototype, 'values', {
	value: function () {
		return Object.values(this);
	}
});
Object.defineProperty(Object.prototype, 'keys', {
	value: function () {
		return Object.keys(this);
	}
});
Object.defineProperty(Object.prototype, 'entries', {
	value: function () {
		return Object.entries(this);
	}
});
