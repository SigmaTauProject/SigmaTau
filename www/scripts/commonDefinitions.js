"use strict";
﻿
// Can be set from url arg
// no buttons should show
var noUi = false

function emptyEl(el) {
	while (el.hasChildNodes()) {
		el.removeChild(el.lastChild);
	}
}
function id(a) {
	return a;
}

function ifUndefined(a,b) {
	if (typeof a != "undefined")
		return a;
	else
		return b
}
function isUndefined(a) {
	return (typeof a == "undefined");
}
function isDefined(a) {
	return (typeof a != "undefined");
}

function replaceAll(item, find, replacer) {
	while (item.indexOf(find) != -1) {
		item = item.replace(find, replacer)
	}
	return item
}

Array.prototype.remove = function (item) {
	if (this.indexOf(item)!=-1)
		this.splice(this.indexOf(item),1);
}

