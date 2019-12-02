"use strict";


Object.defineProperty(HTMLElement.prototype, "removeAllChildren", {value:
	function () {
		while (this.firstChild) {
			this.removeChild(this.firstChild);
		}
	}
});

function replaceAll(item, find, replacer) {
	while (item.indexOf(find) != -1) {
		item = item.replace(find, replacer)
	}
	return item
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



function uppercaseFirst(str) {
	return str.charAt(0).toUpperCase() + str.substring(1);
}


var _;


function promiseTimeout(delay) {
	return new Promise((r)=>setTimeout(()=>r(), delay));
}


/** Sourced from `https://stackoverflow.com/a/32922084/2521261`.
*/
function deepEqual(x, y) {
  const ok = Object.keys, tx = typeof x, ty = typeof y;
  return x && y && tx === 'object' && tx === ty ? (
    ok(x).length === ok(y).length &&
      ok(x).every(key => deepEqual(x[key], y[key]))
  ) : (x === y);
}
Object.defineProperty(Object.prototype, "deepEquals", {value:function (y) {return deepEqual(this,y);}});


