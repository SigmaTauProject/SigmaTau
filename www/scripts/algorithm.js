"use strict";

Object.defineProperty(Array.prototype, 'concatMap', {
	value: function (callback) {
		let newArray = [];
		for (var item of this) {
			newArray.push(...callback(item));
		}
		return newArray;
	}
});

Object.defineProperty(Array.prototype, 'groupBy', {
	value: function (group_callback) {
		return this.reduce((groups,x)=>{
			let [k,v] = group_callback(x);
			(groups[k]=groups[k]||[]).push(v);
			return groups;
		},{});
	}
});

Object.defineProperty(Array.prototype, 'any', {
	value: function (f=v=>v) {
		for (let v of this) {
			if (f(v)) return true;
		}
		return false;
	}
});
Object.defineProperty(Array.prototype, 'all', {
	value: function (f=v=>v) {
		for (let v of this) {
			if (!f(v)) return false;
		}
		return true;
	}
});
Object.defineProperty(Array.prototype, 'head', {
	value: function () {
		return this[0];
	}
});
Object.defineProperty(Array.prototype, 'tail', {
	value: function () {
		return this.slice(1,this.length);
	}
});
Object.defineProperty(Array.prototype, 'last', {
	value: function () {
		return this[this.length-1];
	}
});
Object.defineProperty(Array.prototype, 'init', {
	value: function () {
		return this.slice(0,this.length-1);
	}
});

Object.defineProperty(Object.prototype, 'pipe', {
	value: function (callback) {
		return callback(this);
	}
});
Object.defineProperty(Object.prototype, 'pipeIf', {
	value: function (cond, t,f=a=>a) {
		return cond?t(this):f(this);
	}
});

Object.defineProperty(Array.prototype, 'chunk', {
	value: function(chunkSize) {
		var R = [];
		for (var i = 0; i < this.length; i += chunkSize)
			R.push(this.slice(i, i + chunkSize));
		return R;
	}
});


function curry(f, ...args) {
	return (...moreArgs)=>f(...args,...moreArgs);
}




function zip(a,b) {
	var c = [];
	for (var i=0; i<Math.max(a.length,b.length); i++){
		c.push([a[i], b[i]]);
	}
	return c;
}

