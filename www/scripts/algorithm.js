"use strict";

Array.prototype.concatMap = function (callback) {
	let newArray = [];
	for (var item of this) {
		newArray.push(...callback(item));
	}
	return newArray;
};

Array.prototype.groupBy = function (group_callback) {
	return this.reduce((groups,x)=>{
		let [k,v] = group_callback(x);
		(groups[k]=groups[k]||[]).push(v);
		return groups;
	},{});
};

Array.prototype.any = function () {
	return this.reduce((a,b)=>a||b);
}
Array.prototype.all = function () {
	return this.reduce((a,b)=>a&&b);
}
Array.prototype.head = function () {
	return this[0];
}
Array.prototype.tail = function () {
	return this.slice(1,this.length);
}
Array.prototype.last = function () {
	return this[this.length-1];
}
Array.prototype.init = function () {
	return this.slice(0,this.length-1);
}

Object.prototype.pipe = function (callback) {
	return callback(this);
}
Object.prototype.pipeIf = function (cond, t,f=a=>a) {
	return cond?t(this):f(this);
}


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

