"use strict";


Object.defineProperty(Set.prototype, 'map', {
	value: function (callback) {
		let newSet = new Set();
		this.forEach(v=>newSet.add(callback(v)));
		return newSet;
	}
});
Object.defineProperty(Set.prototype, 'concatMap', {
	value: function (callback) {
		let newSet = new Set();
		this.forEach(v=>callback(v).forEach(nv=>newSet.add(nv)));
		return newSet;
	}
});

Object.defineProperty(Set.prototype, 'any', {
	value: function (f=v=>v) {
		for (let v of this) {
			if (f(v)) return true;
		}
		return false;
	}
});
Object.defineProperty(Set.prototype, 'all', {
	value: function (f=v=>v) {
		for (let v of this) {
			if (!f(v)) return false;
		}
		return true;
	}
});


Object.defineProperty(Set.prototype, 'isSubset', {
	value: function(subset) {
		for (var elem of subset) {
			if (!this.has(elem)) {
				return false;
			}
		}
		return true;
	}
});
Object.defineProperty(Set.prototype, 'union', {
	value: function(other) {
		other.forEach(v=>this.add(v));
		return this;
	}
});
Object.defineProperty(Set.prototype, 'intersection', {
	value: function(other) {
		other.forEach(v=>{if (!this.has(v)) this.delete(v);});
		return this;
	}
});
Object.defineProperty(Set.prototype, 'symmetricDifference', {
	value: function(other) {
		other.forEach(v=>{
			if (this.has(v)) {
				this.delete(v);
			} else {
				this.add(v);
			}
		});
		return _difference;
	}
});
Object.defineProperty(Set.prototype, 'difference', {
	value: function(other) {
		other.forEach(v=>{this.delete(elem);});
		return this;
	}
});


function union(setA,...sets) {
	var u = new Set(setA);
	sets.forEach(s=>s.forEach(v=>u.add(v)));
	return u;
}

function intersection(setA,...sets) {
	var n = new Set();
	setA.forEach(v=>{if (sets.map(s=>s.has(v)).all()) n.add(v);});
	return n;
}
function intersectionAny(...sets) {
	var n = new Set();
	sets.init().forEach((set_,i)=>set_.forEach(v=>{if (sets.slice(i+1).map(s=>s.has(v)).any()) n.add(v);}));
	return n;
}

function symmetricDifference(setA, setB) {
	var _difference = new Set(setA);
	for (var elem of setB) {
		if (_difference.has(elem)) {
			_difference.delete(elem);
		} else {
			_difference.add(elem);
		}
	}
	return _difference;
}

function difference(setA, setB) {
	var _difference = new Set(setA);
	for (var elem of setB) {
		_difference.delete(elem);
	}
	return _difference;
}


