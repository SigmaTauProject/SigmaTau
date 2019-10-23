

Array.prototype.each = function(callback) {
	var newArray = [];
	for (let item of this) {
		newArray.push(callback(item));
	}
	return newArray;
}


