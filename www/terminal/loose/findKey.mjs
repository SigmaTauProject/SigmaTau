
export default function findKey(object, value) {
	for (let key of Object.keys(object))
		if (object[key] == value) return key;

	return "key is not found";
}
