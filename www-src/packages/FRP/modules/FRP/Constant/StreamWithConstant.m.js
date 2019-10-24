import {Stream} from "../Base/Stream.m.js";
import {ConstantStream,makeConstantStream} from "./ConstantStream.m.js";

Stream.prototype.mapTo = function(value) {
	let n = makeConstantStream(value,this._root);
	this._root.addNode((scope)=>{
		scope[n.nodeIdentifier]=n.value;
	});
	return n;
}



