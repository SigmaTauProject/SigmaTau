

function assert(cond, ...args) {
	if (!cond) console.log(...args);
}

if (!ArrayBuffer.transfer) {
	ArrayBuffer.transfer = function(source, length) {
		if (!(source instanceof ArrayBuffer))
			throw new TypeError('Source must be an instance of ArrayBuffer');
		if (length <= source.byteLength)
			return source.slice(0, length);
		var sourceView = new Uint8Array(source),
			destView = new Uint8Array(new ArrayBuffer(length));
		destView.set(sourceView);
		return destView.buffer;
	};
}

/**
	* Creates a new Uint8Array based on two different ArrayBuffers
	*
	* @private
	* @param {ArrayBuffers} buffer1 The first buffer.
	* @param {ArrayBuffers} buffer2 The second buffer.
	* @return {ArrayBuffers} The new ArrayBuffer created out of the two.
	*/
if (!Uint8Array.append) {
	Uint8Array.append = function(buffer1, buffer2) {
			var tmp = new Uint8Array(buffer1.byteLength + buffer2.byteLength);
			tmp.set(new Uint8Array(buffer1), 0);
			tmp.set(new Uint8Array(buffer2), buffer1.byteLength);
			return tmp;
	};
}

export function encodeNetMsg(msg, addedHeader, msgStructure) {
	addedHeader = new Uint8Array(addedHeader);
	function enm(msg, msgStructure) {
		if (msgStructure.type=="array") {
			assert(msg.length<=255);
			let len = msg.length;
			let msgData = new Uint8Array(0);
			for (let val of msg) {
				msgData = Uint8Array.append(msgData, enm(val,msgStructure.content));
			}
			if (msgStructure.length==="dynamic") {
				let ne = new Uint8Array(msgData.byteLength+1);
				ne.set(msgData,1);
				ne[0] = len;
				msgData = ne;
			}
			else {
				assert(len == msgStructure.length);
			}
			return msgData;
		}
		else if (msgStructure.type=="object") {
			let msgData =new Uint8Array();
			for (let msgStruct of msgStructure.values){
				let val = msg[msgStruct.name];
				msgData = Uint8Array.append(msgData,enm(val,msgStruct));
			}
			return msgData;
		}
		else {
			let msgData;
			if (msgStructure.type=="ubyte") {
				assert(msg<256&&msg>=0);
				assert(msg%1==0);
				msgData = new Uint8Array(1);
				msgData[0] = msg;
			}
			else if (msgStructure.type=="ushort") {
				assert(msg<256&&msg>=0);
				assert(msg%1==0);
				msgData = new Uint16Array(1);
				msgData[0] = msg;
				msgData = msgData.buffer;
			}
			else if (msgStructure.type=="float") {
				assert(msg<256&&msg>=0);
				msgData = new Float32Array(1);
				msgData[0] = msg;
				msgData = msgData.buffer;
			}
			return msgData;
		}
	}
	let msgBody = enm(msg, msgStructure);
	let msgLen = (1+addedHeader.length+msgBody.length);
	assert(msgLen<256);
	let msgData = new Uint8Array(msgLen);
	msgData[0] = msgLen;
	msgData.set(addedHeader,1);
	msgData.set(msgBody,1+addedHeader.length);
	return msgData;
}
export function decodeNetMsg(msgData, addedHeader, msgStructure) {
	function dnm(msgData,msgStructure) {
		var d = dnm;
		var offset = 0;
		if (msgStructure.type=="array") {
			let len;
			if (msgStructure.length==="dynamic") {
				len = msgData[0];
				offset++;
			}
			else {
				len = msgStructure.length;
			}
			let msg = [];
			for (var val=0;val<len;val++) {
				let body,used;
				let rd = dnm(msgData.slice(offset), msgStructure.content);
				body = rd.msg;
				used = rd.offset;
				
				msg.push(body);
				offset+=used;
			}
			return {msg:msg,offset:offset};
		}
		else if (msgStructure.type=="object") {
			let msg = {};
			for (let val of msgStructure.values){
				let body,used;
				let rd = dnm(msgData.slice(offset), val);
				body = rd.msg;
				used = rd.offset;
				msg[val.name] = body;
				offset+=used;
			}
			return {msg:msg,offset:offset};
		}
		else {
			if (msgStructure.type=="ubyte") {
				return {msg:msgData[0],offset:1};
			}
			else if (msgStructure.type=="ushort") {
				return {offset:2,msg:(new Uint16Array(msgData.buffer.slice(0,2)))[0]};
			}
			else if (msgStructure.type=="float") {
				return {offset:4,msg:(new Float32Array(msgData.buffer.slice(0,4)))[0]};
			}
		}
	}
	assert(msgData.slice(1,addedHeader.length)==addedHeader);
	dnm(msgData.slice(1+addedHeader.length), msgStructure);
	let msg,used;
	let rd = dnm(msgData.slice(1+addedHeader.length), msgStructure);
	msg = rd.msg;
	used = rd.offset;
	assert(used+1+addedHeader.length==msgData.length);
	return msg;
}



/**
Sample msgStructure

var msgStructure = {
	type:"object",
	values:[{type:"array",name:"components",length:"dynamic",content:{type:"ubyte"}}],
}
*/