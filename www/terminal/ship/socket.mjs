

export default class Socket {
	constructor() {
		this.connected	= false	;
		this._msgs	= []	;
		this._onConnectedCallbacks	= []	;
		
		this._connect();
	}
	_connect() {
		console.log("[Socket]","Connecting...");
		this.socket = new WebSocket(_getURL());
		this.socket.binaryType="arraybuffer";
		
		this.socket.onopen	= this._onOpen	.bind(this);
		this.socket.onmessage	= this._onMsg	.bind(this);
		this.socket.onclose	= this._onClose	.bind(this);
		this.socket.onerror	= this._onError	.bind(this);
	}
	
	send(msg) {
		this.socket.send(msg);
	}
	getMsgs() {
		var msgs	= this._msgs	;
		this._msgs	= []	;
		return msgs;
	}
	close() {
		socket.close();
	}
	
	[Symbol.iterator]() {
		return this.getMsgs()[Symbol.iterator]();
	}
	
	onConnected(callback) {
		this._onConnectedCallbacks.push(callback);
	}
	
	_onMsg(e) {
		this._msgs.push(new Uint8Array(e.data));
	}
	_onOpen() {
		console.log("[Socket]","Connected");
		this.connected = true;
		for (let callback of this._onConnectedCallbacks) {
			callback();
		}
	}
	_onClose() {
		console.log("[Socket]","Disconnected");
		this.connected = false;
	}
	_onError(e) {
		console.log("[Socket]",e);
	}
	
}


function _getURL() {
	var baseURL;
	{
		let href = window.location.href.substring(7); // strip "http://"
		let idx = href.indexOf("/");
		baseURL = "ws://" + href.substring(0, idx);
	}
	return baseURL+"/ws";
}

