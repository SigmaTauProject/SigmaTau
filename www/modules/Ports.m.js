
import {div,Div} from "/modules/Div.m.js";

import {cell} from "/modules/FRP/Cell.m.js";

export
class Port {
	constructor(send, id,type) {
		this.sendMessage = send;
		this.id = id;
		this.type = type;
	}
}

export
class Wire extends Port {
	constructor(send, id) {
		super(send, id,"wire");
		this.value = cell(0).caching();
	}
	set(newValue) {
		this.value.change(newValue);
		
		{
			let value = networkFloat(newValue,16,true);
			let builder = new flatbuffers.Builder(1024);
			
			Msg.Wire.Set.startSet(builder);
			Msg.Wire.Set.addValue(builder, value);
			let msgContent = Msg.Wire.Set.endSet(builder);
			
			Msg.Wire.UpMsg.startUpMsg(builder);
			Msg.Wire.UpMsg.addContentType(builder, Msg.Wire.UpMsgContent.Set);
			Msg.Wire.UpMsg.addContent(builder, msgContent);
			let msg = Msg.Wire.UpMsg.endUpMsg(builder);
			
			builder.finish(msg);
			let mainBytes = builder.asUint8Array();
			let bytes = new Uint8Array(4+mainBytes.length);
			bytes.set(mainBytes, 4);
			new Uint32Array(bytes.buffer)[0] = this.id;
			this.sendMessage(bytes);
		}
	}
	adjust(amount) {
		this.value.change(this.value.grab()+amount);
		
		{
			let value = networkFloat(amount,16,true);
			let builder = new flatbuffers.Builder(1024);
			
			Msg.Wire.Adjust.startAdjust(builder);
			Msg.Wire.Adjust.addValue(builder, value);
			let msgContent = Msg.Wire.Adjust.endAdjust(builder);
			
			Msg.Wire.UpMsg.startUpMsg(builder);
			Msg.Wire.UpMsg.addContentType(builder, Msg.Wire.UpMsgContent.Adjust);
			Msg.Wire.UpMsg.addContent(builder, msgContent);
			let msg = Msg.Wire.UpMsg.endUpMsg(builder);
			
			builder.finish(msg);
			let mainBytes = builder.asUint8Array();
			let bytes = new Uint8Array(4+mainBytes.length);
			bytes.set(mainBytes, 4);
			new Uint32Array(bytes.buffer)[0] = this.id;
			this.sendMessage(bytes);
		}
	}
}

export
class LA extends Port { // Location Array
	constructor(send, id) {
		super(send, id,"la");
		this.locations = [];
	}
	receiveMessage(bytes) {
		let msg = Msg.Down.DownMsg.getRootAsDownMsg(new flatbuffers.ByteBuffer(bytes));
		let updateMsg = msg.content(new Msg.Down.LAUpdate());
		let locations = [];
		for (let i=0; i<updateMsg.valuesLength(); i++) {
			let vec3 = updateMsg.values(i);
			locations.push([vec3.x(),vec3.y(),vec3.z()]);
		}
		this.locations = locations;
		console.log(this.locations);
	}
}

export
class HackEV extends Port { // Hack Entity View
	constructor(send, id) {
		super(send, id,"hackEV");
		this.entities = cell([]);
	}
	receiveMessage(bytes) {
		let msg = Msg.HackEV.DownMsg.getRootAsDownMsg(new flatbuffers.ByteBuffer(bytes));
		if (msg.contentType() == Msg.HackEV.DownMsgContent.Update) {
			let updateMsg = msg.content(new Msg.HackEV.Update());
			let entities = [];
			for (let i=0; i<updateMsg.entitiesLength(); i++) {
				let entity = updateMsg.entities(i);
				entities.push(	{ pos	:	[ entity.pos().x()
							, entity.pos().y()
							, entity.pos().z()
							]
					, ori	:	[ entity.ori().a()
							, entity.ori().b()
							, entity.ori().c()
							, entity.ori().d()
							].map(v=>unnetworkFloat(v,8,true))
					, meshID	:entity.mesh()
					});
			}
			this.entities.change(entities);
		}
	}
}

export
class RadarArc extends Port {
	constructor(send, id) {
		super(send, id,"radarArc");
		this.pings = cell([]);
	}
	receiveMessage(bytes) {
		let msg = Msg.RadarArc.DownMsg.getRootAsDownMsg(new flatbuffers.ByteBuffer(bytes));
		if (msg.contentType() == Msg.RadarArc.DownMsgContent.Update) {
			let updateMsg = msg.content(new Msg.RadarArc.Update());
			let pings = [];
			for (let i=0; i<updateMsg.pingsLength(); i++) {
				let ping = updateMsg.pings(i);
				pings.push(	[ ping.x()
					, ping.y()
					, ping.z()
					]
				);
			}
			this.pings.change(pings);
		}
	}
}

export
class Bridge  extends Port {
	constructor(send) {
		super(send,0,"bridge");
		this.ports = cell([this]);
		this.cachedPorts = this.ports.cache();
	}
	handleMessage(dataBuffer) {
		let portID = new Uint32Array(dataBuffer)[0];
		let bytes = new Uint8Array(dataBuffer).slice(4);
		let ports = this.cachedPorts.grab()
		if (portID < ports.length) 
			ports[portID].receiveMessage(bytes);
	}
	receiveMessage(bytes) {
		let msg = Msg.Bridge.DownMsg.getRootAsDownMsg(new flatbuffers.ByteBuffer(bytes));
		if (msg.contentType() == Msg.Bridge.DownMsgContent.AddPorts) {
			let innerMsg = msg.content(new Msg.Bridge.AddPorts());
			let ports = this.cachedPorts.grab();
			for (let i=0; i<innerMsg.portsLength(); i++) {
				let port = innerMsg.ports(i);
				ports.push(	(()=>{
						if (port==Msg.Bridge.PortType.Wire)
							return new Wire(this.sendMessage, ports.length);
						if (port==Msg.Bridge.PortType.LA)
							return new LA(this.sendMessage, ports.length);
						if (port==Msg.Bridge.PortType.RadarArc)
							return new RadarArc(this.sendMessage, ports.length);
						if (port==Msg.Bridge.PortType.HackEV)
							return new HackEV(this.sendMessage, ports.length);
					})()
				);
			}
			this.ports.change(ports);
		}
		else if (msg.contentType() == Msg.Bridge.DownMsgContent.RemovePorts) {
			let innerMsg = msg.content(new Msg.Bridge.RemovePorts());
			let ports = this.cachedPorts.grab();
			for (let i=0; i<innerMsg.portsLength(); i++) {
				let port = innerMsg.ports(i);
				ports.splice(port,1);
			}
			this.ports.change(ports);
		}
	}
}

export
function portBuilder(send) {
	let nextID = 0;
	let ports = [];
	let ob = {};
	ob.done = () => ports;
	ob.ref = (f) => {f(ports[ports.length-1]); return ob;};
	ob.wire = () => {ports.push(new Wire(send,nextID++)); return ob;};
	ob.la = () => {ports.push(new LA(send,nextID++)); return ob;};
	ob.hackEV = () => {ports.push(new HackEV(send,nextID++)); return ob;};
	ob.radarArc = () => {ports.push(new RadarArc(send,nextID++)); return ob;};
	return ob;
}


function networkFloat(value,bits,signed=true) {
	value = Math.min(1,Math.max(signed?-1:0,value));
	return Math.trunc(value*maxBound(bits,signed));
}
function unnetworkFloat(value,bits,signed=true) {
	value = value / maxBound(bits,signed);
	return Math.min(1,Math.max(signed?-1:0,value));
}
function maxBound(bits,signed=true) {
	if (signed)
		return Math.pow(2,bits)/2-1;
	else
		return Math.pow(2,bits)-1;
}
function minBound(bits,signed=true) {
	if (signed)
		return -Math.pow(2,bits)/2;
	else
		return -Math.pow(2,bits);
}