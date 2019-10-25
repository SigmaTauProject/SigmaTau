
import {div,Div} from "/modules/Div.m.js";

import {cell} from "/modules/FRP/Cell.m.js";

export
class Port {
	constructor(send, id,type) {
		this.send = send;
		this.id = id;
		this.type = type;
	}
}

export
class Wire extends Port {
	constructor(send, id) {
		super(send, id,"wire");
		this.value = 0;
	}
	set(value) {
		console.log(this.id,value);
		this.value = 0;
		
			let rawValue = value;// Hack so inner scope can redefine value but also use it.
		{
			let value = networkFloat(rawValue,16,true);
			let builder = new flatbuffers.Builder(1024);
			
			Msg.Up.WireSet.startWireSet(builder);
			Msg.Up.WireSet.addId(builder, 0);
			Msg.Up.WireSet.addValue(builder, value);
			let msgContent = Msg.Up.WireSet.endWireSet(builder);
			
			Msg.Up.UpMsg.startUpMsg(builder);
			Msg.Up.UpMsg.addContentType(builder, Msg.Up.MsgContent.WireSet);
			Msg.Up.UpMsg.addContent(builder, msgContent);
			let msg = Msg.Up.UpMsg.endUpMsg(builder);
			
			builder.finish(msg);
			let bytes = builder.asUint8Array();
			this.send(bytes);
		}
	}
	adjust(amount) {
		console.log(this.id,amount);
		this.value += amount;
		
		{
			let value = networkFloat(amount,16,true);
			let builder = new flatbuffers.Builder(1024);
			
			Msg.Up.WireAdjust.startWireAdjust(builder);
			Msg.Up.WireAdjust.addId(builder, 0);
			Msg.Up.WireAdjust.addValue(builder, value);
			let msgContent = Msg.Up.WireAdjust.endWireAdjust(builder);
			
			Msg.Up.UpMsg.startUpMsg(builder);
			Msg.Up.UpMsg.addContentType(builder, Msg.Up.MsgContent.WireAdjust);
			Msg.Up.UpMsg.addContent(builder, msgContent);
			let msg = Msg.Up.UpMsg.endUpMsg(builder);
			
			builder.finish(msg);
			let bytes = builder.asUint8Array();
			this.send(bytes);
		}
	}
}

export
function portBuilder(send) {
	let nextID = 0;
	let ports = [];
	let ob = {};
	ob.done = () => ports;
	ob.wire = () => {ports.push(new Wire(send,nextID++)); return ob;};
	return ob;
}


function networkFloat(value,bits,signed=true,value100=1) {
	value = Math.min(value100,Math.max(signed?-value100:0,value));
	if (signed)
		value = Math.trunc(value*(Math.pow(2,bits)/2-1));
	else
		value = Math.trunc(value*(Math.pow(2,bits)-1));
	return value;
}