import {div,Div} from "/modules/Div.m.js";
import {portBuilder,Port,Wire} from "/modules/Ports.m.js";
import makeLayout from "./Layout.m.js";

function startNetworking() {
	let ws = new WebSocket("ws://"+document.location.host);
	
	ws.addEventListener("open",e=>console.log("Open ",e));
	ws.addEventListener("error",e=>console.log("Error ",e));
	ws.addEventListener("message",e=>{
		e.data.arrayBuffer().then(d=>new Uint8Array(d)).then(d=>{
			hackEVPort.receiveMessage(d);
		});
	});
	ws.addEventListener("close",e=>console.log("Close ",e));
	
	window.ws = ws;
}


function send(bytes) {
	ws.send(bytes);
}
////function send(value) {
////	value = networkFloat(value,16,true);
////	
////	let builder = new flatbuffers.Builder(1024);
////	Data.Msg.Up.SetThruster.startSetThruster(builder);
////	Data.Msg.Up.SetThruster.addId(builder, 0);
////	Data.Msg.Up.SetThruster.addValue(builder, value);
////	let thruster = Data.Msg.Up.SetThruster.endSetThruster(builder);
////	Data.Msg.Up.UpMsg.startUpMsg(builder);
////	Data.Msg.Up.UpMsg.addContentType(builder, Data.Msg.Up.MsgContent.SetThruster);
////	Data.Msg.Up.UpMsg.addContent(builder, thruster);
////	let msg = Data.Msg.Up.UpMsg.endUpMsg(builder);
////	builder.finish(msg);
////	let buf = builder.asUint8Array();
////	ws.send(buf);
////}
////window.send = send;

function networkFloat(value,bits,signed=true,value100=1) {
	value = Math.min(value100,Math.max(signed?-value100:0,value));
	if (signed)
		value = Math.trunc(value*(Math.pow(2,bits)/2-1));
	else
		value = Math.trunc(value*(Math.pow(2,bits)-1));
	return value;
}


////let [componentsPush,controls] = makeLayout();
startNetworking();

let hackEVPort;
let iframe = div("iframe");
document.body.appendChild(iframe);
setTimeout(()=>iframe.contentDocument.body.parentElement.replaceChild(
	makeLayout(	portBuilder(send)
		.wire()
		.wire()
		.hackEV().ref(r=>hackEVPort=r)
		.done()
	),
	iframe.contentDocument.body
));
////iframe.contentDocument.bodyj.appendChild(controls);
////document.body.appendChild(iframe);
////
////
////
////
////
////
////Object.defineProperty(Blob.prototype, 'arrayBuffer', {
////  configurable: true,
////  enumerable: true,
////  writable: true,
////  value: function arrayBuffer() {
////    return new Response(this).arrayBuffer();
////  }
////});



