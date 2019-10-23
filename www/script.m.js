import {div,Div} from "/modules/div.m.js";
////import makeLayout from "./Layout.m.js";

function startNetworking() {
	let ws = new WebSocket("ws://"+document.location.host);
	
	ws.addEventListener("open",e=>console.log("Open ",e));
	ws.addEventListener("error",e=>console.log("Error ",e));
	ws.addEventListener("message",e=>{
		console.log("Message ",e)
		e.data.arrayBuffer().then(d=>{
			console.log(d);
		});
	});
	ws.addEventListener("close",e=>console.log("Close ",e));
}




////let [componentsPush,controls] = makeLayout();
startNetworking();

////let iframe = div("iframe");
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


