import {Stream,makeStream} from "../Base/Stream.m.js";

Stream.prototype.thenOrdered = function() {
	let lastPromise = Promise.resolve();
	let n = makeStream();
	this.forEach(pValue=>{
		lastPromise = new Promise((resolve,reject)=>{
			lastPromise.then(()=>{
				pValue	.then(v=>n._root.send(v))
					.then(()=>resolve())
					.catch(e=>{resove();throw e;});
			});
		});
	});
	return n;
}
Stream.prototype.thenUnordered = function() {
	let n = makeStream();
	this.forEach(pValue=>{
		pValue.then(v=>n._root.send(v));
	});
	return n;
}
Stream.prototype.thenLatest = function() {
	let n = makeStream();
	let cancelLasts = [];
	this.forEach(pValue=>{
		let localCancelLasts = [...cancelLasts];
		cancelLasts.push(then_(pValue, v=>{doCancel(); n._root.send(v);}));
		function doCancel() {
			localCancelLasts.forEach(cl=>cl());
			cancelLasts.splice(0,cancelLasts.indexOf(localCancelLasts.last())+1);
		}
	});
	return n;
	
	function then_(promise,f) {
		let notCanceled = true;
		promise.then((...args)=>{if(notCanceled) f(...args);});
		return ()=>notCanceled=false;
	}
}
