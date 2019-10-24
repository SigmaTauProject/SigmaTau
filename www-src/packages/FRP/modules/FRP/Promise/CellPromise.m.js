import {Cell,makeCell} from "../Base/Cell.m.js";

export {promiseToCell};

Cell.prototype.thenOrdered = function(newInitial) {
	let lastPromise = Promise.resolve();
	let n = makeCell(newInitial);
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
Cell.prototype.thenLatest = function(newInitial) {
	let n = makeCell(newInitial);
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

function promiseToCell(initial,promise) {
	let n = makeCell(initial);
	promise.then(v=>n._root.send(n));
	return n;
}

