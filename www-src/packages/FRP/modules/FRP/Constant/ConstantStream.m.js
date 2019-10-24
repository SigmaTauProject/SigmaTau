import {Stream,RootStream,merge as basicMerge} from "../Base/Stream.m.js";
import {} from "./StreamWithConstant.m.js";
import {newRoot,newDeadRoot,joinRoots,joinRootsMap,partialRoot,compareRoots,same,overlapping,discrete} from "../Core/PushRoot.m.js";

export {RootConstantStream,ConstantStream,constantStream,makeConstantStream,};
export {RootEnumStream,EnumStream,enumStream,makeEnumStream,};
export {NeverStream,neverStream,makeNeverStream,};
export {merge,};


class EnumStream extends Stream {
	constructor(values,...args) {
		super(...args);
		this.values = values;
	}
	map(f) {
		let n = makeEnumStream(this.values.map(f),this._root);
		this._root.addNode((scope)=>{
			scope[n.nodeIdentifier]=f(scope[this.nodeIdentifier]);
		});
		return n;
	}
	filter(f) {
		let n = makeEnumStream(this.values.filter(f),partialRoot(this._root),this.nodeIdentifier);
		this._root.addNode(scope=>{
			if (f(scope[this.nodeIdentifier]))
				s._root.sendScope(scope);
		});
		return n;
	}
	merge(...streams) {
		merge(this,...streams);
	}
}
class RootEnumStream extends EnumStream {
	constructor(values) {
		let nodeId = Symbol();
		super(values,newRoot(nodeId),nodeId);
	}
	send(value) {
		if (this.values.indexOf(value)!=-1)
			this._root.send(value);
		else
			console.assert(false);
	}
}


class ConstantStream extends EnumStream {
	constructor(value,...args) {
		super([value],...args);
	}
	get value() {
		return this.values[0];
	}
	map(f) {
		let n = makeConstantStream(f(this.value),this._root);
		this._root.addNode((scope)=>{
			scope[n.nodeIdentifier]=n.value;
		});
		return n;
	}
	filter() {
		console.assert(false,"No logical `filter` for constant stream");
		return n;
	}
	merge(...streams) {
		merge(this,...streams);
	}
}
class RootConstantStream extends ConstantStream {
	constructor(value) {
		let nodeId = Symbol();
		super(value,newRoot(nodeId),nodeId);
	}
	send() {
		this._root.send(this.value);
	}
}
class NeverStream extends EnumStream {
	constructor(...args) {
		super([],...args);
	}
	map(_) {
		return makeNeverStream(this._root);
	}
	forEach(_) {
		return this;
	}
	filter(_) {
		return makeNeverStream(partialRoot(this._root));
	}
	scan(_,__) {
		return makeNeverStream(this._root);
	}
}

function neverStream() {
	return makeNeverStream();
}
NeverStream.stream = neverStream;
Stream.never = neverStream;

function constantStream(value) {
	return new RootConstantStream(value);
}
ConstantStream.stream = constantStream;
Stream.constant = constantStream;

function enumStream(values) {
	if (values.length==0)
		return never();
	else if (values.length==1)
		return constantStream(values[0]);
	else
		return new RootEnumStream(values);
}
EnumStream.stream = enumStream;
Stream.enum = enumStream;

function makeNeverStream(root=null,nnid=null) {
	if (nnid==null)
		nnid = Symbol();
	if (root==null)
		root = newDeadRoot(nnid);
	return new NeverStream(root,nnid);
}
function makeConstantStream(value,root=null,nnid=null) {
	if (nnid==null)
		nnid = Symbol();
	if (root==null)
		root = newRoot(nnid);
	return new ConstantStream(value,root,nnid);
}
function makeEnumStream(values,root=null,nnid=null) {
	if (values.length==0)
		return makeNeverStream(root,nnid);
	else if (values.length==1)
		return makeConstantStream(values[0],root,nnid);
	else {
		if (nnid==null)
			nnid = Symbol();
		if (root==null)
			root = newRoot(nnid);
		return new EnumStream(values,root,nnid);
	}
}

function merge(...streams) {
	if (streams.all(s=>s instanceof EnumStream)) {
		console.assert(compareRoots(...streams.map(s=>s._root)) == discrete);
		let nnid=Symbol();
		return new EnumStream(streams.concatMap(s=>s.values),joinRootsMap(nnid,streams.map(s=>[s._root,s.nodeIdentifier])),nnid);
	}
	else {
		return basicMerge(...streams);
	}
}

