import {Push} from "../Core/Push.m.js";
import {newRoot,newDeadRoot,joinRoots,joinRootsMap,partialRoot,compareRoots,same,overlapping,discrete} from "../Core/PushRoot.m.js";

export {RootStream,Stream,stream,never,merge,};
export {makeStream,makeNeverStream};

class Stream extends Push {
	constructor(...args) {
		super(...args);
	}
	map(f) {
		let n = makeStream(this._root);
		this._root.addNode((scope)=>{
			scope[n.nodeIdentifier]=f(scope[this.nodeIdentifier]);
		});
		return n;
	}
	forEach(f) {
		this._root.addNode((scope)=>{
			f(scope[this.nodeIdentifier])
		});
		return this;
	}
	filter(f) {
		let n = makeStream(partialRoot(this._root),this.nodeIdentifier);
		this._root.addNode(scope=>{
			if (f(scope[this.nodeIdentifier])) 
				n._root.sendScope(scope);
		});
		return n;
	}
	scan(initial,f=(previous,func)=>func(previous)) {
		let last = initial;
		return this.map(v=>last=f(last,v));
	}
	merge(...streams) {
		merge(this,...streams);
	}
}
class RootStream extends Stream {
	constructor() {
		let nodeId = Symbol();
		super(newRoot(nodeId),nodeId);
	}
	send(value) {
		this._root.send(value);
	}
}
class NeverStream extends Stream {
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

function stream() {
	return new RootStream();
}
function never() {
	return makeNeverStream();
}
Stream.stream = stream;
Stream.never = never;

function makeStream(root=null,nnid=null) {
	if (nnid==null)
		nnid = Symbol();
	if (root==null)
		root = newRoot(nnid);
	return new Stream(root,nnid);
}
function makeNeverStream(root=null,nnid=null) {
	if (nnid==null)
		nnid = Symbol();
	if (root==null)
		root = newDeadRoot();
	return new NeverStream(root,nnid);
}

function merge(...streams) {
	console.assert(compareRoots(...streams.map(s=>s._root)) == discrete);
	let nnid=Symbol();
	return new Stream(joinRootsMap(nnid,streams.map(s=>[s._root,s.nodeIdentifier])),nnid);
}

