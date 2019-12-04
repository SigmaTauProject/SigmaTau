
export {newRoot,newDeadRoot,joinRoots,joinRootsMap,partialRoot,compareRoots,same,overlapping,discrete};

class Root {
	constructor(nodeIdentifier) {
		this.nodes = [];
		this.nodeIdentifier = nodeIdentifier;
	}
	addNode(callback) {
		this.nodes.push(callback);
	}
	unsafeRemoveNode(callback) {
		this.nodes.remove(callback);
	}
	send(v) {
		let scope = {[this.nodeIdentifier]:v};
		this.sendScope(scope);
	}
	sendScope(scope) {
		this.nodes.forEach(n=>n(scope));
	}
}
class JoinedRoot {
	constructor(roots,nodeIdentifier) {
		this.nodeIdentifier = nodeIdentifier;
		this.roots = roots;
	}
	addNode(callback) {
		this.roots.forEach(r=>r.addNode(callback));
	}
	unsafeRemoveNode(callback) {
		this.roots.forEach(r=>r.unsafeRemoveNode(callback));
	}
	send() {
		console.assert(false);
	}
	sendScope(scope) {
		this.roots.forEach(r=>r.sendScope(scope));
	}
}
class PartialRoot extends Root {
	constructor(root) {
		super(root.nodeIdentifier);
		this.root = root;
	}
	send() {
		console.assert(false);
	}
}
class DeadRoot {
	addNode(callback) {}
}

function newRoot(nodeIdentifier) {
	return new Root(nodeIdentifier);
}
function newDeadRoot() {
	return new DeadRoot();
}
function joinRoots(nodeIdentifier,roots) {
	roots = new Set(roots);
	roots = roots.concatMap(r=>r.constructor == JoinedRoot?r.roots:[r]);
	if (roots.size>1)
		return new JoinedRoot(roots);
	else
		return [...roots][0];
}
function joinRootsMap(nodeIdentifier,rootIdentifierPairs) {
	rootIdentifierPairs = new Set(rootIdentifierPairs);
	let roots = rootIdentifierPairs.concatMap(([r,id])=>r.constructor == JoinedRoot?r.roots.map(ir=>mapRoot(ir,id,nodeIdentifier)):[mapRoot(r,id,nodeIdentifier)]);
	if (roots.size>1)
		return new JoinedRoot(roots,nodeIdentifier);
	else
		return [...roots][0];
}
function partialRoot(root) {
	if (root.constructor == Root || root.constructor == DeadRoot)
		return new PartialRoot(root);
	else if (root.constructor == JoinedRoot)
		return new JoinedRoot(root.roots.map(partialRoot));
	else// if (root.constructor == PartialRoot)
		return new PartialRoot(root.root);
}

let same = Symbol();
let overlapping = Symbol();
let discrete = Symbol();

function compareRoots(...roots) {
	console.assert(roots.length>=2,"Cannot compare less than 2 roots?!");
	roots = roots.map(r=>(r.constructor==JoinedRoot)?r.roots:new Set([r]));
	
	if (intersectionAny(...roots).size == 0)
		return discrete;
	else if (roots.all(r=>r.size==intersection(...roots).size))
		return same;
	else
		return overlapping;
}


function mapRoot(root,fromID,toID) {
	root.addNode(scope=>scope[toID]=scope[fromID]);
	return root;
}

