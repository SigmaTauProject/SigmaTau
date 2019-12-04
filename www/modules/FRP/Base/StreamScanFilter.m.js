import {Stream,makeStream} from "./Stream.m.js";

Stream.prototype.scanFilter = function(start,f) {
	return this.scan([undefined,start],(x,y)=>[f(x[1],y),y]).filter(v=>v[0]).map(v=>v[1]);
}
