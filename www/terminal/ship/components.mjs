
import IdArray from "../loose/idArray.mjs";

class BaseComponent {
	constructor(type, send, id) {
		this.type	= type	;
		this.send	= send	;
		this.id	= id	;
	}
}

export class MetaRadar extends BaseComponent {
	constructor(...args) {
		super("metaRadar", ...args);
		this.us	= {pos:vec(),rot:0}	;
		this.entities	= {}	;
		this.removedThisTick	= []	;
		this.removed	= []	;
		
		this.send("stream",{});
	}
	msg_add(msg) {
		log("add",msg);
		let entity = {pos:msg.pos,ori:msg.ori};
		this.entities[msg.id] = entity;
	}
	msg_update(msg) {
		log("update",msg);
		let entity	= this.entities[msg.id]	;
		entity.pos	= msg.pos	;
		entity.ori	= msg.ori	;
	}
	msg_remove(msg) {
		log("remove",msg);
		this.removedThisTick.push(this.entities[msg.id]);
		delete this.entities[msg.id];
	}
	update() {
		this.removed = this.removedThisTick;
		this.removedThisTick = [];
	}
}
export class MetaMove extends BaseComponent {
	constructor(...args) {
		super("metaMove", ...args);
		this._forward	= 0	;
		this._strafe	= 0	;
		this.forwardChanged	= false	;
		this.strafeChanged	= false	;
		this.forwardListeners	= []	;
		this.strafeListeners	= []	;
		
		this.forward = {
			_this:this,
			get value(){
				return this._this._forward;
			},
			set value(value){
				if (value>1) {
					value = 1;
				}
				else if (value<-1) {
					value = -1;
				}
				this._this._forward	= value;
				this._this.forwardChanged	= true;
				for (let callback of this._this.forwardListeners) {
					callback(value);
				}
			},
			listen: (callback)=>{
				this.forwardListeners.push(callback);
			},
		}
		this.strafe = {
			_this:this,
			get value(){
				return this._this._strafe;
			},
			set value(value){
				if (value>1) {
					value = 1;
				}
				else if (value<-1) {
					value = -1;
				}
				this._this._strafe	= value;
				this._this.strafeChanged	= true;
				for (let callback of this._this.strafeListeners) {
					callback(value);
				}
			},
			listen: (callback)=>{
				this.strafeListeners.push(callback);
			},
		}
	}
	
	msg_power(msg) {
		this.power	= msg.power;
	}
	
	update(){
		if (this.forwardChanged) {
			this.send("set",{axis:0,value:this.forward.value});
			this.forwardChanged	= false	;
		}
		if (this.strafeChanged) {
			this.send("set",{axis:1,value:this.strafe.value});
			this.strafeChanged	= false	;
		}
	}
}
////Object .defineProperties(MetaMove.prototype, {
////	forward:{
////		get : ()=>{
////			return this._forward;
////		}
////		set : (value)=>{
////			if (value>1) {
////				value = 1;
////			}
////			else if (value<-1) {
////				value = -1;
////			}
////			this._forward	= value;
////			this.forwardChanged	= true;
////		}
////		changed : false;
////	}
////	strafe:{
////		get : ()=>{
////			return this._strafe;
////		}
////		set : (value)=>{
////			if (value>1) {
////				value = 1;
////			}
////			else if (value<-1) {
////				value = -1;
////			}
////			this._strafe	= value;
////			this.strafeChanged	= true;
////		}
////	}
////});
export class UnknownComponent extends BaseComponent {
	constructor(ship, ...args) {
		super("unknownComponent", ...args);
	}
}