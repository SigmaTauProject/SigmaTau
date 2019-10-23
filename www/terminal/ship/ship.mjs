
import IdArray from "../loose/idArray.mjs";
import {decodeNetMsg,encodeNetMsg} from "../loose/netMsg.mjs";
import {MetaRadar, MetaMove, UnknownComponent} from "./components.mjs";


export default class Ship {
	constructor(network) {
		this.network	= network	;
		this.componentTypes	= []	;
		this.components	= []	;
		
		this.fullyConnectedCallbacks	= []	;
		this.fullyConnected	= false	;
	}
	
	init() {
		////{
		////	let res = encodeNetMsg({testByte:246,testArray:[1,6,7]}, [255,0],upMsgStructure.ship.test);
		////	log(res);
		////	this.socket.send(res);
		////}
		////{
		////	let res = encodeNetMsg({}, [0,0],upMsgStructure.metaRadar.read);
		////	log(res);
		////	this.socket.send(res);
		////}
		////{
		////	let res = encodeNetMsg({}, [0,1],upMsgStructure.metaRadar.stream);
		////	log(res);
		////	this.socket.send(res);
		////}
	}
	update() {
		for (let msgData of this.network) {
			if (msgData[1] == 255) {
				var msgType = downMsgType.ship.findKey(msgData[2]);
				let msg = decodeNetMsg(msgData, [255,0],downMsgStructure.ship[msgType]);
				this["msg_"+msgType](msg);
				console.log(msg);
			}
			else {
				let component	= msgData[1];
				let componentType	= this.componentTypes[component];
				let msgType	= downMsgType[this.componentTypes[msgData[1]]] .findKey(msgData[2]);
				let msg = decodeNetMsg	(	msgData	,
						[component,msgData[2]],
						downMsgStructure	[	componentType	]
							[	msgType	]
					);
				this.getComponent(component)["msg_"+msgType](msg);
				////this.ship.msg_components(msg);
			}
			////if (msg.component==-1) {
			////	this.ship["msg_"+msg.type](msg);
			////}
			////else {
			////	try {
			////		this.ship.components[msg.component]["msg_"+msg.type](msg);
			////	}
			////	catch (e) {
			////		console.log(msg);
			////		throw e;
			////	}
			////}
		}
		for (let component of this.components) {
			if (component!=null) {
				component.update();
			}
		}
	}
	
	onFullyConnected(callback) {
		this.fullyConnectedCallbacks.push(callback);
	}
	
	msg_components(msg) {
		this.componentTypes = msg.components.map(typeId=>componentType.findKey(typeId));
		this.components = Array(this.components.length).fill(null);
		this.fullyConnected = true;
		for (let callback of this.fullyConnectedCallbacks) {
			callback();
		}
	}
	
	getComponent(component, which) {
		if (typeof component == "string") {
			if (typeof which == "undefined") {
				return	this.componentTypes
					.map((type,i)=>i)	
					.filter(i=>this.componentTypes[i]==component)	
					.map(i=>this.getComponent(i))	;
			}
			else {
				return	this.getComponent(	this.componentTypes
						.map((type,i)=>i)	
						.filter(i=>this.componentTypes[i]==component)	
						[which]	);
			}
		}
		else {
			if (this.components[component] == null) {
				this.components[component] = this.createComponent(this.componentTypes[component], component);
			}
			return this.components[component];
		}
	}
	createComponent(type,id) {
		switch (type) {
			case "metaRadar":
				return new MetaRadar(((type,msg)=>{this.send(id,type,msg)}).bind(this), id);
			case "metaMove":
				return new MetaMove(((type,msg)=>{this.send(id,type,msg)}).bind(this), id);
			default:
				return null;
		}
	}
	
	send(component, type, msg) {
		let compType = this.componentTypes[component];
		this.network.send(encodeNetMsg(msg,[component,upMsgType[compType][type]], upMsgStructure[compType][type]));
	}
	////addEntity(args) {
	////	if (!args) args={};
	////	
	////	var entity = {};
	////	if (args.entity)
	////		entity = args.entity;
	////	
	////	if (!args.pos	) {	args.pos	= vec()	;	}
	////	if (!args.rot	) {	args.rot	= 0	;	}
	////	entity.pos = args.pos;
	////	entity.rot = args.rot;
	////	
	////	if (args.player	) {	this.player	= entity	;	}
	////	
	////	this.ui	.onAddEntity(entity);
	////	this.entities.push(entity);
	////	
	////	return entity;
	////}
}


////class ShipView {
////	constructor(ship) {
////		this.ship	= ship	;
////		this.ships	= []	;
////		this.us	= {pos:vec(),rot:0}	;
////		this.type	= "shipView"	;
////	}
////	msg_setUs(msg) {
////		var ship = {};
////		this.us.pos	= msg.pos	;
////		this.us.rot	= msg.rot	;
////	}
////	msg_addShip(msg) {
////		var ship = {};
////		ship.pos	= msg.pos	;
////		ship.rot	= msg.rot	;
////		this.ships.push(ship);
////	}
////}
////class Thruster {
////	constructor(ship) {
////		this.ship	= ship	;
////		this.state	= 0	;
////	}
////	msg_setState(msg) {
////		this.state = msg.state;
////	}
////}

var componentType = {
	"metaRadar"	: 0	,
	"metaMove"	: 1	,
}


var downMsgType = {
	ship: {
		components	: 0	,
	},
	metaRadar:{
		add	: 0	,
		update	: 1	,
		remove	: 2	,
		moveAll	: 3	,
	},
	metaMove:{
		update	: 0	,
	},
}
var upMsgType = {
	ship:{
		test	: 0	,
	},
	metaRadar:{
		read	: 0	,
		stream	: 1	,
	},
	metaMove:{
		read	: 0	,
		stream	: 1	,
		"set"	: 2	,
	},
}



var downMsgStructure = {
	ship:{
		components: {
			type:"object",
			values:[
				{name:"components",type:"array",length:"dynamic",content:{type:"ubyte"},},
			],
		}
	},
	metaRadar:{
		add: {
			type:"object",
			values:[
				{	name	: "id"	,
					type	: "ushort"	,
				},
				{	name	: "pos"	,
					type	: "array"	,
					length	: 2	,
					content	: {type:"float"}	,
				},
				{	name	: "ori"	,
					type	: "float"	,
				},
			],
		},
		update: {
			type:"object",
			values:[
				{	name	: "id"	,
					type	: "ushort"	,
				},
				{	name	: "pos"	,
					type	: "array"	,
					length	: 2	,
					content	: {type:"float"}	,
				},
				{	name	: "ori"	,
					type	: "float"	,
				},
			],
		},
		remove: {
			type:"object",
			values:[
				{	name	: "id"	,
					type	: "ushort"	,
				},
			],
		},
		moveAll: {
			type:"object",
			values:[
				{	name	: "pos"	,
					type	: "array"	,
					length	: 2	,
					content	: {type:"float"}	,
				},
				{	name	: "ori"	,
					type	: "float"	,
				},
			],
		},
	},
	metaMove:{
		update: {
			type:"object",
			values:[
				{	name	: "axis"	,
					type	: "ubyte"	,
				},
				{	name	: "value"	,
					type	: "float"	,
				},
			],
		},
	},
}

var upMsgStructure = {
	ship:{
		test: {
			type:"object",
			values:[
				{	name	: "testByte"	,
					type	: "ubyte"	,
				},
				{	name	: "testArray"	,
					type	: "array"	,
					length	: "dynamic"	,
					content	: {type:"ubyte"}	,
				},
			],
		},
	},
	metaRadar:{
		read: {
			type:"object",
			values:[],
		},
		stream: {
			type:"object",
			values:[],
		},
	},
	metaMove:{
		read: {
			type:"object",
			values:[],
		},
		stream: {
			type:"object",
			values:[],
		},
		"set": {
			type:"object",
			values:[
				{	name	: "axis"	,
					type	: "ubyte"	,
				},
				{	name	: "value"	,
					type	: "float"	,
				},
			],
		},
	},
}



































