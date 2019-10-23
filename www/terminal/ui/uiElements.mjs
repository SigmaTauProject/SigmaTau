
class Base {
	constructor(getComponent,id) {
		this.getComponent	= getComponent	;
		this.id	= id	;
	}
	update() {
		throw new Error("not implemented");
	}
	
	getComponentValue(value) {
		let type, num, attribute;
		[type,num]	= value.split(":");
		[num,attribute]	= num.split(".");
		return this.getComponent(type, Number(num))[attribute];
	}
}

export class MetaRadar extends Base {
	constructor(...args) {
		super(...args);
				
		this.view	= null	;
		this.el	= svg (	"svg", "radar",
				"radar-circle",
				Div.attributes({viewBox:"-1 -1 2 2"}),
				svg (	"defs",
					svg (	"clipPath", Div.id("clipCircle"),
						svg (	"circle",
							Div.attributes({cx:"0",cy:"0",r:"1",}),
						),
					),
				),
				svg (	"g",
					Div.attributes({"clip-path":"url(#clipCircle)"}),
					svg (	"rect",
						"radar-background",
						Div.attributes({x:"-1",y:"-1",width:"2",height:"2",}),
					),
					svg (	"g",
						"radar-view",
						(el)=>{this.view=el},
						Div.attributes({transform:"scale(0.05)"}),
					),
				),
				svg (	"circle",
					Div.attributes({cx:"0",cy:"0",r:"1",fill:"none",stroke:"black","stroke-width":"0.01",}),
				),
			);
	}
	getEl() {
		return this.el;
	}
	update() {
		let metaRadar = this.getComponent("metaRadar",0);
		if (metaRadar) {
			let pos = vec();//// metaRadar.us.pos;
			////for (var removed of metaRadar.removed) {
			////	this.view.removeChild(removed.ui_radar[this.id].el);
			////}
			for (var entity of Object.values(metaRadar.entities)) {
				renderShip.bind(this)(entity);
			}
			////renderShip.bind(this)(metaRadar.us);
			function renderShip(ship) {
				if (!ship.ui_radar) ship.ui_radar = {};
				if (!ship.ui_radar[this.id]) ship.ui_radar[this.id] = {};
				if (!ship.ui_radar[this.id].el) {
					ship.ui_radar[this.id] = {};
					ship.ui_radar[this.id].el = svg("polygon", "entity", Div.attributes({points:"-0.5,0.5 0,-0.5 0.5,0.5 0,0.25"}));
					this.view.appendChild(ship.ui_radar[this.id].el);
				}
				ship.ui_radar[this.id].el.setAttribute("transform",`translate(${ship.pos[0]-pos[0]},${-(ship.pos[1]-pos[1])}) rotate(${Math.degrees(ship.ori)})`);
			}
		}
	}
}


export class Button extends Base {
	constructor(text, ...args) {
		super(...args);
		
		this.el	= div (	"button", "button",
				Div.text(text),
			);
	}
	
	
	
	getEl() {
		return this.el;
	}
	update() {
	}
}

export class Slider extends Base {
	constructor(...args) {
		super(...args);
		
		this.input	= null	;
		this.el	= div (	"div", "verticalSlider-outer",
				div (	"input", "slider","verticalSlider",
					(el)=>{this.input=el},
					Div.attributes({type:"range"}),
				),
			);
	}
	
	
	attach(to) {
		var attached = this.getComponentValue(to);
		attached.listen((value)=>{
			this.input.value = value;
		});
		this.input.addEventListener("input",(e)=>{
			attached.value = Number(e.target.value);
		});
		return this;
	}
	min(v){
		this.input.setAttribute("min",v);
		return this;
	}
	max(v){
		this.input.setAttribute("max",v);
		return this;
	}
	step(v){
		this.input.setAttribute("step",v);
		return this;
	}
	
	
	getEl() {
		return this.el;
	}
	update() {
	}
}




















































