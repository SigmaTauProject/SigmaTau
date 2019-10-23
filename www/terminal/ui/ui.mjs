
import calcLayout from "./calcLayout.mjs";


export default class UI {
	constructor(ship, /*input*/) {
		this.ship	= ship	;
		this.getComponent	= this.ship.getComponent.bind(this.ship);
		////this.input	= input	;
		
		this.uiElements	= [];
		this.el = div("div","ui","full");
		document.body.appendChild(this.el);
		////{
		////	let radar = new UI_MetaRadar("0",this.ship);
		////	this.uiElements	.push(	radar	);
		////	radar.getEl().classList.add("ui-full");
		////	document.body.appendChild(radar.getEl());
		////	
		////	////let button = new UI_Button("1",this.ship, "Click ME!");
		////	////this.uiElements	.push(	button	);
		////	////button.getEl().classList.add("ui-bottomLeft");
		////	////document.body.appendChild(button.getEl());
		////	
		////	let slider = new UI_Slider("1",this.ship,0,1,0.01);
		////	this.uiElements	.push(	slider	);
		////	slider.getEl().classList.add("ui-bottomLeft");
		////	document.body.appendChild(slider.getEl());
		////	slider.addEventListener("input",(e)=>{
		////		ship.setSpeed(e.target.value)
		////	});
		////}
	}
	init() {
		this.create();
	}
	update() {
		for (var uiEl of this.uiElements) {
			uiEl.update();
		}
	}
	create() {
		let rd = calcLayout(
			`
				styles = \`
					.bl {
						position	: fixed	;
						bottom	: 0	;
						left	: 0	;
						display	: inline-flex	;
					}
					.bl > * + * {
						margin-;left	: 5px	;
					}
				\`;
				body = [
					div( "div", "full", "stacked",
						metaRadar(),
						div("div","bl",
							////Div.style(\`
							////	top	: 0	;
							////	left	: 0	;
							////	display	: flex	;
							////	flex-direction	: row	;
							////\`),
							slider()
								.attach("metaMove:0.forward")	
								.max(0.5)	
								.min(-0.5)	
								.step(0.01)	,
							slider()
								.attach("metaMove:0.forward")	
								.max(1)	
								.min(-1)	
								.step(0.01)	,
							div("div",button("hi!"),),
						)
					),
				];
			`,
			this.getComponent,
			"0"
		)
		let frame = div("iframe");
		this.el.appendChild(frame);
		setTimeout((()=>{
			frame.contentDocument.head.appendChild(div("link",Div.attributes({rel:"stylesheet",type:"text/css",href:"styles/ui.css"})));
			frame.contentDocument.head.appendChild(div("link",Div.attributes({rel:"stylesheet",type:"text/css",href:"styles/slider.css"})));
			for (let style of rd.styles) {
				frame.contentDocument.head.appendChild(div("style",Div.child(style)));
			}
			for (let el of rd.domEls) {
				frame.contentDocument.body.appendChild(el);
			}
			this.uiElements.push(...rd.uiEls);
		}).bind(this),0);
	}
}





