
import UI	from "./ui/ui.mjs"	;
import Ship	from "./ship/ship.mjs"	;
import Socket	from "./ship/socket.mjs"	;
////import Network	from "./ship/network.mjs"	;


export default class Main {
	constructor() {
		this.socket	= new Socket	(	);
		this.ship	= new Ship	(this.socket	);
	////	////this.gameLogic	= new GameLogic	(this.world	);
	////	////this.input	= new Input	(this.gameLogic	);
		this.ui	= new UI	(this.ship, /*this.gameLogic, this.input*/	);
		////this.network	= new Network	(this.ship, this.socket	);
		
		this.socket.onConnected((()=>{
			this.ship.init();
			this.ship.onFullyConnected((()=>{
				this.ui.init();
				////	this.ui.	init();
				////	this.network	.init();
				////	/*this.gameLogic	.init();*/
				////	/*this.input	.init();*/
				////	
				////	////setTimeout(this.update.bind(this), 1000);			
			}).bind(this));
			setInterval(this.update.bind(this), 50);
		}).bind(this));
	}
	update() {
	////	////this.ui	.updateInput();
	////	////this.input	.update();
	////	////this.gameLogic	.update();
		this.ship	.update();
		if (this.ship.fullyConnected) {
			this.ui	.update();
		}
	}
}


