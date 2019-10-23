





export class Thruster {
	constructor () {
		this.value = newListenerPushPull();
	}
}

export class DirFlight
	constructor () {
		this.forward	= newListenerPushPull();
		this.strafe	= newListenerPushPull();
		this.rot	= newListenerPushPull();
	}
}

export class MetaRadar {
	constructor () {
		this.entities = newListenerMap();
	}
}

export class Com {
	constructor () {
		this.entities = newLisenterPush();
	}
}







