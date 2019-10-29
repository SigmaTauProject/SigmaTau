
export function divNS(ns, type, ...args) {
	var el =document.createElement(type);
	var refFunctions = [];
	handleArgs(args);
	for(let refFunction of refFunctions) {
		refFunction(el);
	}
	function handleArgs(args) {
		for(let arg of args) {
			if (arg == null) {
				continue;
			}
			else if (typeof arg=="string") {
				el.classList.add(arg);
			}
			else if (typeof arg=="function") {
				refFunctions.push(arg);
			}
			else if (arg instanceof Array) {
				handleArgs(arg);
			}
			else if (arg instanceof HTMLElement) {
				el.appendChild(arg);
			}
			else if ((typeof arg.nodeType != "undefined") && arg.nodeType==Node.TEXT_NODE) {
				el.appendChild(arg);
			}
			else if (typeof arg._eventListener != "undefined") {
				el.addEventListener(arg._eventListener.event, arg._eventListener.callback);
			}
			else if (typeof arg._eventListeners != "undefined") {
				for (let ev of Object.keys(arg._eventListeners)) {
					el.addEventListener(ev, arg._eventListeners[ev]);
				}
			}
			else if (typeof arg._innerHTML != "undefined") {
				el.innerHTML += arg._innerHTML;
			}
			else if (typeof arg.el != "undefined") {
				el.appendChild(arg.el);
			}
			else if (typeof arg.getEl != "undefined") {
				el.appendChild(arg.getEl());
			}
			else if (typeof arg == "object") {
				for (let key of Object.keys(arg)) {
					let value = arg[key];
					if (key=="class") {
						if (value=="string")
							el.className += " " + value;
						else
							value.forEach(c=>el.classList.add(c));
					}
					else {
						el.setAttribute(key, value);
					}
				}
			}
		}
	}
	return el;
}

export function div(type, ...args) {
	return divNS("http://www.w3.org/1999/xhtml", type, ...args);
}
export function svg(type, ...args) {
	return divNS("http://www.w3.org/2000/svg", type, ...args);
}


export const Div = {
	id: function(id) {
		return {id:id};
	},
	
	attribute: function(at,v) {
		return {[at]:v};
	},
	attributes: function(ats) {
		return ats;
	},
	
	eventListener: function(event, callback) {
		return {_eventListener:{event:event, callback:callback}};
	},
	eventListeners: function(eventListeners) {
		return {_eventListeners:eventListeners};
	},
	event: function(event, callback) {
		return {_eventListener:{event:event, callback:callback}};
	},
	events: function(eventListeners) {
		return {_eventListeners:eventListeners};
	},
	on: function(event, callback) {
		return {_eventListener:{event:event, callback:callback}};
	},
	ons: function(eventListeners) {
		return {_eventListeners:eventListeners};
	},
	
	child: function(child) {
		return (typeof child=="string") ? {_innerHTML:child} : child;
	},
	children: function(children) {
		return children.map(child=>(typeof child=="string") ? {_innerHTML:child} : child);
	},
	
	text: function(text) {
		return document.createTextNode(text);
	},
}

