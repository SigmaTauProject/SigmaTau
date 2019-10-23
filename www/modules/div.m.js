export function div (type) {
	var el =document.createElement(type);
	{
		var refFunctions = [];
	
		for(var i=1;i<arguments.length;i++) {
			if (arguments[i] == null) {
				continue;
			}
			else if (typeof arguments[i]=="string") {
				el.classList.add(arguments[i]);
			}
			else if (typeof arguments[i]=="function") {
				refFunctions.push(arguments[i]);
			}
			else if (arguments[i] instanceof HTMLElement) {
				el.appendChild(arguments[i]);
			}
			else if (arguments[i].nodeType!="undefined" && arguments[i].nodeType==Node.TEXT_NODE) {
				el.appendChild(arguments[i]);
			}
			else if(arguments[i].type=="classes") {
				if (!Array.isArray(arguments[i].classes)) {
						el.classList.add(arguments[i].classes);
				}
				else {
					for (let cls of arguments[i].classes) {
						el.classList.add(cls);
					}
				}
			}
			else if(arguments[i].type=="id") {
				el.id = arguments[i].id;
			}
			else if(arguments[i].type=="eventListener") {
				if (typeof arguments[i].event != "undefined") {
					el.addEventListener(arguments[i].event, arguments[i].callback);
				}
				if (typeof arguments[i].eventListeners != "undefined") {
					if (Array.isArray(arguments[i].eventListeners)) {
						for (let listener of arguments[i].eventListeners) {
							el.addEventListener(...listener);
						}
					}
					else {
						for (let listener of Object.keys(arguments[i].eventListeners)) {
							el.addEventListener(listener, arguments[i].eventListeners[listener]);
						}
					}
				}
			}
			else if(arguments[i].type=="attribute") {
				if (typeof arguments[i].attribute != "undefined") {
					el.setAttribute(arguments[i].attribute, arguments[i].value);
				}
				if (typeof arguments[i].attributes != "undefined") {
					if (Array.isArray(arguments[i].attributes)) {
						for (let attribute of arguments[i].attributes) {
							el.setAttribute(...attribute);
						}
					}
					else {
						for (let attribute of Object.keys(arguments[i].attributes)) {
							el.setAttribute(attribute, arguments[i].attributes[attribute]);
						}
					}
				}
			}
			else if(arguments[i].type=="child") {
				if (typeof arguments[i].child != "undefined") {
					if (typeof arguments[i].child=="string") {
						el.innerHTML += arguments[i].child;
					}
					else {
						el.appendChild(arguments[i].child);
					}
				}
				if (typeof arguments[i].children != "undefined") {
					for (let child of arguments[i].children) {
						if (typeof child=="string") {
							el.innerHTML += child;
						}
						else {
							el.appendChild(child);
						}
					}
				}
			}
		}
	
		for(let refFunction of refFunctions) {
			refFunction(el);
		}
	}
	return el;
}


export const Div = {
	cls: function(classes) {
		return {type:"classes", classes:classes};
	},
	classes: function(classes) {
		return {type:"classes", classes:classes};
	},
	classList: function(classes) {
		return {type:"classes", classes:classes};
	},
	
	id: function(id) {
		return {type:"id", id:id};
	},
	
	
	eventListener: function(event, callback) {
		return {type:"eventListener", event:event, callback:callback};
	},
	eventListeners: function(eventListeners) {
		return {type:"eventListener", eventListeners:eventListeners};
	},
	event: function(event, callback) {
		return {type:"eventListener", event:event, callback:callback};
	},
	events: function(eventListeners) {
		return {type:"eventListener", eventListeners:eventListeners};
	},
	
	
	attribute: function(attribute, value) {
		return {type:"attribute", attribute:attribute, value:value};
	},
	attributes: function(attributes) {	
		return {type:"attribute", attributes:attributes};
	},
	
	
	child: function(child) {
		return {type:"child", child:child};
	},
	children: function(children) {
		return {type:"child", children:children};
	},
	
	text: function(text) {
		return document.createTextNode(text);
	},
}

