
window.divNs = function (ns, type){
	var el =document.createElementNS(ns, type);
	{
		var refFunctions = [];
	
		for(var i=2;i<arguments.length;i++) {
			if (arguments[i] == null) {
				continue;
			}
			else if (typeof arguments[i]=="string") {
				el.classList.add(arguments[i]);
			}
			else if (typeof arguments[i]=="function") {
				refFunctions.push(arguments[i]);
			}
			else if (arguments[i] instanceof HTMLElement || arguments[i] instanceof SVGElement) {
				el.appendChild(arguments[i]);
			}
			else if (arguments[i].getEl) {
				el.appendChild(arguments[i].getEl());
			}
			else if (arguments[i].nodeType!="undefined" && arguments[i].nodeType==Node.TEXT_NODE) {
				el.appendChild(arguments[i]);
			}
			else if(arguments[i].type=="classes") {
				if (!Array.isArray(arguments[i].classes)) {
						el.classList.add(arguments[i].classes);
				}
				else {
					for (cls of arguments[i].classes) {
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
						for (listener of arguments[i].eventListeners) {
							el.addEventListener(...listener);
						}
					}
					else {
						for (listener of Object.keys(arguments[i].eventListeners)) {
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
						for (attribute of arguments[i].attributes) {
							el.setAttribute(...attribute);
						}
					}
					else {
						for (attribute of Object.keys(arguments[i].attributes)) {
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
					else if (arguments[i].child.getEl) {
						el.appendChild(arguments[i].child.getEl());
					}
					else {
						el.appendChild(arguments[i].child);
					}
				}
				if (typeof arguments[i].children != "undefined") {
					for (child of attributes[i].children) {
						if (typeof child=="string") {
							el.innerHTML += child;
						}
						else if (child.getEl) {
							el.appendChild(child.getEl());
						}
						else {
							el.appendChild(child);
						}
					}
				}
			}
		}
	
		for(refFunction of refFunctions) {
			refFunction(el);
		}
	}
	return el;
}

window.div = function() {
	return window.divNs("http://www.w3.org/1999/xhtml", ...arguments);
}


window.Div = {
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
		if (typeof callback == "undefined" || callback == null) {
			return {type:"eventListener", eventListeners:event};
		}
		else {
			return {type:"eventListener", event:event, callback:callback};
		}
	},
	eventListeners	: function(event, callback) {	return window.Div.eventListener(event, callback);	},
	event	: function(event, callback) {	return window.Div.eventListener(event, callback);	},
	events	: function(event, callback) {	return window.Div.eventListener(event, callback);	},
	listener	: function(event, callback) {	return window.Div.eventListener(event, callback);	},
	listeners	: function(event, callback) {	return window.Div.eventListener(event, callback);	},
	on	: function(event, callback) {	return window.Div.eventListener(event, callback);	},
	
	
	
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