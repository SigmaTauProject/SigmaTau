function loadFile(filename){
	return new Promise((resolve,reject)=>{
		var el;
		if (filename.endswith(".js")){ //if filename is a external JavaScript file
			el=document.createElement('script');
			el.setAttribute("type","text/javascript");
			el.setAttribute("src", filename);
		}
		else if (filename.endswith(".css")){ //if filename is an external CSS file
			el=document.createElement("link");
			el.setAttribute("rel", "stylesheet");
			el.setAttribute("type", "text/css");
			el.setAttribute("href", filename);
		}
		else {
			reject();
		}
		el.addEventListener("load",resolve);
		document.head.appendChild(el);
	});
}

function unloadFile(filename){
	var nodeType;// type to create nodelist from
	var targetAttr;// corresponding attribute to test for
	if (filename.endswith(".js")){ //if filename is a external JavaScript file
		nodeType = "script";
		targetAttr = "src";
	}
	else if (filename.endswith(".css")){ //if filename is an external CSS file
		nodeType = "link";
		targetAttr = "href";
	}
	var els=document.head.getElementsByTagName(nodeType);
	for (var i=els.length; i>=0; i--){
		if (els[i] && els[i].getAttribute(targetAttr)!=null && els[i].getAttribute(targetAttr).indexOf(filename)!=-1)
			els[i].parentNode.removeChild(els[i]);
	}
}
