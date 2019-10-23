
import * as __ui_el_ from "./uiElements.mjs";


export default function __calcLayout(layoutScript, getComponent, id) {
	return new Function(
		"__getComponent",
		"__idHead",
		"__ui_el_",
		
		`
		var __lastId	= 0;
		var __uiEls	= [];
		${(()=>{
			var createElDefs = [];
			for (let uiElType of Object.keys(__ui_el_)) {
				let funName = uiElType[0].toLowerCase()+uiElType.slice(1);
				createElDefs.push(`
					function ${funName}(...args) {
						var uiEl = new __ui_el_["${uiElType}"](...args,__getComponent,__idHead+(__lastId++));
						__uiEls.push(uiEl);
						return uiEl;
					}
				`);
			}
			return createElDefs.join("\n");
		})()}
		
		var body	= [];
		var styles	= [];
		
		${layoutScript}
		
		
		if (typeof styles == "string") {
			styles = [styles];
		}
		if (body instanceof Element) {
			return {uiEls:__uiEls,domEls:[body],styles:styles};
		}
		else if (body instanceof Array) {
			for (var i=0; i<body.length; i++) {
				if (!(body[i] instanceof Element)) {
					if (body[i].getEl) {
						body[i] = body[i].getEl();
					}
					else {
						throw new Error("Layout is an Array but the "+i+" index is not a DOM Elemement: Layout must be an Element or an Array of Elements");
					}
				}
			}
			return {uiEls:__uiEls,domEls:body,styles:styles};
		}
		else {
			throw new Error("Layout is not a DOM Elment or an Array: Layout must be an Element or an Array of Elements");
		}
		`
	)(getComponent, String(id),__ui_el_);
}




////function __createUIEl(type, ...args) {
////	var uiEl = __uiEl.metaRadar(...args,__getComponent,__idHead+(__lastId++));
////	__uiEls.push(uiEl);
////	return uiEl.getEl();
////}
////function metaRadar(...args) {
////	var uiEl = __uiEl.metaRadar(...args,__getComponent,__idHead+(__lastId++));
////	__uiEls.push(uiEl);
////	return uiEl.getEl();
////}
////function button(...args) {
////	return __uiEl.button(...args,__getComponent,__idHead+(__lastId++));
////}
////function metaRadar(...args) {
////	return __uiEl.metaRadar(...args,__getComponent,__idHead+(__lastId++));
////}


