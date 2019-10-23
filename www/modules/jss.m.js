import validateConfig from "/modules/config.m.js";
import {div,Div} from "/modules/div.m.js";

export default function jss(css, end="dom", extra={}) {
	if (end.toLowerCase()=="dom") {
		extra.addToDOM=true;
		return jssEl(css,extra);
	}
	else if (end.toLowerCase()=="el")
		return jssEl(css,extra);
	else if (end.toLowerCase()=="array")
		return jssArray(css,extra);
	else if (end.toLowerCase()=="string")
		return jssString(css,extra);
	else console.assert(false);
}

export function jssArray(css,extra={}) {
	return css.keys().map(b=>createStyleBlock(b,css[b]));
}
export function jssString(css,extra={}) {
	if (typeof css == "object" && !(css.constructor == Array))
		css = jssArray(css,extra);
	return css.join("\n\n");
}
export function jssEl(css,extra={}) {
	if (typeof css != "string")
		css = jssString(css,extra);
	extra = validateConfig({el:null,replace:true,addToDOM:false,id:null}, extra);
	
	let el;
	if (extra.el!=null) {
		el = extra.el;
	}
	else {
		if (extra.id!=null) {
			el = document.getElementById(extra.id);
			if (el==null)
				el = div("style",Div.id(extra.id));
			else
				extra.addToDOM=false;
			if (extra.addToDOM)
				document.getElementsByTagName('head')[0].appendChild(style);
		}
		else {
			el = div("style");
		}
	}
	
	if (extra.replace)
		el.innerHTML = css;
	else
		el.appendChild(document.createTextNode(css));
	return el;
}

function createStyleBlock(selector, rules) {
	return selector + ' {\n' + parseRules(rules) + '\n}'
}

function parseRules(rules) {
	return rules.keys().map(r=>'\t'+r+': '+rules[r]+';').join('\n');
}



