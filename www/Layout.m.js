import {div,Div} from "/modules/div.m.js";
import * as Type from "./ComponentType.m.js";
import * as UI from "./UI.m.js";
import {cache} from "/modules/FRP/Data/Pull.m.js";
import {} from "/modules/FRP/Data/PushMap.m.js";
import {} from "/modules/FRP/Data/PushSet.m.js";
import {pushSort,pushMap,pushFilter} from "/modules/FRP/Control/PushSet.m.js";

export default function makeLayout() {
	////let thrusters = components.filter(c=>c.type==Type.Thruster);
	////let thrusterSliders = thrusters.map(t=>UI.slider(t.value));
	////let sortedThrusterSliders = thrusters.keys().sort().map(id=>thrusterSliders.value(id));
	////return div("div", sortedThrusterSliders);
	
	////let sortedThrusterSlidersPull = newPullArray();
	////let sortedThrusterSlidersPush = sortedThrusterSlidersPull.cache();
	////let thrusterSlidersPush = sortedThrusterSliders.sort(([k1,_0],[k2,_1]=>k1<k2).keyValuePairs();
	////let thrustersPush = thrusterSlidersPush.map(t=>UI.slider(t.value));
	////let componentsPush = thrusterSlidersPush.filter(c=>c.type==Type.Thruster);
	////return [componentsPush, div("div", sortedThrusterSlidersPull)];
	
	let [sortedThrusterSlidersPush,sortedThrusterSlidersPull] = cache();
	let thrusterSlidersPush =	pushSort( ([k1,_0],[k2,_1])=>k1<k2,
		toPushArrayTuple(
		sortedThrusterSlidersPush
		));
	let thrustersPush =	valueMap( t=>UI.slider(t.value),
		thrusterSlidersPush
		);
	let componentsPush =	pushFilter( c=>c.type==Type.Thruster,
		thrusterSlidersPush
		);
	return [componentsPush, div("div", sortedThrusterSlidersPull)];
}



