import {RootStream,Stream,stream,never,/*merge,*/} from "./Base/Stream.m.js";
import {} from "./Base/StreamWithCell.m.js";
import {} from "./Base/StreamToCell.m.js";
import {} from "./Promise/StreamPromise.m.js";
import {} from "./Base/StreamScanFilter.m.js";

import	{ RootConstantStream,ConstantStream,constantStream
	, RootEnumStream,EnumStream,enumStream
	, merge
	} from "./Constant/ConstantStream.m.js";
import {} from "./Constant/StreamWithConstant.m.js";
import {} from "./Constant/StreamToConstantCell.m.js";

import {map} from "./Base/Shared.m.js";
import {lift} from "./Base/Lift.m.js";

/* Override default merge which does not have handling for `ConstantStream`s. */
Stream.merge = function (...streams) {
	merge(streams);
}

export	{ RootStream,Stream,stream,never
	, RootConstantStream,ConstantStream,constantStream
	, RootEnumStream,EnumStream,enumStream
	, merge
	, map
	, lift
	}

