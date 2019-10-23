import {send} from "../Push.m.js";
import {kill} from "../Lifetime.m.js";

export function newLifetimePush(push,lifetime) {
	return	{ _push	: push
		, _lifetime	: lifetime
		, _send_callback	: send(push)
		, _kill_callback	: kill(lifetime)
		);
}
export function makeLifetimePush(send_callback,kill_callback) {
	return	{ _send_callback	: send_callback
		, _kill_callback	: kill_callback
		);
}


