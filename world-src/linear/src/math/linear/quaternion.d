module math.linear.quaternion;

import std.math;

import math.linear.vector;
import math.linear.axis_rot;

public import math.linear._qv;
public import math.linear._qa;
alias opBinaryImpl = math.linear._qv.opBinaryImpl;

// TODO: Add tests to ensure T is a compotable type (number, etc...).
struct Quat(T) {
	union {
		T[4] data;
		struct {
			T angle;
			T[3] axis;
		}
		struct {
			T w;
			T x;
			T y;
			T z;
		}
	}
	
	this(T angle, T[3] axis ...) {
		this.angle = angle;
		this.axis = axis;
	}
	this(T[4] data) {
		this.data = data;
	}
	this(T[3] axis, T angle) {
		this.angle = angle;
		this.axis = axis;
	}
	this(typeof(this) v) {
		this.data = v.data;
	}
	
	this(AxisRot!T data) {
		if (data.angle==0)
			this.data = [1,0,0,0];
		this.w = cos(data.angle/2);
		this.axis[] = data.axis[] * sin(data.angle/2);
	}
	
	auto opBinary(string op, T)(T b) {////if (__traits(compiles, opBinaryImpl!op(this, b))){
		return opBinaryImpl!op(this, b);
	}
	auto opBinaryRight(string op, T)(T a) if (__traits(compiles, opBinaryImpl!op(a, this))){
		return opBinaryImpl!op(a,this);
	}
	auto opOpAssign(string op, T)(T b) if (__traits(compiles, opOpAssignImpl!op(this, b))){
		return opOpAssignImpl!op(this, b);
	}
}
auto quat(T)(T[4] data) {
	return Quat!T(data);
}
auto quat(T)(T angle, T[3] axis ...) {
	return Quat!T(angle, axis);
}
auto quat(T)(T[3] axis, T angle) {
	return Quat!T(angle, axis);
}
auto quat(T)(Quat!T data) {
	return Quat!T(data);
}

auto quat(T)(AxisRot!T data) {
	return Quat!T(data);
}

Quat!T fromAxisRot(T)(AxisRot!T a) {
	return Quat!T(a);
}

T magnitudeSquared(T)(Quat!T t) {
	return t.w^^2 + t.x^^2 + t.y^^2 + t.z^^2;
}

T magnitude(T)(Quat!T t) {
	return sqrt(t.magnitudeSquared);
}
void normalize(T)(Quat!T t) {
	t.data[] /= t.magnitude;
}
Quat!T normalized(T)(Quat!T t) {
	Quat!T n;
	n.data[] = t.data[] / t.magnitude;
	return n;
}


Quat!T identity(T)() {
	return quat!T(1,[0,0,0]);
}


void invert(T)(Quat!T t) {
	t.x = -t.x;
	t.y = -t.y;
	t.z = -t.z;
}
alias conjugate = invert;

Quat!T inverse(T)(Quat!T t) {
	return Quat!T(t.w, [-t.x, -t.y, -t.z]);
}
alias conjugated = inverse;



auto opBinaryImpl(string op:"*", T,U)(Quat!T a, Quat!U b) 
if	( __traits(compiles, typeof(mixin("a.data[0]*b.data[0]")))
	&& __traits(compiles, typeof(mixin("a.data[0]+b.data[0]")))
	&& __traits(compiles, typeof(mixin("a.data[0]-b.data[0]")))
	&& __traits(compiles, typeof(mixin("-a.data[0]")))
	&& __traits(compiles, typeof(mixin("-b.data[0]")))
	)
{
	return quat	(	-	a.x * b.x	-	a.y * b.y	-	a.z * b.z	+	a.w * b.w
		,		a.x * b.w	+	a.y * b.z	-	a.z * b.y	+	a.w * b.x
		,	-	a.x * b.z	+	a.y * b.w	+	a.z * b.x	+	a.w * b.y
		,		a.x * b.y	-	a.y * b.x	+	a.z * b.w	+	a.w * b.z
		);
}

 




auto opOpAssignImpl(string op:"*", size_t size,T,U)(ref Quat!T a, Quat!T b) 
if	( __traits(compiles, typeof(mixin("a.data[0]*=b.data[0]")))
	&& __traits(compiles, typeof(mixin("a.data[0]+b.data[0]")))
	&& __traits(compiles, typeof(mixin("a.data[0]-b.data[0]")))
	&& __traits(compiles, typeof(mixin("-a.data[0]")))
	&& __traits(compiles, typeof(mixin("-b.data[0]")))
	)
{
	a.w =	-	a.x * b.x	-	a.y * b.y	-	a.z * b.z	+	a.w * b.w	;
	a.x =		a.x * b.w	+	a.y * b.z	-	a.z * b.y	+	a.w * b.x	;
	a.y =	-	a.x * b.z	+	a.y * b.w	+	a.z * b.x	+	a.w * b.y	;
	a.z =		a.x * b.y	-	a.y * b.x	+	a.z * b.w	+	a.w * b.z	;
	return a;
}

