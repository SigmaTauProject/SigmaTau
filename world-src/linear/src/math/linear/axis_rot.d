module math.linear.axis_rot;

import std.math;

import math.linear.vector;
alias normalized = math.linear.vector.normalized;

public import math.linear._qa;

// TODO: Add tests to ensure T is a compotable type (number, etc...).
struct AxisRot(T) {
	union {
		T[4] data;
		struct {
			T angle;
			Vec3!T axis;
		}
		struct {
			T a;
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
	
	Quat!T _toQuat() {
		return Quat!T(this);
	}
	alias _toQuat this;
	
	auto opBinary(string op, T)(T b) if (__traits(compiles, opBinaryImpl!op(this, b))){
		return opBinaryImpl!op(this, b);
	}
	auto opBinaryRight(string op, T)(T a) if (__traits(compiles, opBinaryImpl!op(a, this))){
		return opBinaryImpl!op(a,this);
	}
	auto opOpAssign(string op, T)(T b) if (__traits(compiles, opOpAssignImpl!op(this, b))){
		return opOpAssignImpl!op(this, b);
	}
}
auto axisRot(T)(T[4] data) {
	return AxisRot!T(data);
}
auto axisRot(T)(T angle, T[3] axis ...) {
	return AxisRot!T(angle, axis);
}
auto axisRot(T)(T[3] axis, T angle) {
	return AxisRot!T(angle, axis);
}
auto axisRot(T)(axisRot!T data) {
	return AxisRot!T(data);
}


Quat!T toQuat(T)(AxisRot!T a) {
	return Quat!T(a);
}


AxisRot!T identity(T)() {
	return AxisRot!T(0,[0,0,0]);
}

void normalize(T)(AxisRot!T t) {
	if (t.angle==0) return;
	t.axis.normalize;
}
AxisRot!T normalized(T)(AxisRot!T t) {
	if (t.angle==0) return identity;
	return AxisRot!T(t.angle,t.axis.normalized);
}


void invert(T)(AxisRot!T t) {
	t.axis.invert;
}
AxisRot!T inverse(T)(AxisRot!T t) {
	return AxisRot!T(t.a, [-t.x, -t.y, -t.z]);
}



auto opBinaryImpl(string op:"*", T,U)(AxisRot!T a, AxisRot!U b) 
if	(__traits(compiles, typeof(mixin("a.data[0]*b.data[0]")))
	&& __traits(compiles, typeof(mixin("a.data[0]+b.data[0]")))
	&& __traits(compiles, typeof(mixin("a.data[0]-b.data[0]")))
	)
{
	if (a.angle == 0) return b;
	if (b.angle == 0) return a;
	return axisRot	(	acos(cos(a.a)*cos(b.a) - dot(a.axis*sin(a.a),(b.axis*sin(b.a))))
		,	((cos(a.a))*(b.axis*sin(b.a)) + (cos(b.a))*(a.axis*sin(a.a)) + cross((a.axis*sin(a.a)),(b.axis*sin(b.a)))).normalized
		);
}

auto opBinaryImpl(string op:"*", T,U)(AxisRot!T a, U b) 
if	(__traits(compiles, typeof(mixin("a.data[0]*b"))))
{
	return axisRot(a.angle*b, a.axis);
}
 




auto opOpAssignImpl(string op, size_t size,T,U)(ref Vec!(T, size) a, Vec!(U, size) b) 
if (__traits(compiles, typeof(mixin("a.data[0]"~op~"=b.data[0]"))))
{
	mixin("a.data[]"~op~"=b.data[];");
	return a;
}
auto opOpAssignImpl(string op, size_t size,T,U)(ref Vec!(T, size) a, U[size] b) 
if (__traits(compiles, typeof(mixin("a.data[0]"~op~"=b[0]"))))
{
	mixin("a.data[]"~op~"=b[];");
	return a;
}
auto opOpAssignImpl(string op, size_t size,T,U)(ref Vec!(T, size) a, U b) 
if (__traits(compiles, typeof(mixin("a[0]"~op~"=b"))))
{
	mixin("a.data[]"~op~"=b;");
	return a;
}

