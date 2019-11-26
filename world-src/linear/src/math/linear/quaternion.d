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
			T dataAngle;
			T[3] dataAxis;
		}
		struct {
			T w;
			T x;
			T y;
			T z;
		}
	}
	
	this(T dataAngle, T[3] dataAxis ...) {
		this.dataAngle = dataAngle;
		this.dataAxis = dataAxis;
	}
	this(T[4] data) {
		this.data = data;
	}
	this(T[3] dataAxis, T dataAngle) {
		this.dataAngle = dataAngle;
		this.dataAxis = dataAxis;
	}
	this(typeof(this) v) {
		this.data = v.data;
	}
	
	this(AxisRot!T data) {
		import std.stdio;
		if (data.angle==0)
			this.data = [1,0,0,0];
		else {
			this.w = cos(data.angle/2);
			this.dataAxis[] = data.axis[] * sin(data.angle/2);
		}
	}
	
	auto opBinary(string op, T)(T b) if (__traits(compiles, opBinaryImpl!op(this, b))){
		return opBinaryImpl!op(this, b);
	}
	auto opBinaryRight(string op, T)(T a) if (__traits(compiles, opBinaryImpl!op(a, this))){
		return opBinaryImpl!op(a,this);
	}
	auto opOpAssign(string op, T)(T b) if (__traits(compiles, opOpAssignImpl!op(this, b))){
		return opOpAssignImpl!op(this, b);
	}
	
	
	static {
		Quat!T fromAxisRot(T)(AxisRot!T a) {
			return Quat!T(a);
		}
		Quat!T fromAxisRot(T)(Vec!(T,3) axis, T angle) {
			return Quat!T(AxisRot!T(axis,angle));
		}
		Quat!T fromAxisRot(T)(T angle, Vec!(T,3) axis) {
			return Quat!T(AxisRot!T(axis,angle));
		}
		Quat!T fromMagnitudeVector(T)(Vec!(T,3) a) {
			return Quat!T(AxisRot!T(a.magnitude,a.normalized));
		}
	}
	
	@property
	T magnitudeSquared() {
		return this.w^^2 + this.x^^2 + this.y^^2 + this.z^^2;
	}
	@property
	T magnitude() {
		return sqrt(this.magnitudeSquared);
	}
	void normalize() {
		this.data[] /= this.magnitude;
	}
	Quat!T normalized() {
		Quat!T n;
		n.data[] = this.data[] / this.magnitude;
		return n;
	}
	
	@property {
		T angle() {
			return 2 * acos(this.w);
		}
		Vec!(T,3) axis() {
			Vec!(T,3) n;
			n.data[] = this.dataAxis[] / sqrt(1 - this.w*this.w);
			return n;
		}
		void angle(T n) {
			this.w = cos(n/2);
		}
		void axis(Vec!(T,3) n) {
			this.dataAxis[] = n[] * sin(this.angle/2);
		}
	}
	
	
	static
	Quat!T identity() {
		return quat!T(1,[0,0,0]);
	}
	
	
	void invert() {
		this.x = -this.x;
		this.y = -this.y;
		this.z = -this.z;
	}
	alias conjugate = invert;
	
	Quat!T inverse() {
		return Quat!T(this.w, [-this.x, -this.y, -this.z]);
	}
	alias conjugated = inverse;
}

auto quat(T)(T[4] data) {
	return Quat!T(data);
}
auto quat(T)(T dataAngle, T[3] dataAxis ...) {
	return Quat!T(dataAngle, dataAxis);
}
auto quat(T)(T[3] dataAxis, T dataAngle) {
	return Quat!T(dataAngle, dataAxis);
}
auto quat(T)(Quat!T data) {
	return Quat!T(data);
}

auto quat(T)(AxisRot!T data) {
	return Quat!T(data);
}



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

