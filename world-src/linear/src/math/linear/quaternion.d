module math.linear.quaternion;

import std.math;
import core.internal.traits : Unconst;
////import std.traits : Unconst;
import std.traits;

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
			this.w = cast(T) cos(cast(real) data.angle/2);
			this.dataAxis[] = data.axis[] * cast(T) sin(cast(real) data.angle/2);
		}
	}
	
	const
	Quat!NT castType(NT)() {
		return Quat!NT(data.arrayCast!NT);
	}
	
	inout
	auto opBinary(string op, T)(inout T b) if (__traits(compiles, opBinaryImpl!op(this, b))){
		return opBinaryImpl!op(this, b);
	}
	inout
	auto opBinaryRight(string op, T)(inout T a) if (__traits(compiles, opBinaryImpl!op(a, this))){
		return opBinaryImpl!op(a,this);
	}
	auto opOpAssign(string op, T)(T b) if (__traits(compiles, opOpAssignImpl!op(this, b))){
		return opOpAssignImpl!op(this, b);
	}
	
	
	static const {
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
	
	@property inout
	T magnitudeSquared() {
		return this.w^^2 + this.x^^2 + this.y^^2 + this.z^^2;
	}
	@property inout
	T magnitude() {
		return cast(T) sqrt(cast(real) this.magnitudeSquared);
	}
	void normalize() {
		this.data[] /= this.magnitude;
	}
	const
	Quat!T normalized() {
		Quat!T n;
		n.data[] = this.data[] / this.magnitude;
		return n;
	}
	
	@property {
		const
		T angle() {
			return 2 * cast(T) acos(cast(real) this.w);
		}
		const
		Vec!(T,3) axis() {
			Vec!(T,3) n;
			n.data[] = this.dataAxis[] / cast(T) sqrt(cast(real) 1 - this.w*this.w);
			return n;
		}
		void angle(T n) {
			this.w = cast(T) cos(cast(real) n/2);
		}
		void axis(Vec!(T,3) n) {
			this.dataAxis[] = n[] * cast(T) sin(cast(real) this.angle/2);
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
	
	inout
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




auto opBinaryImpl(string op:"*", T,U)(const Quat!T a, const Quat!U b)
if (isNumeric!T && isNumeric!U)
{
	alias NT = Unconst!(typeof(rvalueOf!T*rvalueOf!U));
	return Quat!(NT)	(	-	a.x * b.x	-	a.y * b.y	-	a.z * b.z	+	a.w * b.w
		,		a.x * b.w	+	a.y * b.z	-	a.z * b.y	+	a.w * b.x
		,	-	a.x * b.z	+	a.y * b.w	+	a.z * b.x	+	a.w * b.y
		,		a.x * b.y	-	a.y * b.x	+	a.z * b.w	+	a.w * b.z
		);
}

 




void opOpAssignImpl(string op:"*", size_t size,T,U)(ref Quat!T a, const Quat!U b) 
if (isNumeric!T && isNumeric!U)
{
	a.w =	-	a.x * b.x	-	a.y * b.y	-	a.z * b.z	+	a.w * b.w	;
	a.x =		a.x * b.w	+	a.y * b.z	-	a.z * b.y	+	a.w * b.x	;
	a.y =	-	a.x * b.z	+	a.y * b.w	+	a.z * b.x	+	a.w * b.y	;
	a.z =		a.x * b.y	-	a.y * b.x	+	a.z * b.w	+	a.w * b.z	;
}








private {
	NT[] arrayCast(NT,OT)(OT[] xs) {
		NT[] nxs = new NT[xs.length];
		foreach (i,e; xs) {
			nxs[i] = cast(NT) e;
		}
		return nxs;
	}
	NT[L] arrayCast(NT,OT,size_t L)(OT[L] xs) {
		NT[L] nxs;
		foreach (i,e; xs) {
			nxs[i] = cast(NT) e;
		}
		return nxs;
	}
}







unittest {
	void testValues(A,B)(A a1, A a2, A a3, A a4, B b1, B b2, B b3, B b4) {
		{
			Quat!A a = [a1,a2,a3,a4];
			Quat!B b = [b1,b2,b3,b4];
			static assert(is(typeof(a*b) == Quat!(typeof(a1+b1))));
		}
		{
			const Quat!A a = [a1,a2,a3,a4];
			const Quat!B b = [b1,b2,b3,b4];
			static assert(is(typeof(a*b) == Quat!(typeof(a1+b1))));
		}
		{
			const Quat!A a = [a1,a2,a3,a4];
			Quat!B b = [b1,b2,b3,b4];
			static assert(is(typeof(a*b) == Quat!(typeof(a1+b1))));
		}
		{
			Quat!A a = [a1,a2,a3,a4];
			const Quat!B b = [b1,b2,b3,b4];
			static assert(is(typeof(a*b) == Quat!(typeof(a1+b1))));
		}
	}
	testValues!(int,int)(1,2,3,4,2,3,4,5);
	testValues!(float,float)(1.5,2.5,3,4,2.5,3,4.5,5);
	testValues!(int,float)(1,2,3,4,2.5,3,4.5,5);
	testValues!(float,double)(1.5,2.5,3,4,2.5,3,4.5,5);
}


