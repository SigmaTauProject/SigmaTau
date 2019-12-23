module math.linear.vector;

import core.internal.traits : Unconst;
////import std.traits : Unconst;
import std.traits;
import std.math;
import std.range;
import std.algorithm;

public import math.linear._qv;

// TODO: Add tests to ensure T is a compotable type (number, etc...).
struct Vec(T, size_t size) {
	union {
		T[size] data;
		struct {
			static if (size>=1)
				T x;
			static if (size>=2)
				T y;
			static if (size>=3)
				T z;
			static if (size>=4)
				T w;
		}
	}
	alias data this;
	
	this(T[size] data ...) {
		this.data = data;
	}
	this(T v) {
		this.data[] = v;
	}
	
	const
	Vec!(NT, size) castType(NT)() {
		return Vec!(NT,size)(data.arrayCast!NT);
	}
	
	inout
	T opIndex(const size_t i) {
		return data[i];
	}
	void opIndex(const size_t i, T n) {
		data[i] = n;
	}
	
	const
	auto opBinary(string op, T)(T b) if (__traits(compiles, opBinaryImpl!op(this, b))){
		return opBinaryImpl!op(this, b);
	}
	const
	auto opBinaryRight(string op, T)(T a) if (__traits(compiles, opBinaryImpl!op(a, this))){
		return opBinaryImpl!op(a,this);
	}
	auto opOpAssign(string op, T)(T b) if (__traits(compiles, opOpAssignImpl!op(this, b))){
		return opOpAssignImpl!op(this, b);
	}
	
	
	const
	T magnitudeSquared() {
		import std.algorithm;
		return this.data[].map!"a^^2".sum;
	}
	const
	T magnitude() {
		return cast(T) sqrt(cast(real) this.magnitudeSquared);
	}
	void normalize(bool zero=false)() {
		if (zero && this.magnitude == 0)
			this.data[] = 0;
		else
			this.data[] /= this.magnitude;
	}
	const
	Vec!(T,size) normalized(bool zero=false)() {
		Vec!(T,size) n;
		if (zero && this.magnitude == 0)
			n.data[] = 0;
		else
			n.data[] = this.data[] / this.magnitude;
		return n;
	}
	
	
	
	void invert() {
		this.data[] = -this.data[];
	}
	const
	Vec!(T,size) inverse() {
		Vec!(T,size) n;
		n.data[] = -this.data[];
		return n;
	}
}
auto vec(T, size_t size)(T[size] data ...) {
	return Vec!(T, size)(data);
}
auto vec(size_t size, T)(T data) {
	return Vec!(T, size)(data);
}

auto cross(T, U)(const Vec!(T,3) a, const Vec!(U,3) b) {
	return Vec!(typeof(a[0]*b[0]),3)	( a.y * b.z - b.y * a.z
		, a.z * b.x - b.z * a.x
		, a.x * b.y - b.x * a.y
		);
}
auto dot(T, U)(const Vec!(T,3) a, const Vec!(U,3) b) {
	import std.algorithm; import std.range;
	return cast(typeof(a[0]*b[0])) zip(a.data[],b.data[]).map!"a[0]*a[1]".sum();// `cast` because sum will increase precision of type.
}



alias Vec2(T) = Vec!(T, 2);
alias Vec3(T) = Vec!(T, 3);
alias Vec4(T) = Vec!(T, 4);

auto opBinaryImpl(string op, size_t size,T,U)(const Vec!(T, size) a, const Vec!(U, size) b) 
if (isNumeric!T && isNumeric!U)
{
	alias NT = Unconst!(typeof(rvalueOf!T*rvalueOf!U));
	Vec!(NT, size) n;
	n.data[] = mixin("a.data[]"~op~"b.data[]");
	return n;
}
auto opBinaryImpl(string op, size_t size,T,U)(const Vec!(T, size) a, const U[size] b) 
if (isNumeric!T && isNumeric!U)
{
	alias NT = Unconst!(typeof(rvalueOf!T*rvalueOf!U));
	Vec!(NT, size) n;
	n.data[] = mixin("a.data[]"~op~"b[]");
	return n;
}
auto opBinaryImpl(string op, size_t size,T,U)(const T[size] a, const Vec!(U, size) b) 
if (isNumeric!T && isNumeric!U)
{
	alias NT = Unconst!(typeof(rvalueOf!T*rvalueOf!U));
	Vec!(NT, size) n;
	n.data[] = mixin("a[]"~op~"b.data[]");
	return n;
}

auto opBinaryImpl(string op, size_t size,T,U)(const Vec!(T, size) a, const U b)
if (isNumeric!T && isNumeric!U)
{
	alias NT = Unconst!(typeof(rvalueOf!T*rvalueOf!U));
	Vec!(NT, size) n;
	n.data[] = mixin("a.data[]"~op~"b.repeat.take(size).array[]");
	return n;
}

auto opBinaryImpl(string op, size_t size,T,U)(const T a, const Vec!(U, size) b) 
if (isNumeric!T && isNumeric!U)
{
	alias NT = Unconst!(typeof(rvalueOf!T*rvalueOf!U));
	Vec!(NT, size) n;
	n.data[] = mixin("a.repeat.take(size).array[]"~op~"b.data[]");
	return n;
}

 




auto opOpAssignImpl(string op, size_t size,T,U)(ref Vec!(T, size) a, const Vec!(U, size) b) 
if (isNumeric!T && isNumeric!U)
{
	mixin("a.data[]"~op~"=b.data[];");
	return a;
}
auto opOpAssignImpl(string op, size_t size,T,U)(ref Vec!(T, size) a, const U[size] b) 
if (isNumeric!T && isNumeric!U)
{
	mixin("a.data[]"~op~"=b[];");
	return a;
}
auto opOpAssignImpl(string op, size_t size,T,U)(ref Vec!(T, size) a, const U b) 
if (isNumeric!T && isNumeric!U)
{
	mixin("a.data[]"~op~"=b;");
	return a;
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
	void testOp(string op)() {
		void testValues(A,B)(A a1, A a2, A a3, B b1, B b2, B b3) {
			{
				Vec!(A,3) a = [a1,a2,a3];
				Vec!(B,3) b = [b1,b2,b3];
				static assert(is(typeof(mixin("a"~op~"b")) == Vec!(typeof(a1+b1),3)));
				assert(mixin("a"~op~"b")== vec([mixin("a1"~op~"b1"),mixin("a2"~op~"b2"),mixin("a3"~op~"b3")]));
				static if(op=="*") {
					static assert(is(typeof(cross(a,b)) == Vec!(typeof(a1+b1),3)));
					static assert(is(typeof(dot(a,b)) == typeof(a1+b1)));
				}
			}
			{
				const Vec!(A,3) a = [a1,a2,a3];
				const Vec!(B,3) b = [b1,b2,b3];
				static assert(is(typeof(mixin("a"~op~"b")) == Vec!(typeof(a1+b1),3)));
				assert(mixin("a"~op~"b") == vec([mixin("a1"~op~"b1"),mixin("a2"~op~"b2"),mixin("a3"~op~"b3")]));
				static if(op=="*") {
					static assert(is(typeof(cross(a,b)) == Vec!(typeof(a1+b1),3)));
					static assert(is(typeof(dot(a,b)) == typeof(a1+b1)));
				}
			}
			{
				const Vec!(A,3) a = [a1,a2,a3];
				Vec!(B,3) b = [b1,b2,b3];
				static assert(is(typeof(mixin("a"~op~"b")) == Vec!(typeof(a1+b1),3)));
				assert(mixin("a"~op~"b") == vec([mixin("a1"~op~"b1"),mixin("a2"~op~"b2"),mixin("a3"~op~"b3")]));
				static if(op=="*") {
					static assert(is(typeof(cross(a,b)) == Vec!(typeof(a1+b1),3)));
					static assert(is(typeof(dot(a,b)) == typeof(a1+b1)));
				}
			}
			{
				Vec!(A,3) a = [a1,a2,a3];
				const Vec!(B,3) b = [b1,b2,b3];
				static assert(is(typeof(mixin("a"~op~"b")) == Vec!(typeof(a1+b1),3)));
				assert(mixin("a"~op~"b") == vec([mixin("a1"~op~"b1"),mixin("a2"~op~"b2"),mixin("a3"~op~"b3")]));
				static if(op=="*") {
					static assert(is(typeof(cross(a,b)) == Vec!(typeof(a1+b1),3)));
					static assert(is(typeof(dot(a,b)) == typeof(a1+b1)));
				}
			}
		}
		testValues!(int,int)(1,2,3,2,3,4);
		testValues!(float,float)(1.5,2.5,3,2.5,3,4.5);
		testValues!(int,float)(1,2,3,2.5,3,4.5);
		testValues!(float,double)(1.5,2.5,3,2.5,3,4.5);
	}
	testOp!"+";
	testOp!"-";
	testOp!"*";
	testOp!"/";
	testOp!"%";
}





