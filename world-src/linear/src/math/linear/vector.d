module math.linear.vector;

import std.math;

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
	
	auto opBinary(string op, T)(T b) if (__traits(compiles, opBinaryImpl!op(this, b))){
		return opBinaryImpl!op(this, b);
	}
	auto opBinaryRight(string op, T)(T a) if (__traits(compiles, opBinaryImpl!op(a, this))){
		return opBinaryImpl!op(a,this);
	}
	auto opOpAssign(string op, T)(T b) if (__traits(compiles, opOpAssignImpl!op(this, b))){
		return opOpAssignImpl!op(this, b);
	}
	
	
	T magnitudeSquared() {
		import std.algorithm;
		return this.data[].map!"a^^2".sum;
	}
	T magnitude() {
		return cast(T) sqrt(cast(real) this.magnitudeSquared);
	}
	void normalize(bool zero=false)() {
		if (zero && this.magnitude == 0)
			this.data[] = 0;
		else
			this.data[] /= this.magnitude;
	}
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


auto cross(T, U)(Vec!(T,3) a, Vec!(U,3) b) {
	return Vec3!T	( a.y * b.z - b.y * a.z
		, a.z * b.x - b.z * a.x
		, a.x * b.y - b.x * a.y
		);
}
auto dot(T, U)(Vec!(T,3) a, Vec!(U,3) b) {
	import std.algorithm; import std.range;
	return zip(a.data[],b.data[]).map!"a[0]*a[1]".sum();
}



alias Vec2(T) = Vec!(T, 2);
alias Vec3(T) = Vec!(T, 3);
alias Vec4(T) = Vec!(T, 4);

auto opBinaryImpl(string op, size_t size,T,U)(Vec!(T, size) a, Vec!(U, size) b) 
if (__traits(compiles, typeof(mixin("a.data[0]"~op~"b.data[0]"))))
{
	alias NT = typeof(mixin("a.data[0]"~op~"b.data[0]"));
	Vec!(NT, size) n;
	n.data[] = mixin("a.data[]"~op~"b.data[]");
	return n;
}
auto opBinaryImpl(string op, size_t size,T,U)(Vec!(T, size) a, U[size] b) 
if (__traits(compiles, typeof(mixin("a.data[0]"~op~"b[0]"))))
{
	alias NT = typeof(mixin("a.data[0]"~op~"b[0]"));
	Vec!(NT, size) n;
	n.data[] = mixin("a.data[]"~op~"b[]");
	return n;
}
auto opBinaryImpl(string op, size_t size,T,U)(T[size] a, Vec!(U, size) b) 
if (__traits(compiles, typeof(mixin("a[0]"~op~"b.data[0]"))))
{
	alias NT = typeof(mixin("a[0]"~op~"b.data[0]"));
	Vec!(NT, size) n;
	n.data[] = mixin("a[]"~op~"b.data[]");
	return n;
}

auto opBinaryImpl(string op, size_t size,T,U)(Vec!(T, size) a, U b) 
if (__traits(compiles, typeof(mixin("a[0]"~op~"b"))))
{
	alias NT = typeof(mixin("a.data[0]"~op~"b"));
	Vec!(NT, size) n;
	n.data[] = mixin("a.data[]"~op~"b");
	return n;
}

auto opBinaryImpl(string op, size_t size,T,U)(T a, Vec!(U, size) b) 
if (__traits(compiles, typeof(mixin("a"~op~"b[0]"))))
{
	alias NT = typeof(mixin("a"~op~"b.data[0]"));
	Vec!(NT, size) n;
	n.data[] = mixin("a"~op~"b.data[]");
	return n;
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

