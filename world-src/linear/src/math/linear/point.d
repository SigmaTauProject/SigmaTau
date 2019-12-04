module math.linear.point;

import math.linear.vector: Vec;

// TODO: Add tests to ensure T is a compotable type (number or vector, etc...).
struct Point(T) {
	T vector;
	
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
auto point(T)(T v) {
	return Point!T(v);
}

alias P = Point;
alias PVec(size_t size, T) = P!(Vec!(T, size));
alias PVec2(T) = P!(Vec!(T, 2));
alias PVec3(T) = P!(Vec!(T, 3));
alias PVec4(T) = P!(Vec!(T, 4));

auto opBinaryImpl(string op = "-", T,U)(P!T a, P!U b) 
if	( __traits(compiles, mixin("a.vector"~op~"b.vector"))
	)
{
	return mixin("a.vector"~op~"b.vector");
}
auto opBinaryImpl(string op, T,U)(P!T a, U b) 
if	( __traits(compiles, mixin("a.vector"~op~"b"))
	&& (op=="-" || op=="+")
	)
{
	return point(mixin("a.vector"~op~"b"));
}



auto opOpAssignImpl(string op:"-", T,U)(ref P!T a, P!U b) 
if	( __traits(compiles, mixin("a.vector"~op~"=b.vector"))
	)
{
	mixin("a.vector"~op~"=b.vector;");
	return a;
}
auto opOpAssignImpl(string op, T,U)(ref P!T a, U b) 
if	( __traits(compiles, mixin("a.vector"~op~"=b"))
	&& (op=="-" || op=="+")
	)
{
	mixin("a.vector"~op~"=b;");
	return a;
}
 
