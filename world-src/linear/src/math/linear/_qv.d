module math.linear._qv;

import core.internal.traits : Unconst;
////import std.traits : Unconst;
import std.traits;
public import math.linear.quaternion;
public import math.linear.vector;

auto opBinaryImpl(string op:"*", T,U)(const Quat!T at, const Vec!(U,3) b) 
if (isNumeric!T && isNumeric!U)
{
	alias NT = Unconst!(typeof(rvalueOf!T*rvalueOf!U));
	static if (isFloatingPoint!T && !isFloatingPoint!U)
		auto a = at.castType!real;
	else
		alias a = at;
	
	auto ww	= a.w^^2;
	auto w2	= a.w * 2;
	auto wx2	= w2 * a.x;
	auto wy2	= w2 * a.y;
	auto wz2	= w2 * a.z;
	auto xx	= a.x^^2;
	auto x2	= a.x * 2;
	auto xy2	= x2 * a.y;
	auto xz2	= x2 * a.z;
	auto yy	= a.y^^2;
	auto yz2	= a.y * a.z * 2;
	auto zz	= a.z * a.z;
	
	return Vec!(NT,3)	(	ww * b.x	+	wy2 * b.z	-	wz2 * b.y	+	xx * b.x	+
			xy2 * b.y	+	xz2 * b.z	-	zz * b.x	-	yy * b.x
		,	xy2 * b.x	+	yy * b.y	+	yz2 * b.z	+	wz2 * b.x	-
			zz * b.y	+	ww * b.y	-	wx2 * b.z	-	xx * b.y
		,	xz2 * b.x	+	yz2 * b.y	+	zz * b.z	-	wy2 * b.x	-
			yy * b.z	+	wx2 * b.y	-	xx * b.z	+	ww * b.z
		).castType!U;
}
auto opBinaryImpl(string op:"*", T,U)(const Vec!(T,3) a, const Quat!U b) 
if (isNumeric!T && isNumeric!U)
{
	return b*a;
}


void opOpAssignImpl(string op:"*", T,U)(ref Vec!(T,3) a, const Quat!U b) 
if (isNumeric!T && isNumeric!U)
{
	T ww	= b.w^^2;
	T w2	= b.w * 2;
	T wx2	= w2 * b.x;
	T wy2	= w2 * b.y;
	T wz2	= w2 * b.z;
	T xx	= b.x^^2;
	T x2	= b.x * 2;
	T xy2	= x2 * b.y;
	T xz2	= x2 * b.z;
	T yy	= b.y^^2;
	T yz2	= b.y * b.z * 2;
	T zz	= b.z * b.z;
	
	a.x =	ww * a.x	+	wy2 * a.z	-	wz2 * a.y	+	xx * a.x	+
		xy2 * a.y	+	xz2 * a.z	-	zz * a.x	-	yy * a.x	;
	a.y =	xy2 * a.x	+	yy * a.y	+	yz2 * a.z	+	wz2 * a.x	-
		zz * a.y	+	ww * a.y	-	wx2 * a.z	-	xx * a.y	;
	a.z =	xz2 * a.x	+	yz2 * a.y	+	zz * a.z	-	wy2 * a.x	-
		yy * a.z	+	wx2 * a.y	-	xx * a.z	+	ww * a.z	;
}


unittest {
	void testValues(A,B)(A a1, A a2, A a3, A a4, B b1, B b2, B b3) {
		{
			Quat!A a = [a1,a2,a3,a4];
			Vec!(B,3) b = [b1,b2,b3];
			static assert(is(typeof(a*b) == Vec!(typeof(a1+b1),3)));
		}
		{
			const Quat!A a = [a1,a2,a3,a4];
			const Vec!(B,3) b = [b1,b2,b3];
			static assert(is(typeof(a*b) == Vec!(typeof(a1+b1),3)));
		}
		{
			Quat!A a = [a1,a2,a3,a4];
			const Vec!(B,3) b = [b1,b2,b3];
			static assert(is(typeof(a*b) == Vec!(typeof(a1+b1),3)));
		}
		{
			const Quat!A a = [a1,a2,a3,a4];
			Vec!(B,3) b = [b1,b2,b3];
			static assert(is(typeof(a*b) == Vec!(typeof(a1+b1),3)));
		}
	}
	testValues!(int,int)(1,2,3,4,2,3,4);
	testValues!(float,float)(1.5,2.5,3,4,2.5,3,4.5);
	testValues!(int,float)(1,2,3,4,2.5,3,4.5);
	testValues!(float,double)(1.5,2.5,3,4,2.5,3,4.5);
}

