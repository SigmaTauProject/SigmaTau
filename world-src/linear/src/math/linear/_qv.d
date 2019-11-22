module math.linear._qv;

public import math.linear.quaternion;
public import math.linear.vector;


auto opBinaryImpl(string op:"*", T,U)(Quat!T a, Vec!(U,3) b) 
if	(__traits(compiles, typeof(mixin("a.data[0]*b.data[0]")))
	&& __traits(compiles, typeof(mixin("a.data[0]+b.data[0]")))
	&& __traits(compiles, typeof(mixin("a.data[0]-b.data[0]")))
	)
{
	T ww	= a.w^^2;
	T w2	= a.w * 2;
	T wx2	= w2 * a.x;
	T wy2	= w2 * a.y;
	T wz2	= w2 * a.z;
	T xx	= a.x^^2;
	T x2	= a.x * 2;
	T xy2	= x2 * a.y;
	T xz2	= x2 * a.z;
	T yy	= a.y^^2;
	T yz2	= a.y * a.z * 2;
	T zz	= a.z * a.z;
	
	return vec	(	ww * b.x	+	wy2 * b.z	-	wz2 * b.y	+	xx * b.x	+
			xy2 * b.y	+	xz2 * b.z	-	zz * b.x	-	yy * b.x
		,	xy2 * b.x	+	yy * b.y	+	yz2 * b.z	+	wz2 * b.x	-
			zz * b.y	+	ww * b.y	-	wx2 * b.z	-	xx * b.y
		,	xz2 * b.x	+	yz2 * b.y	+	zz * b.z	-	wy2 * b.x	-
			yy * b.z	+	wx2 * b.y	-	xx * b.z	+	ww * b.z
		);
}
auto opBinaryImpl(string op:"*", T,U)(Vec!(T,3) a, Quat!U b) 
if	(__traits(compiles, typeof(mixin("b*a"))))
{
	return b*a;
}


auto opOpAssignImpl(string op:"*", T,U)(ref Vec3!T a, Quat!U b) 
if	(__traits(compiles, typeof(mixin("a.data[0]*=b.data[0]")))
	&& __traits(compiles, typeof(mixin("a.data[0]+b.data[0]")))
	&& __traits(compiles, typeof(mixin("a.data[0]-b.data[0]")))
	)
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
	return a;
}
