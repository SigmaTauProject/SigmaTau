module math.linear._qa;

public import math.linear.quaternion;
public import math.linear.axis_rot;


////auto opBinaryImpl(string op:"*", T,U)(Quat!T a, AxisRot!U b) 
////if	(__traits(compiles, typeof(mixin("a.data[0]*b.data[0]")))
////	)
////{
////	return a * Quat!T(b);
////}
////auto opBinaryImpl(string op:"*", T,U)(AxisRot!T a, Quat!U b) 
////if	(__traits(compiles, typeof(mixin("a.data[0]*b.data[0]")))
////	)
////{
////	return Quat!T(a) * b;
////}
////
////
////auto opOpAssignImpl(string op:"*", T,U)(ref Quat!T a, AxisRot!U b) 
////if	(__traits(compiles, typeof(mixin("a.data[0]*=b.data[0]")))
////	)
////{
////	a *= Quat!T(b);
////	return a;
////}
