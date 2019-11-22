module math.linear.vector;


// TODO: Add tests to ensure T is a compotable type (number, etc...).
struct Vec(T, size_t size) {
	T[size] data;
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
}
auto vec(T, size_t size)(T[size] data ...) {
	return Vec!(T, size)(data);
}
auto vec(size_t size, T)(T data) {
	return Vec!(T, size)(data);
}

alias Vec3(T) = Vec!(T, 3);

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

