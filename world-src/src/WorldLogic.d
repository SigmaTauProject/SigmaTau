module WorldLogic;

import cst_;

import std.stdio;
import std.conv;
import std.math;
import core.time;
import core.thread;
import core.sync.mutex;
import std.algorithm;
import std.range;

import gl3n.linalg : Quaternion, AxisRotation;
import math.linear.vector;
import math.linear.point;

import W = World;

public 
enum EntityType : int {
	Asteroid	,
	Station	,
	Ship	,
	Missile	,
}
public// private members & construction
struct World {
	Entity*[] entities;
	MonoTime time;
}
private
struct Entity {
	EntityType type;
	PVec3!long pos;
	Vec3!int vel;
	quatf ori;
	arotf anv;
	Mutex mutex;
	MonoTime time;
}

private
alias EntityRef = size_t;
public
struct EntityAccess {
	size_t er;
}

public
World* newWorld(MonoTime time) {
	World* world = new World([],time);
	new Thread((){
		try {
			worldThread(world)();
		}
		catch (Throwable e) {
			writeln(e);
			throw e;
		}
	}).start();
	return world;
}
private
void delegate() worldThread(World* world) {
	enum loopDur = 500.msecs;// Must be an even fraction of a second to not cause accumulation of errors.
	return () {
		MonoTime updateTime;
		while (true) {
			{
				auto sleepTime = loopDur-(MonoTime.currTime-updateTime);
				Thread.sleep(sleepTime>=0.msecs?sleepTime:0.msecs);
				updateTime = MonoTime.currTime;
			}
			foreach (i,e; world.entities) {
				withEntity(world,i,(_){
					e.time = updateTime;
					e.pos = e.pos + getDurVel(e.vel,loopDur);
					e.ori = getDurAnv(e.anv,loopDur) * e.ori;
					e.ori.normalize();
				});
			}
		}
	};
}
public
void forwardWorld(World* world, MonoTime time) {
	world.time = time;
}

/**	Gain mutexed, multithreaded access to an entity.
	Must call `doneAccessingEntity` to release.
	Release as quick as possable, as it is holding off other threads.
	`withEntity` is recomened inplace of this when possable.
*/
public
EntityAccess accessEntity(World* world, EntityRef er) {
	world.entities[er].mutex.lock();
	return EntityAccess(er);
}

/**	Release mutexed access to entity.
	The `EntityAccess` must NOT be used after this call.
*/
public
void doneAccessingEntity(World* world, EntityAccess ea) {
	world.entities[ea.er].mutex.unlock();
}

/**	Mutexed, multithreaded access to an entity.
	This automatically releases.
	Keep the callback as quick as possable, as it is holding off other threads.
	This is recomended over using `accessEntity`.
*/
public
T withEntity(T)(World* world, EntityRef er, T delegate(EntityAccess) callback) {
	auto ea = accessEntity(world, er);
	scope(exit) doneAccessingEntity(world, ea);
	return callback(ea);
}

public
EntityRef createEntity	( World*	world	
	, EntityType	type	
	, PVec3!long	pos	=point(vec!long(0,0,0))
	, quatf	ori	=quatf.identity()
	, Vec3!int	vel	=vec!int(0,0,0)
	, arotf	anv	=arotf.identity()
) {
	EntityRef addEntity(World* world, Entity* entity) {
		// TODO: Fix this, it is still unsafe, as `world.entities` is not mutex locked.
		world.entities ~= entity;
		return world.entities.length-1;
	}
	return addEntity(world, new Entity(type,pos,vel,ori,anv,new Mutex(),world.time));
}

public
void moveEntity(World* world, EntityAccess ea, PVec3!long a) {
	world.entities[ea.er].pos += a;
}
public
void forceEntity(World* world, EntityAccess ea, Vec3!int a) {
	world.entities[ea.er].vel += a;
}
public
void rotateEntity(World* world, EntityAccess ea, quatf a) {
	world.entities[ea.er].ori = a * world.entities[ea.er].ori;
	world.entities[ea.er].ori.normalize();
}
public
void angularForceEntity(World* world, EntityAccess ea, arotf a) {
	a.writeln;
	world.entities[ea.er].anv.writeln;
	world.entities[ea.er].anv = a * world.entities[ea.er].anv;
	world.entities[ea.er].anv.writeln;
	////world.entities[ea.er].anv.normalize();
}

public
PVec3!long getEntityPos(World* world, EntityAccess ea) {
	return world.entities[ea.er].pos + getDurVel(world.entities[ea.er].vel, world.time-world.entities[ea.er].time);
	////return world.entities[ea.er].pos;
}
public
quatf getEntityOri(World* world, EntityAccess ea) {
	return getDurAnv(world.entities[ea.er].anv, world.time-world.entities[ea.er].time) * world.entities[ea.er].ori;
}



private {
	Vec3!int getDurVel(Vec3!int vel, Duration dur) {
		return vel * dur.total!"msecs".cst!int;
	}
	arotf getDurAnv(arotf anv, Duration dur) {
		return anv * (dur.total!"msecs".cst!float/1000);
	}
}



public {
	
	alias quatf = Quaternion!float;
	
	alias arotf = AxisRotation!float;
	
	T[size] ffiVec(T, size_t size)(Vec!(T,size) xs) {
		return xs.data;
	}
	NT[size] ffiVec(NT, T, size_t size)(Vec!(T,size) xs) {
		return xs.data.arrayCast!NT;
	}
	
	T[4] ffiQuat(T)(Quaternion!T xs) {
		return xs.quaternion;
	}
	NT[4] ffiQuat(NT, T)(Quaternion!T xs) {
		return xs.quaterion.arrayCast!NT;
	}
	
	NT[] arrayCast(NT,OT)(OT[] xs) {
		return xs.map!(cst!(NT,OT)).array();
	}
	NT[L] arrayCast(NT,OT,size_t L)(OT[L] xs) {
		NT[L] nxs;
		foreach (i,e; xs) {
			nxs[i] = e.cst!NT;
		}
		return nxs;
	}
	
	Vec!(NT,L) vecCast(NT,OT,size_t L)(Vec!(OT,L) xs) {
		return vec(xs.data.arrayCast!NT);
	}
	
	
	
}




