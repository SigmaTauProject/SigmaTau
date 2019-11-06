module WorldLogic;

import cst_;

import std.stdio;
import core.time;
import core.thread;
import core.sync.mutex;
import std.algorithm;
import std.range;

import gl3n.linalg;

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
	vec3i pos;
	vec3i vel;
	quatf ori;
	quatf anv;
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
	new Thread(worldThread(world)).start();
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
					e.pos += getDurVel(e.vel,loopDur);
					e.ori = getDurAnv(e.anv,loopDur) * e.ori;
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
	, vec3i	pos	=vec3i(0,0,0)
	, quatf	ori	=quatf.identity()
	, vec3i	vel	=vec3i(0,0,0)
	, quatf	anv	=quat.identity()
) {
	EntityRef addEntity(World* world, Entity* entity) {
		// TODO: Fix this, it is still unsafe, as `world.entities` is not mutex locked.
		world.entities ~= entity;
		return world.entities.length-1;
	}
	return addEntity(world, new Entity(type,pos,vel,ori,anv,new Mutex(),world.time));
}

public
void moveEntity(World* world, EntityAccess ea, vec3i a) {
	world.entities[ea.er].pos += a;
}
public
void forceEntity(World* world, EntityAccess ea, vec3i a) {
	world.entities[ea.er].vel += a;
}
public
void rotateEntity(World* world, EntityAccess ea, quatf a) {
	world.entities[ea.er].ori = a * world.entities[ea.er].ori;
}
public
void angularForceEntity(World* world, EntityAccess ea, quatf a) {
	world.entities[ea.er].anv = a * world.entities[ea.er].anv;
}

public
vec3i getEntityPos(World* world, EntityAccess ea) {
	return world.entities[ea.er].pos + getDurVel(world.entities[ea.er].vel, world.time-world.entities[ea.er].time);
	////return world.entities[ea.er].pos;
}
public
quatf getEntityOri(World* world, EntityAccess ea) {
	return world.entities[ea.er].ori + getDurAnv(world.entities[ea.er].anv, world.time-world.entities[ea.er].time);
}



private {
	vec3i getDurVel(vec3i vel, Duration dur) {
		return vel * dur.total!"msecs".cst!int;
	}
	quatf getDurAnv(quatf anv, Duration dur) {
		return anv * dur.total!"msecs".cst!float;
	}
}



public {
	alias vec2f = vec2;
	alias vec3f = vec3;
	alias vec4f = vec4;
	alias vec2l = Vector!(long,2);
	alias vec3l = Vector!(long,3);
	alias vec4l = Vector!(long,4);
	alias Vec2(Type) = Vector!(Type,2);
	alias Vec3(Type) = Vector!(Type,3);
	alias Vec4(Type) = Vector!(Type,4);
	
	alias quatf = Quaternion!float;
	
	Type[L] ffiVec(Type, size_t L)(Vector!(Type,L) xs) {
		return xs.vector;
	}
	NT[L] ffiVec(NT, Type, size_t L)(Vector!(Type,L) xs) {
		return xs.vector.arrayCast!NT;
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

	Vector!(NT,L) vecCast(NT,OT,size_t L)(Vector!(OT,L) xs) {
		return Vector!(NT,L)(xs.vector.arrayCast!NT);
	}
}




