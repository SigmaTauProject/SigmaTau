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
}
private
struct Entity {
	Mutex mutex;
	EntityType type;
	vec3i pos;
	vec3i vel;
}

private
alias EntityRef = size_t;
public
struct EntityAccess {
	size_t er;
}

public
World* newWorld() {
	World* world = new World();
	new Thread(worldThread(world)).start();
	return world;
}
private
void delegate() worldThread(World* world) {
	return (){
		while (true) {
			Thread.sleep(50.msecs);
			foreach (i,e; world.entities) {
				withEntity(world,i,(_){
					e.pos += e.vel;
				});
			}
		}
	};
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
EntityRef createEntity(World* world, EntityType type, vec3i pos, vec3i vel=vec3i(0,0,0)) {
	EntityRef addEntity(World* world, Entity* entity) {
		// TODO: Fix this, it is still unsafe, as `world.entities` is not mutex locked.
		world.entities ~= entity;
		return world.entities.length-1;
	}
	return addEntity(world, new Entity(new Mutex(),type,pos,vel));
}

public
void moveEntity(World* world, EntityAccess ea, vec3i a) {
	world.entities[ea.er].pos += a;
}
public
void forceEntity(World* world, EntityAccess ea, vec3i a) {
	world.entities[ea.er].vel += a;
}

vec3i getEntityPos(World* world, EntityAccess ea) {
	return world.entities[ea.er].pos;
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




