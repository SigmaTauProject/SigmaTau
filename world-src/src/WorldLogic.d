module WorldLogic;

import cst_;

import std.stdio;
import core.time;
import core.thread;
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
public
struct World {
	Entity*[] entities;
}
public
struct Entity {
	EntityType type;
	vec3i pos;
	vec3i vel;
}

alias EntityRef = size_t;


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
				e.pos += e.vel;
			}
		}
	};
}

public
EntityRef addEntity(World* world, Entity* entity) {
	world.entities ~= entity;
	return world.entities.length-1;
}
public
void moveEntity(World* world, EntityRef er, vec3i a) {
	world.entities[er].pos += a;
}
public
void forceEntity(World* world, EntityRef er, vec3i a) {
	world.entities[er].vel += a;
}



public {
	alias vec3f = vec3;
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




