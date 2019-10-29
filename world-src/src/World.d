module World;

import cst_;

import std.stdio;
import core.memory;
import std.algorithm;
import std.range;

import gl3n.linalg;

enum EntityType : int {
	Asteroid	,
	Station	,
	Ship	,
	Missile	,
}

struct World {
	Entity*[] entities;
}

struct Entity {
	EntityType type;
	vec3i pos;
	vec3i vel;
}

alias EntityRef = uint;

extern(C) export
World* newWorld() {
	writeln("World Created");
	World* world = new World();
	GC.addRoot(world);
	return world;
}
extern(C) export
void destroyWorld(World* world) {
	writeln("World Destroyed");
	GC.removeRoot(world);
}

extern(C) export
void updateWorld(World* world) {
	writeln("World Updated");
	foreach (i,e; world.entities) {
		e.pos += e.vel;
	}
}


extern(C) export
EntityRef newEntity(World* world, EntityType type, int x, int y, int z) {
	writeln("New Entity: ",x," ",y," ",z);
	world.entities ~= new Entity(type, vec3i(x,y,z));
	return world.entities.length.cst!uint - 1;
}
extern(C) export
void moveEntity(World* world, EntityRef er, int x, int y, int z) {
	world.entities[er].pos += vec3i(x,y,z);
	writeln("Move Entity: ",world.entities[er].pos);
}
extern(C) export
void forceEntity(World* world, EntityRef er, float x, float y, float z) {
	world.entities[er].vel += (vec3f(x,y,z)*256).vecCast!int;
	writeln("Force Entity: ",world.entities[er].vel);
}

extern(C) export
float[3]* getEntityPos(World* world, EntityRef rer, EntityRef er) {
	return [((world.entities[er].pos - world.entities[rer].pos) / 256).ffiVec!float].ptr;
}

extern(C) export
void doEntities(World* world, void function(EntityRef) callback) {
	foreach (EntityRef i, e; world.entities) {
		callback(i);
	}
}



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

