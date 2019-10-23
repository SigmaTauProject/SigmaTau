module World;

import cst_;

import std.stdio;
import core.memory;
import std.algorithm;

enum EntityType {
	Asteroid	,
	Station	,
	Ship	,
}

struct World {
	Entity*[] entities;
}

struct Entity {
	int[3] pos;
	int[3] vel;
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
		e.pos[] += e.vel[];
	}
}


extern(C) export
EntityRef newEntity(World* world, int x, int y, int z) {
	writeln("New Entity: ",x," ",y," ",z);
	world.entities ~= new Entity([x,y,z]);
	return world.entities.length.cst!uint - 1;
}
extern(C) export
void moveEntity(World* world, EntityRef er, int x, int y, int z) {
	world.entities[er].pos[] += [x,y,z];
	writeln("Move Entity: ",world.entities[er].pos);
}
extern(C) export
void forceEntity(World* world, EntityRef er, int x, int y, int z) {
	world.entities[er].vel[] += [x,y,z];
	writeln("Force Entity: ",world.entities[er].vel);
}

extern(C) export
int[3]* getEntityPos(World* world, EntityRef rer, EntityRef er) {
	int[3]* relPos = (new int[3]).ptr.cst!(int[3]*);
	(*relPos)[] = world.entities[er].pos[] - world.entities[rer].pos[];
	return relPos;
}

extern(C) export
void doEntities(World* world, void function(EntityRef) callback) {
	foreach (EntityRef i, e; world.entities) {
		callback(i);
	}
}


