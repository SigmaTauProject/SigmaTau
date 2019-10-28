module World;

import cst_;

import std.stdio;
import core.memory;
import std.algorithm;
import std.range;

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
EntityRef newEntity(World* world, EntityType type, int x, int y, int z) {
	writeln("New Entity: ",x," ",y," ",z);
	world.entities ~= new Entity(type, [x,y,z]);
	return world.entities.length.cst!uint - 1;
}
extern(C) export
void moveEntity(World* world, EntityRef er, int x, int y, int z) {
	world.entities[er].pos[] += [x,y,z];
	writeln("Move Entity: ",world.entities[er].pos);
}
extern(C) export
void forceEntity(World* world, EntityRef er, float x, float y, float z) {
	world.entities[er].vel[] += [x,y,z].map!"a*256".map!(cst!(int,float)).array()[];
	writeln("Force Entity: ",world.entities[er].vel);
}

extern(C) export
float[3]* getEntityPos(World* world, EntityRef rer, EntityRef er) {
	float[3]* relPos = (new float[3]).ptr.cst!(float[3]*);
	int[3] tmp;
	tmp[] = world.entities[er].pos[] - world.entities[rer].pos[];
	(*relPos)[] = tmp[0..3].map!(cst!(float,int)).array()[];
	return relPos;
}

extern(C) export
void doEntities(World* world, void function(EntityRef) callback) {
	foreach (EntityRef i, e; world.entities) {
		callback(i);
	}
}


