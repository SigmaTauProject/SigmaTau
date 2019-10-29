module World;

import cst_;

import std.stdio;
import core.memory;
import std.algorithm;
import std.range;

import WorldLogic;
alias World = WorldLogic.World; // To cover this module (also called `World`).

import gl3n.linalg;


alias EntityRef = uint;

extern(C) export {
	World* newWorld() {
		writeln("World Created");
		World* world = WorldLogic.newWorld();
		GC.addRoot(world);
		return world;
	}
	extern(C) export
	void destroyWorld(World* world) {
		writeln("World Destroyed");
		GC.removeRoot(world);
	}
	
	
	EntityRef newEntity(World* world, EntityType type, int x, int y, int z) {
		return WorldLogic.addEntity(world, new Entity(type, vec3i(x,y,z))).cst!uint;
	}
	void moveEntity(World* world, EntityRef er, int x, int y, int z) {
		WorldLogic.moveEntity(world,er,vec3i(x,y,z));
	}
	void forceEntity(World* world, EntityRef er, float x, float y, float z) {
		WorldLogic.forceEntity(world,er, (vec3f(x,y,z)*256).vecCast!int);
	}
	
	float[3]* getEntityPos(World* world, EntityRef rer, EntityRef er) {
		return [((world.entities[er].pos - world.entities[rer].pos) / 256).ffiVec!float].ptr;
	}
	
	void doEntities(World* world, void function(EntityRef) callback) {
		foreach (EntityRef i, e; world.entities) {
			callback(i);
		}
	}
}



