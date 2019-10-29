module World;

import cst_;

import std.stdio;
import core.memory;
import std.algorithm;
import std.range;

import WorldLogic;
alias World = WorldLogic.World; // To cover this module (also called `World`).

import gl3n.linalg;


alias Entity = uint;

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
	
	
	Entity newEntity(World* world, EntityType type, int x, int y, int z) {
		return WorldLogic.createEntity(world, type, vec3i(x,y,z)).cst!Entity;
	}
	void moveEntity(World* world, Entity er, int x, int y, int z) {
		auto ea = accessEntity(world,er);
		WorldLogic.moveEntity(world,ea,vec3i(x,y,z));
		doneAccessingEntity(world,ea);
	}
	void forceEntity(World* world, Entity er, float x, float y, float z) {
		auto ea = accessEntity(world,er);
		WorldLogic.forceEntity(world,ea, (vec3f(x,y,z)*256).vecCast!int);
		doneAccessingEntity(world,ea);
	}
	
	float[3]* getEntityPos(World* world, Entity rer, Entity er) {
		return [((world.entities[er].pos - world.entities[rer].pos) / 256).ffiVec!float].ptr;
	}
	
	void doEntities(World* world, void function(Entity) callback) {
		foreach (Entity i, e; world.entities) {
			callback(i);
		}
	}
}



