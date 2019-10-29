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
		withEntity(world,er,(ea){
			WorldLogic.moveEntity(world,ea,vec3i(x,y,z));
		});
	}
	void forceEntity(World* world, Entity er, float x, float y, float z) {
		withEntity(world,er,(ea){
			WorldLogic.forceEntity(world,ea, (vec3f(x,y,z)*256).vecCast!int);
		});
	}
	
	float[3]* getEntityPos(World* world, Entity rer, Entity er) {
		// TODO: Fix this, deadlocking is theoretically possable.  Should not hold a mutex while reaching for another.
		return withEntity(world,rer,(rea)=>withEntity(world,er,(ea){
			return [((WorldLogic.getEntityPos(world,rea) - WorldLogic.getEntityPos(world,ea)) / 256).ffiVec!float].ptr;
		}));
	}
	
	// TODO: Optimise this.
	void doEntities(World* world, void function(Entity) callback) {
		// TODO: Remove use of private access to content of world.
		// TODO: (As in the last todo, this is considered illegal access for this file (thus this kind of unsafe should not be possable)) Fix this, it is still unsafe, as `world.entities` is not mutex locked.
		foreach (Entity er; 0..(world.entities.length.cst!Entity)) {
			callback(er);
		}
	}
}



