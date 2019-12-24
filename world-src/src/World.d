module World;

import cst_;

import std.stdio;
import std.math;
import core.time;
import core.memory;
import std.algorithm;
import std.range;

import WorldLogic;
alias World = WorldLogic.World; // To cover this module (also called `World`).

import math.linear.vector;
import math.linear.point;
import math.linear.quaternion;
import math.linear.axis_rot;


alias Entity = uint;

private pragma(inline)
T* returnPtr(T)(T* ptr) {
	holdPtr(ptr);
	return ptr;
}
extern(C) export {
	void holdPtr(void* ptr) {
		GC.addRoot(ptr);
		GC.setAttr(ptr, GC.BlkAttr.NO_MOVE);
	}
	void releasePtr(void* ptr) {
		GC.removeRoot(ptr);
		GC.clrAttr(ptr, GC.BlkAttr.NO_MOVE);
	}
	World* newWorld() {
		writeln("World Created");
		return returnPtr(WorldLogic.newWorld(MonoTime.currTime()));
	}
	void updateWorld(World* world) {
		WorldLogic.forwardWorld(world, MonoTime.currTime());
	}
	
	
	Entity newEntity(World* world, EntityType type, int x, int y, int z) {
		return WorldLogic.createEntity(world, type, point(vec!long(x,y,z)*pow(2,16))).cst!Entity;
	}
	void locatedForceEntity(World* world, Entity er, float fx, float fy, float fz, float px, float py, float pz) {
		withEntity(world,er,(ea){
			auto force = vec!float(fx,fy,fz) * WorldLogic.getEntityOri(world, ea);
			auto point = vec!float(px,py,pz) * WorldLogic.getEntityOri(world, ea);
			WorldLogic.forceEntity(world,ea, (force*(pow(2f,16f)/1000f)).vecCast!int);
			WorldLogic.angularForceEntity(world,ea, cross(point,force));
		});
	}
	void moveEntity(World* world, Entity er, float x, float y, float z) {
		//TODO: Update this to use floats and relative ori
		withEntity(world,er,(ea){
			WorldLogic.moveEntity(world,ea, (WorldLogic.getEntityOri(world, ea) * vec!float(x,y,z)*pow(2,16)).vecCast!long);
		});
	}
	void forceEntity(World* world, Entity er, float x, float y, float z) {
		withEntity(world,er,(ea){
			auto force = vec!float(x,y,z) * WorldLogic.getEntityOri(world, ea);
			WorldLogic.forceEntity(world,ea, (force*(pow(2f,16f)/1000f)).vecCast!int);
		});
	}
	void rotateEntity(World* world, Entity er, float w, float x, float y, float z) {
		withEntity(world,er,(ea){
			////WorldLogic.rotateEntity(world,ea, Quat!float(w,vec!float(x,y,z) * WorldLogic.getEntityOri(world, ea).inverse).normalized);
			////WorldLogic.rotateEntity(world,ea, WorldLogic.getEntityOri(world, ea).inverse * Quat!float(w,[x,y,z]));
			Quat!float rot = Quat!float(w,vec!float(x,y,z));
			rot.axis = rot.axis * WorldLogic.getEntityOri(world, ea);
			WorldLogic.rotateEntity(world,ea, rot);
		});
	}
	void angularForceEntity(World* world, Entity er, float x, float y, float z) {
		withEntity(world,er,(ea){
			WorldLogic.angularForceEntity(world,ea, Vec3!float(x,y,z)*WorldLogic.getEntityOri(world, ea));
		});
	}
	void angularEulerForceEntity(World* world, Entity er, float yaw, float pitch, float roll) {
		//TODO: Implement this.
		////withEntity(world,er,(ea){
		////	WorldLogic.angularForceEntity(world,ea,arotf.euler_rotation(yaw, pitch, roll));
		////});
	}
	
	float[3]* getEntityPos(World* world, Entity rer, Entity er) {
		// TODO: Fix this, deadlocking is theoretically possable.  Should not hold a mutex while reaching for another.
		return withEntity(world,rer,(rea)=>withEntity(world,er,(ea){
			return [(	WorldLogic.getEntityOri(world,rea).inverse
				*
				(	(	(	WorldLogic.getEntityPos(world,ea)
							-
							WorldLogic.getEntityPos(world,rea)
						)
					).vecCast!float
					/
					pow(2f,16f)
				)
			).ffiVec].ptr;
		})).returnPtr;
	}
	float[4]* getEntityOri(World* world, Entity rer, Entity er) {
		// TODO: Fix this, deadlocking is theoretically possable.  Should not hold a mutex while reaching for another.
		return withEntity(world,rer,(rea)=>withEntity(world,er,(ea){
			return [(	WorldLogic.getEntityOri(world,rea).inverse
				*
				WorldLogic.getEntityOri(world,ea)
			).ffiQuat].ptr;
		})).returnPtr;
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


unittest {
	auto world = newWorld();
	newEntity(world, EntityType.Ship, 0, 0, 0);
	updateWorld(world);
}

unittest {
	//	Ensure that the array representation is as expected for use in FFI.
	struct MArray {
		size_t length;
		int* ptr;
	}
	int[] a = [3,5];
	MArray am = *cast(MArray*) &a;
	assert(am.length == 2);
	assert(*am.ptr == 3);
	assert(*(am.ptr+1) == 5);
}


