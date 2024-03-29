// this file is generated by incarnation.py
#include "Core/LuaScript/LuaRegistrationManager.h"

#include "E:/ZeloEngine2/Engine/Sandbox/PhysicsBook/cyclone/body.h"

#include "sol/sol.hpp"
#include "rttr/registration"
#include "imgui.h"

namespace Zelo
{
void luaBindRigidBody(sol::state_view &L)
{
    auto getAcceleration = sol::overload(
            sol::resolve<void(cyclone::Vector3 *)const>(&cyclone::RigidBody::getAcceleration), 
            sol::resolve<cyclone::Vector3()const>(&cyclone::RigidBody::getAcceleration)
    );
    auto getInertiaTensor = sol::overload(
            sol::resolve<void(cyclone::Matrix3 *)const>(&cyclone::RigidBody::getInertiaTensor), 
            sol::resolve<cyclone::Matrix3()const>(&cyclone::RigidBody::getInertiaTensor)
    );
    auto getInertiaTensorWorld = sol::overload(
            sol::resolve<void(cyclone::Matrix3 *)const>(&cyclone::RigidBody::getInertiaTensorWorld), 
            sol::resolve<cyclone::Matrix3()const>(&cyclone::RigidBody::getInertiaTensorWorld)
    );
    auto getInverseInertiaTensor = sol::overload(
            sol::resolve<void(cyclone::Matrix3 *)const>(&cyclone::RigidBody::getInverseInertiaTensor), 
            sol::resolve<cyclone::Matrix3()const>(&cyclone::RigidBody::getInverseInertiaTensor)
    );
    auto getInverseInertiaTensorWorld = sol::overload(
            sol::resolve<void(cyclone::Matrix3 *)const>(&cyclone::RigidBody::getInverseInertiaTensorWorld), 
            sol::resolve<cyclone::Matrix3()const>(&cyclone::RigidBody::getInverseInertiaTensorWorld)
    );
    auto getLastFrameAcceleration = sol::overload(
            sol::resolve<void(cyclone::Vector3 *)const>(&cyclone::RigidBody::getLastFrameAcceleration), 
            sol::resolve<cyclone::Vector3()const>(&cyclone::RigidBody::getLastFrameAcceleration)
    );
    auto getOrientation = sol::overload(
            sol::resolve<void(cyclone::Quaternion *)const>(&cyclone::RigidBody::getOrientation), 
            sol::resolve<cyclone::Quaternion()const>(&cyclone::RigidBody::getOrientation), 
            sol::resolve<void(cyclone::Matrix3 *)const>(&cyclone::RigidBody::getOrientation), 
            sol::resolve<void(cyclone::real [9])const>(&cyclone::RigidBody::getOrientation)
    );
    auto getPosition = sol::overload(
            sol::resolve<void(cyclone::Vector3 *)const>(&cyclone::RigidBody::getPosition), 
            sol::resolve<cyclone::Vector3()const>(&cyclone::RigidBody::getPosition)
    );
    auto getRotation = sol::overload(
            sol::resolve<void(cyclone::Vector3 *)const>(&cyclone::RigidBody::getRotation), 
            sol::resolve<cyclone::Vector3()const>(&cyclone::RigidBody::getRotation)
    );
    auto getTransform = sol::overload(
            sol::resolve<void(cyclone::Matrix4 *)const>(&cyclone::RigidBody::getTransform), 
            sol::resolve<void(cyclone::real [16])const>(&cyclone::RigidBody::getTransform), 
            sol::resolve<cyclone::Matrix4()const>(&cyclone::RigidBody::getTransform)
    );
    auto getVelocity = sol::overload(
            sol::resolve<void(cyclone::Vector3 *)const>(&cyclone::RigidBody::getVelocity), 
            sol::resolve<cyclone::Vector3()const>(&cyclone::RigidBody::getVelocity)
    );
    auto setAcceleration = sol::overload(
            sol::resolve<void(const cyclone::Vector3 &)>(&cyclone::RigidBody::setAcceleration), 
            sol::resolve<void(const cyclone::real,const cyclone::real,const cyclone::real)>(&cyclone::RigidBody::setAcceleration)
    );
    auto setOrientation = sol::overload(
            sol::resolve<void(const cyclone::Quaternion &)>(&cyclone::RigidBody::setOrientation), 
            sol::resolve<void(const cyclone::real,const cyclone::real,const cyclone::real,const cyclone::real)>(&cyclone::RigidBody::setOrientation)
    );
    auto setPosition = sol::overload(
            sol::resolve<void(const cyclone::Vector3 &)>(&cyclone::RigidBody::setPosition), 
            sol::resolve<void(const cyclone::real,const cyclone::real,const cyclone::real)>(&cyclone::RigidBody::setPosition)
    );
    auto setRotation = sol::overload(
            sol::resolve<void(const cyclone::Vector3 &)>(&cyclone::RigidBody::setRotation), 
            sol::resolve<void(const cyclone::real,const cyclone::real,const cyclone::real)>(&cyclone::RigidBody::setRotation)
    );
    auto setVelocity = sol::overload(
            sol::resolve<void(const cyclone::Vector3 &)>(&cyclone::RigidBody::setVelocity), 
            sol::resolve<void(const cyclone::real,const cyclone::real,const cyclone::real)>(&cyclone::RigidBody::setVelocity)
    );
    L.new_usertype<cyclone::RigidBody>("RigidBody",
        sol::constructors<cyclone::RigidBody()>(),
        "addForce", &cyclone::RigidBody::addForce,
        "addForceAtBodyPoint", &cyclone::RigidBody::addForceAtBodyPoint,
        "addForceAtPoint", &cyclone::RigidBody::addForceAtPoint,
        "addRotation", &cyclone::RigidBody::addRotation,
        "addTorque", &cyclone::RigidBody::addTorque,
        "addVelocity", &cyclone::RigidBody::addVelocity,
        "calculateDerivedData", &cyclone::RigidBody::calculateDerivedData,
        "clearAccumulators", &cyclone::RigidBody::clearAccumulators,
        "getAngularDamping", &cyclone::RigidBody::getAngularDamping,
        "getAwake", &cyclone::RigidBody::getAwake,
        "getCanSleep", &cyclone::RigidBody::getCanSleep,
        "getDirectionInLocalSpace", &cyclone::RigidBody::getDirectionInLocalSpace,
        "getDirectionInWorldSpace", &cyclone::RigidBody::getDirectionInWorldSpace,
        "getGLTransform", &cyclone::RigidBody::getGLTransform,
        "getInverseMass", &cyclone::RigidBody::getInverseMass,
        "getLinearDamping", &cyclone::RigidBody::getLinearDamping,
        "getMass", &cyclone::RigidBody::getMass,
        "getPointInLocalSpace", &cyclone::RigidBody::getPointInLocalSpace,
        "getPointInWorldSpace", &cyclone::RigidBody::getPointInWorldSpace,
        "hasFiniteMass", &cyclone::RigidBody::hasFiniteMass,
        "integrate", &cyclone::RigidBody::integrate,
        "setAngularDamping", &cyclone::RigidBody::setAngularDamping,
        "setAwake", &cyclone::RigidBody::setAwake,
        "setCanSleep", &cyclone::RigidBody::setCanSleep,
        "setDamping", &cyclone::RigidBody::setDamping,
        "setInertiaTensor", &cyclone::RigidBody::setInertiaTensor,
        "setInverseInertiaTensor", &cyclone::RigidBody::setInverseInertiaTensor,
        "setInverseMass", &cyclone::RigidBody::setInverseMass,
        "setLinearDamping", &cyclone::RigidBody::setLinearDamping,
        "setMass", &cyclone::RigidBody::setMass,
        "getAcceleration", getAcceleration,
        "getInertiaTensor", getInertiaTensor,
        "getInertiaTensorWorld", getInertiaTensorWorld,
        "getInverseInertiaTensor", getInverseInertiaTensor,
        "getInverseInertiaTensorWorld", getInverseInertiaTensorWorld,
        "getLastFrameAcceleration", getLastFrameAcceleration,
        "getOrientation", getOrientation,
        "getPosition", getPosition,
        "getRotation", getRotation,
        "getTransform", getTransform,
        "getVelocity", getVelocity,
        "setAcceleration", setAcceleration,
        "setOrientation", setOrientation,
        "setPosition", setPosition,
        "setRotation", setRotation,
        "setVelocity", setVelocity,
        "__DUMMY", [](){}
    );
}

void rttrRegisterRigidBody()
{
    rttr::registration::class_<cyclone::RigidBody>("RigidBody")
    
    ;
}

struct AutoRegisterf07505
{
    AutoRegisterf07505()
    {
        std::cout << "Creating reflection for class: RigidBody" << std::endl;
        rttrRegisterRigidBody();
        LuaRegistrationManager::getInstance().addRegistry(luaBindRigidBody);
    }
};

static AutoRegisterf07505 GAutoRegisterf07505;
}
