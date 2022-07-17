// this file is generated by incarnation.py
#include "Core/LuaScript/LuaRegistrationManager.h"

#include "E:/ZeloEngine2/Engine/Sandbox/PhysicsBook/cyclone/particle.h"

#include "sol/sol.hpp"
#include "rttr/registration"
#include "imgui.h"

namespace Zelo
{
void luaBindParticle(sol::state_view &L)
{
    auto getAcceleration = sol::overload(
            sol::resolve<void(cyclone::Vector3 *)const>(&cyclone::Particle::getAcceleration), 
            sol::resolve<cyclone::Vector3()const>(&cyclone::Particle::getAcceleration)
    );
    auto getPosition = sol::overload(
            sol::resolve<void(cyclone::Vector3 *)const>(&cyclone::Particle::getPosition), 
            sol::resolve<cyclone::Vector3()const>(&cyclone::Particle::getPosition)
    );
    auto getVelocity = sol::overload(
            sol::resolve<void(cyclone::Vector3 *)const>(&cyclone::Particle::getVelocity), 
            sol::resolve<cyclone::Vector3()const>(&cyclone::Particle::getVelocity)
    );
    auto setAcceleration = sol::overload(
            sol::resolve<void(const cyclone::Vector3 &)>(&cyclone::Particle::setAcceleration), 
            sol::resolve<void(const cyclone::real,const cyclone::real,const cyclone::real)>(&cyclone::Particle::setAcceleration)
    );
    auto setPosition = sol::overload(
            sol::resolve<void(const cyclone::Vector3 &)>(&cyclone::Particle::setPosition), 
            sol::resolve<void(const cyclone::real,const cyclone::real,const cyclone::real)>(&cyclone::Particle::setPosition)
    );
    auto setVelocity = sol::overload(
            sol::resolve<void(const cyclone::Vector3 &)>(&cyclone::Particle::setVelocity), 
            sol::resolve<void(const cyclone::real,const cyclone::real,const cyclone::real)>(&cyclone::Particle::setVelocity)
    );
    L.new_usertype<cyclone::Particle>("Particle",
        sol::constructors<cyclone::Particle()>(),
        "addForce", &cyclone::Particle::addForce,
        "clearAccumulator", &cyclone::Particle::clearAccumulator,
        "getDamping", &cyclone::Particle::getDamping,
        "getInverseMass", &cyclone::Particle::getInverseMass,
        "getMass", &cyclone::Particle::getMass,
        "hasFiniteMass", &cyclone::Particle::hasFiniteMass,
        "integrate", &cyclone::Particle::integrate,
        "setDamping", &cyclone::Particle::setDamping,
        "setInverseMass", &cyclone::Particle::setInverseMass,
        "setMass", &cyclone::Particle::setMass,
        "getAcceleration", getAcceleration,
        "getPosition", getPosition,
        "getVelocity", getVelocity,
        "setAcceleration", setAcceleration,
        "setPosition", setPosition,
        "setVelocity", setVelocity,
        "__DUMMY", [](){}
    );
}

void rttrRegisterParticle()
{
    rttr::registration::class_<cyclone::Particle>("Particle")
    
    ;
}

struct AutoRegisterf87417
{
    AutoRegisterf87417()
    {
        std::cout << "Creating reflection for class: Particle" << std::endl;
        rttrRegisterParticle();
        LuaRegistrationManager::getInstance().addRegistry(luaBindParticle);
    }
};

static AutoRegisterf87417 GAutoRegisterf87417;
}