// this file is generated by incarnation.py
#include "Core/LuaScript/LuaRegistrationManager.h"

#include "E:/ZeloEngine2/Engine/Sandbox/PhysicsBook/cyclone/pcontacts.h"

#include "sol/sol.hpp"
#include "rttr/registration"
#include "imgui.h"

namespace Zelo
{
void luaBindParticleContact(sol::state_view &L)
{
    L.new_usertype<cyclone::ParticleContact>("ParticleContact",
        sol::constructors<cyclone::ParticleContact()>(),
        "restitution", &cyclone::ParticleContact::restitution,
        "contactNormal", &cyclone::ParticleContact::contactNormal,
        "penetration", &cyclone::ParticleContact::penetration,
        "particleMovement", &cyclone::ParticleContact::particleMovement,
        "__DUMMY", [](){}
    );
}

void rttrRegisterParticleContact()
{
    rttr::registration::class_<cyclone::ParticleContact>("ParticleContact")
    
        .property("restitution", &cyclone::ParticleContact::restitution)
        .property("contactNormal", &cyclone::ParticleContact::contactNormal)
        .property("penetration", &cyclone::ParticleContact::penetration)
        .property("particleMovement", &cyclone::ParticleContact::particleMovement)
    ;
}

void luaBindParticleContactResolver(sol::state_view &L)
{
    L.new_usertype<cyclone::ParticleContactResolver>("ParticleContactResolver",
        sol::constructors<cyclone::ParticleContactResolver(unsigned int)>(),
        "resolveContacts", &cyclone::ParticleContactResolver::resolveContacts,
        "setIterations", &cyclone::ParticleContactResolver::setIterations,
        "__DUMMY", [](){}
    );
}

void rttrRegisterParticleContactResolver()
{
    rttr::registration::class_<cyclone::ParticleContactResolver>("ParticleContactResolver")
    	.constructor<unsigned int>()
    ;
}

void luaBindParticleContactGenerator(sol::state_view &L)
{
    L.new_usertype<cyclone::ParticleContactGenerator>("ParticleContactGenerator",
        sol::constructors<cyclone::ParticleContactGenerator()>(),
        "addContact", &cyclone::ParticleContactGenerator::addContact,
        "__DUMMY", [](){}
    );
}

void rttrRegisterParticleContactGenerator()
{
    rttr::registration::class_<cyclone::ParticleContactGenerator>("ParticleContactGenerator")
    
    ;
}

struct AutoRegisterf28102
{
    AutoRegisterf28102()
    {
        std::cout << "Creating reflection for class: ParticleContact" << std::endl;
        rttrRegisterParticleContact();
        LuaRegistrationManager::getInstance().addRegistry(luaBindParticleContact);
        std::cout << "Creating reflection for class: ParticleContactResolver" << std::endl;
        rttrRegisterParticleContactResolver();
        LuaRegistrationManager::getInstance().addRegistry(luaBindParticleContactResolver);
        std::cout << "Creating reflection for class: ParticleContactGenerator" << std::endl;
        rttrRegisterParticleContactGenerator();
        LuaRegistrationManager::getInstance().addRegistry(luaBindParticleContactGenerator);
    }
};

static AutoRegisterf28102 GAutoRegisterf28102;
}