// this file is generated by incarnation.py
#include "Core/LuaScript/LuaRegistrationManager.h"

#include "E:/ZeloEngine2/Engine/Sandbox/PhysicsBook/cyclone/core.h"

#include "sol/sol.hpp"
#include "rttr/registration"
#include "imgui.h"

namespace Zelo
{
void luaBindVector3(sol::state_view &L)
{
    L.new_usertype<cyclone::Vector3>("Vector3",
        sol::constructors<cyclone::Vector3(),cyclone::Vector3(const cyclone::real,const cyclone::real,const cyclone::real)>(),
        "x", &cyclone::Vector3::x,
        "y", &cyclone::Vector3::y,
        "z", &cyclone::Vector3::z,
        "addScaledVector", &cyclone::Vector3::addScaledVector,
        "clear", &cyclone::Vector3::clear,
        "componentProduct", &cyclone::Vector3::componentProduct,
        "componentProductUpdate", &cyclone::Vector3::componentProductUpdate,
        "invert", &cyclone::Vector3::invert,
        "magnitude", &cyclone::Vector3::magnitude,
        "normalise", &cyclone::Vector3::normalise,
        "scalarProduct", &cyclone::Vector3::scalarProduct,
        "squareMagnitude", &cyclone::Vector3::squareMagnitude,
        "trim", &cyclone::Vector3::trim,
        "unit", &cyclone::Vector3::unit,
        "vectorProduct", &cyclone::Vector3::vectorProduct,
        "__DUMMY", [](){}
    );
}

void rttrRegisterVector3()
{
    rttr::registration::class_<cyclone::Vector3>("Vector3")
    	.constructor<>()
		.constructor<const cyclone::real,const cyclone::real,const cyclone::real>()
        .property("x", &cyclone::Vector3::x)
        .property("y", &cyclone::Vector3::y)
        .property("z", &cyclone::Vector3::z)
    ;
}

void luaBindQuaternion(sol::state_view &L)
{
    L.new_usertype<cyclone::Quaternion>("Quaternion",
        sol::constructors<cyclone::Quaternion(),cyclone::Quaternion(const cyclone::real,const cyclone::real,const cyclone::real,const cyclone::real)>(),
        "r", &cyclone::Quaternion::r,
        "i", &cyclone::Quaternion::i,
        "j", &cyclone::Quaternion::j,
        "k", &cyclone::Quaternion::k,
        "addScaledVector", &cyclone::Quaternion::addScaledVector,
        "normalise", &cyclone::Quaternion::normalise,
        "rotateByVector", &cyclone::Quaternion::rotateByVector,
        "__DUMMY", [](){}
    );
}

void rttrRegisterQuaternion()
{
    rttr::registration::class_<cyclone::Quaternion>("Quaternion")
    	.constructor<>()
		.constructor<const cyclone::real,const cyclone::real,const cyclone::real,const cyclone::real>()
        .property("r", &cyclone::Quaternion::r)
        .property("i", &cyclone::Quaternion::i)
        .property("j", &cyclone::Quaternion::j)
        .property("k", &cyclone::Quaternion::k)
    ;
}

void luaBindMatrix4(sol::state_view &L)
{
    L.new_usertype<cyclone::Matrix4>("Matrix4",
        sol::constructors<cyclone::Matrix4()>(),
        "data", &cyclone::Matrix4::data,
        "fillGLArray", &cyclone::Matrix4::fillGLArray,
        "getAxisVector", &cyclone::Matrix4::getAxisVector,
        "getDeterminant", &cyclone::Matrix4::getDeterminant,
        "inverse", &cyclone::Matrix4::inverse,
        "invert", &cyclone::Matrix4::invert,
        "setDiagonal", &cyclone::Matrix4::setDiagonal,
        "setInverse", &cyclone::Matrix4::setInverse,
        "setOrientationAndPos", &cyclone::Matrix4::setOrientationAndPos,
        "transform", &cyclone::Matrix4::transform,
        "transformDirection", &cyclone::Matrix4::transformDirection,
        "transformInverse", &cyclone::Matrix4::transformInverse,
        "transformInverseDirection", &cyclone::Matrix4::transformInverseDirection,
        "__DUMMY", [](){}
    );
}

void rttrRegisterMatrix4()
{
    rttr::registration::class_<cyclone::Matrix4>("Matrix4")
    	.constructor<>()
        .property("data", &cyclone::Matrix4::data)
    ;
}

void luaBindMatrix3(sol::state_view &L)
{
    L.new_usertype<cyclone::Matrix3>("Matrix3",
        sol::constructors<cyclone::Matrix3(),cyclone::Matrix3(const cyclone::Vector3 &,const cyclone::Vector3 &,const cyclone::Vector3 &),cyclone::Matrix3(cyclone::real,cyclone::real,cyclone::real,cyclone::real,cyclone::real,cyclone::real,cyclone::real,cyclone::real,cyclone::real)>(),
        "data", &cyclone::Matrix3::data,
        "getAxisVector", &cyclone::Matrix3::getAxisVector,
        "getRowVector", &cyclone::Matrix3::getRowVector,
        "inverse", &cyclone::Matrix3::inverse,
        "invert", &cyclone::Matrix3::invert,
        "linearInterpolate", &cyclone::Matrix3::linearInterpolate,
        "setBlockInertiaTensor", &cyclone::Matrix3::setBlockInertiaTensor,
        "setComponents", &cyclone::Matrix3::setComponents,
        "setDiagonal", &cyclone::Matrix3::setDiagonal,
        "setInertiaTensorCoeffs", &cyclone::Matrix3::setInertiaTensorCoeffs,
        "setInverse", &cyclone::Matrix3::setInverse,
        "setOrientation", &cyclone::Matrix3::setOrientation,
        "setSkewSymmetric", &cyclone::Matrix3::setSkewSymmetric,
        "setTranspose", &cyclone::Matrix3::setTranspose,
        "transform", &cyclone::Matrix3::transform,
        "transformTranspose", &cyclone::Matrix3::transformTranspose,
        "transpose", &cyclone::Matrix3::transpose,
        "__DUMMY", [](){}
    );
}

void rttrRegisterMatrix3()
{
    rttr::registration::class_<cyclone::Matrix3>("Matrix3")
    	.constructor<>()
		.constructor<const cyclone::Vector3 &,const cyclone::Vector3 &,const cyclone::Vector3 &>()
		.constructor<cyclone::real,cyclone::real,cyclone::real,cyclone::real,cyclone::real,cyclone::real,cyclone::real,cyclone::real,cyclone::real>()
        .property("data", &cyclone::Matrix3::data)
    ;
}

struct AutoRegisterf59606
{
    AutoRegisterf59606()
    {
        std::cout << "Creating reflection for class: Vector3" << std::endl;
        rttrRegisterVector3();
        LuaRegistrationManager::getInstance().addRegistry(luaBindVector3);
        std::cout << "Creating reflection for class: Quaternion" << std::endl;
        rttrRegisterQuaternion();
        LuaRegistrationManager::getInstance().addRegistry(luaBindQuaternion);
        std::cout << "Creating reflection for class: Matrix4" << std::endl;
        rttrRegisterMatrix4();
        LuaRegistrationManager::getInstance().addRegistry(luaBindMatrix4);
        std::cout << "Creating reflection for class: Matrix3" << std::endl;
        rttrRegisterMatrix3();
        LuaRegistrationManager::getInstance().addRegistry(luaBindMatrix3);
    }
};

static AutoRegisterf59606 GAutoRegisterf59606;
}