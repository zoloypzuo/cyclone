# created on 2020/2/18
# author @zoloypzuo

add_library(cyclone
        body.cpp
        collide_coarse.cpp
        collide_fine.cpp
        contacts.cpp
        core.cpp
        fgen.cpp
        joints.cpp
        particle.cpp
        pcontacts.cpp
        pfgen.cpp
        plinks.cpp
        pworld.cpp
        random.cpp
        world.cpp
        )

add_library(cyclone_py SHARED
        body.cpp
        collide_coarse.cpp
        collide_fine.cpp
        contacts.cpp
        core.cpp
        fgen.cpp
        joints.cpp
        particle.cpp
        pcontacts.cpp
        pfgen.cpp
        plinks.cpp
        pworld.cpp
        random.cpp
        world.cpp
        cyclone_wrapper.cxx
        )
target_include_directories(cyclone_py PRIVATE $ENV{PYTHON_INCLUDE})
target_link_libraries(cyclone_py $ENV{PYTHON_LIB})

add_subdirectory(demos)
