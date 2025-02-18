cmake_minimum_required(VERSION 3.11)
project(raygizmo
    LANGUAGES C
)
set(PROJECT_VERSION 0)

# Directory Variables
set(RGIZMO_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/)
set(RGIZMO_SRC ${RGIZMO_ROOT}/src)


option(BUILD_SHARED_LIBS "Build as a shared library" OFF)


if (NOT BUILD_SHARED_LIBS)
 add_library(raygizmo SHARED ${RGIZMO_SRC}/raygizmo.c)
else()
 add_library(raygizmo STATIC ${RGIZMO_SRC}/raygizmo.c)
endif()

# Dependenices
set_target_properties(raygizmo PROPERTIES VERSION ${PROJECT_VERSION})

target_link_libraries(raygizmo m pthread z gcc)
