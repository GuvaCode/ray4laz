cmake_minimum_required(VERSION 3.8)
project("raymedia")

# This section configures MSVC (Visual Studio) to enable "Edit and Continue" debugging

if(POLICY CMP0141)
    cmake_policy(SET CMP0141 NEW)
    set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

# Determine the project's root directory and check if it's the main project

set(RMEDIA_ROOT_PATH ${CMAKE_CURRENT_SOURCE_DIR})

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    set(RMEDIA_IS_MAIN ON)
else()
    set(RMEDIA_IS_MAIN OFF)
endif()

# Project configuration options

option(RMEDIA_BUILD_SHARED "Build raymedia as a shared library" OFF)
option(RMEDIA_BUILD_EXAMPLES "Build the examples" ${RMEDIA_IS_MAIN})


SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/CMakeModules/")
FIND_PACKAGE(FFMPEG REQUIRED)

# Include standard cmake scripts

include(CheckLibraryExists)

# Submodule check for raylib

set(RAYLIB_PATH "${RMEDIA_ROOT_PATH}/external/raylib")

if(EXISTS "${RAYLIB_PATH}/CMakeLists.txt")
  if(RMEDIA_BUILD_SHARED)
   set(BUILD_SHARED_LIBS ON)
  endif() 
    message(STATUS "Submodule raylib found and valid.")
    add_subdirectory("${RAYLIB_PATH}")
else()
    find_package(raylib QUIET)
    if(TARGET raylib)
        message(STATUS "raylib found via find_package.")
    else()
        message(FATAL_ERROR "The 'raylib' submodule is missing or incomplete, and 'find_package(raylib)' failed. Please make sure the submodule is initialized and updated (git submodule update --init --recursive).")
    endif()
endif()

# Create the raygizmo library by adding all the source files

set(RMEDIA_LIB_TYPE STATIC)

if(RMEDIA_BUILD_SHARED)
    set(RMEDIA_LIB_TYPE SHARED)
    add_definitions(-DRMEDIA_BUILD_SHARED)
endif()

add_library(${PROJECT_NAME} ${RMEDIA_LIB_TYPE}
    "${RMEDIA_ROOT_PATH}/src/rmedia.c"
    
)

# Link project dependencies

target_link_libraries(${PROJECT_NAME} PUBLIC raylib avcodec avformat avutil swresample swscale)

check_library_exists(m cos "" HAVE_LIB_M)

if(HAVE_LIB_M)
    target_link_libraries(${PROJECT_NAME} PUBLIC m)
endif()

# Specify the include directories needed for the project

target_include_directories(${PROJECT_NAME} PUBLIC
    "${RMEDIA_ROOT_PATH}/include"
    "${RMEDIA_ROOT_PATH}/external/raylib/src"
)

# Ensure the C standard is set for MSVC if using CMake version greater than 3.12

if(CMAKE_VERSION VERSION_GREATER 3.12)
    set_property(TARGET raymedia PROPERTY C_STANDARD 99)
endif()

# Example configuration

if(RMEDIA_BUILD_EXAMPLES)
    include("${RMEDIA_ROOT_PATH}/examples/CMakeLists.txt")
endif()
