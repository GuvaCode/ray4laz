cmake_minimum_required(VERSION 3.11)
project(rmedia
    LANGUAGES C
)
set(PROJECT_VERSION 0)

# Directory Variables
set(RMEDIA_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/)
set(RMEDIA_SRC ${RMEDIA_ROOT}/src)


option(BUILD_SHARED_LIBS "Build as a shared library" OFF)


if (NOT BUILD_SHARED_LIBS)
 add_library(rmedia SHARED ${RMEDIA_SRC}/rmedia.c)
else()
 add_library(rmedia STATIC ${RMEDIA_SRC}/rmedia.c)
endif()

# Dependenices
set_target_properties(rmedia PROPERTIES VERSION ${PROJECT_VERSION})

target_link_libraries(rmedia m pthread z gcc avcodec avformat avutil swresample swscale)
