cmake_minimum_required(VERSION 3.6)
project(tg1553)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES cachesimulator.cpp)
add_executable(tg1553 ${SOURCE_FILES})