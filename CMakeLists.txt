cmake_minimum_required(VERSION 3.22.2)

project(kitsu_transpiler VERSION 0.0.0)

set(CMAKE_CXX_STANDARD 20)

#<target>

    add_library(trans_target INTERFACE target/link.hpp)

    target_include_directories(trans_target INTERFACE ./target)

#</target>

#<test>

    add_executable(.comp_test test/try_compile.cpp)

    target_link_libraries(.comp_test PUBLIC trans_target)

    target_include_directories(.comp_test PUBLIC ./target)

#<test/>