# clang++ -std=c++20 -fPIC -shared -Iinclude -I$(ocamlc -where) src/ast/*.cpp main.cpp -o my_ast_bindings.so

cmake_minimum_required(VERSION 3.16)
project(my_ast_project LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

execute_process(
    COMMAND which clang++
    OUTPUT_VARIABLE CLANG
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(CMAKE_CXX_COMPILER ${CLANG})

execute_process(
    COMMAND ocamlc -where
    OUTPUT_VARIABLE OCAML_INCLUDE_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

include_directories(
    include
    ${OCAML_INCLUDE_DIR}
)

file(GLOB_RECURSE SOURCES
    src/ast/*.cpp
    main.cpp
)

add_library(convert SHARED ${SOURCES})

set_target_properties(convert PROPERTIES OUTPUT_NAME "convert")
