# clang++ -std=c++20 -fPIC -shared -Iinclude -I$(ocamlc -where) src/bridge/*.cpp main.cpp -o my_ast_bindings.so

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
    src/bridge/*.cpp
    # src/ast/*.cpp
    # src/codegen/*.cpp
    main.cpp
)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

find_path(GMP_INCLUDE_DIR NAMES gmp.h)
find_library(GMP_LIBRARY NAMES gmp libgmp)
find_library(GMPXX_LIBRARY NAMES gmpxx libgmpxx)

if(NOT GMP_INCLUDE_DIR OR NOT GMP_LIBRARY)
    message(FATAL_ERROR "GMP not found! Please install it (e.g. sudo dnf install gmp-devel)")
endif()

include_directories(${GMP_INCLUDE_DIR})
message(STATUS "Found GMP include dir: ${GMP_INCLUDE_DIR}")
message(STATUS "Found GMP libraries: ${GMP_LIBRARY}, ${GMPXX_LIBRARY}")


include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs
    core
    irreader
    support
    bitreader
    asmparser
    passes
    instcombine
    scalaropts
    analysis
    transformutils
    ipo
    aggressiveinstcombine
    vectorize
    frontendopenmp
    globalisel
)

add_compile_options(-stdlib=libc++)
add_link_options(-stdlib=libc++)

add_library(convert_static STATIC ${SOURCES})
set_target_properties(convert_static PROPERTIES
    OUTPUT_NAME "convert"
    ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/../frontend/lib
)

add_library(convert SHARED ${SOURCES})
set_target_properties(convert PROPERTIES
    OUTPUT_NAME "convert"
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/../frontend/lib
    PREFIX "dll"  # OCaml expects 'dllconvert.so'
)

target_link_libraries(convert PRIVATE ${llvm_libs} ${GMPXX_LIBRARY} ${GMP_LIBRARY})
target_link_libraries(convert_static PRIVATE ${llvm_libs} ${GMPXX_LIBRARY} ${GMP_LIBRARY})
