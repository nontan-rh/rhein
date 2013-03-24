cmake_minimum_required(VERSION 2.6)

set(CMAKE_C_COMPILER "clang")
set(CMAKE_CXX_COMPILER "clang++")

project(rheinvm)

set(CMAKE_C_FLAGS "-g -W -Wall -Wextra -O0")
set(CMAKE_CXX_FLAGS "-g -W -Wall -Wextra -O0 -std=c++11")

include_directories(./)
add_executable(
    rheinvm
    vmmain.cc vm.cc operate.cc byteio.cc basic/basic.cc object/object.cc object/array.cc
    object/function.cc object/imstring.cc object/record.cc object/hashtable.cc)

target_link_libraries(
    rheinvm
    gc)

install(TARGETS rheinvm RUNTIME DESTINATION bin)

