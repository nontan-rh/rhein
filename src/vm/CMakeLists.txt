
set(CMAKE_CXX_COMPILER "clang++")

set(CMAKE_C_FLAGS "-g -W -Wall -Wextra -O0")
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CMAKE_CXX_FLAGS "-g -W -Wall -Wextra -O0 -std=c++11")
else()
    set(CMAKE_CXX_FLAGS "-g -W -Wall -Wextra -O0 -std=c++11")
endif()

include_directories(./)
add_executable(
    rhein
    main.cc vm.cc operate.cc byteio.cc loader.cc basic/basic.cc
    object.cc array.cc function.cc string.cc symbol.cc
    record.cc hashtable.cc basic/builtin.cc)

target_link_libraries(
    rhein
    gc)

install(TARGETS rhein RUNTIME DESTINATION bin)

