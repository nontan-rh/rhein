
set(CMAKE_C_FLAGS "-g -W -Wall -Wextra -O0")
set(CMAKE_CXX_FLAGS "-g -W -Wall -Wextra -O0 -std=c++11")

include_directories(./)
add_executable(
    rhein
    main.cc vm.cc operate.cc byteio.cc loader.cc basic/basic.cc
    object/object.cc object/array.cc object/function.cc object/imstring.cc
    object/record.cc object/hashtable.cc basic/builtin.cc)

target_link_libraries(
    rhein
    gc)

install(TARGETS rhein RUNTIME DESTINATION bin)

