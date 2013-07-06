
set(CMAKE_CXX_COMPILER "g++-4.8")

set(CMAKE_C_FLAGS "-g -W -Wall -Wextra -O0")
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CMAKE_CXX_FLAGS "-g -W -Wall -Wextra -O0 -std=c++11")
else()
    set(CMAKE_CXX_FLAGS "-g -W -Wall -Wextra -O0 -std=c++11")
endif()

set(parser_odir "${CMAKE_CURRENT_SOURCE_DIR}/parser/")
include_directories(${CMAKE_CURRENT_SOURCE_DIR} ${parser_odir})

include_directories(./)
add_executable(
    rhein
    main.cc vm.cc operate.cc byteio.cc loader.cc basic/basic.cc
    object.cc array.cc function.cc string.cc symbol.cc
    record.cc hashtable.cc basic/builtin.cc basic/port.cc)

add_custom_command(
    OUTPUT "${parser_odir}lex.re.cc"
    COMMAND re2c -i -o "${parser_odir}lex.re.cc" "${CMAKE_CURRENT_SOURCE_DIR}/parser/lex.re"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/parser/lex.re"
    )

add_executable(
    test
    test.cc vm.cc operate.cc byteio.cc loader.cc basic/basic.cc
    object.cc array.cc function.cc string.cc symbol.cc parser/scanner.cc
    parser/lex.re.cc record.cc hashtable.cc basic/builtin.cc basic/port.cc)

target_link_libraries(
    rhein
    gc)

target_link_libraries(
    test
    gc)

install(TARGETS rhein RUNTIME DESTINATION bin)

