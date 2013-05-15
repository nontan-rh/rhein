
file(GLOB SCHEME_SOURCES *.scm)

install(FILES ${SCHEME_SOURCES} DESTINATION share/gauche/site/lib)
install(PROGRAMS rhc.sh DESTINATION bin)

