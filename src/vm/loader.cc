//
// loader.cc
//

#include <unistd.h>

#include <cstdio>

#include <gc.h>

#include "error.h"
#include "vm.h"

namespace rhein {

bool load_script(State *R, const char* fn) {
    // Create pipe
    int pipes[2];
    if (EXIT_SUCCESS != pipe(pipes)) {
        fatal("Cannot create pipe");
    }

    // Launch compiler process
    int pid = fork();
    if (pid == -1) { // Failed to fork
        fatal("Cannot fork");
    } else if (pid == 0) { // Child process
        close(pipes[0]); // Close read side
        dup2(pipes[1], 1); // Connect to stdout
        execlp("rhc.sh", "rhc.sh", fn, (char *)NULL);
        // Never returns if succeeded

        fatal("Failed to exec");
    }

    // Parent process
    close(pipes[1]); // Close write side

    FILE* fp = fdopen(pipes[0], "r");
    if (fp == nullptr) {
        fatal("Cannot fdopen pipe\n");
    }
    R->load_file(fp);

    fclose(fp);
    return false;
}

}

