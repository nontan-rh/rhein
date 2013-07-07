//
// main.cc
//

#include <iostream>
#include <cstdio>
#include <gc.h>

#include "vm.h"
#include "loader.h"
#include "basic/basic.h"
#include "basic/builtin.h"
#include "basic/port.h"
#include "parser/scanner.h"

using namespace std;
using namespace rhein;
using namespace rhein::basic;
using namespace rhein::builtin;
using namespace rhein::port;

int main(int argc, char** argv) {
    if (argc != 2) { cout << "argumenterrr" << endl; return 1; }

    GC_init();

    State *R = new (GC_malloc(sizeof(State))) State();
    R->load_module(BasicModule::create(R));
    R->load_module(BuiltinModule::create(R));
    R->load_module(FileModule::create(R));
    R->load_module(ScannerModule::create(R));

    File* f = File::create(R, String::create(R, argv[1]),
            File::RWFlags::Read,
            File::PosFlags::Head);
    Scanner* s = Scanner::create(R, f);
    for ( ;!s->eof() ;) {
        cout << to_str(s->get_token(R)->get_kind()) << endl;
    }
    return 0;
}

