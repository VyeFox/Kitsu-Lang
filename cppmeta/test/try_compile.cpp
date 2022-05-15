
#include "link.hpp"
using namespace kitsu;
/*
*   C++ code that tests simple compilation as well as template compilation and instantiation
*/

static_assert(not scope<int>, "int is wrongly a valid scope");

struct global{using captures = std::tuple<>;};

static_assert(scope<global>, "singleton global is not valid scope");

int main() {
   
    struct main_scope{using captures = std::tuple<global>;};

    static_assert(scope<main_scope>, "singleton main_scope is not valid scope");

    return 0;
}
