// ls - ok
// pwd - ok
// ps
// kill
// exit - ok
// any process

#include "command.hpp"

int main()
{
    do {
    } while (!execCommand(readCommand()));

    return 0;
}
