#include <iostream>

#include "command.hpp"

void help()
{
    std::cout << "Available commands:" << std::endl;
    std::cout << "ls" << std::endl;
    std::cout << "pwd" << std::endl;
    std::cout << "ps" << std::endl;
    std::cout << "kill <PID> <SIG>" << std::endl;
    std::cout << "<path to executable [with arguments]>" << std::endl;
    std::cout << "exit" << std::endl << std::endl;
}

int main()
{
    help();
    do {
    } while (!execCommand(readCommand()));

    return 0;
}
