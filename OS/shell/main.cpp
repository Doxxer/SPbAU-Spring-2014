#include <iostream>
#include <sstream>
#include <string>

#include "command.hpp"
#include "utils.hpp"

void help()
{
    std::cout << "Available commands:" << std::endl;
    std::cout << "ls" << std::endl;
    std::cout << "pwd" << std::endl;
    std::cout << "ps" << std::endl;
    std::cout << "kill <SIG> <PID>" << std::endl;
    std::cout << "<path to executable [with arguments]>" << std::endl;
    std::cout << "exit" << std::endl << std::endl;
}

Command readCommand()
{
    std::string userInput;

    do {
        std::cout << "> ";
        getline(std::cin, userInput);
        trim(userInput);
    } while (userInput.size() == 0);

    std::stringstream ss(userInput);
    Command c;

    ss >> c.command;
    std::string arg;
    while (!ss.eof()) {
        ss >> arg;
        c.args.push_back(arg);
    }
    return c;
}

int execCommand(Command const &cmd)
{
    if (cmd.command == "ls")
        return execlsCommand(cmd);

    if (cmd.command == "pwd")
        return execpwdCommand(cmd);

    if (cmd.command == "ps")
        return execpsCommand(cmd);

    if (cmd.command == "kill")
        return execkillCommand(cmd);

    if (cmd.command == "exit")
        return 1;

    return runProcess(cmd);
}

int main()
{
    help();
    do {
    } while (!execCommand(readCommand()));

    return 0;
}
