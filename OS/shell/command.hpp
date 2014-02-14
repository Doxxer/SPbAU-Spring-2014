#ifndef COMMAND_HPP
#define COMMAND_HPP

#include <string>
#include <vector>

struct Command {
    Command(std::string cmd = "") : command(cmd)
    {
    }

    std::string command;
    std::vector<std::string> args;
};

int execlsCommand(Command const &);
int execpwdCommand(Command const &);
int execpsCommand(Command const &);
int execkillCommand(Command const &);
int runProcess(Command const &);

#endif /* end of include guard: COMMAND_HPP */
