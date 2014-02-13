#include <string>
#include <vector>

struct Command {
    Command(std::string cmd = "") : command(cmd)
    {
    }

    std::string command;
    std::vector<std::string> args;
};

Command readCommand();

int execCommand(Command const &cmd);
