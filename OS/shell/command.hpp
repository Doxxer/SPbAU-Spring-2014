#include <string>
#include <vector>

using std::string;
using std::vector;

struct Command {
    Command(string cmd = "") : command(cmd)
    {
    }

    string command;
    vector<string> args;
};

Command readCommand();

int execCommand(Command const &cmd);
