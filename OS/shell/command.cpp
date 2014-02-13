#include "command.hpp"

#include <iostream>
#include <sstream>
#include <cstring>

#include <sys/param.h>
#include <unistd.h>
#include <dirent.h>

std::string GetCurrentDirectory()
{
    char path[MAXPATHLEN];
    getcwd(path, MAXPATHLEN);
    return std::string(path);
}

void trim(string &s)
{
    while (isspace(*s.begin()))
        s.erase(s.begin());
    while (s.size() > 0 && isspace(*s.rbegin()))
        s.erase(s.end() - 1);
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
    string arg;
    while (!ss.eof()) {
        ss >> arg;
        c.args.push_back(arg);
    }
    return c;
}

int execlsCommand(Command const &cmd)
{
    if (!cmd.args.empty()) {
        std::cout << "ls command doesn't have parameters" << std::endl;
        return 0;
    }

    DIR *directory;
    string current_dir = GetCurrentDirectory();

    if ((directory = opendir(current_dir.c_str())) == 0)
        std::cout << "Failed to read current directory: " << current_dir << std::endl;
    else {
        dirent *file;
        while ((file = readdir(directory)) != 0) {
            if (strcmp(file->d_name, ".") && strcmp(file->d_name, ".."))
                std::cout << file->d_name << std::endl;
        }
        closedir(directory);
    }

    return 0;
}

int execpwdCommand(Command const &cmd)
{
    if (!cmd.args.empty()) {
        std::cout << "pwd command doesn't have parameters" << std::endl;
        return 0;
    }
    std::cout << GetCurrentDirectory() << std::endl;
    return 0;
}

int execpsCommand(Command const &cmd)
{
    if (!cmd.args.empty()) {
        std::cout << "ps command doesn't have parameters" << std::endl;
        return 0;
    }

    return 0;
}

int execkillCommand(Command const &cmd)
{
    return 0;
}

int runProcess(Command const &cmd)
{
    std::cout << "Running process <" << cmd.command << "> args: ";
    for (size_t i = 0; i < cmd.args.size(); ++i)
        std::cout << cmd.args[i] << " ";
    std::cout << std::endl;
    return 0;
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
