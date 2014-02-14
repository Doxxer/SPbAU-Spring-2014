#include "command.hpp"
#include "utils.hpp"

#include <iostream>
#include <sstream>
#include <cstring>
#include <fstream>
#include <cstdlib>

#include <sys/param.h>
#include <unistd.h>
#include <dirent.h>
#include <signal.h>

std::string GetCurrentDirectory()
{
    char path[MAXPATHLEN];
    getcwd(path, MAXPATHLEN);
    return std::string(path);
}

int execlsCommand(Command const &cmd)
{
    if (!cmd.args.empty()) {
        std::cout << "ls command doesn't have parameters" << std::endl;
        return 0;
    }

    DIR *directory;
    std::string current_dir = GetCurrentDirectory();

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

    DIR *procdir;

    std::string pathprocdir = "/proc/";

    if ((procdir = opendir(pathprocdir.c_str())) == 0)
        std::cout << "Failed to read proc directory" << std::endl;
    else {
        dirent *pid;
        std::string commandLine;
        while ((pid = readdir(procdir)) != 0) {
            if (pid->d_type == DT_DIR && IsNumeric(pid->d_name)) {
                std::string cmdFilePath = pathprocdir + std::string(pid->d_name) + "/comm";
                std::ifstream cmdFile(cmdFilePath.c_str());

                if (cmdFile.is_open()) {
                    getline(cmdFile, commandLine);
                    std::cout << pid->d_name << ": " << commandLine << std::endl;
                }
                cmdFile.close();
            }
        }
        closedir(procdir);
    }

    return 0;
}

int execkillCommand(Command const &cmd)
{
    if (cmd.args.size() != 2 || !IsNumeric(cmd.args[0].c_str()) ||
        !IsNumeric(cmd.args[1].c_str())) {
        std::cout << "Invalid command parameters" << std::endl;
    } else {
        int signal = atoi(cmd.args[0].c_str());
        int pid = atoi(cmd.args[1].c_str());        
        kill(pid, signal);
    }
    return 0;
}

int runProcess(Command const &cmd)
{
    std::stringstream ss;
    ss << cmd.command;

    for (size_t i = 0; i < cmd.args.size(); ++i)
        ss << " " << cmd.args[i];

    system(ss.str().c_str());
    return 0;
}
