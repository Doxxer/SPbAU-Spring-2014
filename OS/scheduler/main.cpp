#include <string>
#include <iostream>

#include "scheduler.hpp"
#include "process.hpp"

int Process::quant = 0;

int main()
{
    //freopen("input.txt", "r", stdin);

    Scheduler scheduler;
    std::cin >> Process::quant;

    std::string line;
    while (getline(std::cin, line)) {
        if (line.length() == 0)
            continue;
        scheduler.add_process(Process(line));
    }

    scheduler.run();
    return 0;
}
