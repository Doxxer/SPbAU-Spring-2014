#include <string>
#include <iostream>

#include "scheduler.hpp"
#include "process.hpp"

using std::cin;
using std::endl;
using std::cout;

int Process::quant = 0;

int main()
{
    freopen("input.txt", "r", stdin);

    Scheduler scheduler;
    cin >> Process::quant;

    string line;
    while (getline(cin, line)) {
        if (line.length() == 0)
            continue;
        scheduler.add_process(Process(line));
    }

    scheduler.run();
    return 0;
}
