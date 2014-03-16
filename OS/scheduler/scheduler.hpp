#ifndef SCHEDULER_HPP
#define SCHEDULER_HPP

#include <vector>
#include <deque>
#include <memory>
#include <string>
#include <algorithm>

#include "process.hpp"

typedef std::shared_ptr<Process> ProcessPtr;

class Scheduler {
private:
    std::deque<Process> nonstarted;
    std::deque<Process> waiting;
    std::vector<Process> finished;
    std::vector<Process> ioWaiting;
    ProcessPtr current_process;

    int timer;
    size_t processCount;
    std::string prevPrintedProcess;
    int prevPrintedTime;

    // void debug_print();
    void processNonStarted();
    void processIO();
    void processCurrent();
    void processWaiting();
    void printChanges();

public:
    Scheduler() : timer(0), processCount(0), prevPrintedProcess(""), prevPrintedTime(0)
    {
    }

    void add_process(Process const &p)
    {
        nonstarted.push_back(p);
        processCount++;
    }

    void run()
    {
        sort(nonstarted.begin(), nonstarted.end());

        while (finished.size() < processCount) {
            processNonStarted();
            processIO();
            processCurrent();
            processWaiting();
            printChanges();

            // debug_print();
            timer++;
        }
    }
};

#endif /* end of include guard: SCHEDULER_HPP */
