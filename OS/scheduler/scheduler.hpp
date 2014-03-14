#ifndef SCHEDULER_HPP
#define SCHEDULER_HPP

#include <vector>
#include <queue>
#include <iostream>
#include <functional>
#include <memory>

#include "process.hpp"

using std::vector;
using std::queue;
using std::priority_queue;

typedef std::shared_ptr<Process> ProcessPtr;

class Scheduler {
private:
    std::deque<Process> nonstarted; // TODO убрать и вернуть queue
    vector<Process> waiting;
    vector<Process> finished;
    vector<Process> ioWaiting;
    ProcessPtr current_process;
    size_t n;
    int timer;
    string prev;
    int prevTime;

    void print()
    {
        std::cout << "Scheduler. Quant = " << Process::quant << " TIME = " << timer << std::endl;
        std::cout << "non-started: " << nonstarted.size() << std::endl;
        for (auto p : nonstarted) {
            p.print();
            std::cout << std::endl;
        }
        std::cout << "waiting (Q2): " << waiting.size() << std::endl;
        for (auto p : waiting) {
            p.print();
            std::cout << std::endl;
        }
        std::cout << "CURRENT PROCESS: ";
        if (current_process != NULL) {
            current_process->print();
            std::cout << std::endl;
        }

        std::cout << "IO: " << ioWaiting.size() << std::endl;
        for (auto p : ioWaiting) {
            p.print();
            std::cout << std::endl;
        }
        std::cout << "FINISHED: " << finished.size() << std::endl;
        for (auto p : finished) {
            p.print();
            std::cout << std::endl;
        }
    }

    void processNonStarted()
    {
        while (!nonstarted.empty() && nonstarted.front().start <= timer) {
            waiting.push_back(nonstarted.front());
            nonstarted.pop_front();
        }
    }

    void processIO()
    {
        auto it = remove_if(
            ioWaiting.begin(), ioWaiting.end(), std::mem_fun_ref(&Process::updateIOWaitingTime));
        std::copy(it, ioWaiting.end(), back_inserter(waiting));
        ioWaiting.erase(it, ioWaiting.end());
    }

    void processCurrent()
    {
        if (current_process == NULL)
            return;

        switch (current_process->updateWorkingTime()) {
            case Process::WorkingEvent::OK:
                break;
            case Process::WorkingEvent::QUANT_OVER:
                waiting.push_back(*current_process);
                current_process = NULL;
                break;
            case Process::WorkingEvent::IO_OCCURRED:
                ioWaiting.push_back(*current_process);
                current_process = NULL;
                break;
            case Process::WorkingEvent::FINISHED:
                finished.push_back(*current_process);
                current_process = NULL;
                break;
        }
    }

    void processWaiting()
    {
        if (current_process != NULL || waiting.empty())
            return;
        sort(waiting.begin(), waiting.end(), std::mem_fun_ref(&Process::compareWithPriority));
        current_process = ProcessPtr(new Process(waiting.back()));
        waiting.pop_back();
    }

    void printChanges()
    {
        if (finished.size() == n)
            return;
        string cur = current_process == NULL ? "IDLE" : current_process->name;
        if (cur != prev || prevTime + Process::quant == timer) {
            std::cout << timer << " " << cur << std::endl;
            prev = cur;
            prevTime = timer;
        }
    }

public:
    void add_process(Process const &p)
    {
        nonstarted.push_back(p);
        n++;
    }

    void run()
    {
        sort(nonstarted.begin(), nonstarted.end());

        while (finished.size() < n) {
            processNonStarted();
            processIO();
            processCurrent();
            processWaiting();
            printChanges();

            // print();
            timer++;
        }
    }
};

#endif /* end of include guard: SCHEDULER_HPP */
