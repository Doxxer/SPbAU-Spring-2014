#include <functional>
#include <algorithm>
#include <iostream>

#include "scheduler.hpp"

void Scheduler::processNonStarted()
{
    auto it = std::find_if(
        nonstarted.begin(), nonstarted.end(), [&](Process const & p) { return p.start > timer; });
    std::copy(nonstarted.begin(), it, back_inserter(waiting));
    nonstarted.erase(nonstarted.begin(), it);
}

void Scheduler::processIO()
{
    auto it = remove_if(
        ioWaiting.begin(), ioWaiting.end(), std::mem_fun_ref(&Process::updateIOWaitingTime));
    std::copy(it, ioWaiting.end(), back_inserter(waiting));
    ioWaiting.erase(it, ioWaiting.end());
}

void Scheduler::processCurrent()
{
    if (current_process == NULL)
        return;

    switch (current_process->updateWorkingTime()) {
        case Process::WorkingEvent::OK:
            break;
        case Process::WorkingEvent::QUANT_OVER:
            waiting.push_front(*current_process);
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

void Scheduler::processWaiting()
{
    if (current_process != NULL || waiting.empty())
        return;
    
    std::stable_partition(
        waiting.begin(), waiting.end(), std::mem_fun_ref(&Process::compareWithPriority));
    current_process = ProcessPtr(new Process(waiting.front()));
    waiting.pop_front();
}

void Scheduler::printChanges()
{
    if (finished.size() == processCount)
        return;
    std::string cur = current_process == NULL ? "IDLE" : current_process->name;
    if (cur != prevPrintedProcess || prevPrintedTime + Process::quant == timer) {
        std::cout << timer << " " << cur << std::endl;
        prevPrintedProcess = cur;
        prevPrintedTime = timer;
    }
}

// void Scheduler::debug_print()
// {
//     std::cout << "Scheduler. Quant = " << Process::quant << " TIME = " << timer << std::endl;
//     std::cout << "non-started: " << nonstarted.size() << std::endl;
//     for (auto p : nonstarted) {
//         p.print();
//         std::cout << std::endl;
//     }
//     std::cout << "waiting (Q2): " << waiting.size() << std::endl;
//     for (auto p : waiting) {
//         p.print();
//         std::cout << std::endl;
//     }
//     std::cout << "CURRENT PROCESS: ";
//     if (current_process != NULL) {
//         current_process->print();
//         std::cout << std::endl;
//     }
//
//     std::cout << "IO: " << ioWaiting.size() << std::endl;
//     for (auto p : ioWaiting) {
//         p.print();
//         std::cout << std::endl;
//     }
//     std::cout << "FINISHED: " << finished.size() << std::endl;
//     for (auto p : finished) {
//         p.print();
//         std::cout << std::endl;
//     }
// }
