#include <sstream>
#include "process.hpp"

Process::Process(std::string const &s) : ioTime(0), totalWorkingTime(0), localWorkingTime(0)
{
    std::stringstream ss(s);
    ss >> name >> start >> length;
    int a, b;
    while (ss >> a >> b) {
        ioBlocks.push_back(std::make_pair(a, b));
    }
}

bool Process::operator<(Process const &p) const
{
    return start < p.start;
}

bool Process::compareWithPriority() const
{
    if (totalWorkingTime + quant >= length)
        return true;
    if (!ioBlocks.empty() && totalWorkingTime + quant >= ioBlocks.front().first)
        return true;
    return false;
}

bool Process::updateIOWaitingTime()
{
    ioTime++;
    totalWorkingTime++;
    bool flag = false;
    if (!ioBlocks.empty() && ioTime >= ioBlocks.front().second) {
        flag = true;
        ioBlocks.pop_front();
        ioTime = 0;
    }
    return flag;
}

Process::WorkingEvent Process::updateWorkingTime()
{
    totalWorkingTime++;
    localWorkingTime++;
    if (totalWorkingTime == length) {
        localWorkingTime = 0;
        return WorkingEvent::FINISHED;
    }
    if (!ioBlocks.empty() && totalWorkingTime == ioBlocks.front().first) {
        localWorkingTime = 0;
        return WorkingEvent::IO_OCCURRED;
    }
    if (localWorkingTime == quant) {
        localWorkingTime = 0;
        return WorkingEvent::QUANT_OVER;
    }
    return WorkingEvent::OK;
}

// void Process::debug_print() const
// {
//     std::cout << name << " " << start << " " << length << " ";
//     for (size_t i = 0; i < ioBlocks.size(); ++i) {
//         std::cout << ioBlocks[i].start << " " << ioBlocks[i].length << " ";
//     }
// }
