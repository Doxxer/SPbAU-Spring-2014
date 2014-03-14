#ifndef TASK_HPP
#define TASK_HPP

#include <string>
#include <deque>
#include <sstream>
#include <iostream>

using std::queue;
using std::deque;
using std::string;
using std::stringstream;

struct Process {
    enum WorkingEvent {
        OK,
        QUANT_OVER,
        IO_OCCURRED,
        FINISHED
    };

    static int quant;

    explicit Process(string const &s) : ioTime(0)
    {
        stringstream ss(s);
        ss >> name >> start >> length;
        int a, b;
        while (ss >> a >> b) {
            ioBlocks.push_back(IO(a, b));
        }
    }

    bool operator<(Process const &p) const
    {
        return start < p.start;
    }

    bool compareWithPriority(Process const &p) const
    {
        if (p.totalWorkingTime + quant >= p.length)
            return true;
        if (!p.ioBlocks.empty() && p.totalWorkingTime + quant >= p.ioBlocks.front().length)
            return true;
        return false;
    }

    bool updateIOWaitingTime()
    {
        ioTime++;
        totalWorkingTime++;
        bool flag = false;
        if (!ioBlocks.empty() && ioTime >= ioBlocks.front().length) {
            flag = true;
            ioBlocks.pop_front();
            ioTime = 0;
        }
        return flag;
    }

    WorkingEvent updateWorkingTime()
    {
        totalWorkingTime++;
        localWorkingTime++;
        if (totalWorkingTime == length) {
            localWorkingTime = 0;
            return WorkingEvent::FINISHED;
        }
        if (!ioBlocks.empty() && totalWorkingTime == ioBlocks.front().start) {
            localWorkingTime = 0;
            return WorkingEvent::IO_OCCURRED;
        }
        if (localWorkingTime == quant) {
            localWorkingTime = 0;
            return WorkingEvent::QUANT_OVER;
        }
        return WorkingEvent::OK;
    }

    void print() const
    {
        std::cout << name << " " << start << " " << length << " ";
        for (size_t i = 0; i < ioBlocks.size(); ++i) {
            std::cout << ioBlocks[i].start << " " << ioBlocks[i].length << " ";
        }
    }

    int start;
    string name;

private:
    struct IO {
        IO(int s, int l) : start(s), length(l)
        {
        }

        int start;
        int length;
    };

    int length;

    int ioTime;
    int totalWorkingTime;
    int localWorkingTime;

    deque<IO> ioBlocks; // TODO replace to queue
};

#endif /* end of include guard: TASK_HPP */
