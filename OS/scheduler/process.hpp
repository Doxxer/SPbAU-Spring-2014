#ifndef PROCESS_HPP
#define PROCESS_HPP

#include <string>
#include <deque>

class Process {
public:
    static int quant;
    int start;
    std::string name;

    enum WorkingEvent {
        OK,
        QUANT_OVER,
        IO_OCCURRED,
        FINISHED
    };

    explicit Process(std::string const &s);
    bool operator<(Process const &p) const;
    bool compareWithPriority() const;
    bool updateIOWaitingTime();
    WorkingEvent updateWorkingTime();
    // void debug_print() const;

private:
    int length;
    int ioTime;
    int totalWorkingTime;
    int localWorkingTime;
    std::deque<std::pair<int, int> > ioBlocks;
};

#endif /* end of include guard: PROCESS_HPP */
