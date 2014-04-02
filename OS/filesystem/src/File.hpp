#ifndef __File_H_
#define __File_H_

#include <string>
#include <cstdlib>
#include <fstream>
#include <vector>

using std::string;
using std::ifstream;
using std::ofstream;
using std::vector;

struct File {

    File() : modified_time(time(0))
    {
    }

    explicit File(string const &name, size_t size_in_bytes)
        : name(name), size(size_in_bytes), modified_time(time(0))
    {
    }

    File(File const &f)
        : name(f.name), size(f.size), modified_time(f.modified_time), blocks(f.blocks)
    {
    }

    bool operator==(File const &f)
    {
        return f.name == name && f.size == size && f.blocks == blocks &&
               f.modified_time == modified_time;
    }

    string get_info() const;

    void load(ifstream &, string const& location);

    void save(ofstream &) const;

    void addUsedBlock(size_t block);

    string name;
    size_t size;
    time_t modified_time;
    vector<size_t> blocks;
};

#endif //__File_H_
