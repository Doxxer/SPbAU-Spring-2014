#ifndef FILESYSTEMWALKER_HPP
#define FILESYSTEMWALKER_HPP

#include <string>
#include <vector>
#include <boost/filesystem.hpp>
#include <boost/tuple/tuple.hpp>
#include "ThreadedTaskQueue.hpp"
#include "Utilities.hpp"
#include "SuffixIterator.hpp"

namespace fs = boost::filesystem;

using std::string;
using std::vector;

class FileSystemWalker {
public:
    FileSystemWalker(string const &root) : root_(root)
    {
    }

    boost::tuple<vector<fs::path>, suffixies> scan();

private:
    string root_;
    ThreadedTaskQueue taskQueue_;

    void add_task(fs::path path);
};

#endif /* end of include guard: FILESYSTEMWALKER_HPP */
