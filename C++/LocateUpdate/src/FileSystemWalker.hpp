#ifndef FILESYSTEMWALKER_HPP
#define FILESYSTEMWALKER_HPP

#include <string>
#include <vector>
#include <boost/filesystem.hpp>
#include "ThreadPool.hpp"

namespace fs = boost::filesystem;

using std::string;
using std::vector;

class FileSystemWalker {
public:
    FileSystemWalker(string const &root) : root_(root)
    {
    }

    vector<fs::path> scan();

private:
    string root_;
    ThreadPool<vector<fs::path>> threadPool_;

    void add_task(fs::path path);
};

#endif /* end of include guard: FILESYSTEMWALKER_HPP */
