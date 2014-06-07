#ifndef FILESYSTEMWALKER_HPP
#define FILESYSTEMWALKER_HPP

#include <string>
#include <boost/function.hpp>
#include <boost/filesystem.hpp>
#include "ThreadPool.hpp"

namespace fs = boost::filesystem;

using std::string;

class FileSystemWalker {
public:
    FileSystemWalker(string const &root, boost::function<void(fs::path)> const &callback)
        : root_(root), callback_(callback)
    {
    }

    void scan();

private:
    string root_;
    ThreadPool threadPool;

    boost::function<void(fs::path)> callback_;

    void add_task(fs::path path)
    {
        threadPool.add_task(fs_scanner_worker(this, path));
    }

    class fs_scanner_worker {
    private:
        fs::path path_;
        FileSystemWalker *fileSystemWalker_;

    public:
        void operator()();

        fs_scanner_worker(FileSystemWalker *fileSystemWalker, fs::path path)
            : path_(path), fileSystemWalker_(fileSystemWalker)
        {
        }
    };
};

#endif /* end of include guard: FILESYSTEMWALKER_HPP */
