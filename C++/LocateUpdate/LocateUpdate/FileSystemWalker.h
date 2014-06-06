#ifndef __FileSystemWalker_H_
#define __FileSystemWalker_H_

#include <string>
#include <boost/function.hpp>
#include <boost/filesystem.hpp>

namespace fs=boost::filesystem;

using std::string;

class fs_scanner_worker {
private:
    boost::function<void(fs::path)> callback_;
    fs::path path_;
public:

    void operator()() {
        try {
            for (fs::directory_iterator entry = fs::directory_iterator(path_);
                 entry != fs::directory_iterator();
                 ++entry) {
                if (is_symlink(entry->status()))
                    continue;
                callback_(entry->path());
            }
        }
        catch (fs::filesystem_error const& e) {
            std::cerr << e.what() << std::endl;
        }
    }

    fs_scanner_worker(boost::function<void(fs::path)> const &cb, fs::path path) : callback_(cb), path_(path) {

    }

    fs_scanner_worker(fs_scanner_worker const& other) : callback_(other.callback_), path_(other.path_) {}
};

class FileSystemWalker {
private:
    string root_;
public:

    FileSystemWalker(string const &root) : root_ (root) {
    }

    void scan();
};


#endif //__FileSystemWalker_H_
