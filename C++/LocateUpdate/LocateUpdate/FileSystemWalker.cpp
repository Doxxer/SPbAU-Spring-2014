#include "FileSystemWalker.hpp"

namespace fs = boost::filesystem;

void FileSystemWalker::scan()
{
    fs::path rootPath(root_);

    if (!exists(rootPath))
        throw std::runtime_error("Directory " + root_ + " not found");
    if (!is_directory(rootPath))
        throw std::runtime_error(root_ + " is not a directory");

    add_task(rootPath);
    threadPool.start_and_wait();
    threadPool.stop();
}

void FileSystemWalker::fs_scanner_worker::operator()()
{
    try
    {
        for (fs::directory_iterator entry = fs::directory_iterator(path_);
             entry != fs::directory_iterator();
             ++entry) {
            if (is_symlink(entry->status()))
                continue;
            if (is_directory(entry->status()))
                fileSystemWalker_->add_task(entry->path());
            fileSystemWalker_->callback_(entry->path());
        }
    }
    catch (fs::filesystem_error const &e)
    {
        std::cerr << e.what() << std::endl;
    }
}