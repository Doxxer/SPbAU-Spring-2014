#include "FileSystemWalker.hpp"

vector<fs::path> FileSystemWalker::scan()
{
    fs::path rootPath(root_);

    if (!exists(rootPath))
        throw std::runtime_error("Directory " + root_ + " not found");
    if (!is_directory(rootPath))
        throw std::runtime_error(root_ + " is not a directory");

    add_task(rootPath);
    threadPool_.start_and_wait();
    threadPool_.stop();

    vector<fs::path> result;
    for (vector<fs::path> const &v : threadPool_.get_result()) {
        // for (fs::path const &q : v) {
        //     std::cout << q << std::endl;
        // }
        result.insert(result.end(), v.begin(), v.end());
    }

    return result;
}

void FileSystemWalker::add_task(fs::path path)
{
    threadPool_.add_task([=](vector<fs::path> & pathCollection) {
        try
        {
            for (fs::directory_iterator entry = fs::directory_iterator(path);
                 entry != fs::directory_iterator();
                 ++entry) {
                if (is_symlink(entry->status()))
                    continue;
                if (is_directory(entry->status()))
                    add_task(entry->path());
                pathCollection.push_back(entry->path());
            }
        }
        catch (fs::filesystem_error const &e)
        {
            std::cerr << e.what() << std::endl;
        }
    });
}