#include <boost/range/algorithm/copy.hpp>
#include "FileSystemWalker.hpp"

boost::tuple<vector<fs::path>, suffixies> FileSystemWalker::scan()
{
    fs::path rootPath(root_);

    if (!exists(rootPath))
        throw std::runtime_error("Directory " + root_ + " not found");
    if (!is_directory(rootPath))
        throw std::runtime_error(root_ + " is not a directory");

    add_task(rootPath);
    taskQueue_.wait();

    vector<fs::path> pathes;
    suffixies suff_array;
    for (size_t i = 0; i < taskQueue_.getThreadCount(); ++i) {
        vector<fs::path> const &p = taskQueue_.getPathes(i);
        suffixies &s = taskQueue_.getSuffixies(i);

        std::for_each(
            s.begin(), s.end(), boost::bind(&suffix::increment_position, _1, pathes.size()));

        suff_array.insert(suff_array.end(), s.begin(), s.end());
        pathes.insert(pathes.end(), p.begin(), p.end());
    }
    return boost::make_tuple(pathes, suff_array);
}

void FileSystemWalker::add_task(fs::path path)
{
    taskQueue_.add_task([=](vector<fs::path> & pathes, suffixies & suffs) {
        try
        {
            for (fs::directory_iterator entry = fs::directory_iterator(path);
                 entry != fs::directory_iterator();
                 ++entry) {
                if (is_symlink(entry->status()))
                    continue;
                fs::path p = entry->path();
                if (is_directory(entry->status()))
                    add_task(p);
                boost::copy(boost::make_iterator_range(
                                SuffixIterator(p.filename().string(), pathes.size()), {}),
                            std::back_inserter(suffs));
                pathes.push_back(p);
            }
        }
        catch (fs::filesystem_error const &e)
        {
            std::cerr << e.what() << std::endl;
        }
    });
}