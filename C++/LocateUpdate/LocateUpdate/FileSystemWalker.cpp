#include <exception>
#include "FileSystemWalker.h"

void FileSystemWalker::scan() {
    fs::path rootPath(root_);

    if (!exists(rootPath))
        throw std::runtime_error("Directory " + root_ + " not found");
    if (!is_directory(rootPath))
        throw std::runtime_error(root_ + " is not a directory");

    for (fs::directory_iterator entry = fs::directory_iterator(rootPath);
         entry != fs::directory_iterator();
         ++entry) {
        if (is_symlink(entry->status()))
            continue;

        if (is_directory(entry->status())) {
            // scan(entry->path().string());
        }
        std::cout << entry->path() << std::endl;
    }
}
