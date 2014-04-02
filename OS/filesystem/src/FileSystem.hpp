#ifndef __FileSystem_Hpp_
#define __FileSystem_Hpp_

#include <string>
#include <vector>
#include <algorithm>
#include "utilities.hpp"
#include "Directory.hpp"

using std::string;
using std::vector;

class FileSystem {

public:
    explicit FileSystem(string const &root);

    void run_init();

    void run_format();

    void run_import(const string &host_fileName, string const &fs_filename);

    void run_ls(const string &fileName);

    void save();

    void load();

    void run_export(string const &fs_filename, string const &host_filename);

    void run_mkdir(string const &directory);

    void run_rm(string const &filename);

    void run_copy(string const &source, string const &destination);

    void run_move(string const &source, string const &destination);

private:
    static const string FAT_FILENAME;
    static const string MAP_FILENAME;
    static const string CONFIG_FILENAME;
    string location;
    size_t blocksCount;
    size_t blockSize;
    vector<char> usedBlocks;
    Directory root;

    void read_config();

    bool good() const
    {
        return root.good();
    }

    size_t getNextFreeBlock() const
    {
        if (!good())
            return (size_t) - 1;

        auto t = std::find(usedBlocks.begin(), usedBlocks.end(), 0);
        if (t == usedBlocks.end())
            return (size_t) - 1;

        return t - usedBlocks.begin();
    }

    size_t getFreeBlocksCount() const
    {
        return (size_t)std::count_if(
            usedBlocks.begin(), usedBlocks.end(), [](char const & f) { return f == 0; });
    }



    void remove_file(Directory &dir, File &file);

    void remove_dir(Directory &dir, Directory &target);

    void copy_file(File const &file, Directory &targetDirectory, string const &);

    void copy_directory(Directory const &dir, Directory &targetDirectory);

    void copy_dir_to(Directory const &, const string &destination);

    void copy_file_to(File const &file, const string &destination);

    void move_file_to(File const &file, Directory *parentDir, const string &destination);

    void move_file(File const &file,
                   Directory *parentDir,
                   Directory &targetDirectory,
                   string const &newName);

    bool exists(Path const &path) {
        Directory *d = root.findLastDirectory(path);
        return d == nullptr ? false
                : d->existsFile(path.getFileName()) || d->existsDir(path.getFileName());
    }

    bool isDirectory(Path const &path) {
        Directory *d = root.findLastDirectory(path);
        return d == nullptr ? false : d->existsDir(path.getFileName());
    }

    bool isFile(Path const &path) {
        Directory *d = root.findLastDirectory(path);
        return d == nullptr ? false : d->existsFile(path.getFileName());
    }

};

#endif //__FileSystem_Hpp_
