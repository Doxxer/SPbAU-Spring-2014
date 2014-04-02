#ifndef __Directory_Hpp_
#define __Directory_Hpp_

#include <string>
#include <map>
#include <utility>
#include <algorithm>
#include <fstream>
#include <stdexcept>
#include <ctime>

#include "File.hpp"
#include "Path.hpp"

using std::string;
using std::ifstream;
using std::ofstream;
using std::map;

class Directory {

public:
    Directory() : modified_time(time(0))
    {
    }

    explicit Directory(string const &name) : name(name), modified_time(time(0))
    {
    }

    // Directory(Directory const &other)
    //     : name(other.name),
    //       modified_time(other.modified_time),
    //       files(other.files),
    //       directories(other.directories)
    // {
    // }

    // void swap(Directory &directory)
    // {
    //     using std::swap;
    //     swap(name, directory.name);
    //     swap(modified_time, directory.modified_time);
    //     swap(files, directory.files);
    //     swap(directories, directory.directories);
    // }
    // 
    // Directory &operator=(Directory const &other)
    // {
    //     if (&other != this)
    //         Directory(other).swap(*this);
    //     return *this;
    // }
    // 
    string get_info();

    void load(ifstream&, string const& location);

    void save(ofstream &) const;

    Directory &getSubDir(string const &name)
    {
        if (existsDir(name))
            return directories[name];
        throw std::runtime_error("There is no subdirectory with name " + name);
    }

    File &getFile(string const &name)
    {
        if (existsFile(name))
            return files[name];
        throw std::runtime_error("There is no file with name " + name + " in the directory " +
                                 this->name);
    }

    const string &getName() const
    {
        return name;
    }

    bool good() const
    {
        return !name.empty();
    }

    bool existsFile(string const &f) const
    {
        return files.find(f) != files.end();
    }

    bool existsDir(string const &f) const
    {
        return directories.find(f) != directories.end() || (this->getName() == "/" && f.empty());
    }

    void addFile(File const &f)
    {
        if (existsDir(f.name))
            throw std::runtime_error("There is a directory with the same name");

        files[f.name] = f;
    }

    void removeFile(File const &f)
    {
        files.erase(f.name);
    }

    void removeDir(Directory const &d)
    {
        directories.erase(d.getName());
    }

    void addDirectory(Directory const &d)
    {
        if (existsFile(d.getName()))
            throw std::runtime_error("There is a directory with the same name");
        directories[d.getName()] = d;
    }

    Directory *findLastDirectory(Path const &);

    Directory *findDirectory(Path const &path)
    {
        Directory *d = findLastDirectory(path);
        if (!d->existsDir(path.getFileName()))
            throw std::runtime_error("Directory not found");
        return &d->directories[path.getFileName()];
    }

    File *findFile(Path const &path)
    {
        Directory *d = findLastDirectory(path);
        if (!d->existsFile(path.getFileName()))
            throw std::runtime_error("FIle not found");
        return &d->files[path.getFileName()];
    }

    vector<Directory> getAllDirectories() const
    {
        vector<Directory> res;
        for (auto &d : directories)
            res.push_back(d.second);
        return res;
    }

    vector<File> getAllFiles() const
    {
        vector<File> res;
        for (auto &f : files)
            res.push_back(f.second);
        return res;
    }

    void fillUsedBlocks(vector<char> &);

private:
    string name;
    time_t modified_time;
    map<string, File> files;
    map<string, Directory> directories;
};

#endif //__Directory_Hpp_
