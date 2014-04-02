#include <sstream>
#include <iostream>
#include "Directory.hpp"
#include "utilities.hpp"

using std::endl;

string Directory::get_info()
{
    std::stringstream ss;
    for (auto it = directories.begin(); it != directories.end(); ++it) {
        ss << "D " << it->first << endl;
    }
    for (auto it = files.begin(); it != files.end(); ++it) {
        ss << it->second.get_info() << endl;
    }
    return ss.str();
}

void Directory::load(ifstream &s, string const &location)
{
    name = utils::read_string(s);
    s.read(reinterpret_cast<char *>(&modified_time), sizeof(modified_time));
    size_t d, f;
    s.read(reinterpret_cast<char *>(&d), sizeof(d));
    s.read(reinterpret_cast<char *>(&f), sizeof(f));
    for (size_t i = 0; i < d; i++) {
        Directory directory;
        directory.load(s, location);
        directories[directory.name] = directory;
    }
    for (size_t i = 0; i < f; i++) {
        File file;
        file.load(s, location);
        files[file.name] = file;
    }
}

void Directory::save(ofstream &s) const
{
    utils::write_string(s, name);
    s.write(reinterpret_cast<const char *>(&modified_time), sizeof(modified_time));
    size_t dirSize = directories.size();
    s.write(reinterpret_cast<const char *>(&dirSize), sizeof(dirSize));
    size_t filesSize = files.size();
    s.write(reinterpret_cast<const char *>(&filesSize), sizeof(filesSize));
    for (auto it = directories.begin(); it != directories.end(); ++it) {
        it->second.save(s);
    }
    for (auto it = files.begin(); it != files.end(); ++it) {
        it->second.save(s);
    }
}

Directory *Directory::findLastDirectory(Path const &path)
{
    Directory *current_dir = this;

    vector<string> const &t = path.getSplittedPath();

    for (int i = 0; i < (int)t.size() - 1; ++i) {
        if (current_dir->directories.find(t[i]) == current_dir->directories.end())
            return nullptr;
        current_dir = &current_dir->directories[t[i]];
    }
    return current_dir;
}

void Directory::fillUsedBlocks(vector<char> &used)
{
    for (auto d : getAllDirectories())
        d.fillUsedBlocks(used);
    for (auto f : getAllFiles())
        for (auto b : ((File)f).blocks)
            used[b] = 1;
}
