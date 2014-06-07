#ifndef DATABASEBUILDER_HPP
#define DATABASEBUILDER_HPP

#include <string>
#include <fstream>
#include <exception>
#include <boost/filesystem.hpp>
#include <boost/thread.hpp>
#include <boost/range/algorithm/copy.hpp>
#include "FileSystemWalker.hpp"
#include "utilities.hpp"

using std::string;
using std::ofstream;

class DatabaseBuilder {
private:
    string root_;
    ofstream outputFile_;
    boost::mutex mutex_;
    suffixies suffixies_;

public:
    DatabaseBuilder(string const &root, string const &outputFile)
            : root_(root), outputFile_(outputFile)
    {
        if (!outputFile_)
            throw std::runtime_error("File " + outputFile + "can't be opened");
    }

    void build()
    {
        write_suffixies_offset(0);
        FileSystemWalker fileSystemWalker(root_, boost::bind(&DatabaseBuilder::callback, this, _1));
        fileSystemWalker.scan();

        // TODO sort

        write_suffixies_offset(outputFile_.tellp());
        // write_suffixies();        
    }

    void write_suffixies_offset(size_t offset)
    {
        size_t pos = outputFile_.tellp();
        outputFile_.seekp(0);
        utilities::write(outputFile_, offset);
        if (pos > 0)
            outputFile_.seekp(pos);
    }

    void write_suffixies() {

    }

    // called when fs_scanner reach file or folder
    void callback(boost::filesystem::path path);
};

#endif /* end of include guard: DATABASEBUILDER_HPP */