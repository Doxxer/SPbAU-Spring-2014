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
using std::cout;
using std::endl;

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
        utilities::write(outputFile_, 0, 0);
        cout << "scanning file system..." << endl;
        FileSystemWalker fileSystemWalker(root_, boost::bind(&DatabaseBuilder::callback, this, _1));
        fileSystemWalker.scan();

        // TODO parallel sort ???
        cout << "sorting..." << endl;
        sort(suffixies_.begin(), suffixies_.end());        
        
        cout << "writing database..." << endl;
        utilities::write(outputFile_, 0, outputFile_.tellp());
        write_suffixies();
        outputFile_.close();
    }

    void write_suffixies();

    // called when fs_scanner reach file or folder
    void callback(boost::filesystem::path path);
};

#endif /* end of include guard: DATABASEBUILDER_HPP */