#ifndef DATABASEBUILDER_HPP
#define DATABASEBUILDER_HPP

#include <string>
#include <fstream>
#include <exception>
#include <boost/filesystem.hpp>
#include <boost/thread.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <tbb/parallel_sort.h>
#include "FileSystemWalker.hpp"
#include "Utilities.hpp"

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
    string outputFileName_;

public:
    DatabaseBuilder(string const &root, string const &outputFile)
        : root_(root), outputFileName_(outputFile)
    {
        outputFile_ = ofstream(outputFileName_, std::ios_base::binary);
        if (!outputFile_)
            throw std::runtime_error("File " + outputFile + " can't be opened");
        outputFile_.close();
    }

    void build()
    {
        outputFile_.open(outputFileName_, std::ios_base::binary);
        utilities::write(outputFile_, 0, 0);
        cout << "scanning file system..." << endl;
        FileSystemWalker fileSystemWalker(root_, boost::bind(&DatabaseBuilder::callback, this, _1));
        fileSystemWalker.scan();

        tbb::parallel_sort(suffixies_.begin(), suffixies_.end());

        cout << "writing database..." << endl;
        utilities::write(outputFile_, 0, outputFile_.tellp());
        write_suffixies();
        outputFile_.close();
    }

private:
    void write_suffixies();

    // called when fs_scanner reach file or folder
    void callback(boost::filesystem::path path);
};

#endif /* end of include guard: DATABASEBUILDER_HPP */