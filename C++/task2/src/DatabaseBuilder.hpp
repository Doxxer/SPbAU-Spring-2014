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
    boost::mutex mutex_;
    suffixies suffixies_;
    string outputFileName_;

public:
    DatabaseBuilder(string const &root, string const &outputFileName)
        : root_(root), outputFileName_(outputFileName)
    {
        ofstream outputFile(outputFileName_, std::ios_base::binary);
        if (!outputFile)
            throw std::runtime_error("File " + outputFileName_ + " can't be opened");
    }

    void build()
    {
        ofstream outputFile(outputFileName_, std::ios_base::binary);
        utilities::write(outputFile, 0, 0);
        cout << "scanning file system..." << endl;
        FileSystemWalker fileSystemWalker(root_, boost::bind(&DatabaseBuilder::callback, this, boost::ref(outputFile), _1));
        fileSystemWalker.scan();

        tbb::parallel_sort(suffixies_.begin(), suffixies_.end());

        cout << "writing database..." << endl;
        utilities::write(outputFile, 0, outputFile.tellp());
        write_suffixies(outputFile);
    }

private:
    void write_suffixies(ofstream& outputFile);

    // called when fs_scanner reach file or folder
    void callback(ofstream& outputFile, boost::filesystem::path path);
};

#endif /* end of include guard: DATABASEBUILDER_HPP */