#ifndef DATABASEBUILDER_HPP
#define DATABASEBUILDER_HPP

#include <string>
#include <fstream>
#include <exception>
#include <boost/filesystem.hpp>
#include <boost/thread.hpp>
#include <tbb/parallel_sort.h>
#include "FileSystemWalker.hpp"
#include "Utilities.hpp"

using std::string;
using std::ofstream;

class DatabaseBuilder {
private:
    string root_;
    string outputFileName_;

public:
    DatabaseBuilder(string const &root, string const &outputFileName)
        : root_(root), outputFileName_(outputFileName)
    {
        ofstream outputFile(outputFileName_, std::ios_base::binary);
        if (!outputFile)
            throw std::runtime_error("File " + outputFileName_ + " can't be opened");
    }

    void build();

private:
    void write_suffixies(suffixies const &suff_array,
                         vector<size_t> const &pathPositions,
                         ofstream &outputFile);
};

#endif /* end of include guard: DATABASEBUILDER_HPP */