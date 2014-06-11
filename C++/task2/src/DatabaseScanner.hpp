#ifndef DATABASESCANNER_HPP
#define DATABASESCANNER_HPP

#include <string>
#include <fstream>
#include <iostream>
#include <set>
#include <vector>
#include <boost/filesystem.hpp>
#include "Utilities.hpp"

using std::string;
using std::ifstream;
using std::vector;
using std::set;
using std::cout;
using std::endl;

class DatabaseScanner {
private:
    ifstream index_database_;
    suffixies suffixies_;

public:
    DatabaseScanner(string const &index_database)
        : index_database_(index_database, std::ios_base::binary)
    {
        if (!index_database_)
            throw std::runtime_error("File " + index_database + " can't be opened");
        cout << "reading database... " << endl;
        read_suffixes();
    }

    set<boost::filesystem::path> search(string pattern);

private:
    void read_suffixes();

    vector<boost::filesystem::path> get_pathes(size_t position);
};

#endif /* end of include guard: DATABASESCANNER_HPP */
