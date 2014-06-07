#ifndef UTILITIES_HPP
#define UTILITIES_HPP

#include <string>
#include <fstream>
#include <vector>
#include <utility>

using std::string;
using std::ofstream;
using std::ifstream;

struct suffix {
    string suff;
    size_t position;
};

typedef std::vector<suffix> suffixies;

namespace utilities {

size_t write(ofstream &, string const &);

void write(ofstream &, size_t);

size_t read(ifstream &);

string read_string(ifstream &);
}

#endif /* end of include guard: UTILITIES_HPP */
