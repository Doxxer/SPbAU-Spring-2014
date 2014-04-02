#ifndef UTILITIES_HPP
#define UTILITIES_HPP

#include <string>
#include <fstream>
#include <stdexcept>
#include <iterator>
#include <algorithm>
#include <ctime>
#include "File.hpp"

using std::ifstream;
using std::ofstream;
using std::string;

namespace utils {
    std::string pathAppend(string const &path, string const &folder = "");

    void write_file_to_block_with_meta(ifstream &inputStream, string const &blockFile, size_t size, File const &);

    void write_file_to_block(ifstream &inputStream, string const &blockFile, size_t size);

    void writeFile(ifstream &input, ofstream &output, size_t size);

    void read_from_block(ofstream &outputStream, string const &blockFile, size_t size, bool);

    void copy_block_to_block(string const &blockFileIn, string const &blockFileOut, size_t size);

    string tts(time_t);

    size_t readMetadata(File *file, ifstream &in);

    size_t readNextBlockNumber(ifstream &);

    void overwrite_nextBlockNumber(const string &path, size_t blockNumber);

    void overwrite_metadata(File const &file, string const &path);

    string read_string(ifstream &in);

    size_t write_string(ofstream &s, string const &str);
}

#endif /* end of include guard: UTILITIES_HPP */
