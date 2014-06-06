#include <exception>
#include <string>
#include <iostream>
#include <boost/program_options.hpp>

namespace po = boost::program_options;

using std::string;
using std::cout;
using std::cerr;
using std::endl;

int main(int argc, const char *argv[]) {
    string databaseRootPath, outputFile;
    po::options_description desc("Program options");
    desc.add_options()
            ("database-root", po::value<string>(&databaseRootPath)->required(), "Indexation root directory")
            ("output", po::value<string>(&outputFile)->required(), "Index file name");
    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).options(desc).run(), vm);
    try {
        po::notify(vm);
    }
    catch (std::exception const &e) {
        cerr << e.what() << endl;
        return 1;
    }


    cout << "Database: " << databaseRootPath << endl
            << "Output file: " << outputFile << endl;
    return 0;
}
