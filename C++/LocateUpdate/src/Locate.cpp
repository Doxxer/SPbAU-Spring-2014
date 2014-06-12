#include <exception>
#include <string>
#include <iostream>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include "Utilities.hpp"
#include "DatabaseScanner.hpp"

using std::string;
using std::cout;
using std::cerr;
using std::endl;

void parse_parameters(int argc, char const *argv[], string &database, string &pattern)
{
    boost::program_options::options_description desc("Program options");
    desc.add_options()("database",
                       boost::program_options::value<string>(&database)->required(),
                       "Index file name");
    desc.add_options()("pattern",
                       boost::program_options::value<string>(&pattern)->required(),
                       "Pattern to search");
    boost::program_options::positional_options_description positional_options_description;
    positional_options_description.add("pattern", 1);
    boost::program_options::variables_map vm;

    store(boost::program_options::command_line_parser(argc, argv)
              .options(desc)
              .positional(positional_options_description)
              .allow_unregistered()
              .run(),
          vm);
    notify(vm);
}

int main(int argc, const char *argv[])
{
    string database, pattern;

    try
    {
        parse_parameters(argc, argv, database, pattern);
        DatabaseScanner scanner(database);
        for (boost::filesystem::path const &path : scanner.search(pattern)) {
            if (boost::filesystem::exists(path)) {
                cout << path.string() << endl;
            }
        }
    }
    catch (std::exception const &e)
    {
        cerr << e.what() << endl;
        return 1;
    }
    return 0;
}
