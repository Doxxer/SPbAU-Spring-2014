#include <exception>
#include <string>
#include <iostream>
#include <boost/program_options.hpp>
#include "utilities.hpp"

using std::string;
using std::cout;
using std::cerr;
using std::endl;

void parse_parameters(int argc, char const *argv[], string &database, string &pattern)
{
    boost::program_options::options_description desc("Program options");
    desc.add_options()("database",
                       boost::program_options::value<string>(&database)->required(),
                       "Index file name")(
        "pattern",
        boost::program_options::value<string>(&pattern)->required(),
        "Pattern to search");
    boost::program_options::positional_options_description positional_options_description;
    positional_options_description.add("pattern", 1);
    boost::program_options::variables_map vm;

    try
    {
        store(boost::program_options::command_line_parser(argc, argv)
                  .options(desc)
                  .positional(positional_options_description)
                  .allow_unregistered()
                  .run(),
              vm);
        notify(vm);
    }
    catch (std::exception const &e)
    {
        cerr << e.what() << endl;
        cerr << desc << endl;
        throw;
    }
}

int main(int argc, const char *argv[])
{
    string database, pattern;

    try
    {
        parse_parameters(argc, argv, database, pattern);
    }
    catch (...)
    {
        return 1;
    }

    try
    {
        // TODO add code here
        cout << "Database: " << database << " | pattern = " << pattern << endl;
    }
    catch (std::exception const &e)
    {
        cerr << e.what() << endl;
        return 1;
    }
    return 0;
}
