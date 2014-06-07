#include <exception>
#include <string>
#include <iostream>
#include <boost/program_options.hpp>
#include <boost/timer/timer.hpp>
#include "DatabaseBuilder.hpp"
#include "utilities.hpp"

using std::string;
using std::cout;
using std::cerr;
using std::endl;

/*
FILE FORMAT:
<HEADER> -- позиция, следующая за последним абсолютным путем: начало списка суффиксов
<size_1><Absolute path 1> -- полные пути к файлам/папкам (вместе с размером, чтобы можно было
прочитать utilities::read_string)
<size_2><Absolute path 2>
...
<size_n><Absolute path n>
<SUFFIX_1:string><number of refernces><reference1><reference2>...<reference_n> -- суффикс, число
ссылок на полные пути, сами ссылки в виде позиций в файле
<SUFFIX_2:string><number of refernces><reference1><reference2>...<reference_n>
*/

void parse_parameters(int argc, char const *argv[], string &databaseRootPath, string &outputFile)
{
    boost::program_options::options_description desc("Program options");
    desc.add_options()("database-root",
                       boost::program_options::value<string>(&databaseRootPath)->required(),
                       "Indexation root directory")(
        "output",
        boost::program_options::value<string>(&outputFile)->required(),
        "Index file name");
    boost::program_options::variables_map vm;
    store(boost::program_options::command_line_parser(argc, argv).options(desc).run(), vm);
    try
    {
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
    string databaseRootPath, outputFile;

    try
    {
        parse_parameters(argc, argv, databaseRootPath, outputFile);
    }
    catch (...)
    {
        return 1;
    }

    try
    {
        cout << "Database: " << databaseRootPath << endl << "Output file: " << outputFile << endl;
        boost::timer::cpu_timer timer;

        DatabaseBuilder databaseBuilder(databaseRootPath, outputFile);
        databaseBuilder.build();

        // TODO revove
        std::ifstream f(outputFile);
        utilities::read(f);
        for (size_t i = 0; i < 2; ++i) {
            cout << utilities::read_string(f) << endl;
        }

        while (f.good()) {
            cout << utilities::read_string(f);
            size_t rc = utilities::read(f);
            cout << " rc = " << rc;
            for (size_t i = 0; i < rc; ++i) {
                cout << " #" << utilities::read(f);
            }
            cout << endl;
        }

        // END TODO

        boost::timer::cpu_times elapsed_times(timer.elapsed());
        cout << endl << "scanning throught takes " << format(elapsed_times, 9) << endl;
    }
    catch (std::exception const &e)
    {
        cerr << e.what() << endl;
        return 1;
    }
    return 0;
}
