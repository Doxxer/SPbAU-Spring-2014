#include <exception>
#include <string>
#include <iostream>
#include <boost/program_options.hpp>
#include <boost/timer/timer.hpp>
#include <thread>
#include "FileSystemWalker.h"
#include "ThreadPool.h"

using std::string;
using std::cout;
using std::cerr;
using std::endl;

ThreadPool pool;
boost::mutex mutex_;

int q = 0;

void printer(boost::filesystem::path p) {
    boost::lock_guard<boost::mutex> lock(mutex_);
    if (is_directory(p)) {
        pool.add_task(fs_scanner_worker(&printer, p));
    }
    q++;
    //cout << p.string() << endl;
}

void parse_parameters(int argc, char const *argv[], string &databaseRootPath, string &outputFile) {
    boost::program_options::options_description desc("Program options");
    desc.add_options()
            ("database-root", boost::program_options::value<string>(&databaseRootPath)->required(), "Indexation root directory")
            ("output", boost::program_options::value<string>(&outputFile)->required(), "Index file name");
    boost::program_options::variables_map vm;
    store(boost::program_options::command_line_parser(argc, argv).options(desc).run(), vm);
    try {
        notify(vm);
    }
    catch (std::exception const &e) {
        cerr << e.what() << endl;
        cerr << desc << endl;
        throw;
    }
}

int main(int argc, const char *argv[]) {
    string databaseRootPath, outputFile;

    try {
        parse_parameters(argc, argv, databaseRootPath, outputFile);
    }
    catch (...) {
        return 1;
    }

    cout << "Database: " << databaseRootPath << endl
            << "Output file: " << outputFile << endl;

    boost::timer::cpu_timer timer;

    pool.add_task(fs_scanner_worker(&printer, fs::path(databaseRootPath)));
    pool.wait_and_stop();

    boost::timer::cpu_times elapsed_times(timer.elapsed());

    std::cout << "scanning throught takes "
            << format(elapsed_times, 9)
            << " q = " << q
            << std::endl;

    return 0;
}
