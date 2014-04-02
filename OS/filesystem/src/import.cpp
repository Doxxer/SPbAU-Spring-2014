#include <iostream>
#include "FileSystem.hpp"

int main(int argc, char *argv[])
{
    if (argc < 4) {
        std::cerr << "incorrect parameters" << std::endl;
        return 1;
    }
    try
    {
        FileSystem fs(argv[1]);
        fs.load();
        fs.run_import(argv[2], argv[3]);
        fs.save();
    }
    catch (std::runtime_error const &e)
    {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    return 0;
}
