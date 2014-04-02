#include <iostream>
#include "FileSystem.hpp"

int main(int argc, char *argv[])
{
    if (argc < 2) {
        std::cerr << "incorrect parameters" << std::endl;
        return 1;
    }
    try
    {
        FileSystem fs(argv[1]);
        fs.run_format();
        fs.save();
    }
    catch (std::runtime_error const &e)
    {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    return 0;
}
