#include <string>
#include <cstdint>
#include <vector>
#include <fstream>

#include <iostream>

#include "fileSystemConfig.hpp"

int main(int argc, char *argv[])
{
    if (argc != 2)
        return 1;

    std::string dirname(argv[1]);
    FileSystemConfigPtr config = FileSystemConfig::read(dirname);

    if (config == NULL)
        return 2;

    std::vector<char> buffer(config->blockSize, 0);
    for (size_t i = 0; i < config->blockCount; ++i) {
        std::ofstream file;
        file.open(utils::pathAppend(dirname, std::to_string(i)), std::ios::out | std::ios::binary);
        if (!file.is_open())
            return 1;
        file.write(buffer.data(), config->blockSize);
    }

    return 0;
}
