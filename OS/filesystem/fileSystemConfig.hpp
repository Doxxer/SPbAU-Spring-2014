#ifndef FILESYSTEMCONFIG_HPP
#define FILESYSTEMCONFIG_HPP

#include <cstddef>
#include <string>
#include <fstream>
#include <memory>

#include "utilities.hpp"

struct FileSystemConfig;
typedef std::shared_ptr<FileSystemConfig> FileSystemConfigPtr;

struct FileSystemConfig {
    size_t blockSize;
    size_t blockCount;

    FileSystemConfig(size_t blockSize, size_t blockCount)
        : blockSize(blockSize), blockCount(blockCount)
    {
    }

    static FileSystemConfigPtr read(std::string const &path)
    {
        std::ifstream f;
        f.open(utils::pathAppend(path, "config"));
        if (f.is_open()) {
            int size, count;
            f >> size >> count;
            if (count <= 0 || size < 1024)
                return NULL;
            return FileSystemConfigPtr(new FileSystemConfig(size, count));
        }
        return NULL;
    }
};

#endif /* end of include guard: FILESYSTEMCONFIG_HPP */
