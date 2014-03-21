#ifndef UTILITIES_HPP
#define UTILITIES_HPP

#include <string>
namespace utils {

std::string pathAppend(std::string const &path, std::string const &folder)
{
    std::string resultPath(path);
    if (resultPath[resultPath.length()] != '/')
        resultPath += '/';
    return resultPath + folder;
}


}

#endif /* end of include guard: UTILITIES_HPP */
