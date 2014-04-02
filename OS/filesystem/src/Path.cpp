#include "Path.hpp"

#include <sstream>
#include <algorithm>
#include <iterator>

using std::stringstream;

Path::Path(string const &path)
{
    string s(path);
    std::replace(s.begin(), s.end(), '/', ' ');
    stringstream ss(s);
    directories.assign(std::istream_iterator<string>(ss), std::istream_iterator<string>());
}