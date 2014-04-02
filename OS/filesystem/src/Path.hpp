#ifndef __Path_Hpp_
#define __Path_Hpp_

#include <string>
#include <vector>
#include <memory>

using std::string;
using std::vector;

class Path;

typedef std::shared_ptr<Path> PathPtr;

class Path {

public:
    Path(string const &path);

    string getFileName() const
    {
        if (directories.empty())
            return "";
        return directories.back();
    }

    vector<string> const &getSplittedPath() const
    {
        return directories;
    }

    PathPtr getParentPath() const
    {
        string q;
        for (int i = 0; i < (int)directories.size() - 1; ++i)
            q += directories[i] + " ";
        return PathPtr(new Path(q));
    }

private:
    vector<string> directories;
};

#endif //__Path_H_
