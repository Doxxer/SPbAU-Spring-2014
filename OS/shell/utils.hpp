#ifndef UTILS_HPP
#define UTILS_HPP

inline bool IsNumeric(const char *str)
{
    for (; *str; str++)
        if (*str < '0' || *str > '9')
            return 0;
    return 1;
}

inline void trim(std::string &s)
{
    while (isspace(*s.begin()))
        s.erase(s.begin());
    while (s.size() > 0 && isspace(*s.rbegin()))
        s.erase(s.end() - 1);
}

#endif /* end of include guard: UTILS_HPP */

