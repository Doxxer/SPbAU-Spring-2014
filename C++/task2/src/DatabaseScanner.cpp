#include <algorithm>
#include <boost/range/algorithm/lower_bound.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/foreach.hpp>
#include "DatabaseScanner.hpp"

void DatabaseScanner::read_suffixes()
{
    index_database_.seekg(0, index_database_.end);
    std::streamoff end = index_database_.tellg();
    index_database_.seekg(0);
    index_database_.seekg(utilities::read(index_database_));

    while (index_database_.tellg() < end) {
        string suff = utilities::read_string(index_database_);
        size_t pos = index_database_.tellg();
        suffixies_.push_back({suff, pos});
        index_database_.seekg(utilities::read(index_database_) * sizeof(size_t),
                              std::ios_base::cur);
    }
    index_database_.seekg(0);
}

set<boost::filesystem::path> DatabaseScanner::search(string pattern)
{
    std::set<boost::filesystem::path> result;

    auto current_match = boost::lower_bound(suffixies_, suffix({pattern, 0}));

    while (boost::starts_with(current_match->suff, pattern)) {
        BOOST_FOREACH(boost::filesystem::path path, get_pathes(current_match->position))
        {
            result.insert(path);
        }
        ++current_match;
    }
    return result;
}

vector<boost::filesystem::path> DatabaseScanner::get_pathes(size_t position)
{
    index_database_.seekg(position);
    vector<boost::filesystem::path> result;

    set<size_t> files_positions;
    size_t count = utilities::read(index_database_);
    for (size_t i = 0; i < count; ++i) {
        files_positions.insert(utilities::read(index_database_));
    }

    for (size_t pos : files_positions) {
        index_database_.seekg(pos);
        string path = utilities::read_string(index_database_);
        result.push_back(boost::filesystem::path(path));
    }
    return result;
}