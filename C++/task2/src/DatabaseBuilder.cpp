#include "DatabaseBuilder.hpp"
#include "SuffixIterator.hpp"

void DatabaseBuilder::callback(boost::filesystem::path path)
{
    boost::lock_guard<boost::mutex> lock(mutex_);

    size_t path_position_in_db = utilities::write(outputFile_, path.string());

    // TODO ??? it doesn't work -- SEGFAULT
    // boost::copy(boost::make_iterator_range(
    //                 SuffixIterator(path.filename().string(), path_position_in_db), {}),
    //             std::back_inserter(suffixies_));

    // ugly workaround
    for (SuffixIterator suffix = SuffixIterator(path.filename().string(), path_position_in_db);
         suffix != SuffixIterator();
         ++suffix)
        suffixies_.push_back(*suffix);
}

void DatabaseBuilder::write_suffixies()
{
    typedef std::vector<size_t> refs;
    typedef std::pair<string, refs> suffix_refs;

    suffix_refs current = std::make_pair("", refs());
    std::vector<suffix_refs> compressed_suffixes;

    for (suffix const &s : suffixies_) {
        if (s.suff != current.first) {
            if (current.second.size() > 0)
                compressed_suffixes.push_back(current);
            current = std::make_pair(s.suff, refs());
        }
        current.second.push_back(s.position);
    }
    compressed_suffixes.push_back(current);

    for (suffix_refs const &s : compressed_suffixes) {
        utilities::write(outputFile_, s.first);
        utilities::write(outputFile_, s.second.size());
        for (size_t i = 0; i < s.second.size(); ++i) {
            utilities::write(outputFile_, s.second[i]);
        }
    }
}