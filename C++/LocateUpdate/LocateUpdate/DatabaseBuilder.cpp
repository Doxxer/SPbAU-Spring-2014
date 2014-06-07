#include "DatabaseBuilder.hpp"
#include "SuffixIterator.hpp"

void DatabaseBuilder::callback(boost::filesystem::path path) {
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