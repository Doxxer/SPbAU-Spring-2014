#include "DatabaseBuilder.hpp"
#include "SuffixIterator.hpp"
#include "Utilities.hpp"

using std::cout;
using std::endl;

void DatabaseBuilder::build()
{
    ofstream outputFile(outputFileName_, std::ios_base::binary);
    utilities::write(outputFile, 0, 0);
    cout << "scanning file system..." << endl;

    FileSystemWalker fileSystemWalker(root_);
    vector<fs::path> &&pathes = fileSystemWalker.scan();

    suffixies suff_array;
    
    for (fs::path const &path : pathes) {
        size_t path_position_in_db = utilities::write(outputFile, path.string());
        boost::copy(boost::make_iterator_range(
                        SuffixIterator(path.filename().string(), path_position_in_db), {}),
                    std::back_inserter(suff_array));
    }
    
    tbb::parallel_sort(suff_array.begin(), suff_array.end());
    
    cout << "writing database..." << endl;
    utilities::write(outputFile, 0, outputFile.tellp());
    write_suffixies(suff_array, outputFile);
}

void DatabaseBuilder::write_suffixies(suffixies const &suff_array, ofstream &outputFile)
{
    typedef std::vector<size_t> refs;
    typedef std::pair<string, refs> suffix_refs;

    suffix_refs current = std::make_pair("", refs());
    std::vector<suffix_refs> compressed_suffixes;

    for (suffix const &s : suff_array) {
        if (s.suff != current.first) {
            if (current.second.size() > 0)
                compressed_suffixes.push_back(current);
            current = std::make_pair(s.suff, refs());
        }
        current.second.push_back(s.position);
    }
    compressed_suffixes.push_back(current);

    for (suffix_refs const &s : compressed_suffixes) {
        utilities::write(outputFile, s.first);
        utilities::write(outputFile, s.second.size());
        for (size_t i = 0; i < s.second.size(); ++i) {
            utilities::write(outputFile, s.second[i]);
        }
    }
}