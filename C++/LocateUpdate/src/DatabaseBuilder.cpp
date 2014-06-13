#include <boost/timer/timer.hpp>
#include <boost/tuple/tuple.hpp>
#include "DatabaseBuilder.hpp"
#include "Utilities.hpp"

using std::cout;
using std::endl;
using std::vector;

void DatabaseBuilder::build()
{
    ofstream outputFile(outputFileName_, std::ios_base::binary);
    utilities::write(outputFile, 0, 0);

    FileSystemWalker fileSystemWalker(root_);
    suffixies suff_array;
    vector<fs::path> pathes;
    boost::tie(pathes, suff_array) = fileSystemWalker.scan();

    vector<size_t> pathPositions(pathes.size());
    for (size_t i = 0; i < pathes.size(); ++i) {
        pathPositions[i] = utilities::write(outputFile, pathes[i].string());
    }
    tbb::parallel_sort(suff_array.begin(), suff_array.end());

    cout << "writing database..." << endl;
    utilities::write(outputFile, 0, outputFile.tellp());
    write_suffixies(suff_array, pathPositions, outputFile);
}

void DatabaseBuilder::write_suffixies(suffixies const &suff_array,
                                      vector<size_t> const &pathPositions,
                                      ofstream &outputFile)
{
    typedef vector<size_t> refs;
    typedef std::pair<string, refs> suffix_refs;

    suffix_refs current = std::make_pair("", refs());
    std::vector<suffix_refs> compressed_suffixes;

    for (suffix const &s : suff_array) {
        if (s.suff != current.first) {
            if (current.second.size() > 0)
                compressed_suffixes.push_back(current);
            current = std::make_pair(s.suff, refs());
        }
        current.second.push_back(pathPositions[s.position]);
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