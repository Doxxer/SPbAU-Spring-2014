#include "File.hpp"
#include "utilities.hpp"

#include <sstream>
#include <iomanip>

using std::endl;

string File::get_info() const
{
    std::stringstream ss;
    ss << "F ";
    ss << std::setw(11) << name << " | ";
    ss << std::setw(11) << size << " bytes"
       << " | ";
    ss << std::setw(6) << blocks.size() << " blocks"
       << " | ";
    ss << utils::tts(modified_time);
    return ss.str();
}

void File::load(ifstream &s, string const& location)
{
    size_t fileBlock;
    s.read(reinterpret_cast<char *>(&fileBlock), sizeof(fileBlock));
    addUsedBlock(fileBlock);

    ifstream fileStream(utils::pathAppend(location, std::to_string(fileBlock)), std::ios_base::binary);

    size_t nextBlock = utils::readMetadata(this, fileStream);
    while(nextBlock != 0) {
        blocks.push_back(nextBlock);
        ifstream in(utils::pathAppend(location, std::to_string(nextBlock)), std::ios_base::binary);
        nextBlock = utils::readNextBlockNumber(in);
    }
}

void File::save(ofstream &s) const
{
    if (blocks.size() < 1)
        return;
    size_t firstBlock = blocks[0];
    s.write(reinterpret_cast<const char *>(&firstBlock), sizeof(firstBlock));
}

void File::addUsedBlock(size_t block)
{
    blocks.push_back(block);
}
