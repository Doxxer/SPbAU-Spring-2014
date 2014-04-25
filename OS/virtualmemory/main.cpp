#include <iostream>
#include <cstdint>
#include <vector>
#include <stdexcept>

using namespace std;
typedef vector<uint64_t> table;

void readTable(table &t)
{
    size_t tableLen;
    uint64_t temp;
    cin >> dec >> tableLen;
    for (size_t i = 0; i < tableLen; ++i) {
        cin >> hex >> temp;
        t.push_back(temp);
    }
}

template <typename T> T getBits(T address, size_t p, size_t count = 1)
{
    return (address >> p) & ~(~((T)0) << count);
}

uint32_t segmentation(uint32_t offset, uint16_t segmentSelector, table const &GDT, table const &LDT)
{
    bool TI_flag = getBits(segmentSelector, 2) == 1;

    table const &t = (TI_flag == 0) ? GDT : LDT;
    uint16_t index = getBits(segmentSelector, 3, 13);

    if (TI_flag == 0 && index == 0)
        throw runtime_error("Get 0 entry from GTD");

    uint64_t segment_descriptor = t[index];

    if (getBits(segment_descriptor, 7) == 0)
        throw runtime_error("Segment is not present");

    bool granularity = (getBits(segment_descriptor, 23 + 32) == 1);
    uint32_t limit =
        ((getBits(segment_descriptor, 16 + 32, 4) << 16) + getBits(segment_descriptor, 0, 16))
        << (granularity ? 12 : 0);

    if (offset > limit)
        throw runtime_error("Offset greater than the segment limit");

    uint32_t base = (getBits(segment_descriptor, 24 + 32, 8) << 24) +
                    (getBits(segment_descriptor, 0 + 32, 8) << 16) +
                    (getBits(segment_descriptor, 16, 16));
    return base + offset;
}

uint32_t paging(uint32_t linear_address, table const &PDT, table const &PTT)
{
    uint16_t directory = getBits(linear_address, 22, 10);
    uint16_t table = getBits(linear_address, 12, 10);
    uint16_t offset = getBits(linear_address, 0, 12);

    uint64_t PDE = PDT[directory], PTE = PTT[table];

    if (getBits(PDE, 0) == 0)
        throw runtime_error("Page directory entry is not present");
    if (getBits(PTE, 0) == 0)
        throw runtime_error("Page table entry is not present");

    uint32_t page_frame = getBits(PTE, 12, 20) << 12;
    return page_frame + offset;
}

int main()
{
    uint32_t logicalAddress;
    uint16_t segmentSelector;
    table GDT, LDT, PDT, PTT;

    cin >> hex >> logicalAddress >> segmentSelector;
    readTable(GDT);
    readTable(LDT);
    readTable(PDT);
    readTable(PTT);

    try
    {
        uint32_t linear_address = segmentation(logicalAddress, segmentSelector, GDT, LDT);
        uint32_t physical_address = paging(linear_address, PDT, PTT);
        cout << hex << physical_address << endl;
    }
    catch (runtime_error const &_)
    {
        cout << "INVALID" << endl;
    }
    return 0;
}
