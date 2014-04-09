#include <utility>
#include <algorithm>
#include <climits>
#include "MemoryAllocator.hpp"

std::pair<MCB *, MCB *> MemoryAllocator::findFirstFreeSpaceBetweenBlocksGEQThan(int size) const
{
    MCB *current = firstMCB;
    MCB *next = current->next;

    int minResult = INT_MAX;
    MCB *minCurrent = nullptr;
    MCB *minNext = nullptr;

    while (next) {
        int freeSizeBetweenBlocks = next->owner - current->owner - current->size;
        if (freeSizeBetweenBlocks >= size) {
            if (minResult > freeSizeBetweenBlocks) {
                minResult = freeSizeBetweenBlocks;
                minCurrent = current;
                minNext = next;
            }
        }
        current = next;
        next = current->next;
    }
    return std::make_pair(minCurrent, minNext);
}

int MemoryAllocator::calcMaxFreeSpaceBetweenBlocks()
{
    MCB *current = firstMCB;
    MCB *next = current->next;

    int pMax = INT_MIN;
    while (next) {
        pMax = std::max(pMax, next->owner - current->owner - current->size);
        current = next;
        next = current->next;
    }

    return std::max(-1, pMax);
}

int MemoryAllocator::allocateMax()
{
    int right = calcRightFreeSpace();
    if (right < 0)
        return -1;
    return std::max(calcMaxFreeSpaceBetweenBlocks(), right);
}

int MemoryAllocator::allocate(int size)
{
    if (size > allocateMax())
        return -1;

    auto mcb = createNewMCB();
    auto block = findFirstFreeSpaceBetweenBlocksGEQThan(size);

    if (block.first == nullptr)
        block = std::make_pair(getMinMCB(), nullptr);

    insertMCB(block.first, mcb, block.second);
    mcb->size = size;
    mcb->owner = block.first->owner + block.first->size;
    userMemorySize += mcb->size;
    userBlocksReserved++;
    return mcb->owner;
}

MCB *MemoryAllocator::createNewMCB()
{
    MCB *res = reinterpret_cast<MCB *>(data + memorySize - (userBlocksReserved + 2) * sizeof(MCB));
    res->owner = 0;
    res->prev = 0;
    res->next = 0;
    res->size = 0;
    return res;
}

void MemoryAllocator::insertMCB(MCB *prev, MCB *cur, MCB *next)
{
    // remove cur
    if (cur->prev)
        cur->prev->next = cur->next;
    if (cur->next)
        cur->next->prev = cur->prev;

    // insert cur
    cur->prev = prev;
    cur->next = next;
    prev->next = cur;
    if (next)
        next->prev = cur;
}

std::string MemoryAllocator::getMap() const
{
    std::string str(memorySize, 'f');
    MCB *current = firstMCB;
    auto it = str.rbegin();
    while (current) {
        auto dataIt = str.begin() + current->owner;
        it = std::fill_n(it, sizeof(MCB), 'm');
        std::fill_n(dataIt, current->size, 'u');
        current = current->next;
    }

    return str;
}

bool MemoryAllocator::free(int address)
{
    MCB *mcb = findMCBWithSpecificUserAddress(address);
    if (!mcb)
        return false;

    userMemorySize -= mcb->size;
    userBlocksReserved--;
    removeMCB(mcb);
    return true;
}

void MemoryAllocator::removeMCB(MCB *mcb)
{
    MCB *prev = mcb->prev;
    MCB *next = mcb->next;
    prev->next = next;
    if (next != 0)
        next->prev = prev;
    mcb->next = 0;
    mcb->prev = 0;

    // find min
    MCB *min = getMinMCB();
    if (min != mcb) {
        if (min->prev)
            min->prev->next = mcb;
        if (min->next)
            min->next->prev = mcb;

        mcb->owner = min->owner;
        mcb->size = min->size;
        mcb->next = min->next;
        mcb->prev = min->prev;

        min->next = 0;
        min->owner = 0;
        min->prev = 0;
        min->size = 0;
    }
}