#include <utility>
#include <algorithm>
#include <climits>
#include "MemoryAllocator.hpp"

MCB *MemoryAllocator::findFirstNonusedMCB() const
{
    MCB *current = firstMCB;
    while (current) {
        if (!current->used)
            return current;
        current = current->next;
    }
    return nullptr;
}

std::pair<MCB *, MCB *> MemoryAllocator::calcFreeSpaceBetweenBlocks(int size) const
{
    MCB *current = firstMCB;
    MCB *next = current->next;

    int minResult = INT_MAX;
    MCB *minCurrent = nullptr;
    MCB *minNext = nullptr;

    while (next) {
        if (!next->used) {
            next = next->next;
            continue;
        }
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

    int pMax = INT_MAX;
    while (next) {
        if (!next->used) {
            next = next->next;
            continue;
        }
        pMax = std::min(pMax, next->owner - current->owner - current->size);
        current = next;
        next = current->next;
    }

    return pMax == INT_MAX ? -1 : pMax;
}

int MemoryAllocator::allocateMax()
{
    auto mcb = findFirstNonusedMCB();
    int right = calcRightFreeSpace(mcb);

    if (right < 0)
        return -1;
    return std::max(calcMaxFreeSpaceBetweenBlocks(), right);
}

int MemoryAllocator::allocate(int size)
{
    if (size > allocateMax())
        return -1;

    auto mcb = findFirstNonusedMCB();
    auto block = calcFreeSpaceBetweenBlocks(size);

    if (mcb == nullptr)
        mcb = createNewMCB();
    if (block.first == nullptr)
        block = std::make_pair(getLastUsedMCB(), nullptr);

    insertMCB(block.first, mcb, block.second);
    mcb->used = true;
    mcb->size = size;
    mcb->owner = block.first->owner + block.first->size;
    userMemorySize += mcb->size;
    return mcb->owner;
}

MCB *MemoryAllocator::createNewMCB()
{
    MCB *res =
        reinterpret_cast<MCB *>(data + memorySize - (getNumberOfLastUsedBlock() + 1) * sizeof(MCB));
    res->used = false;
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
        if (current->used)
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
    mcb->used = false;
    userMemorySize -= mcb->size;
    if (mcb == getLastMCB())
        removeMCB(mcb);
    return true;
}

MCB *MemoryAllocator::findMCBWithSpecificUserAddress(int address) const
{
    auto cur = firstMCB->next;
    while (cur) {
        if (cur->used && cur->owner == address)
            return cur;
        cur = cur->next;
    }
    return nullptr;
}

void MemoryAllocator::removeMCB(MCB *mcb)
{
    MCB *prev = mcb->prev;
    MCB *next = mcb->next;
    prev->next = next;
    if (next != 0)
        next->prev = prev;
}