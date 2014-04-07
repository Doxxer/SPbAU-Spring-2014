#ifndef __MemoryAllocator_Hpp_
#define __MemoryAllocator_Hpp_

#include <string>

struct MCB {
    MCB *prev;
    MCB *next;
    bool used;
    int owner;
    int size;
};

class MemoryAllocator {

public:
    MemoryAllocator(int N)
        : data(new char[N]),
          memorySize(N),
          userMemorySize(0),
          firstMCB(reinterpret_cast<MCB *>(data + memorySize - sizeof(MCB)))
    {
        firstMCB->next = nullptr;
        firstMCB->prev = nullptr;
        firstMCB->owner = 0;
        firstMCB->size = 0;
        firstMCB->used = true;
    }

    virtual ~MemoryAllocator()
    {
        delete[] data;
    }

    int getUsedBlocksCount() const
    {
        int res = 0;
        MCB *current = firstMCB;
        while (current) {
            if (current->used)
                ++res;
            current = current->next;
        }
        return res - 1;
    }

    int getUsedMemory() const
    {
        return userMemorySize;
    }

    int allocate(int size);

    int allocateMax();

    bool free(int address);

    std::string getMap() const;

private:
    char *data;
    int memorySize;
    int userMemorySize;
    MCB *const firstMCB;

    int getNumberOfLastUsedBlock() const
    {
        return getNumber(getLastUsedMCB());
    }

    MCB *findFirstNonusedMCB() const;

    std::pair<MCB *, MCB *> calcFreeSpaceBetweenBlocks(int size) const;

    MCB *getLastMCB() const
    {
        MCB *current = firstMCB;
        while (current->next) {
            current = current->next;
        }
        return current;
    }

    MCB *getLastUsedMCB() const
    {
        MCB *current = firstMCB;
        MCB *next = current->next;
        while (next) {
            if (!next->used) {
                next = next->next;
                continue;
            }
            current = next;
            next = current->next;
        }
        return current;
    }

    int getNumber(MCB *mcb) const
    {
        int k = 0;
        while (mcb) {
            ++k;
            mcb = mcb->prev;
        }
        return k;
    }

    int calcRightFreeSpace(MCB *mcb) const
    {
        auto lastUsed = getLastUsedMCB();

        int res = memorySize - lastUsed->owner - lastUsed->size - getNumber(lastUsed) * sizeof(MCB);
        if (!mcb || (mcb && getNumber(mcb) > getNumber(lastUsed)))
            return res - sizeof(MCB);
        return res;
    }

    MCB *createNewMCB();

    void insertMCB(MCB *prev, MCB *cur, MCB *next);

    MCB *findMCBWithSpecificUserAddress(int address) const;

    void removeMCB(MCB *mcb);

    int calcMaxFreeSpaceBetweenBlocks();
};

#endif //__MemoryAllocator_H_
