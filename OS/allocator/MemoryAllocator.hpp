#ifndef __MemoryAllocator_Hpp_
#define __MemoryAllocator_Hpp_

#include <string>

struct MCB {
    MCB *prev;
    MCB *next;
    int owner;
    int size;
};

class MemoryAllocator {

public:
    MemoryAllocator(int N)
        : data(new char[N]),
          memorySize(N),
          userMemorySize(0),
          userBlocksReserved(0),
          firstMCB(reinterpret_cast<MCB *>(data + memorySize - sizeof(MCB)))
    {
        firstMCB->next = nullptr;
        firstMCB->prev = nullptr;
        firstMCB->owner = 0;
        firstMCB->size = 0;
    }

    virtual ~MemoryAllocator()
    {
        delete[] data;
    }

    int getUsedMemory() const
    {
        return userMemorySize;
    }

    int getUserBlocksReserved() const
    {
        return userBlocksReserved;
    }

    int allocate(int size);

    int allocateMax();

    bool free(int address);

    std::string getMap() const;

private:
    char *data;
    int memorySize;
    int userMemorySize;
    int userBlocksReserved;
    MCB *const firstMCB;

    // most left MCB
    MCB *getMinMCB() const
    {
        MCB *res = firstMCB;
        MCB *current = firstMCB;
        while (current) {
            res = std::min(res, current);
            current = current->next;
        }
        return res;
    }

    //    // MCB represent most right block
    MCB *getLastMCB() const
    {
        MCB *current = firstMCB;
        while (current->next) {
            current = current->next;
        }
        return current;
    }

    int calcRightFreeSpace() const
    {
        auto lastUsed = getLastMCB();
        return memorySize - lastUsed->owner - lastUsed->size -
               (userBlocksReserved + 2) * sizeof(MCB);
    }

    std::pair<MCB *, MCB *> findFirstFreeSpaceBetweenBlocksGEQThan(int size) const;

    MCB *createNewMCB();

    void insertMCB(MCB *prev, MCB *cur, MCB *next);

    MCB *findMCBWithSpecificUserAddress(int address) const
    {
        auto cur = firstMCB->next;
        while (cur) {
            if (cur->owner == address)
                return cur;
            cur = cur->next;
        }
        return nullptr;
    }

    void removeMCB(MCB *mcb);

    int calcMaxFreeSpaceBetweenBlocks();
};

#endif //__MemoryAllocator_H_
