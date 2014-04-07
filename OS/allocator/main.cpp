#include <iostream>
#include "MemoryAllocator.hpp"

using namespace std;

int main()
{
    int N = 0;
    cin >> N;
    if (N < 100 || N > 10000) {
        cerr << "N bust be between 100 and 10000" << endl;
        return 0;
    }

    MemoryAllocator allocator(N);

    while (true) {
        string command = "";
        int param = 0;
        cin >> command;

        if (command == "ALLOC") {
            cin >> param;
            if (param < 0) {
                cerr << "S bust be non-negative" << endl;
                continue;
            }
            int res = allocator.allocate(param);
            (res < 0 ? cout << "-" : cout << "+ " << res) << endl;
        } else if (command == "FREE") {
            cin >> param;
            bool res = allocator.free(param);
            (res ? cout << "+" : cout << "-") << endl;
        } else if (command == "INFO") {
            cout << allocator.getUsedBlocksCount() << " " << allocator.getUsedMemory() << " "
                 << allocator.allocateMax() << endl;
        } else if (command == "MAP") {
            cout << allocator.getMap() << endl;
        } else if (command.empty())
            break;
    }

    return 0;
}
