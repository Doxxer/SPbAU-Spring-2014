#include <iostream>
#include "Game.hpp"

using std::string;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

#define DEPTH 3

int main() {
    Game game;
    Player me = 0;
    string command;

    cin >> command;
    if (command == "Go") {
        game.moveTo(3, me);
        cout << 3 << endl;
        cin >> command;
    } else {
        me ^= 1;
    }

    while (true) {
        int foeMove = std::stoi(command);
        game.moveTo(foeMove, me ^ 1);

#ifdef DEBUG_MODE
        game.print(cerr);
#endif

        int bestMove = game.getBestMove(me, DEPTH);
        game.moveTo(bestMove, me);

#ifdef DEBUG_MODE
        game.print(cerr);
#endif
        cout << bestMove << endl;
        cin >> command;

        //break; // todo remove!
    }

    return 0;
}
