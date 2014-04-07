#include <iostream>
#include <stdexcept>
#include "Game.hpp"

using std::string;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

const int DEPTH = 3;

int main()
{
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
        if (foeMove < 0 || foeMove > 6 || !game.canMoveTo(foeMove)) {
            throw std::runtime_error("INVALID MOVE");
            return -1;
        }
        game.moveTo(foeMove, me ^ 1);

        int bestMove = game.getBestMove(me, DEPTH);
        game.moveTo(bestMove, me);

        cout << bestMove << endl;
        cin >> command;
    }
    return 0;
}
