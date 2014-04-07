#include <stdexcept>
#include <string>
#include <iostream>
#include "Game.hpp"

using std::string;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

int main()
{
    Game game;
    Player me = Player::white;

    string command;
    cin >> command;

    if (command == "Go") {
        game.moveTo(3, me);
        cout << 3 << endl;
        cin >> command;
    } else {
        me = getRival(me);
    }

    while (true) {
        int foeMove = std::stoi(command);
        if (foeMove < 0 || foeMove > 6 || !game.canMoveTo(foeMove)) {
            throw std::runtime_error("INVALID MOVE");
        }
        game.moveTo(foeMove, getRival(me));

        int bestMove = game.getBestMove(me, DEPTH);
        game.moveTo(bestMove, me);

        cout << bestMove << endl;
        cin >> command;
    }
    return 0;
}
