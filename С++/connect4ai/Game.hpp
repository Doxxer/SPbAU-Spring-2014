#ifndef __board_Hpp_
#define __board_Hpp_

#include "utils.hpp"
#include <climits>
#include <vector>
#include <cstdint>
#include <iostream>

using std::ostream;
using std::vector;

class Game {

public:
    static const int width = 7;
    static const int height = 6;


    Game() {
        disks.assign(width, 0);
    }

    bool canMoveTo(int pos) {
        return disks[pos] < height;
    }

    int getBestMove(Player me, int depth) {
        int minmax = INT_MIN;
        int bestMove = 3;
        for (int move = 0; move < width; move++) {
            if (canMoveTo(move)) {

                // simulate step
                moveTo(move, me);

                // test our step
                int test = runNegamax(me, depth);
                if (test > minmax) {
                    minmax = test;
                    bestMove = move;
                }

                // restore prev state
                unMoveFrom(move, me); // make me unseen that
            }
        }
        return bestMove;
    }

    void moveTo(int column, Player player);

    void print(ostream &out);

private:
    vector<int> disks;
    int64_t boards[2] = {0, 0};

    int runNegamax(Player player, int depth, int alpha = INT_MIN, int beta = INT_MAX);

    void unMoveFrom(int column, Player player);

    int getHeuristicEvaluation();
};


#endif //__board_H_
