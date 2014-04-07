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
    static const int COLUMNS = 7;
    static const int ROWS = 6;

    Game()
    {
        disks.assign(COLUMNS, 0);
    }

    bool canMoveTo(int pos)
    {
        return disks[pos] < ROWS;
    }

    int getBestMove(Player me, int depth)
    {
        int bestResult = INT_MIN;
        int bestMove = 3;
        for (int move = 0; move < COLUMNS; move++) {
            if (canMoveTo(move)) {
                moveTo(move, me);

                int test = runNegamax(me, depth);
                
                if (test >= bestResult) {
                    bestResult = test;
                    bestMove = move;
                }
                unMoveFrom(move, me);
            }
        }
        return bestMove;
    }

    void moveTo(int column, Player player);

    void print(ostream &out);

private:
    vector<int> disks;
    int64_t boards[2] = { 0, 0 };

    int runNegamax(Player player, int depth, int alpha = INT_MIN, int beta = INT_MAX);

    void unMoveFrom(int column, Player player);

    int getHeuristicEvaluation(Player player);

    bool isWinner(Player player);

    static int getDisksNumberInSegment(int64_t board, int64_t mask);
};

#endif //__board_H_
