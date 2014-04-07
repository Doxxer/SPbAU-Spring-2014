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
        calculateMasks();
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

    void calculateMasks();

    vector<uint64_t> masks;

private:
    vector<int> disks;

    uint64_t boards[2] = { 0, 0 };

    int runNegamax(Player player, int depth, int alpha = INT_MIN, int beta = INT_MAX);

    int getHeuristicEvaluation(Player player);

    bool isWinner(Player player);

    static int getDisksNumberInSegment(uint64_t board, uint64_t mask);

    void unMoveFrom(int column, Player player);

    void generateColumnMasks();

    void generateRowMasks();

    void generateLURDdiag();

    void processDiagonals(vector<vector<int> > const &board);

    void generateLDRUdiag();
};

#endif //__board_H_
