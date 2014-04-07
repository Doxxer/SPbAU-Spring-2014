#ifndef __game_Hpp_
#define __game_Hpp_

#include "Player.hpp"
#include <climits>
#include <vector>
#include <cstdint>
#include <iostream>
#include <iterator>

using std::ostream;
using std::vector;

class Game {

public:
    static const int COLUMNS = 7;
    static const int ROWS = 6;

    Game()
    {
        disks.assign(COLUMNS, 0);
        generateMasks();
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
    vector<uint64_t> masks;

    uint64_t boards[2] = { 0, 0 };

    void generateMasks();

    int runNegamax(Player player, int depth, int alpha = INT_MIN, int beta = INT_MAX);

    int getHeuristicEvaluation(Player player);

    bool isWinner(Player player);

    static int getDisksNumberInSegment(uint64_t board, uint64_t mask);

    void unMoveFrom(int column, Player player);

    void generateMaskInDirection(int col, int row, int dCol, int dRow, int len = 4);

    int getCellNumber(int col, int row)
    {
        return col * ROWS + row;
    }
};

#endif
