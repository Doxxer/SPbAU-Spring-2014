#include "Game.hpp"

void Game::moveTo(int column, Player player)
{
    int64_t pos = ((int64_t)1 << (disks[column] + column * ROWS));
    boards[player] ^= pos;
    disks[column]++;
}

void Game::unMoveFrom(int column, Player player)
{
    disks[column]--;
    int64_t pos = ((int64_t)1 << (disks[column] + column * ROWS));
    boards[player] ^= pos;
}

int64_t Game::runNegamax(Player player, int depth, int64_t alpha, int64_t beta)
{
    if (depth == 0)
        return getHeuristicEvaluation(player);

    int64_t bestResult = INT_MIN;
    for (int column = 0; column < COLUMNS; ++column) {
        if (canMoveTo(column)) {
            // simulate step
            moveTo(column, player ^ 1);

            // test our step
            int64_t test = runNegamax(player ^ 1, depth - 1, -beta, -alpha);
            bestResult = std::max(bestResult, test);

            // restore prev state
            unMoveFrom(column, player ^ 1);

            // alpha-beta pruning
            alpha = std::max(alpha, test);
            if (alpha >= beta)
                break;
        }
    }
    return getHeuristicEvaluation(player) - bestResult;
}

int64_t Game::getHeuristicEvaluation(Player player)
{
    if (isWinner(player))
        return 1000000;
    if (isWinner(player ^ 1))
        return -1000000;

    int weigths[] = { 0, 1, 2, 16, 0 }; // 0 1 2 3 4
    int myScore[] = { 0, 0, 0, 0, 0 };
    int foeScore[] = { 0, 0, 0, 0, 0 };

    for (auto mask : masks4cells) {
        int myDisks = getDisksNumberInSegment(boards[player], mask);
        int foeDisks = getDisksNumberInSegment(boards[player ^ 1], mask);

        if (myDisks + foeDisks == 4)
            continue;

        if (foeDisks == 0) {
            myScore[myDisks] += 1;
        } else if (myDisks == 0) {
            foeScore[foeDisks] += 1;
        }
    }

    // zip
    int mySum = 0;
    int foeSum = 0;
    for (int i = 0; i < 5; ++i) {
        mySum += myScore[i] * weigths[i];
        foeSum += foeScore[i] * weigths[i];
    }
    return mySum - foeSum;
}

bool Game::isWinner(Player player)
{
    for (auto mask : masks4cells) {
        if (getDisksNumberInSegment(boards[player], mask) == 4)
            return true;
    }
    return false;
}

void Game::print(ostream &out)
{
    for (int row = ROWS - 1; row >= 0; --row) {
        for (int column = 0; column < COLUMNS; column++) {
            int64_t pos = ((int64_t)1 << (row + column * ROWS));

            bool b0 = (boards[0] & pos) != 0; // x
            bool b1 = (boards[1] & pos) != 0; // o

            if (!b0 && !b1) {
                out << ". ";
            } else if (b0) {
                out << "x ";
            } else {
                out << "o ";
            }
        }
        out << std::endl;
    }
    out << std::endl;
}

int Game::getDisksNumberInSegment(int64_t board, int64_t mask)
{
    int res = 0;
    while (mask) {
        // some bitwise magic
        int64_t ffs = mask & ((~mask) + 1); // find first set
        res += ((board & ffs) != 0);
        mask ^= ffs;
    }
    return res;
}
