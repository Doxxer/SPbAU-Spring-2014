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

int Game::runNegamax(Player player, int depth, int alpha, int beta)
{
    Player rival = getRival(player);
    if (depth == 0)
        return getHeuristicEvaluation(player);

    int bestResult = INT_MIN;
    for (int column = 0; column < COLUMNS; ++column) {
        if (canMoveTo(column)) {

            moveTo(column, rival);

            int newBeta = (beta == INT_MIN) ? INT_MAX : -beta;
            int newAlpha = (alpha == INT_MIN) ? INT_MAX : -alpha;

            int test = runNegamax(rival, depth - 1, newBeta, newAlpha);
            bestResult = std::max(bestResult, test);

            unMoveFrom(column, rival);

            alpha = std::max(alpha, test);
            if (alpha >= beta)
                break;
        }
    }

    int heuristic = getHeuristicEvaluation(player);
    if (bestResult > 0 && heuristic < bestResult + INT_MIN)
        return INT_MIN;
    else if (bestResult < 0 && heuristic > bestResult + INT_MAX)
        return INT_MAX;
    else
        return heuristic - bestResult;
}

int Game::getHeuristicEvaluation(Player player)
{
    if (isWinner(player))
        return INT_MAX;
    if (isWinner(getRival(player)))
        return INT_MIN;

    int weights[] = { 0, 0, 4, 64, 0 }; // 0 1 2 3 4
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

    int mySum = 0;
    int foeSum = 0;
    for (int i = 0; i < 5; ++i) {
        mySum += myScore[i] * weights[i];
        foeSum += foeScore[i] * weights[i];
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
