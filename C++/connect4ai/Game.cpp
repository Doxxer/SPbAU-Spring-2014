#include "Game.hpp"

void Game::moveTo(int column, Player player)
{
    uint64_t pos = ((uint64_t)1 << (disks[column] + column * ROWS));
    boards[player] ^= pos;
    disks[column]++;
}

void Game::unMoveFrom(int column, Player player)
{
    disks[column]--;
    uint64_t pos = ((uint64_t)1 << (disks[column] + column * ROWS));
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
    Player rival = getRival(player);

    if (isWinner(player))
        return INT_MAX;
    if (isWinner(rival))
        return INT_MIN;

    int weights[] = { 0, 0, 4, 64, 0 }; // 0 1 2 3 4
    int myScore[] = { 0, 0, 0, 0, 0 };
    int rivalScore[] = { 0, 0, 0, 0, 0 };

    for (auto mask : masks) {
        int myDisks = getDisksNumberInSegment(boards[player], mask);
        int rivalDisks = getDisksNumberInSegment(boards[rival], mask);

        if (myDisks + rivalDisks == 4)
            continue;

        if (rivalDisks == 0) {
            myScore[myDisks] += 1;
        } else if (myDisks == 0) {
            rivalScore[rivalDisks] += 1;
        }
    }

    int mySum = 0;
    int rivalSum = 0;
    for (int i = 0; i < 5; ++i) {
        mySum += myScore[i] * weights[i];
        rivalSum += rivalScore[i] * weights[i];
    }
    return mySum - rivalSum;
}

bool Game::isWinner(Player player)
{
    for (auto mask : masks) {
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

int Game::getDisksNumberInSegment(uint64_t board, uint64_t mask)
{
    int res = 0;
    while (mask) {
        uint64_t ffs = mask & ((~mask) + 1); // find first set
        res += ((board & ffs) != 0);
        mask ^= ffs;
    }
    return res;
}

void Game::generateMasks()
{
    for (int col = 0; col < COLUMNS; ++col) {
        generateMaskInDirection(col, 0, 0, +1);
        generateMaskInDirection(col, 0, +1, +1);
        generateMaskInDirection(col, 0, -1, +1);
    }
    for (int row = 0; row < ROWS; ++row) {
        generateMaskInDirection(0, row, +1, 0);
        if (row != 0) {
            generateMaskInDirection(0, row, +1, +1);
            generateMaskInDirection(COLUMNS - 1, row, -1, +1);
        }
    }
}

void Game::generateMaskInDirection(int col, int row, int dCol, int dRow, int len)
{
    int x = col, y = row;
    uint64_t mask = 0;
    while (x >= 0 && x < COLUMNS && y >= 0 && y < ROWS && len) {
        mask += ((1ULL) << getCellNumber(x, y));
        len--;
        x += dCol;
        y += dRow;
    }
    if (len == 0) {
        masks.push_back(mask);
        generateMaskInDirection(col + dCol, row + dRow, dCol, dRow);
    }
}
