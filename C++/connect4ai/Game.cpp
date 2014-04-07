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

static uint64_t generateMask(vector<int>::const_iterator it, size_t len = 4)
{
    uint64_t res = 0;
    while (len--) {
        res += 1ULL << *it++;
    }
    return res;
}

void Game::calculateMasks()
{
    masks.reserve(69);

    generateColumnMasks();
    generateRowMasks();
    generateLURDdiag();
    generateLDRUdiag();
}

void Game::generateColumnMasks()
{
    vector<vector<int> > board(COLUMNS, vector<int>(ROWS, 0));

    for (int i = 0; i < COLUMNS; ++i) {
        for (int j = 0; j < ROWS; ++j) {
            board[i][j] = j + i * ROWS;
        }
    }

    for (int col = 0; col < COLUMNS; ++col) {
        for (int j = 0; j < ROWS - 3; ++j) {
            masks.push_back(generateMask(board[col].begin() + j));
        }
    }
}

void Game::generateRowMasks()
{
    vector<vector<int> > board(ROWS, vector<int>(COLUMNS, 0));

    for (int i = 0; i < ROWS; ++i) {
        for (int j = 0; j < COLUMNS; ++j) {
            board[i][j] = i + j * ROWS;
        }
    }

    for (int row = 0; row < ROWS; ++row) {
        for (int j = 0; j < COLUMNS - 3; ++j) {
            masks.push_back(generateMask(board[row].begin() + j));
        }
    }
}

void Game::generateLURDdiag()
{
    vector<vector<int> > board(COLUMNS, vector<int>(ROWS, 0));

    for (int i = 0; i < COLUMNS; ++i) {
        for (int j = 0; j < ROWS; ++j) {
            board[i][j] = j + i * ROWS;
        }
    }

    processDiagonals(board);
}

void Game::processDiagonals(vector<vector<int> > const &board)
{
    vector<int> diag;
    for (int row = 0; row < ROWS - 3; ++row) {
        int x = 0, y = row;
        diag.clear();
        while (x < COLUMNS && y < ROWS) {
            diag.push_back(board[x][y]);
            //            std::cout << board[x][y] << " ";
            x++;
            y++;
        }
        //        std::cout << std::endl;
        for (size_t i = 0; i < diag.size() - 3; ++i)
            masks.push_back(generateMask(diag.begin() + i));
    }
    for (int col = 1; col < COLUMNS - 3; ++col) {
        int x = col, y = 0;
        diag.clear();
        while (x < COLUMNS && y < ROWS) {
            diag.push_back(board[x][y]);
            //            std::cout << board[x][y] << " ";
            x++;
            y++;
        }
        //        std::cout << std::endl;
        for (size_t i = 0; i < diag.size() - 3; ++i)
            masks.push_back(generateMask(diag.begin() + i));
    }
}

void Game::generateLDRUdiag()
{
    vector<vector<int> > board(COLUMNS, vector<int>(ROWS, 0));

    for (int i = 0; i < COLUMNS; ++i) {
        for (int j = 0; j < ROWS; ++j) {
            board[i][ROWS - 1 - j] = j + i * ROWS;
        }
    }

    processDiagonals(board);
}
