#include "Game.hpp"

void Game::moveTo(int column, Player player) {
    int64_t pos = ((int64_t) 1 << (disks[column] + column * height));
    boards[player] ^= pos;
    disks[column]++;
}

void Game::unMoveFrom(int column, Player player) {
    disks[column]--;
    int64_t pos = ((int64_t) 1 << (disks[column] + column * height));
    boards[player] ^= pos;
}

int Game::runNegamax(Player player, int depth, int alpha, int beta) {
    if (depth == 0)
        return getHeuristicEvaluation();

    int bestResult = INT_MIN;
    for (int column = 0; column < width; column++) {
        if (canMoveTo(column)) {
            // simulate step
            moveTo(column, player ^ 1);

            // test our step
            int test = runNegamax(player ^ 1, depth - 1, -beta, -alpha);
            bestResult = std::max(bestResult, test);

            unMoveFrom(column, player ^ 1);

            // alpha-beta pruning
            alpha = std::max(alpha, test);
            if (alpha >= beta) //todo > ?
                break;
        }
    }
    return getHeuristicEvaluation() - bestResult;
}

int Game::getHeuristicEvaluation() {
    return 42;
}

void Game::print(ostream &out) {
    for (int i = height - 1; i >= 0; --i) {
        for (int j = 0; j < width; j++) {
            int64_t pos = ((int64_t) 1 << (i + j * height));

            bool b0 = (boards[0] & pos) != 0;
            bool b1 = (boards[1] & pos) != 0;

            if (!b0 && !b1) {
                out << ". ";
            } else if (b0) {
                out << "x ";
            }
            else {
                out << "o ";
            }
        }
        out << std::endl;
    }
    out << std::endl;
}
