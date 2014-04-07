#ifndef __segment_Generator_Hpp_
#define __segment_Generator_Hpp_

#include <iterator>
#include <vector>

using std::vector;

template <typename OutputIterator> class SegmentsGenerator {
public:
    SegmentsGenerator(int width, int height, OutputIterator iterator)
        : width(width), height(height), outputIterator(iterator)
    {
        generateBoards();
    }

    void run()
    {
        generateColumnMasks();
        generateRowMasks();
        processDiagonals(normalBoard);
        processDiagonals(inverseBoard);
    }

private:
    int width;
    int height;
    OutputIterator outputIterator;

    typedef vector<vector<int> > Board;
    Board normalBoard;
    Board transposedBoard;
    Board inverseBoard;

    void generateBoards()
    {
        normalBoard.assign(width, vector<int>(height, 0));
        transposedBoard.assign(height, vector<int>(width, 0));
        inverseBoard.assign(width, vector<int>(height, 0));

        for (int i = 0; i < width; ++i) {
            for (int j = 0; j < height; ++j) {
                normalBoard[i][j] = j + i * height;
                inverseBoard[i][height - 1 - j] = j + i * height;
            }
        }

        for (int i = 0; i < height; ++i) {
            for (int j = 0; j < width; ++j) {
                transposedBoard[i][j] = i + j * height;
            }
        }
    }

    void processDiagonals(vector<vector<int> > const &board)
    {
        vector<int> diagonal;
        for (int row = 0; row < height - 3; ++row) {
            int x = 0, y = row;
            diagonal.clear();
            while (x < width && y < height) {
                diagonal.push_back(board[x][y]);
                x++;
                y++;
            }
            for (size_t i = 0; i < diagonal.size() - 3; ++i) {
                *outputIterator = generateMask(diagonal.begin() + i);
                ++outputIterator;
            }
        }

        for (int col = 1; col < width - 3; ++col) {
            int x = col, y = 0;
            diagonal.clear();
            while (x < width && y < height) {
                diagonal.push_back(board[x][y]);
                x++;
                y++;
            }
            for (size_t i = 0; i < diagonal.size() - 3; ++i) {
                *outputIterator = generateMask(diagonal.begin() + i);
                ++outputIterator;
            }
        }
    }

    void generateColumnMasks()
    {
        for (int col = 0; col < width; ++col) {
            for (int j = 0; j < height - 3; ++j) {
                *outputIterator = generateMask(normalBoard[col].begin() + j);
                ++outputIterator;
            }
        }
    }

    void generateRowMasks()
    {
        for (int row = 0; row < height; ++row) {
            for (int j = 0; j < width - 3; ++j) {
                *outputIterator = generateMask(transposedBoard[row].begin() + j);
                ++outputIterator;
            }
        }
    }

    static uint64_t generateMask(vector<int>::const_iterator it, size_t len = 4)
    {
        uint64_t res = 0;
        while (len--) {
            res += 1ULL << *it++;
        }
        return res;
    }
};

#endif //__utils_H_