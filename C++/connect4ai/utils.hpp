#ifndef __utils_Hpp_
#define __utils_Hpp_

#include <cstdint>

enum Player {
    white = 0,
    black = 1
};

inline Player getRival(Player player) {
    return (Player) (player ^ 1);
}

#endif //__utils_H_
