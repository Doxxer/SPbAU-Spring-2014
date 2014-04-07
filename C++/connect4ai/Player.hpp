#ifndef __Player_HPP_
#define __Player_HPP_

enum Player {
    white = 0,
    black = 1
};

inline Player getRival(Player player)
{
    return (Player)(player ^ 1);
}

#endif
