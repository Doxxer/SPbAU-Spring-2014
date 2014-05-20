package ru.spbau.turaevT.task5;

import java.util.List;

/**
 * Represents AI player
 */
public interface IPlayer {
    /**
     * Make decision, make step
     *
     * @return -1 0 or 1, where -1 and 1 -- step to the left or to the right respectively. 0 -- try to kill victim
     */
    int move();

    /**
     * Returns player number (i.e. ID)
     *
     * @return player number (i.e. ID)
     */
    int getNumber();

    /**
     * Returns current room number
     *
     * @return current room number
     */
    int getRoomNumber();

    /**
     * Returns maximal room number (i.e. field size)
     *
     * @return maximal room number (i.e. field size)
     */
    int getMaxRoomNumber();

    /**
     * Returns number of next victim
     *
     * @return number of next victim
     */
    int getNextVictim();

    /**
     * Returns information of all live players
     *
     * @return information of all live players
     */
    List<PlayerInfo> getPlayersInfo();

    /**
     * Returns player name
     *
     * @return player name
     */
    String getName();
}
