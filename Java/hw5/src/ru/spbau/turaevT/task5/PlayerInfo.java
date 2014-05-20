package ru.spbau.turaevT.task5;

/**
 * Represents Player information
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class PlayerInfo {
    private final int number;
    private final int roomNumber;

    /**
     * Constructs new player information class
     *
     * @param number     player number (ID)
     * @param roomNumber player's room number
     */
    public PlayerInfo(int number, int roomNumber) {
        this.number = number;
        this.roomNumber = roomNumber;
    }

    /**
     * Returns player number (i.e. ID)
     *
     * @return player number (i.e. ID)
     */
    public int getNumber() {
        return number;
    }

    /**
     * Returns current room number
     *
     * @return current room number
     */
    public int getRoomNumber() {
        return roomNumber;
    }
}
