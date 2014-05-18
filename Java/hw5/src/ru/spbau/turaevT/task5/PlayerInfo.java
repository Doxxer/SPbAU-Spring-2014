package ru.spbau.turaevT.task5;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class PlayerInfo {
    private final int number;
    private final int roomNumber;

    public PlayerInfo(int number, int roomNumber) {
        this.number = number;
        this.roomNumber = roomNumber;
    }

    public int getNumber() {
        return number;
    }

    public int getRoomNumber() {
        return roomNumber;
    }
}
