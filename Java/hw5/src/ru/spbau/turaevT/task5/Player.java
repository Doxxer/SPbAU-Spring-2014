package ru.spbau.turaevT.task5;

import java.util.ArrayList;
import java.util.List;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public abstract class Player implements IPlayer {

    private final Tournament tournament;
    private final int number;
    private int roomNumber;
    private int nextVictim;
    private final String name;

    protected Player(Tournament tournament, int number, int roomNumber, int nextVictim, String name) {
        this.tournament = tournament;
        this.number = number;
        this.roomNumber = roomNumber;
        this.nextVictim = nextVictim;
        this.name = name;
    }

    public void setRoomNumber(int roomNumber) {
        this.roomNumber = roomNumber;
    }

    public void setNextVictim(int nextVictim) {
        this.nextVictim = nextVictim;
    }

    @Override
    public abstract int move();

    @Override
    public int getNumber() {
        return number;
    }

    @Override
    public int getRoomNumber() {
        return roomNumber;
    }

    @Override
    public int getNextVictim() {
        return nextVictim;
    }

    @Override
    public List<PlayerInfo> getPlayersInfo() {
        List<PlayerInfo> result = new ArrayList<>();
        for (Player player : tournament.getPlayers()) {
            result.add(new PlayerInfo(player.getNumber(), player.getRoomNumber()));
        }
        return result;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public int getMaxRoomNumber() {
        return tournament.getFieldSize() - 1;
    }
}
