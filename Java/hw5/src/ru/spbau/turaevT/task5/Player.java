package ru.spbau.turaevT.task5;

import java.util.ArrayList;
import java.util.List;

/**
 * AI player abstract class
 *
 * @author Turaev Timur
 * @version 1.0
 */
public abstract class Player implements IPlayer {

    private final Tournament tournament;
    private final int number;
    private final String name;
    private int roomNumber;
    private int nextVictim;

    protected Player(Tournament tournament, int number, int roomNumber, int nextVictim, String name) {
        this.tournament = tournament;
        this.number = number;
        this.roomNumber = roomNumber;
        this.nextVictim = nextVictim;
        this.name = name;
    }

    /**
     * @inheritDoc
     */
    @Override
    public abstract int move();

    /**
     * @inheritDoc
     */
    @Override
    public int getNumber() {
        return number;
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getRoomNumber() {
        return roomNumber;
    }

    /**
     * Set player's room number
     *
     * @param roomNumber player's room number
     */
    public void setRoomNumber(int roomNumber) {
        this.roomNumber = roomNumber;
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getNextVictim() {
        return nextVictim;
    }

    /**
     * Set number of next victim
     *
     * @param nextVictim number of next victim
     */
    public void setNextVictim(int nextVictim) {
        this.nextVictim = nextVictim;
    }

    /**
     * @inheritDoc
     */
    @Override
    public List<PlayerInfo> getPlayersInfo() {
        List<PlayerInfo> result = new ArrayList<>();
        for (Player player : tournament.getPlayers()) {
            result.add(new PlayerInfo(player.getNumber(), player.getRoomNumber()));
        }
        return result;
    }

    /**
     * @inheritDoc
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * @inheritDoc
     */
    @Override
    public int getMaxRoomNumber() {
        return tournament.getFieldSize() - 1;
    }
}
