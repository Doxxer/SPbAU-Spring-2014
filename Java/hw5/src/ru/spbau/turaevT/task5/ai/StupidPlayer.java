package ru.spbau.turaevT.task5.ai;

import ru.spbau.turaevT.task5.Player;
import ru.spbau.turaevT.task5.PlayerInfo;
import ru.spbau.turaevT.task5.Tournament;

import java.util.Random;

/**
 * Sample random playing AI implementation
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class StupidPlayer extends Player {
    private static final Random random = new Random();

    /**
     * Constructs random playing AI class
     *
     * @param tournament tournament which player belongs
     * @param number     number of player, his ID
     * @param roomNumber number if first room, where player will be spawned
     * @param nextVictim number of next victim
     * @param name       player name
     */
    public StupidPlayer(Tournament tournament, int number, int roomNumber, int nextVictim, String name) {
        super(tournament, number, roomNumber, nextVictim, name);
    }

    /**
     * @inheritDoc
     */
    @Override
    public int move() {
        for (PlayerInfo playerInfo : getPlayersInfo()) {
            if (playerInfo.getNumber() == getNextVictim() && playerInfo.getRoomNumber() == getRoomNumber()) {
                return 0;
            }
        }
        if (getRoomNumber() == 0)
            return 1;
        if (getRoomNumber() == getMaxRoomNumber()) {
            return -1;
        }
        return random.nextInt(3) - 1;
    }
}
