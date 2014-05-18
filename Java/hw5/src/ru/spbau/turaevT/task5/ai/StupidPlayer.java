package ru.spbau.turaevT.task5.ai;

import ru.spbau.turaevT.task5.Player;
import ru.spbau.turaevT.task5.PlayerInfo;
import ru.spbau.turaevT.task5.Tournament;

import java.util.List;
import java.util.Random;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class StupidPlayer extends Player {
    private static final Random random = new Random();

    public StupidPlayer(Tournament tournament, int number, int roomNumber, int nextVictim, String name) {
        super(tournament, number, roomNumber, nextVictim, name);
    }

    @Override
    public int move() {
        List<PlayerInfo> playersInfo = getPlayersInfo();
        for (PlayerInfo playerInfo : playersInfo) {
            if (playerInfo.getNumber() == getNextVictim() && playerInfo.getRoomNumber() == getRoomNumber()) {
                return 0;
            }
        }
        if (getRoomNumber() == 0)
            return 1;
        if (getRoomNumber() == getMaxRoomNumber()){
            return -1;
        }
        return random.nextInt(3) - 1;
    }
}
