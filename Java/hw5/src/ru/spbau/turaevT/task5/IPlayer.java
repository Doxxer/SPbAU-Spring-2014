package ru.spbau.turaevT.task5;

import java.util.List;

public interface IPlayer {
    int move();

    int getNumber();

    int getRoomNumber();

    int getMaxRoomNumber();

    int getNextVictim();

    List<PlayerInfo> getPlayersInfo();

    String getName();
}
