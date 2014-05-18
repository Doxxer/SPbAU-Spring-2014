package ru.spbau.turaevT.task5;

import java.lang.reflect.InvocationTargetException;
import java.util.*;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Tournament implements Runnable {

    private final List<Player> players;
    private Map<Player, Thread> threads = new HashMap<>();
    private Map<Player, Boolean> stepGate = new HashMap<>();
    private Map<String, Integer> score = new HashMap<>();
    private Map<Player, Integer> decision = Collections.synchronizedMap(new HashMap<Player, Integer>());

    private final Object aiLock = new Object();
    private int stepsLeft = 0;
    private final Game game;
    private int fieldSize;

    public Tournament(Game game, List<Class<? extends Player>> playersClassList, int fieldSize) {
        this.game = game;
        this.fieldSize = fieldSize;
        this.players = new ArrayList<>();
        List<Integer> rooms = generateRoomsLayout(fieldSize);
        List<Integer> victims = generateVictimsLayout(playersClassList.size());

        for (int i = 0, playersClassListSize = playersClassList.size(); i < playersClassListSize; i++) {
            Class<? extends Player> playerClass = playersClassList.get(i);
            try {
                Player player = playerClass
                        .getConstructor(Tournament.class, int.class, int.class, int.class, String.class)
                        .newInstance(this, i, rooms.get(i), victims.get(i), playerClass.getSimpleName());
                players.add(player);
                stepGate.put(player, false);
                score.put(player.getName(), 0);
            } catch (NoSuchMethodException ignored) {

            } catch (InvocationTargetException | InstantiationException | IllegalAccessException e) {
                System.err.println("Error occurred while instantiating AI class: " + e.getMessage());
            }
        }

        for (final Player player : players) {
            threads.put(player, new Thread(new Runnable() {
                @Override
                public void run() {
                    while (true) {
                        try {
                            synchronized (aiLock) {
                                while (!stepGate.get(player)) {
                                    aiLock.wait();
                                }
                                stepsLeft--;
                                stepGate.put(player, false);
                                decision.put(player, player.move());
                                aiLock.notifyAll();
                            }
                        } catch (InterruptedException e) {
                            break;
                        }
                    }
                }
            }));
        }
    }

    public Map<String, Integer> getScore() {
        return score;
    }

    private List<Integer> generateVictimsLayout(int maxSize) {
        List<Integer> result = new ArrayList<>();
        for (int i = 0; i < maxSize; i++) {
            result.add(i);
        }
        while (maxSize > 1 && hasCollision(result)) {
            Collections.shuffle(result);
        }

        return result;
    }

    private boolean hasCollision(List<Integer> result) {
        for (int i = 0; i < result.size(); i++) {
            if (result.get(i) == i)
                return true;
        }
        return false;
    }

    private List<Integer> generateRoomsLayout(int maxSize) {
        List<Integer> rooms = new ArrayList<>();
        for (int i = 0; i < maxSize; i++) {
            rooms.add(i);
        }
        Collections.shuffle(rooms);
        return rooms;
    }

    @Override
    public void run() {
        for (final Player player : players) {
            threads.get(player).start();
        }
        while (players.size() > 1) {
            try {
                synchronized (aiLock) {
                    stepsLeft = players.size();
                    allowAIStep();
                    aiLock.notifyAll();
                    while (stepsLeft != 0) {
                        aiLock.wait();
                    }

                    for (Map.Entry<Player, Integer> entry : decision.entrySet()) {
                        Player murderer = entry.getKey();
                        if (murderer.getRoomNumber() != -1 && entry.getValue() == 0) {
                            Player kenny = null;
                            for (Player victim : players) {
                                if (victim.getNumber() == murderer.getNextVictim() && victim.getRoomNumber() == murderer.getRoomNumber()) {
                                    kenny = victim;
                                    break;
                                }
                            }
                            if (kenny == null)
                                continue; // no victim in murderer's room

                            threads.get(kenny).interrupt();
                            players.remove(kenny);
                            murderer.setNextVictim(kenny.getNextVictim());
                            kenny.setRoomNumber(-1);
                            score.put(murderer.getName(), score.get(murderer.getName()) + 2);
                        }
                    }
                    for (Map.Entry<Player, Integer> entry : decision.entrySet()) {
                        Player player = entry.getKey();
                        if (player.getRoomNumber() != -1 && entry.getValue() != 0) {
                            player.setRoomNumber(player.getRoomNumber() + entry.getValue());
                        }
                    }
                    decision.clear();
                }
            } catch (InterruptedException e) {
                break;
            }
        }

        if (players.size() == 1) {
            String winner = players.get(0).getName();
            score.put(winner, score.get(winner) + 5);
        }

        for (Map.Entry<Player, Thread> entry : threads.entrySet()) {
            entry.getValue().interrupt();
        }

        synchronized (Game.gameLock) {
            game.tournamentOver();
        }

    }

    private void allowAIStep() {
        for (Player player : players) {
            stepGate.put(player, true);
        }
    }

    public List<Player> getPlayers() {
        return players;
    }

    public int getFieldSize() {
        return fieldSize;
    }
}
