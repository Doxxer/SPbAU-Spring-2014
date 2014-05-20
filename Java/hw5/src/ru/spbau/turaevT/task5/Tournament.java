package ru.spbau.turaevT.task5;

import java.lang.reflect.InvocationTargetException;
import java.util.*;

/**
 * Represents tournament
 * Runs all AI in separate threads
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Tournament implements Runnable {

    private final List<Player> players;
    private final Object lock = new Object();
    private final Game game;
    private final Map<Player, Thread> threads = new HashMap<>();
    private final Map<Player, Boolean> stepGate = new HashMap<>();
    private final Map<Player, Integer> score = new HashMap<>();
    private final Map<Player, Integer> decision = Collections.synchronizedMap(new HashMap<Player, Integer>());
    private final int fieldSize;
    private int stepsLeft = 0;

    /**
     * Constructs new Tournament and initializes threads
     *
     * @param game             game witch tournament belongs to
     * @param playersClassList players' AI
     * @param fieldSize        number of rooms
     */
    public Tournament(Game game, List<Class<? extends Player>> playersClassList, int fieldSize) {
        this.game = game;
        this.fieldSize = fieldSize;
        this.players = new ArrayList<>();

        initializeAI(playersClassList);
        initializeThreads();
    }

    private void initializeThreads() {
        for (final Player player : players) {
            threads.put(player, new Thread(new Runnable() {
                @Override
                public void run() {
                    while (true) {
                        try {
                            synchronized (lock) {
                                while (!stepGate.get(player)) {
                                    lock.wait();
                                }
                                stepsLeft--;
                                stepGate.put(player, false);
                                decision.put(player, player.move());
                                lock.notifyAll();
                            }
                        } catch (InterruptedException e) {
                            break;
                        }
                    }
                }
            }));
        }
    }

    private void initializeAI(List<Class<? extends Player>> playersClassList) {
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
                score.put(player, 0);
            } catch (NoSuchMethodException ignored) {

            } catch (InvocationTargetException | InstantiationException | IllegalAccessException e) {
                System.err.println("Error occurred while instantiating AI class: " + e.getMessage());
            }
        }
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
                synchronized (lock) {
                    stepsLeft = players.size();
                    allowAIStep();
                    lock.notifyAll();
                    while (stepsLeft != 0) {
                        lock.wait();
                    }
                    movePlayers();
                }
            } catch (InterruptedException e) {
                break;
            }
        }

        if (players.size() == 1) {
            Player winner = players.get(0);
            score.put(winner, score.get(winner) + 5);
        }
        for (Map.Entry<Player, Thread> entry : threads.entrySet()) {
            entry.getValue().interrupt();
        }
        synchronized (Game.gameLock) {
            game.tournamentOver();
        }
    }

    private void movePlayers() {
        // kill kenny
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
                if (kenny == null) {
                    continue; // no victim in murderer's room
                }

                threads.get(kenny).interrupt();
                players.remove(kenny);
                murderer.setNextVictim(kenny.getNextVictim());
                kenny.setRoomNumber(-1);
                score.put(murderer, score.get(murderer) + 2 + score.get(kenny));
            }
        }
        // move luckers
        for (Map.Entry<Player, Integer> entry : decision.entrySet()) {
            Player player = entry.getKey();
            if (player.getRoomNumber() != -1 && entry.getValue() != 0) {
                player.setRoomNumber(player.getRoomNumber() + entry.getValue());
            }
        }
        decision.clear();
    }

    private void allowAIStep() {
        for (Player player : players) {
            stepGate.put(player, true);
        }
    }

    /**
     * Returns score table for this tournament
     *
     * @return score table for this tournament
     */
    public Map<Player, Integer> getScore() {
        return score;
    }

    /**
     * Returns all live players
     *
     * @return all live players
     */
    public List<Player> getPlayers() {
        return players;
    }

    /**
     * Returns maximal room number (i.e. field size)
     *
     * @return maximal room number (i.e. field size)
     */
    public int getFieldSize() {
        return fieldSize;
    }
}
