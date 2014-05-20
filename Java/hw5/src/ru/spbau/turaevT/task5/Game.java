package ru.spbau.turaevT.task5;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Represents game with tournaments
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Game {
    public final static Object gameLock = new Object();
    private final List<Thread> tournamentsThreads = new ArrayList<>();
    private final List<Tournament> tournaments = new ArrayList<>();
    private int tournamentsLeft;

    /**
     * Constructs new game. It loads player AIs and constructs tournaments and starts them.
     *
     * @param fieldSize        field size (number of rooms)
     * @param pathToAI         directory with AI classes
     * @param tournamentsCount number of tournaments
     * @throws MalformedURLException if either no legal protocol could be found in a specification string or the
     *                               string could not be parsed.
     * @throws GameException         if game error occurred
     */
    public Game(int fieldSize, String pathToAI, int tournamentsCount) throws MalformedURLException, GameException {
        List<Class<? extends Player>> players = loadPlayersAI(pathToAI);

        if (fieldSize < players.size()) {
            throw new GameException("Field size must be greater or equal than players count");
        }

        if (players.size() == 0) {
            throw new GameException("Must be at least 1 player");
        }

        tournamentsLeft = tournamentsCount;
        for (int i = 0; i < tournamentsCount; i++) {
            Tournament tournament = new Tournament(this, players, fieldSize);
            tournaments.add(tournament);
            tournamentsThreads.add(new Thread(tournament));
        }
    }

    /**
     * Runs the game. After the game prints result table.
     */
    public void run() {
        for (Thread tournamentsThread : tournamentsThreads) {
            tournamentsThread.start();
        }
        try {
            synchronized (gameLock) {
                while (tournamentsLeft != 0) {
                    gameLock.wait();
                }
            }
        } catch (InterruptedException ignored) {
        }

        for (Thread tournamentsThread : tournamentsThreads) {
            tournamentsThread.interrupt();
        }

        printResultTable();
    }

    private List<Class<? extends Player>> loadPlayersAI(String pathToAI) throws MalformedURLException, GameException {
        List<Class<? extends Player>> players = new ArrayList<>();
        File classDirectory = new File(pathToAI);
        URLClassLoader classLoader = new URLClassLoader(new URL[]{classDirectory.toURI().toURL()});

        File[] files = classDirectory.listFiles();
        if (files == null)
            throw new GameException("Directory with AIs is not exist or is empty");

        for (File file : files) {
            String aiClassFileName = file.getName();

            if (!aiClassFileName.endsWith(".class") || aiClassFileName.equals(".") || aiClassFileName.equals("..") || aiClassFileName.startsWith("."))
                continue;

            Class<? extends Player> player = loadAI(classLoader, aiClassFileName.substring(0, aiClassFileName.lastIndexOf('.')));
            if (player != null)
                players.add(player);
        }
        return players;
    }

    private Class<? extends Player> loadAI(URLClassLoader classLoader, String aiClassFileName) {
        try {
            Class<?> givenClass;
            try {
                givenClass = classLoader.loadClass(aiClassFileName);
            } catch (NoClassDefFoundError e) {
                String errorMessage = e.getMessage();
                int startIndex = errorMessage.lastIndexOf(" ") + 1;
                int endIndex = errorMessage.length() - 1;
                String className = e.getMessage().substring(startIndex, endIndex).replace('/', '.');
                givenClass = classLoader.loadClass(className);
            }
            if (givenClass == null) {
                throw new ClassNotFoundException("");
            }
            Class<? extends Player> player = givenClass.asSubclass(Player.class);
            //check proper constructor
            player.getConstructor(Tournament.class, int.class, int.class, int.class, String.class);
            System.out.println("Loaded " + player.getSimpleName() + "AI player...");
            return player;
        } catch (ClassNotFoundException e) {
            System.err.println("Failed to load AI from " + aiClassFileName + " class. There is no class in given file.");
        } catch (ClassCastException e) {
            System.err.println("Failed to load AI from " + aiClassFileName + " class. Class not extends Player class");
        } catch (NoSuchMethodException e) {
            System.err.println("Failed to load AI from " + aiClassFileName + " class. There is no proper constructor in given class");
        }
        return null;
    }

    private void printResultTable() {
        Map<String, List<Integer>> gameResult = new HashMap<>();

        for (Tournament tournament : tournaments) {
            Map<Player, Integer> tournamentScore = tournament.getScore();
            for (Map.Entry<Player, Integer> entry : tournamentScore.entrySet()) {
                List<Integer> scores = gameResult.get(entry.getKey().getName());
                if (scores == null) {
                    gameResult.put(entry.getKey().getName(), new ArrayList<Integer>());
                    scores = gameResult.get(entry.getKey().getName());
                }
                scores.add(entry.getValue());
            }
        }

        StringBuilder stringBuilder = new StringBuilder();
        for (Map.Entry<String, List<Integer>> entry : gameResult.entrySet()) {
            String player = entry.getKey();
            List<Integer> scores = entry.getValue();
            stringBuilder.append(player);
            stringBuilder.append(" ");
            int sum = 0;
            for (Integer score : scores) {
                stringBuilder.append(score);
                stringBuilder.append(" ");
                sum += score;
            }
            stringBuilder.append(sum);
            stringBuilder.append(System.lineSeparator());
        }
        System.out.print(stringBuilder.toString());
    }

    /**
     * Ends the tournament.
     */
    public void tournamentOver() {
        synchronized (gameLock) {
            tournamentsLeft--;
            gameLock.notifyAll();
        }
    }
}
