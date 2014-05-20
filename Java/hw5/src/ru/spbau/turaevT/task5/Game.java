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
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Game {
    public final static Object gameLock = new Object();
    private final List<Thread> tournamentsThreads = new ArrayList<>();
    private final List<Tournament> tournaments = new ArrayList<>();
    private int tournamentsLeft;

    public Game(int fieldSize, String pathToAI, int tournamentsCount) throws MalformedURLException, GameException {
        List<Class<? extends Player>> players = new ArrayList<>();

        File classDirectory = new File(pathToAI);
        URLClassLoader urlLoader = new URLClassLoader(new URL[]{classDirectory.toURI().toURL()});

        File[] files = classDirectory.listFiles();
        if (files == null)
            throw new GameException("Directory with AIs is empty");

        for (File file : files) {
            String filename = file.getName();

            if (!filename.endsWith(".class") || filename.equals(".") || filename.equals("..") || filename.startsWith("."))
                continue;

            filename = filename.substring(0, filename.lastIndexOf('.'));
            try {
                Class<?> givenClass;
                try {
                    givenClass = urlLoader.loadClass(filename);
                } catch (NoClassDefFoundError e) {
                    String errorMessage = e.getMessage();
                    int startIndex = errorMessage.lastIndexOf(" ") + 1;
                    int endIndex = errorMessage.length() - 1;
                    String className = e.getMessage().substring(startIndex, endIndex).replace('/', '.');
                    givenClass = urlLoader.loadClass(className);
                }
                if (givenClass == null) {
                    throw new ClassNotFoundException("");
                }
                Class<? extends Player> aiClass = givenClass.asSubclass(Player.class);
                aiClass.getConstructor(Tournament.class, int.class, int.class, int.class, String.class); //check proper constructor
                players.add(aiClass);
                System.out.println("Loaded " + aiClass.getSimpleName() + "...");
            } catch (ClassNotFoundException e) {
                System.err.println("Failed to load AI from " + filename + " class. There is no class in given file.");
            } catch (ClassCastException e) {
                System.err.println("Failed to load AI from " + filename + " class. Class not extends Player class");
            } catch (NoSuchMethodException e) {
                System.err.println("Failed to load AI from " + filename + " class. There is no proper constructor in given class");
            }
        }

        if (fieldSize < players.size()) {
            throw new GameException("Field size must be greater or equal than players count");
        }
        tournamentsLeft = tournamentsCount;
        for (int i = 0; i < tournamentsCount; i++) {
            Tournament tournament = new Tournament(this, players, fieldSize);
            tournaments.add(tournament);
            tournamentsThreads.add(new Thread(tournament));
        }
    }

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

        Map<String, List<Integer>> gameResult = new HashMap<>();

        for (Tournament tournament : tournaments) {
            Map<String, Integer> tournamentScore = tournament.getScore();
            for (Map.Entry<String, Integer> entry : tournamentScore.entrySet()) {
                List<Integer> scores = gameResult.get(entry.getKey());
                if (scores == null) {
                    gameResult.put(entry.getKey(), new ArrayList<Integer>());
                    scores = gameResult.get(entry.getKey());
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

    public void tournamentOver() {
        synchronized (gameLock) {
            tournamentsLeft--;
            gameLock.notifyAll();
        }
    }
}
