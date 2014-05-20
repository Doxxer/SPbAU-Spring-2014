package ru.spbau.turaevT.task5;

import java.net.MalformedURLException;

/**
 * Entry point.
 * <p/>
 * Starts new game with specified number of tournaments.
 * After the game prints its results.
 * <p/>
 * First command-line argument is path to directory with AI
 */
public class Main {

    /**
     * Starts new game with specified number of tournaments.
     * After the game prints its results.
     * <p/>
     * First command-line argument is path to directory with AI
     *
     * @param args command-line arguments.
     */
    public static void main(String[] args) {
        if (args.length < 1) {
            showUsage();
            return;
        }
        try {
            Game g = new Game(10, args[0], 50);
            System.out.println("Game started...");
            g.run();
        } catch (MalformedURLException | GameException e) {
            System.err.println("Error occurred while game initializing: " + e.getMessage());
        }
    }

    private static void showUsage() {
        System.err.println("Usage: Main [path to directory with *.class -- AI classes]");
    }
}
