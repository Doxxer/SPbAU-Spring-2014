package ru.spbau.turaevT.task5;

import java.net.MalformedURLException;

public class Main {

    public static void main(String[] args) {
        try {
            Game g = new Game(50,
                    "/Users/doxer/Documents/GitRepositories/SPbAU-Spring-2014/Java/hw5/out/production/hw5/ru/spbau/turaevT/task5/ai",
                    100);
            g.run();
        } catch (MalformedURLException | GameException e) {
            System.err.println("Error occurred while game initializing: " + e.getMessage());
        }
    }
}
