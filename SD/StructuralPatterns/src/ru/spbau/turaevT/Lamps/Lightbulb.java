package ru.spbau.turaevT.Lamps;

import java.util.Random;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Lightbulb implements Switch {
    private static final Random generator = new Random();

    @Override
    public void turnOn() {
        int chance = generator.nextInt(100);
        if (chance <= 5) {
            System.out.println("Lightbulb cracked");
            throw new TurnOnFailedException("Lightbulb cracked");
        } else {
            System.out.println("Lightbulb is turned on successfully");
        }
    }
}

