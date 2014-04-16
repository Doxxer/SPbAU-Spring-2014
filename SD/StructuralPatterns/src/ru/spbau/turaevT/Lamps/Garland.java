package ru.spbau.turaevT.Lamps;

import java.util.ArrayList;
import java.util.List;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Garland implements Switch {

    private final List<Lightbulb> lightbulbs = new ArrayList<>();

    public void addLightbulb(Lightbulb lightbulb) {
        lightbulbs.add(lightbulb);
    }

    @Override
    public void turnOn() {
        int crackedLightbulbsNumber = lightbulbs.size();
        for (Lightbulb l : lightbulbs) {
            try {
                l.turnOn();
            } catch (TurnOnFailedException ignored) {
                crackedLightbulbsNumber++;
            }
        }
        if (crackedLightbulbsNumber == 0) {
            System.out.println("Garland is turn on successfully");
        } else {
            System.out.println("Garland is turn OFF now");
            throw new TurnOnFailedException("Garland is turn OFF now");
        }
    }
}
