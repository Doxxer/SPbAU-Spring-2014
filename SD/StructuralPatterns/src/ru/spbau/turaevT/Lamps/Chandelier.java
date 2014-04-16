package ru.spbau.turaevT.Lamps;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Turaev Timur
 * @version 1.0
 */
public class Chandelier implements Switch {

    private final List<Lightbulb> lightbulbs = new ArrayList<>();

    public void addLightbulb(Lightbulb lightbulb) {
        lightbulbs.add(lightbulb);
    }

    @Override
    public void turnOn() {
        int okLightbulbsNumber = lightbulbs.size();
        for (Lightbulb l : lightbulbs) {
            try {
                l.turnOn();
            } catch (TurnOnFailedException ignored) {
                okLightbulbsNumber--;
            }
        }
        if (okLightbulbsNumber == 0) {
            System.out.println("Chandelier is turn OFF now");
            throw new TurnOnFailedException("Chandelier is turn OFF now");
        } else {
            System.out.println("Chandelier is turn on successfully");
        }
    }
}
