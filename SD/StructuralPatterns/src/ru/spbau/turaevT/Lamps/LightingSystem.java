package ru.spbau.turaevT.Lamps;

import java.util.ArrayList;
import java.util.List;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class LightingSystem implements Switch {
    private final List<Switch> subsystems = new ArrayList<>();

    public void add(Switch switcher) {
        subsystems.add(switcher);
    }


    @Override
    public void turnOn() {
        int okLightbulbsNumber = subsystems.size();
        for (Switch l : subsystems) {
            try {
                l.turnOn();
            } catch (TurnOnFailedException ignored) {
                okLightbulbsNumber--;
            }
        }
        if (okLightbulbsNumber == 0) {
            System.out.println("LightingSystem is turn OFF now");
            throw new TurnOnFailedException("LightingSystem is turn OFF now");
        } else {
            System.out.println("LightingSystem is turn on successfully");
        }
    }
}
