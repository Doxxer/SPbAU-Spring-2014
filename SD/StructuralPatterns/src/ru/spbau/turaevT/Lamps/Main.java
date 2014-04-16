package ru.spbau.turaevT.Lamps;

/**
 * @author Turaev Timur
 * @version 1.0
 */
class Main {
    public static void main(String[] args) {
        Chandelier chandelier = new Chandelier();
        Garland garland1 = new Garland();
        Garland garland2 = new Garland();

        for (int i = 0; i < 3; i++) {
            chandelier.addLightbulb(new Lightbulb());
            garland1.addLightbulb(new Lightbulb());
            garland2.addLightbulb(new Lightbulb());
        }

        LightingSystem system1 = new LightingSystem();
        system1.add(chandelier);
        system1.add(garland1);
        system1.add(garland2);
        system1.add(new Lightbulb());
        system1.add(new Lightbulb());

        LightingSystem system2 = new LightingSystem();
        system2.add(new Lightbulb());
        system2.add(system1);

        try {
            system2.turnOn();
        } catch (TurnOnFailedException ignored) {
        }
    }
}
