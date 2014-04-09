package ru.spbau.turaevT.spaceships;

import java.text.MessageFormat;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class SpaceShip extends SpaceObject {
    public SpaceShip(String name) {
        super(name);
    }

    @Override
    public void hit(SpaceObject spaceObject) {
        spaceObject.processHit(this);
    }

    @Override
    public void processHit(Asteroid asteroid) {
        System.out.println(MessageFormat.format("SpaceShip {0} hit Asteroid {1}", this.name, asteroid.name));
    }

    @Override
    public void processHit(Planet planet) {
        System.out.println(MessageFormat.format("Planet {1} hit SpaceShip {0}", this.name, planet.name));
    }

    @Override
    public void processHit(SpaceShip spaceShip) {
        System.out.println(MessageFormat.format("SpaceShip {0} hit SpaceShip {1}", this.name, spaceShip.name));
    }
}
