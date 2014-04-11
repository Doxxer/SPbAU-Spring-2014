package ru.spbau.turaevT.spaceships;

import java.text.MessageFormat;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Planet extends SpaceObject {
    public Planet(String name) {
        super(name);
    }

    @Override
    public void hit(SpaceObject spaceObject) {
        if (!spaceObject.processHit(this))
            spaceObject.hit(this);
    }

    @Override
    public boolean processHit(Asteroid asteroid) {
        System.out.println(MessageFormat.format("Planet {0} hit Asteroid {1}", this.getName(), asteroid.getName()));
        return true;
    }

    @Override
    public boolean processHit(Planet planet) {
        System.out.println(MessageFormat.format("Planet {1} hit Planet {0}", this.getName(), planet.getName()));
        return true;
    }

    @Override
    public boolean processHit(SpaceShip spaceShip) {
        System.out.println(MessageFormat.format("Planet {0} hit SpaceShip {1}", this.getName(), spaceShip.getName()));
        return true;
    }
}
