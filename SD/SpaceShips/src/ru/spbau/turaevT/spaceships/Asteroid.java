package ru.spbau.turaevT.spaceships;

import java.text.MessageFormat;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Asteroid extends SpaceObject {
    public Asteroid(String name) {
        super(name);
    }

    @Override
    public void hit(SpaceObject spaceObject) {
        if (!spaceObject.processHit(this))
            spaceObject.hit(this);
    }

    @Override
    public boolean processHit(Asteroid asteroid) {
        System.out.println(MessageFormat.format("Asteroid {0} hit Asteroid {1}", this.getName(), asteroid.getName()));
        return true;
    }
}
