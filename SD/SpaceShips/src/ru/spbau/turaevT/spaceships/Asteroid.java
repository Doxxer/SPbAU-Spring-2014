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
    protected int getSerialNumber() {
        return 0;
    }

    @Override
    public void hit(SpaceObject spaceObject) {
        if (this.getSerialNumber() <= spaceObject.getSerialNumber())
            spaceObject.processHit(this);
        else
            spaceObject.hit(this);
    }

    @Override
    public void processHit(Asteroid asteroid) {
        System.out.println(MessageFormat.format("Asteroid {0} hit Asteroid {1}", this.getName(), asteroid.getName()));
    }
}
