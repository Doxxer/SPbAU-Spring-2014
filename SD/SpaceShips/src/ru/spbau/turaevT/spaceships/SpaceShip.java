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
    protected int getSerialNumber() {
        return 1;
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
        System.out.println(MessageFormat.format("SpaceShip {0} hit Asteroid {1}", this.getName(), asteroid.getName()));
    }

    @Override
    public void processHit(SpaceShip spaceShip) {
        System.out.println(MessageFormat.format("SpaceShip {0} hit SpaceShip {1}", this.getName(), spaceShip.getName()));
    }
}
