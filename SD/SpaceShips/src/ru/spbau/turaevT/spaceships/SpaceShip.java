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
        if (!spaceObject.processHit(this))
            spaceObject.hit(this);
    }

    @Override
    public boolean processHit(Asteroid asteroid) {
        System.out.println(MessageFormat.format("SpaceShip {0} hit Asteroid {1}", this.getName(), asteroid.getName()));
        return true;
    }

    @Override
    public boolean processHit(SpaceShip spaceShip) {
        System.out.println(MessageFormat.format("SpaceShip {0} hit SpaceShip {1}", this.getName(), spaceShip.getName()));
        return true;
    }
}
