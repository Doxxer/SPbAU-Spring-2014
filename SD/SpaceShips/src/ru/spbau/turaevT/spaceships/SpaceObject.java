package ru.spbau.turaevT.spaceships;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
abstract class SpaceObject {
    private final String name;

    SpaceObject(String name) {
        this.name = name;
    }

    public abstract void hit(SpaceObject spaceObject);

    protected boolean processHit(Asteroid asteroid) {
        return false;
    }

    protected boolean processHit(Planet planet) {
        return false;
    }

    protected boolean processHit(SpaceShip spaceShip) {
        return false;
    }

    public String getName() {
        return name;
    }
}
