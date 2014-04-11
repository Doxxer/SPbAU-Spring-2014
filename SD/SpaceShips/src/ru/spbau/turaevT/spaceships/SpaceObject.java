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

    protected abstract int getSerialNumber();

    public abstract void hit(SpaceObject spaceObject);

    protected void processHit(Asteroid asteroid) {
    }

    protected void processHit(Planet planet) {
    }

    protected void processHit(SpaceShip spaceShip) {
    }

    public String getName() {
        return name;
    }
}
