package ru.spbau.turaevT.spaceships;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
abstract class SpaceObject {
    final String name;

    SpaceObject(String name) {
        this.name = name;
    }


    public abstract void hit(SpaceObject spaceObject);

    protected abstract void processHit(Asteroid asteroid);

    protected abstract void processHit(Planet planet);

    protected abstract void processHit(SpaceShip spaceShip);

}
