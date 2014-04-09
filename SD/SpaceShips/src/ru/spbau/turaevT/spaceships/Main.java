package ru.spbau.turaevT.spaceships;

class Main {
    public static void main(String[] args) {
        SpaceObject asteroid = new Asteroid("AX-01");
        SpaceObject otherAsteroid = new Asteroid("BC-212");

        asteroid.hit(otherAsteroid);
        otherAsteroid.hit(asteroid);

        SpaceObject spaceShip = new SpaceShip("USS Enterprise");
        SpaceObject otherSpaceShip = new SpaceShip("Galactica");

        spaceShip.hit(otherSpaceShip);
        otherSpaceShip.hit(spaceShip);
        spaceShip.hit(asteroid);
        asteroid.hit(spaceShip);

        Planet planet = new Planet("Earth");
        Planet otherPlanet = new Planet("Mars");

        planet.hit(otherPlanet);
        otherPlanet.hit(planet);

        spaceShip.hit(planet);
        planet.hit(spaceShip);
    }
}
