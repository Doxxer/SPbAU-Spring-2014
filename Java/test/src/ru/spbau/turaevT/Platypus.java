package ru.spbau.turaevT;

interface IBird {
    public void layEgg();
}

interface IMammal {
    public void giveMilk();
}

class Bird implements IBird {
    public void layEgg() {
        System.out.println("Laying eggs...");
    }
}

class Mammal implements IMammal {
    public void giveMilk() {
        System.out.println("Giving milk...");
    }
}

public class Platypus implements IMammal, IBird {

    private class LayingEggAnimal extends Bird {
    }

    private class GivingMilkAnimal extends Mammal {
    }

    private LayingEggAnimal layingEggAnimal = new LayingEggAnimal();

    private GivingMilkAnimal givingMilkAnimal = new GivingMilkAnimal();

    @Override
    public void layEgg() {
        layingEggAnimal.layEgg();
    }

    @Override
    public void giveMilk() {
        givingMilkAnimal.giveMilk();
    }

}