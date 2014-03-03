package ru.spbau.turaevT.drunkard.objects;

public class Bottle extends GameObject{
    @Override
    public char present() {
        return 'B';
    }

    @Override
    public void detectCollision(ICharacter object) {
        object.processColliding(this);
    }
}
