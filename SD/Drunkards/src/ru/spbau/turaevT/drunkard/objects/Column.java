package ru.spbau.turaevT.drunkard.objects;

public class Column extends GameObject {

    @Override
    public char present() {
        return 'C';
    }

    @Override
    public void detectCollision(IMob object) {
        object.processColliding(this);
    }
}
