package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.characters.INPC;

public class Bottle extends StaticObject {
    @Override
    public char present() {
        return 'B';
    }

    @Override
    public void detectCollision(INPC object) {
        object.processColliding(this);
    }
}
