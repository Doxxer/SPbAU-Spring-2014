package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.characters.INPC;

public class Bottle extends PhysicalObject {
    @Override
    public char present() {
        return 'B';
    }

    @Override
    public void detectCollision(INPC npc) {
        npc.processColliding(this);
    }
}
