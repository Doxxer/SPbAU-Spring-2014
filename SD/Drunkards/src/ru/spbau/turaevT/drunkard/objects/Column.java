package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.characters.INPC;

public class Column extends PhysicalObject {

    @Override
    public char present() {
        return 'C';
    }

    @Override
    public void detectCollision(INPC npc) {
        npc.processColliding(this);
    }
}
