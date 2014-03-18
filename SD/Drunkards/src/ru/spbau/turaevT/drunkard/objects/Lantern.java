package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.characters.INPC;

public class Lantern extends StaticObject {
    /**
     * Returns symbol, associating this object
     *
     * @return Symbol, representing the cell
     */
    @Override
    public char present() {
        return 'L';
    }

    @Override
    public void detectCollision(INPC object) {
        object.processColliding(this);
    }
}
