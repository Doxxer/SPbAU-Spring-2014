package ru.spbau.turaevT.drunkard.characters;

import ru.spbau.turaevT.drunkard.objects.*;

/**
 * NPC is a static and active object
 */
public interface INPC extends IActiveObject, IStaticObject {
    void processColliding(Column object);

    void processColliding(Drunkard object);

    void processColliding(Bottle object);

    void processColliding(Lantern object);
}
