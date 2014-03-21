package ru.spbau.turaevT.drunkard.characters;

import ru.spbau.turaevT.drunkard.objects.Bottle;
import ru.spbau.turaevT.drunkard.objects.Column;
import ru.spbau.turaevT.drunkard.objects.Lantern;
import ru.spbau.turaevT.drunkard.objects.StaticObject;

public abstract class NPC extends StaticObject implements INPC {

    @Override
    public void processColliding(Column object) {
        // do nothing
    }

    @Override
    public void processColliding(Drunkard object) {
        // do nothing
    }

    @Override
    public void processColliding(Bottle object) {
        // do nothing
    }

    @Override
    public void processColliding(Lantern object) {
        // do nothing
    }

    @Override
    public void processColliding(Policeman policeman) {
        // do nothing
    }
}
