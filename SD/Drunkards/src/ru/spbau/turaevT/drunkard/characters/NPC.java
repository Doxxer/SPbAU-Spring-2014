package ru.spbau.turaevT.drunkard.characters;

import ru.spbau.turaevT.drunkard.objects.Lantern;
import ru.spbau.turaevT.drunkard.objects.StaticObject;

public abstract class NPC extends StaticObject implements INPC {

    @Override
    public void processColliding(Lantern object) {
    }
}
