package ru.spbau.turaevT.drunkard.characters;

import ru.spbau.turaevT.drunkard.algorithm.IPathFinder;
import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.objects.Bottle;
import ru.spbau.turaevT.drunkard.objects.Column;
import ru.spbau.turaevT.drunkard.objects.Lantern;
import ru.spbau.turaevT.drunkard.objects.PhysicalObject;

public abstract class NPC extends PhysicalObject implements INPC {

    @Override
    public void processColliding(Column object) {
        // do nothing
    }

    protected void moveAlong(IPathFinder route, ICell target) {
        ICell nextCell = route.getNextCell(getCell(), target);
        if (nextCell == null) return;

        if (!nextCell.isEmpty()) {
            nextCell.getFieldObject().detectCollision(this);
        }
        this.setCell(nextCell);
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
    public void processColliding(Policeman object) {
        // do nothing
    }

    @Override
    public void processColliding(Beggar object) {
        // do nothing
    }
}
