package ru.spbau.turaevT.drunkard.characters;

import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.objects.Bottle;
import ru.spbau.turaevT.drunkard.objects.Column;

import java.util.List;
import java.util.Random;

public class Drunkard extends NPC {
    private static final Random random = new Random();
    private DrunkardState state = DrunkardState.WALKING;
    private Bottle bottle = new Bottle();

    @Override
    public void doAction() {
        if (this.state != DrunkardState.WALKING)
            return;

        if (getCell() == null)
            return;

        List<ICell> nearCells = getField().getNearCells(getCell());

        if (nearCells.isEmpty())
            return;

        ICell nextCell = nearCells.get(random.nextInt(nearCells.size()));

        if (nextCell.isEmpty()) {
            ICell old = getCell();
            this.setCell(null);

            if (bottle != null && random.nextInt(30) == 0) {
                bottle.setCell(old);
                bottle = null;
            }
            this.setCell(nextCell);
        } else {
            nextCell.getFieldObject().detectCollision(this);
        }
    }

    @Override
    public char present() {
        switch (this.state) {
            case WALKING:
                return 'D';
            case SLEEPING:
                return 'Z';
            case LYING:
                return '&';
        }
        return '?';
    }

    @Override
    public void detectCollision(INPC npc) {
        npc.processColliding(this);
    }

    @Override
    public void processColliding(Column object) {
        this.setState(DrunkardState.SLEEPING);
    }

    @Override
    public void processColliding(Drunkard object) {
        if (object.getState() == DrunkardState.SLEEPING)
            this.setState(DrunkardState.SLEEPING);
    }

    @Override
    public void processColliding(Bottle object) {
        this.setState(DrunkardState.LYING);
    }

    public DrunkardState getState() {
        return state;
    }

    public void setState(DrunkardState state) {
        this.state = state;
    }

    public enum DrunkardState {
        WALKING,
        SLEEPING,
        LYING
    }
}
