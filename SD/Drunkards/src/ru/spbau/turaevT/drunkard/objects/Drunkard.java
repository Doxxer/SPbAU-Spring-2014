package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.field.ICell;

import java.util.List;
import java.util.Random;

public class Drunkard extends Character {
    private enum DrunkardState {
        WALKING,
        SLEEPING,
        LYING
    }

    private static final Random random = new Random();
    private DrunkardState state = DrunkardState.WALKING;
    private Bottle bottle = new Bottle();

    @Override
    public void doAction() {
        if (this.state != DrunkardState.WALKING)
            return;

        List<ICell> nearCells = getField().getNearCells(getCell());
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
                return 'L';
        }
        return '?';
    }

    @Override
    public void detectCollision(ICharacter object) {
        object.processColliding(this);
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

    private void setState(DrunkardState state) {
        this.state = state;
    }
}
