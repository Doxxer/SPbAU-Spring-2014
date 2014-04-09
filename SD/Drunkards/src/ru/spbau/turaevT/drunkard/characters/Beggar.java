package ru.spbau.turaevT.drunkard.characters;

import ru.spbau.turaevT.drunkard.algorithm.IPathFinder;
import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;
import ru.spbau.turaevT.drunkard.objects.Bottle;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Beggar extends NPC {
    private final ICell beggarHouseLocation;
    private final IPathFinder pathFinder;
    private Bottle carryingBottle;
    private ICell target;

    public Beggar(ICell beggarHouseLocation, IPathFinder pathFinder) {
        this.beggarHouseLocation = beggarHouseLocation;
        this.pathFinder = pathFinder;
    }

    @Override
    public void doAction() {
        if (target == null)
            target = findBottle();

        if (target != null) {
            moveAlong(pathFinder, target);
        }
    }

    private ICell findBottle() {
        IField field = getField();
        for (int y = 0; y < field.getHeight(); y++) {
            for (int x = 0; x < field.getWidth(); x++) {
                ICell cell = getField().getCell(x, y);
                if (cell.getFieldObject() instanceof Bottle) {
                    return cell;
                }
            }
        }
        return null;
    }

    @Override
    public void processColliding(Bottle bottle) {
        bottle.setCell(null);
        this.carryingBottle = bottle;
        target = beggarHouseLocation;
    }


    /**
     * Return symbol, associating this object
     *
     * @return Symbol, representing the cell
     */
    @Override
    public char present() {
        return 'z';
    }

    @Override
    public void detectCollision(INPC npc) {
        npc.processColliding(this);
    }

    public Bottle getCarryingBottle() {
        return carryingBottle;
    }

    public void setCarryingBottle(Bottle carryingBottle) {
        this.carryingBottle = carryingBottle;
    }

    public void setTarget(ICell target) {
        this.target = target;
    }
}
