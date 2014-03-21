package ru.spbau.turaevT.drunkard.characters;

import ru.spbau.turaevT.drunkard.algorithm.IPathFinder;
import ru.spbau.turaevT.drunkard.field.ICell;

/**
 * TODO Add javadoc!
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Policeman extends NPC {
    private final ICell policeStationLocation;
    private final IPathFinder pathFinder;
    private ICell target;
    private Drunkard carryingDrunkard;

    public Policeman(ICell policeStationLocation, IPathFinder pathFinder) {
        this.policeStationLocation = policeStationLocation;
        this.pathFinder = pathFinder;
    }

    @Override
    public void doAction() {
        ICell nextCell = pathFinder.getNextCell(getCell(), target);
        if (nextCell == null) return;

        if (!nextCell.isEmpty()) {
            nextCell.getFieldObject().detectCollision(this);
            if (nextCell == target) {
                target = policeStationLocation;
            }
        }
        this.setCell(nextCell);
    }

    @Override
    public void processColliding(Drunkard drunkard) {
        if (drunkard.getCell() == target) { // found it!
            drunkard.setCell(null);
            this.carryingDrunkard = drunkard;
        }
    }

    /**
     * Return symbol, associating this object
     *
     * @return Symbol, representing the cell
     */
    @Override
    public char present() {
        return 'P';
    }

    @Override
    public void detectCollision(INPC npc) {
        npc.processColliding(this);
    }

    public Drunkard getCarryingDrunkard() {
        return carryingDrunkard;
    }

    public void setCarryingDrunkard(Drunkard carryingDrunkard) {
        this.carryingDrunkard = carryingDrunkard;
    }

    public void setTarget(ICell target) {
        this.target = target;
    }
}
