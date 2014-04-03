package ru.spbau.turaevT.drunkard.buildings;

import ru.spbau.turaevT.drunkard.algorithm.IPathFinder;
import ru.spbau.turaevT.drunkard.characters.Drunkard;
import ru.spbau.turaevT.drunkard.characters.Policeman;
import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;
import ru.spbau.turaevT.drunkard.game.IGame;
import ru.spbau.turaevT.drunkard.objects.IActiveObject;
import ru.spbau.turaevT.drunkard.objects.Lantern;

public class PoliceStation implements IActiveObject {
    private final ICell cell;
    private final IGame game;
    private final Lantern lantern;
    private final Policeman policeman;

    public PoliceStation(IField field, ICell cell, IGame game, Lantern lantern, IPathFinder pathFinder) {
        this.cell = cell;
        this.game = game;
        this.lantern = lantern;

        policeman = new Policeman(cell, pathFinder);
        field.registerStaticObject(policeman, cell.getxCoordinate(), cell.getyCoordinate());
        resetPoliceman();
    }

    private void resetPoliceman() {
        policeman.setCell(null);
        policeman.setTarget(null);
        policeman.setCarryingDrunkard(null);
        game.unregisterActiveObject(policeman);
    }

    private void releasePoliceman(ICell target) {
        policeman.setCell(cell);
        policeman.setTarget(target);
        game.registerActiveObject(policeman);
    }

    @Override
    public void doAction() {
        if (policeman.getCell() == cell && policeman.getCarryingDrunkard() != null) { // policeman has returned
            resetPoliceman();
            return;
        }

        if (!cell.isEmpty() || policeman.getCell() != null) // non free or policeman is out
            return;

        for (ICell cell : lantern.getLightedCells()) {
            if (cell.getFieldObject() instanceof Drunkard) {
                Drunkard drunkard = (Drunkard) cell.getFieldObject();
                if (drunkard.getState() == Drunkard.DrunkardState.LYING
                        || drunkard.getState() == Drunkard.DrunkardState.SLEEPING) {
                    releasePoliceman(drunkard.getCell());
                    break;
                }
            }
        }
    }
}