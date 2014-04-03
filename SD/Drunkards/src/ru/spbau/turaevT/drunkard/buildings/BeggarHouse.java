package ru.spbau.turaevT.drunkard.buildings;

import ru.spbau.turaevT.drunkard.algorithm.IPathFinder;
import ru.spbau.turaevT.drunkard.characters.Beggar;
import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;
import ru.spbau.turaevT.drunkard.game.IGame;
import ru.spbau.turaevT.drunkard.objects.IActiveObject;

/**
 * Beggar house
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class BeggarHouse implements IActiveObject {
    private final ICell cell;
    private final IGame game;
    private int timer = 0;
    private final Beggar beggar;

    public BeggarHouse(IField field, ICell cell, IGame game, IPathFinder pathFinder) {
        this.cell = cell;
        this.game = game;

        beggar = new Beggar(cell, pathFinder);
        field.registerStaticObject(beggar, cell.getxCoordinate(), cell.getyCoordinate());
        releaseBeggar();
    }

    @Override
    public void doAction() {
        if (beggar.getCell() == cell && beggar.getCarryingBottle() != null) { // beggar has returned
            resetBeggar();
            return;
        }

        if (beggar.getCell() != null) { // non free or beggar is out
            return;
        }

        timer++;
        if (timer == 30) {
            if (cell.isEmpty()) {
                releaseBeggar();
            }
            timer = 0;
        }

    }

    private void resetBeggar() {
        beggar.setCell(null);
        beggar.setCarryingBottle(null);
        beggar.setTarget(null);
        game.unregisterActiveObject(beggar);
    }

    private void releaseBeggar() {
        beggar.setCell(cell);
        beggar.setTarget(null);
        game.registerActiveObject(beggar);
    }
}
