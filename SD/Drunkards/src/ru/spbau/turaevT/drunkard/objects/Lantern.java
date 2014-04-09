package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.characters.INPC;
import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;

import java.util.ArrayList;

public class Lantern extends PhysicalObject {
    private final static int RADIUS = 3;

    private ArrayList<ICell> lightedCells = null;

    /**
     * Returns symbol, associating this object
     *
     * @return Symbol, representing the cell
     */
    @Override
    public char present() {
        return 'L';
    }

    @Override
    public void detectCollision(INPC npc) {
        npc.processColliding(this);
    }

    /**
     * Detects is given cell lighted or nor
     *
     * @return true if cell is lighted, otherwise false
     */
    public ArrayList<ICell> getLightedCells() {
        if (lightedCells == null) {
            lightedCells = new ArrayList<>();
            IField field = getField();
            int lanternY = getCell().getyCoordinate(), lanternX = getCell().getxCoordinate();
            for (int y = lanternY - RADIUS; y <= lanternY + RADIUS; y++) {
                for (int x = lanternX - RADIUS; x <= lanternX + RADIUS; x++) {
                    if (field.isValidCoordinates(x, y)) {
                        ICell cell = getField().getCell(x, y);
                        if (getField().getDistanceBetweenCells(this.getCell(), cell) <= RADIUS) {
                            lightedCells.add(cell);
                        }
                    }
                }
            }
        }
        return lightedCells;
    }
}
