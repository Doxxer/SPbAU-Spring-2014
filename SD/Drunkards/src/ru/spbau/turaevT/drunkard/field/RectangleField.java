package ru.spbau.turaevT.drunkard.field;

import java.util.ArrayList;
import java.util.List;

public class RectangleField extends Field {

    public RectangleField(int width, int height) {
        super(width, height);
    }

    /**
     * Return distance between tow given cells
     *
     * @param cell1 first cell
     * @param cell2 second cell
     * @return distance between 2 cells
     */
    @Override
    public int getDistanceBetweenCells(ICell cell1, ICell cell2) {
        return Math.abs(cell1.getxCoordinate() - cell2.getxCoordinate())
                + Math.abs(cell1.getyCoordinate() - cell2.getyCoordinate());

    }

    @Override
    public List<ICell> getNearCells(ICell cell) {
        ArrayList<ICell> nearCells = new ArrayList<>();

        int x = cell.getxCoordinate();
        int y = cell.getyCoordinate();

        if (isValidCoordinates(x - 1, y)) {
            nearCells.add(getCell(x - 1, y));
        }
        if (isValidCoordinates(x + 1, y)) {
            nearCells.add(getCell(x + 1, y));
        }
        if (isValidCoordinates(x, y - 1)) {
            nearCells.add(getCell(x, y - 1));
        }
        if (isValidCoordinates(x, y + 1)) {
            nearCells.add(getCell(x, y + 1));
        }

        return nearCells;
    }

    @Override
    public boolean isValidCoordinates(int x, int y) {
        return 0 <= x && x < getWidth() && 0 <= y && y < getHeight();
    }
}
