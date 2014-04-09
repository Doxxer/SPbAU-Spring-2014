package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IPhysicalObject;

import java.util.ArrayList;
import java.util.List;

public class Field implements IField {
    private final int width;
    private final int height;
    private final ICell[][] cells;

    public Field(int width, int height) {
        this.width = width;
        this.height = height;

        cells = new ICell[width][height];
        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                cells[i][j] = new Cell(i, j);
            }
        }
    }

    @Override
    public int getWidth() {
        return width;
    }

    @Override
    public int getHeight() {
        return height;
    }

    @Override
    public ICell getCell(int x, int y) {
        return cells[x][y];
    }

    @Override
    public void registerStaticObject(IPhysicalObject object, int x, int y) {
        // todo check cell is empty and throw exception else

        object.setField(this);
        object.setCell(cells[x][y]);
        cells[x][y].setFieldObject(object);
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
