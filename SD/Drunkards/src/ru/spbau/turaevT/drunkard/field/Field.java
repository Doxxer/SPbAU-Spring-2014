package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IGameObject;

import java.util.ArrayList;
import java.util.List;

public class Field implements IField {
    private int width;
    private int height;
    private ICell[][] cells;

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
    public void registerStaticObject(IGameObject object, int x, int y) {
        // todo check cell is empty and throw exception else

        object.setField(this);
        object.setCell(cells[x][y]);
        cells[x][y].setFieldObject(object);
    }

    @Override
    public List<ICell> getNearCells(ICell cell) {
        ArrayList<ICell> nearCells = new ArrayList<ICell>();

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

    private boolean isValidCoordinates(int x, int y) {
        return 0 <= x && x < getWidth() && 0 <= y && y < getHeight();
    }
}
