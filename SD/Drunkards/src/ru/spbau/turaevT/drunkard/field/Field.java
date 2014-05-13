package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IPhysicalObject;

public abstract class Field implements IField {
    protected final int width;
    protected final int height;
    protected final ICell[][] cells;

    public Field(int width, int height) {
        cells = new ICell[width][height];
        this.width = width;
        this.height = height;

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
}
