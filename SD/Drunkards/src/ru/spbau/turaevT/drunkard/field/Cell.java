package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IStaticObject;

public class Cell implements ICell {
    private final int xCoordinate;
    private final int yCoordinate;
    private IStaticObject fieldObject;

    public Cell(int xCoordinate, int yCoordinate) {
        this.xCoordinate = xCoordinate;
        this.yCoordinate = yCoordinate;
    }

    @Override
    public char present() {
        if (isEmpty()) {
            return '.';
        } else {
            return fieldObject.present();
        }
    }

    @Override
    public boolean isEmpty() {
        return fieldObject == null;
    }

    @Override
    public IStaticObject getFieldObject() {
        return fieldObject;
    }

    @Override
    public void setFieldObject(IStaticObject fieldObject) {
        this.fieldObject = fieldObject;
    }

    @Override
    public int getyCoordinate() {
        return yCoordinate;
    }

    @Override
    public int getxCoordinate() {
        return xCoordinate;
    }

}
