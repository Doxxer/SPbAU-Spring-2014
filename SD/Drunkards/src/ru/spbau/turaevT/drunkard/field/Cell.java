package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IPhysicalObject;

public class Cell implements ICell {
    private final int xCoordinate;
    private final int yCoordinate;
    private IPhysicalObject fieldObject;

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
    public IPhysicalObject getFieldObject() {
        return fieldObject;
    }

    @Override
    public void setFieldObject(IPhysicalObject fieldObject) {
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
