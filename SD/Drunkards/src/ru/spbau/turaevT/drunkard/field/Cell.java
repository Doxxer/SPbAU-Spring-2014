package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IGameObject;

public class Cell implements ICell {
    private IGameObject fieldObject;
    private int xCoordinate;
    private int yCoordinate;

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
    public void setFieldObject(IGameObject fieldObject) {
        this.fieldObject = fieldObject;
    }

    @Override
    public IGameObject getFieldObject() {
        return fieldObject;
    }

    @Override
    public int getyCoordinate() {
        return yCoordinate;
    }

    @Override
    public void setyCoordinate(int yCoordinate) {
        this.yCoordinate = yCoordinate;
    }

    @Override
    public int getxCoordinate() {
        return xCoordinate;
    }

    @Override
    public void setxCoordinate(int xCoordinate) {
        this.xCoordinate = xCoordinate;
    }
}
