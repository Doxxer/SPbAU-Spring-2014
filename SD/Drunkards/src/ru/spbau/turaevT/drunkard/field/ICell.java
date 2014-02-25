package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IGameObject;

/**
 * Interface ICell provides access to cell in game field
 */
public interface ICell {

    /**
     *
     * @return Symbol, representing the cell
     */
    char present();

    boolean isEmpty();

    void setFieldObject(IGameObject fieldObject);

    IGameObject getFieldObject();

    int getyCoordinate();

    void setyCoordinate(int yCoordinate);

    int getxCoordinate();

    void setxCoordinate(int xCoordinate);
}
