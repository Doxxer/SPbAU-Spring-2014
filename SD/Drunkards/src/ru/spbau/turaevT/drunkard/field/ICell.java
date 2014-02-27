package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IGameObject;

/**
 * The <tt>ICell</tt> interface provides access to cell in the game field
 */
public interface ICell {

    /**
     * Return symbol, associating object in the cell
     *
     * @return Symbol, representing the cell
     */
    char present();

    boolean isEmpty();

    void setFieldObject(IGameObject fieldObject);

    IGameObject getFieldObject();

    int getyCoordinate();

    int getxCoordinate();

}
