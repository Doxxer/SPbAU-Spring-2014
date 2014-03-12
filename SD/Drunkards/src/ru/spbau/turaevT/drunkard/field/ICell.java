package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IStaticObject;

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

    void setFieldObject(IStaticObject fieldObject);

    IStaticObject getFieldObject();

    int getyCoordinate();

    int getxCoordinate();

}
