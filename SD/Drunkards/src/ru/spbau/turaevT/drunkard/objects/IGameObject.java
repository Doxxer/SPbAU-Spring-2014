package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;

/**
 * Provides access to game object
 *
 * GameObject is the object, that can't produce any action, i.e. static object
 */
public interface IGameObject {

    /**
     * Return symbol, associating this object
     *
     * @return Symbol, representing the cell
     */
    char present();

    void setField(IField field);

    void setCell(ICell cell);

    void detectCollision(ICharacter object);
}
