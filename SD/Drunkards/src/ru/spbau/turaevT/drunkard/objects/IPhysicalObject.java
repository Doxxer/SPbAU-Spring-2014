package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.characters.INPC;
import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;

/**
 * Provides access to game object
 * <p>
 * PhysicalObject is the object, that can't produce any action
 */
public interface IPhysicalObject {

    /**
     * Return symbol, associating this object
     *
     * @return Symbol, representing the cell
     */
    char present();

    void setField(IField field);

    void setCell(ICell cell);

    void detectCollision(INPC npc);
}
