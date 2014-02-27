package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;

/**
 * Provides access to game object
 */
public interface IGameObject {
    char present();

    void setField(IField field);

    void setCell(ICell cell);

    void detectCollision(IMob object);
}
