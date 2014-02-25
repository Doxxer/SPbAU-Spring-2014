package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;

/**
 * Provides access to game object
 */
public interface IGameObject {
    char present();

    void setField(IField field);
    IField getField();

    void setCell(ICell cell);
    ICell getCell();

    void detectCollision(IMob object);
}
