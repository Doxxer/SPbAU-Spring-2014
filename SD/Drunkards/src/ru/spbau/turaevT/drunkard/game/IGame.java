package ru.spbau.turaevT.drunkard.game;

import ru.spbau.turaevT.drunkard.field.IField;
import ru.spbau.turaevT.drunkard.objects.IActiveObject;

public interface IGame {
    void setField(IField field);
    IField getField();

    void step();
    void registerActiveObject(IActiveObject object);
}
