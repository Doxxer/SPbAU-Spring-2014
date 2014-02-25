package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IGameObject;

import java.util.List;

public interface IField {
    int getWidth();
    int getHeight();
    ICell getCell(int x, int y);
    List<ICell> getNearCells(ICell cell);

    void registerStaticObject(IGameObject object, int x, int y);
}
