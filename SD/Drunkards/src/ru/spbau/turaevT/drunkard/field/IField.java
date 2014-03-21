package ru.spbau.turaevT.drunkard.field;

import ru.spbau.turaevT.drunkard.objects.IStaticObject;

import java.util.List;

/**
 * The <tt>IField</tt> interface provides access to the game field
 */
public interface IField {
    int getWidth();

    int getHeight();

    ICell getCell(int x, int y);

    List<ICell> getNearCells(ICell cell);

    void registerStaticObject(IStaticObject object, int x, int y);

    /**
     * Return distance between tow given cells
     *
     * @param cell1 first cell
     * @param cell2 second cell
     * @return distance between 2 cells
     */
    int getDistanceBetweenCells(ICell cell1, ICell cell2);

    boolean isValidCoordinates(int x, int y);
}