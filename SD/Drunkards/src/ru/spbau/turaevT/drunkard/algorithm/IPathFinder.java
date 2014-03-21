package ru.spbau.turaevT.drunkard.algorithm;

import ru.spbau.turaevT.drunkard.field.ICell;

/**
 * General path finding algorithm
 *
 * @author Turaev Timurw
 * @version 1.0
 */
public interface IPathFinder {
    ICell getNextCell(ICell start, ICell finish);
}
