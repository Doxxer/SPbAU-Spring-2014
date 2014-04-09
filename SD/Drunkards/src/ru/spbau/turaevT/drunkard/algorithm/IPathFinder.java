package ru.spbau.turaevT.drunkard.algorithm;

import ru.spbau.turaevT.drunkard.field.ICell;

/**
 * General path finding algorithm
 *
 * @author Turaev Timurw
 * @version 1.0
 */
public interface IPathFinder {
    /**
     * Returns next cell in the path from start to finish
     * This cell is always empty, except for finish cell is the non-empty target
     * <p>
     * Returns null if there is no path from start to finish
     *
     * @param start  start cell
     * @param finish finish cell
     * @return next cell in the path from start to finish
     */
    ICell getNextCell(ICell start, ICell finish);
}
