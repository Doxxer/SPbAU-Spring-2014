package ru.spbau.turaevT.drunkard.algorithm;

import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Queue;

/**
 * Simple wave algorithm
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class BFSPathFindingAlgorithm implements IPathFinder {
    private final IField field;
    private final LinkedList<ICell> path = new LinkedList<>();

    public BFSPathFindingAlgorithm(IField field) {
        this.field = field;
    }


    private boolean findPath(ICell start, ICell finish) {
        path.clear();
        HashMap<ICell, ICell> parents = new HashMap<>();

        ICell from = start;

        Queue<ICell> queue = new LinkedList<>();
        queue.add(from);
        parents.put(start, null);

        while (from != finish && !queue.isEmpty()) {
            from = queue.poll();
            for (ICell to : field.getNearCells(from)) {
                if (!parents.containsKey(to) && (to.isEmpty() || to == finish)) {
                    parents.put(to, from);
                    queue.offer(to);
                }
            }
        }

        // traceback
        if (!parents.containsKey(finish)) {
            return false;
        }
        ICell parent = finish;
        while (parent != null) {
            path.addFirst(parent);
            parent = parents.get(parent);
        }
        path.removeFirst();
        return true;
    }

    private boolean checkPath(ICell start, ICell finish) {
        if (path.isEmpty() || !field.getNearCells(start).contains(path.getFirst()) || path.getLast() != finish)
            return false;

        return path.stream().allMatch(c -> c == finish || c.isEmpty());
    }

    @Override
    public ICell getNextCell(ICell start, ICell finish) {
        if (isPathExists(start, finish))
            return path.removeFirst();
        else return null;
    }

    private boolean isPathExists(ICell start, ICell finish) {
        return (start != null && finish != null) && (checkPath(start, finish) || findPath(start, finish));
    }
}
