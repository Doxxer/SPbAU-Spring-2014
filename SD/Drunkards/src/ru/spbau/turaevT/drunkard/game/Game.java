package ru.spbau.turaevT.drunkard.game;

import ru.spbau.turaevT.drunkard.objects.IActiveObject;

import java.util.ArrayList;
import java.util.List;

/**
 * Simple implementation if <tt>IGame</tt> interface<p>
 */
public class Game implements IGame {
    private final List<IActiveObject> activeObjects = new ArrayList<>();
    private final List<IActiveObject> addedObjects = new ArrayList<>();
    private final List<IActiveObject> removedObjects = new ArrayList<>();

    /**
     * {@inheritDoc}
     */
    @Override
    public void makeStep() {
        activeObjects.addAll(addedObjects);
        activeObjects.removeAll(removedObjects);
        addedObjects.clear();
        removedObjects.clear();

        activeObjects.forEach(IActiveObject::doAction);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void registerActiveObject(IActiveObject object) {
        addedObjects.add(object);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unregisterActiveObject(IActiveObject object) {
        removedObjects.add(object);
    }
}
