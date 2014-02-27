package ru.spbau.turaevT.drunkard.game;

import ru.spbau.turaevT.drunkard.objects.IActiveObject;

import java.util.ArrayList;
import java.util.List;

/**
 * Simple implementation if <tt>IGame</tt> interface<p>
 */
public class Game implements IGame {
    private final List<IActiveObject> activeObjects = new ArrayList<IActiveObject>();
    private final List<IActiveObject> addedObjects = new ArrayList<IActiveObject>();

    /**
     * {@inheritDoc}
     */
    @Override
    public void makeStep() {
        activeObjects.addAll(addedObjects);
        addedObjects.clear();

        for (IActiveObject activeObject : this.activeObjects) {
            activeObject.doAction();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void registerActiveObject(IActiveObject object) {
        addedObjects.add(object);
    }
}
