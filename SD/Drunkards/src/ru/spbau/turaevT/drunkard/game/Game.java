package ru.spbau.turaevT.drunkard.game;

import ru.spbau.turaevT.drunkard.field.IField;
import ru.spbau.turaevT.drunkard.objects.IActiveObject;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class Game implements IGame {
    private IField field;
    private List<IActiveObject> activeObjects = new ArrayList<IActiveObject>();
    private List<IActiveObject> addedObjects = new ArrayList<IActiveObject>();

    @Override
    public IField getField() {
        return this.field;
    }

    @Override
    public void setField(IField field) {
        this.field = field;
    }

    @Override
    public void step() {
        activeObjects.addAll(addedObjects);
        addedObjects.clear();

        for (IActiveObject activeObject : this.activeObjects) {
            activeObject.doAction();
        }
    }

    @Override
    public void registerActiveObject(IActiveObject object) {
        addedObjects.add(object);
    }
}
