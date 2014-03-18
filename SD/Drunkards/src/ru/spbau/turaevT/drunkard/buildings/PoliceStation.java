package ru.spbau.turaevT.drunkard.buildings;

import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;
import ru.spbau.turaevT.drunkard.game.IGame;
import ru.spbau.turaevT.drunkard.objects.IActiveObject;

public class PoliceStation implements IActiveObject {
    private final ICell cell;
    private final IField field;
    private final IGame game;

    public PoliceStation(ICell cell, IField field, IGame game) {
        this.cell = cell;
        this.field = field;
        this.game = game;
    }

    @Override
    public void doAction() {

    }
}
