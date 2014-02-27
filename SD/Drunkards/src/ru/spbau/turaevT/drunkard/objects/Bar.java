package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;
import ru.spbau.turaevT.drunkard.game.IGame;

public class Bar implements IActiveObject {
    private int timer;
    private final ICell cell;
    private final IField field;
    private final IGame game;

    public Bar(IField field, ICell cell, IGame game) {
        this.cell = cell;
        this.field = field;
        this.game = game;
        this.timer = 0;
    }

    @Override
    public void doAction() {
        timer++;

        if (timer == 20)
        {
            timer = 0;
            if (cell.isEmpty())
                spawnDrunk();
        }
    }

    private void spawnDrunk() {
        Drunkard drunk = new Drunkard();
        field.registerStaticObject(drunk, cell.getxCoordinate(), cell.getyCoordinate());
        game.registerActiveObject(drunk);
    }
}
