package ru.spbau.turaevT.drunkard.objects;

import ru.spbau.turaevT.drunkard.field.ICell;
import ru.spbau.turaevT.drunkard.field.IField;

public abstract class StaticObject implements IStaticObject {
    private ICell cell;
    private IField field;

    protected IField getField() {
        return this.field;
    }

    public void setField(IField field) {
        this.field = field;
    }

    public ICell getCell() {
        return this.cell;
    }

    public void setCell(ICell cell) {
        if (this.cell != null) {
            this.cell.setFieldObject(null);
        }

        this.cell = cell;

        if (this.cell != null) {
            this.cell.setFieldObject(this);
        }
    }
}
