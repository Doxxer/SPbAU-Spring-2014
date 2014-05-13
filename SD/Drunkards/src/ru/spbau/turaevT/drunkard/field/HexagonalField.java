package ru.spbau.turaevT.drunkard.field;

import java.util.ArrayList;
import java.util.List;

public class HexagonalField extends Field {

    public HexagonalField(int width, int height) {
        super(width, height);
    }

    @Override
    public List<ICell> getNearCells(ICell cell) {
        ArrayList<ICell> nearCells = new ArrayList<>();

        int x = cell.getxCoordinate();
        int y = cell.getyCoordinate();

        if (isValidCoordinates(x - 1, y)) {
            nearCells.add(getCell(x - 1, y));
        }
        if (isValidCoordinates(x + 1, y)) {
            nearCells.add(getCell(x + 1, y));
        }
        if (isValidCoordinates(x, y - 1)) {
            nearCells.add(getCell(x, y - 1));
        }
        if (isValidCoordinates(x, y + 1)) {
            nearCells.add(getCell(x, y + 1));
        }

        if (y % 2 == 0) {
            if (isValidCoordinates(x + 1, y - 1)) {
                nearCells.add(getCell(x + 1, y - 1));
            }
            if (isValidCoordinates(x + 1, y + 1)) {
                nearCells.add(getCell(x + 1, y + 1));
            }
        } else {
            if (isValidCoordinates(x - 1, y - 1)) {
                nearCells.add(getCell(x - 1, y - 1));
            }
            if (isValidCoordinates(x - 1, y + 1)) {
                nearCells.add(getCell(x - 1, y + 1));
            }
        }

        return nearCells;

    }

    @Override
    public int getDistanceBetweenCells(ICell cell1, ICell cell2) {
        int x1 = cell1.getxCoordinate();
        int x2 = cell2.getxCoordinate();
        int y1 = cell1.getyCoordinate();
        int y2 = cell2.getyCoordinate();

        int dx = x2 - x1 + (y2 % 2 == 1 ? y2 - 1 : y2) / 2 - (y1 % 2 == 1 ? y1 - 1 : y1) / 2;
        int dy = y1 - y2;
        if (dx * dy > 0)
            return Math.abs(dx + dy);
        else {
            return Math.max(Math.abs(dx), Math.abs(dy));
        }
    }

    @Override
    public boolean isValidCoordinates(int x, int y) {
        int maxX = y % 2 == 0 ? getWidth() - 1 : getWidth();
        return 0 <= x && x < maxX && 0 <= y && y < getHeight();
    }
}
