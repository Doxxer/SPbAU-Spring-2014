package ru.spbau.turaevT.drunkard.field;

/**
 * Prints field to the console
 */
public class FieldConsolePrinter implements IFieldPrinter {

    @Override
    public void display(RectangleField field) {
        for (int y = 0; y < field.getHeight(); y++) {
            for (int x = 0; x < field.getWidth(); x++) {
                System.out.print(field.getCell(x, y).present());
            }
            System.out.println();
        }
    }

    @Override
    public void display(HexagonalField field) {
        for (int y = 0; y < field.getHeight(); y++) {
            for (int x = 0; x < field.getWidth(); x++) {
                if (!field.isValidCoordinates(x, y))
                    continue;
                if (x == 0 && y % 2 == 0)
                    System.out.print(' ');
                System.out.print(field.getCell(x, y).present());
                System.out.print(' ');
            }
            System.out.println();
        }
    }
}
