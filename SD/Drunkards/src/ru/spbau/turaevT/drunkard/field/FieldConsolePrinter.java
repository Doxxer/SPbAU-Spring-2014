package ru.spbau.turaevT.drunkard.field;

/**
 * Prints field to the console
 */
public class FieldConsolePrinter implements IFieldPrinter {

    @Override
    public void display(IField field) {
        for (int y = 0; y < field.getHeight(); y++) {
            for (int x = 0; x < field.getWidth(); x++) {
                System.out.print(field.getCell(x, y).present());
            }
            System.out.println();
        }
    }
}
