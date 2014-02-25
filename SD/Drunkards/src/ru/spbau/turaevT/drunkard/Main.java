package ru.spbau.turaevT.drunkard;

import ru.spbau.turaevT.drunkard.field.Field;
import ru.spbau.turaevT.drunkard.field.FieldConsolePrinter;
import ru.spbau.turaevT.drunkard.field.IField;
import ru.spbau.turaevT.drunkard.field.IFieldPrinter;
import ru.spbau.turaevT.drunkard.game.Game;
import ru.spbau.turaevT.drunkard.game.IGame;
import ru.spbau.turaevT.drunkard.objects.Bar;
import ru.spbau.turaevT.drunkard.objects.Column;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.concurrent.TimeUnit;

public class Main {

    public static void main(String[] args) throws IOException, InterruptedException {
        IFieldPrinter printer = new FieldConsolePrinter();
        IGame game = new Game();
        IField field = new Field(15, 15);
        int delay = args.length > 0 ? Integer.parseInt(args[0]) : 500;

        Bar bar = new Bar(field, field.getCell(9, 0), game);
        Column column = new Column();

        field.registerStaticObject(column, 7, 7);

        game.setField(field);
        game.registerActiveObject(bar);

        for (int i = 0; i < 1000000; i++) {
            TimeUnit.MILLISECONDS.sleep(delay);
            clearScreen();
            game.step();
            printer.display(field);
            System.out.println(MessageFormat.format("Step #{0}", i));
        }
    }

    private static void clearScreen() {
        System.out.print("\033[H\033[2J");
        System.out.flush();
    }
}
