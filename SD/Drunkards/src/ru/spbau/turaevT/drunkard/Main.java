package ru.spbau.turaevT.drunkard;

import ru.spbau.turaevT.drunkard.buildings.Bar;
import ru.spbau.turaevT.drunkard.field.Field;
import ru.spbau.turaevT.drunkard.field.FieldConsolePrinter;
import ru.spbau.turaevT.drunkard.field.IField;
import ru.spbau.turaevT.drunkard.field.IFieldPrinter;
import ru.spbau.turaevT.drunkard.game.Game;
import ru.spbau.turaevT.drunkard.game.IGame;
import ru.spbau.turaevT.drunkard.objects.Column;
import ru.spbau.turaevT.drunkard.objects.Lantern;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.concurrent.TimeUnit;

/**
 * Entry point.
 *
 * First command line argument (if exists) is delay between steps in milliseconds
 * If it doesn't exist delay is 500 ms by default
 */
public class Main {

    public static void main(String[] args) throws IOException, InterruptedException {
        int delay = args.length > 0 ? Integer.parseInt(args[0]) : 500;

        IFieldPrinter printer = new FieldConsolePrinter();
        IGame game = new Game();
        IField field = new Field(15, 15);
        Bar bar = new Bar(field, field.getCell(9, 0), game);
        Lantern lantern = new Lantern();
        Column column = new Column();

        field.registerStaticObject(column, 7, 7);
        field.registerStaticObject(lantern, 10, 3);
        game.registerActiveObject(bar);

        for (int i = 1; i <= 500; i++) {
            TimeUnit.MILLISECONDS.sleep(delay);
            clearScreen();
            game.makeStep();
            printer.display(field);
            System.out.println(MessageFormat.format("Step #{0}", i));
        }
    }

    private static void clearScreen() {
        System.out.print("\033[H\033[2J");
        System.out.flush();
    }
}
