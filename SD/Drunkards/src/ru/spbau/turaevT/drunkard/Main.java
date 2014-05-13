package ru.spbau.turaevT.drunkard;

import ru.spbau.turaevT.drunkard.algorithm.BFSPathFindingAlgorithm;
import ru.spbau.turaevT.drunkard.buildings.Bar;
import ru.spbau.turaevT.drunkard.buildings.BeggarHouse;
import ru.spbau.turaevT.drunkard.buildings.PoliceStation;
import ru.spbau.turaevT.drunkard.field.FieldConsolePrinter;
import ru.spbau.turaevT.drunkard.field.HexagonalField;
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
 * <p>
 * First command line argument (if exists) is delay between steps in milliseconds
 * If it doesn't exist delay is 500 ms by default
 */
public class Main {

    public static void main(String[] args) throws IOException, InterruptedException {
        int delay = args.length > 0 ? Integer.parseInt(args[0]) : 500;

        IFieldPrinter printer = new FieldConsolePrinter();
        IGame game = new Game();
        //IField field = new RectangleField(15, 15);
        HexagonalField field = new HexagonalField(15, 15);

        Bar bar = new Bar(field, field.getCell(9, 0), game);
        Lantern lantern = new Lantern();
        Column column = new Column();
        PoliceStation policeStation = new PoliceStation(field, field.getCell(14, 3), game, lantern, new BFSPathFindingAlgorithm(field));
        BeggarHouse beggarHouse = new BeggarHouse(field, field.getCell(0, 4), game, new BFSPathFindingAlgorithm(field));

        field.registerStaticObject(column, 7, 7);
        field.registerStaticObject(lantern, 10, 3);
        game.registerActiveObject(bar);
        game.registerActiveObject(policeStation);
        game.registerActiveObject(beggarHouse);

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
