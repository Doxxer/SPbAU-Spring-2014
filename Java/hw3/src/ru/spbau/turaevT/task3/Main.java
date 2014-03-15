package ru.spbau.turaevT.task3;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.Arrays;

/**
 * Entry point.
 * <p/>
 * First command-line argument is 'compress' or 'decompress'
 * <p/>
 * If first command-line argument is 'compress' then second argument is output file name
 * Other arguments is files or directories to be compressed
 * <p/>
 * If first command-line argument is 'compress' then second argument is archive file name.
 */
public class Main {

    /**
     * Compress given files (or directories) into one zip-file
     * <p/>
     * Decompress given zip-archive and creates original folder structure
     *
     * @param args command-line arguments
     */
    public static void main(String[] args) {
        if (args.length < 2) {
            showUsage();
            return;
        }

        if (args[0].equalsIgnoreCase("compress")) {
            compress(args[1], Arrays.copyOfRange(args, 2, args.length));
        } else if (args[0].equalsIgnoreCase("decompress")) {
            decompress(args[1]);
        } else {
            showUsage();
        }
    }

    private static void compress(String outputFileName, String[] files) {
        try (Zipper zipper = new Zipper(outputFileName)) {
            DirectoryWalker directoryWalker = new DirectoryWalker(zipper);
            for (String file : files) {
                directoryWalker.zip(new File(file));
            }
            zipper.flush();
        } catch (FileNotFoundException e) {
            System.err.println(MessageFormat.format("File cannot be opened or created: {0}", outputFileName));
        } catch (IOException e) {
            System.err.println(MessageFormat.format("An I/O exception occurred while zipping: {0}", e.getMessage()));
        }
    }

    private static void decompress(String inputFileName) {
        try (Unzipper z = new Unzipper(inputFileName)) {
            z.extract();
        } catch (FileNotFoundException e) {
            System.err.println(MessageFormat.format("File not found: {0}", inputFileName));
        } catch (IOException e) {
            System.err.println(MessageFormat.format("An I/O exception occurred while unzipping: {0}", e.getMessage()));
        }
    }

    private static void showUsage() {
        System.err.println("usage: Main compress|decompress outputFileName|inputFileName [file ...] ");
    }
}
