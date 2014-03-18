package ru.spbau.turaevT.task3;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.AccessDeniedException;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.zip.ZipException;

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
     * Compresses given files (or directories) into one zip-file
     * <p/>
     * Decompresses given zip-archive and creates original folder structure
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
        DirectoryWalker directoryWalker = new DirectoryWalker();
        for (String file : files) {
            directoryWalker.traverse(new File(file));
        }

        try (Zipper zipper = new Zipper(outputFileName)) {
            for (File file : directoryWalker.getFiles()) {
                try {
                    zipper.zip(file);
                } catch (SecurityException ex) {
                    System.err.println(MessageFormat.format("Access to {0} denied by SecurityManager: {1}", file, ex.getMessage()));
                } catch (FileNotFoundException e) {
                    System.err.println(MessageFormat.format("File not found: {0}", file.getAbsoluteFile()));
                } catch (AccessDeniedException e) {
                    System.err.println(MessageFormat.format("Access denied to {0}", file.getAbsolutePath()));
                } catch (DuplicateFileException e) {
                    System.err.println(e.getMessage());
                }
            }
            zipper.flush();
        } catch (SecurityException ex) {
            System.err.println(MessageFormat.format("Access to {0} denied by SecurityManager: {1}", outputFileName, ex.getMessage()));
        } catch (FileNotFoundException e) {
            System.err.println(MessageFormat.format("Output file cannot be opened or created: {0}", outputFileName));
        } catch (ZipException e) {
            System.err.println(MessageFormat.format("A a ZIP format error occurred: {0}", e.getMessage()));
        } catch (IOException e) {
            System.err.println(MessageFormat.format("An I/O exception occurred while zipping: {0}", e.getMessage()));
        }
    }

    private static void decompress(String inputFileName) {
        try (Unzipper z = new Unzipper(inputFileName)) {
            z.extract();
        } catch (SecurityException ex) {
            System.err.println(MessageFormat.format("Access to {0} denied by SecurityManager: {1}", inputFileName, ex.getMessage()));
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
