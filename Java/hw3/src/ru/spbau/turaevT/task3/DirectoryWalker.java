package ru.spbau.turaevT.task3;

import java.io.File;
import java.text.MessageFormat;

/**
 * <tt>DirectoryWalker</tt> is an object allows to zip files and directories (recursively) via <tt>zip</tt> method.
 * <p/>
 * <tt>DirectoryWalker</tt> uses external <tt>Zipper</tt> to compress files.
 *
 * @author Turaev Timur
 * @version 1.0
 * @see ru.spbau.turaevT.task3.Zipper
 */
public class DirectoryWalker {
    private final Zipper zipper;

    /**
     * Constructs new <tt>DirectoryWalker</tt>
     *
     * @param zipper <tt>Zipper</tt>-object to be used for compress files
     */
    public DirectoryWalker(Zipper zipper) {
        this.zipper = zipper;
    }

    /**
     * Compresses given file or directory via <tt>Zipper</tt>
     *
     * @param file file or directory to be compressed
     */
    public void zip(File file) {
        if (!file.exists()) {
            System.err.println(MessageFormat.format("File does not exist: {0}", file.getAbsolutePath()));
            return;
        }

        if (!file.canRead()) {
            System.err.println(MessageFormat.format("Access denied to {0}", file.getAbsolutePath()));
            return;
        }

        if (!file.isDirectory()) {
            zipper.zip(file);
        } else {
            File[] subDirs = file.listFiles();
            if (subDirs != null) {
                for (File subDir : subDirs) {
                    zip(subDir);
                }
            }
        }
    }
}
