package ru.spbau.turaevT.task3;

import java.io.File;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

/**
 * <tt>DirectoryWalker</tt> is an object allows to traverse files and directories (recursively).
 * <p/>
 * <tt>DirectoryWalker</tt> stores all traversed files.
 * It may be retrieval by the {@link #getFiles()} method.
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class DirectoryWalker {
    private final List<File> files;

    /**
     * Constructs new <tt>DirectoryWalker</tt>
     */
    public DirectoryWalker() {
        files = new ArrayList<>();
    }

    /**
     * Traverse given file or directory
     *
     * @param file file or directory to be traversed
     */
    public void traverse(File file) {
        if (!file.isDirectory()) {
            files.add(file);
        } else {
            if (!file.canRead()) {
                System.err.println(MessageFormat.format("Warning: directory {0} cannot be read", file));
                return;
            }
            File[] directoryContent = file.listFiles();
            if (directoryContent != null) {
                for (File f : directoryContent) {
                    traverse(f);
                }
            }
        }
    }

    /**
     * Retrieve all saved files, that has been traversed
     *
     * @return saved files, that has been traversed
     */
    public List<File> getFiles() {
        return files;
    }
}
