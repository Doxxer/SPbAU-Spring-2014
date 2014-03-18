package ru.spbau.turaevT.task3;

import java.io.*;
import java.nio.file.AccessDeniedException;
import java.text.MessageFormat;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipOutputStream;

/**
 * <tt>Zipper</tt> is object allows to compress given file to zip-archive in specified format:
 * <p/>
 * <pre>
 *     Relative filepath
 *     Size of file
 *     File content
 *     ...
 * </pre>
 * <p/>
 * <tt>Zipper</tt> uses an underlying file stream to write to.
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Zipper implements Closeable, Flushable {
    private final DataOutputStream outputStream;
    private final ZipOutputStream zip;

    /**
     * Constructs new <tt>Zipper</tt>
     *
     * @param outputFileName output zip-archive file name
     * @throws FileNotFoundException if the output file exists but is a directory rather than a regular file,
     *                               does not exist but cannot be created, or cannot be opened for any other reason
     */
    public Zipper(String outputFileName) throws FileNotFoundException {
        zip = new ZipOutputStream(new FileOutputStream(outputFileName));
        outputStream = new DataOutputStream(zip);
    }

    /**
     * Compress given file in specified format
     *
     * @param file the file to be compressed
     * @throws FileNotFoundException  if the file doesn't exist
     * @throws AccessDeniedException  if read operation is denied by operating system
     * @throws DuplicateFileException if the file has already been archived into the same zip-archive
     * @throws ZipException           if a ZIP format error has occurred
     * @throws IOException            if an I/O error occurs
     */
    public void zip(File file) throws FileNotFoundException, AccessDeniedException, DuplicateFileException, ZipException, IOException {
        if (file.exists() && !file.canRead()) {
            throw new AccessDeniedException(MessageFormat.format("Access denied to {0}", file.getAbsolutePath()));
        }

        try (FileInputStream inputStream = new FileInputStream(file)) {
            try {
                zip.putNextEntry(new ZipEntry(file.getAbsolutePath()));
            } catch (ZipException ex) {
                if (ex.getMessage().startsWith("duplicate entry")) {
                    throw new DuplicateFileException(MessageFormat.format("File has already been archived: {0}", file.getAbsoluteFile()));
                }
                throw ex;
            }
            outputStream.writeUTF(file.getPath());
            outputStream.writeLong(file.length());
            Utilities.copy(inputStream, outputStream);
            System.out.println(MessageFormat.format("File has been added to archive: {0}", file.getAbsoluteFile()));
        } finally {
            zip.closeEntry();
        }
    }

    /**
     * Closes this stream and releases any system resources associated with it.
     *
     * @throws IOException if an I/O error occurs
     */
    @Override
    public void close() throws IOException {
        outputStream.close();
    }

    /**
     * Flushes this stream by writing any buffered output to the underlying stream.
     *
     * @throws IOException If an I/O error occurs
     */
    @Override
    public void flush() throws IOException {
        outputStream.flush();
    }
}

