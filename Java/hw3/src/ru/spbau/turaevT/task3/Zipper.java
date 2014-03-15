package ru.spbau.turaevT.task3;

import java.io.*;
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
     * @throws FileNotFoundException if the file exists but is a directory rather than a regular file,
     *                               does not exist but cannot be created, or cannot be opened for any other reason
     */
    public Zipper(String outputFileName) throws FileNotFoundException {
        zip = new ZipOutputStream(new FileOutputStream(outputFileName));
        outputStream = new DataOutputStream(zip);
    }

    /**
     * Compress given file in specified format
     *
     * @param file file to be compressed
     */
    public void zip(File file) {
        try (FileInputStream inputStream = new FileInputStream(file)) {
            zip.putNextEntry(new ZipEntry(file.getPath()));
            outputStream.writeUTF(file.getPath());
            outputStream.writeLong(file.length());
            Utilities.copy(inputStream, outputStream);
            System.out.println(MessageFormat.format("File successfully added to archive: {0}", file.getAbsoluteFile()));
        } catch (FileNotFoundException e) {
            System.err.println(MessageFormat.format("File not found: {0}", file.getAbsolutePath()));
        } catch (ZipException e) {
            System.err.println(MessageFormat.format("A a ZIP format error occurred: {0}", e.getMessage()));
        } catch (IOException e) {
            System.err.println(MessageFormat.format("An I/O exception occurred: {0}", e.getMessage()));
        } finally {
            try {
                zip.closeEntry();
            } catch (IOException e) {
                System.err.println(MessageFormat.format("An I/O exception occurred: {0}", e.getMessage()));
            }
        }
    }

    /**
     * Closes this stream and releases any system resources associated with it.
     *
     * @throws IOException if an I/O error occurs
     */
    @Override
    public void close() throws IOException {
        zip.close();
        outputStream.close();
    }

    /**
     * Flushes this stream by writing any buffered output to the underlying stream.
     *
     * @throws IOException If an I/O error occurs
     */
    @Override
    public void flush() throws IOException {
        zip.flush();
        outputStream.flush();
    }
}
