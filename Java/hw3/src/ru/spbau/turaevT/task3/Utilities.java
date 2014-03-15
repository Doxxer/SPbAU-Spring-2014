package ru.spbau.turaevT.task3;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Utility-class contains methods to buffered copy streams
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Utilities {
    private static final int BLOCK_SIZE = 1024;

    /**
     * Buffered copies data from input stream to output stream while InputStream contains any data.
     *
     * @param in  input stream to be read from.
     * @param out output stream to be write to.
     * @throws IOException if an I/O error occurs
     */
    public static void copy(InputStream in, OutputStream out) throws IOException {
        byte[] buffer = new byte[BLOCK_SIZE];
        while (in.read(buffer) != -1) {
            out.write(buffer);
        }
    }

    /**
     * Buffered copies up to <tt>size</tt> bytes of data from input stream to output stream
     *
     * @param in   input stream to be read from.
     * @param out  output stream to be write to.
     * @param size size of data in input stream to be copied to.
     * @throws IOException if an I/O error occurs
     */
    public static void copy(InputStream in, OutputStream out, long size) throws IOException {
        byte[] buffer = new byte[BLOCK_SIZE];
        int len;
        long remain = size;
        while (remain > 0 && ((len = in.read(buffer, 0, (int) Math.min(remain, BLOCK_SIZE))) != -1)) {
            out.write(buffer, 0, len);
            remain -= len;
        }
    }
}
