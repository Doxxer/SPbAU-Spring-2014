package ru.spbau.turaevT.task3;

import java.io.*;
import java.text.MessageFormat;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * <tt>Unzipper</tt> is object allows to decompress given zip-archive and creates folder structure
 * <p/>
 * Zip-archive contains entries, each entry consist of relative filepath, size of file and file content.
 *
 * @author Turaev Timur
 * @version 1.0
 */
public class Unzipper implements Closeable {
    private final DataInputStream inputStream;
    private final ZipInputStream zipStream;

    /**
     * Constructs new <tt>Unzipper</tt>
     *
     * @param fileName file with specified format to be decompressed.
     * @throws FileNotFoundException if given file does not exists
     */
    public Unzipper(String fileName) throws FileNotFoundException {
        zipStream = new ZipInputStream(new FileInputStream(fileName));
        inputStream = new DataInputStream(zipStream);
    }

    /**
     * Decompresses archive and creates original folder structure
     *
     * @throws IOException if an I/O error occurs
     */
    public void extract() throws IOException {
        ZipEntry zipEntry = zipStream.getNextEntry();

        while (zipEntry != null) {
            String fileName = inputStream.readUTF();
            long fileSize = inputStream.readLong();
            new File(fileName).getParentFile().mkdirs();

            try (FileOutputStream out = new FileOutputStream(fileName)) {
                Utilities.copy(inputStream, out, fileSize);
                System.out.println(MessageFormat.format("Unzipped: {0}", fileName));
            }
            zipEntry = zipStream.getNextEntry();
        }
    }

    /**
     * Closes this stream and releases any system resources associated with it.
     *
     * @throws IOException if an I/O error occurs
     */
    @Override
    public void close() throws IOException {
        inputStream.close();
    }
}
