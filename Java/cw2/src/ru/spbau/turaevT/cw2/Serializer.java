package ru.spbau.turaevT.cw2;

import java.io.FileOutputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.concurrent.Callable;

public class Serializer<T> implements Callable<Integer> {

    private final T objectToSerialize;
    private final Class<T> classInfo;
    private final String fileName;

    public Serializer(T objectToSerialize, Class<T> classInfo, String fileName) {
        this.objectToSerialize = objectToSerialize;
        this.classInfo = classInfo;
        this.fileName = fileName;
    }


    /**
     * Computes a result, or throws an exception if unable to do so.
     *
     * @return computed result
     * @throws Exception if unable to compute a result
     */
    @Override
    public Integer call() throws Exception {
        classInfo.getConstructor();
        OutputStream outputStream = new FileOutputStream(fileName + ".properties");
        Properties properties = new Properties();

        for (Method property : classInfo.getMethods()) {
            if (property.getName().startsWith("get") && !property.getName().equals("getClass")
                    && (property.getReturnType().isPrimitive() || property.getReturnType().equals(String.class))
                    && property.getParameterTypes().length == 0) {
                properties.setProperty(property.getName().substring(3), property.invoke(objectToSerialize).toString());
            }
        }
        properties.store(outputStream, "");
        return 0;
    }
}
