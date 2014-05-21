package ru.spbau.turaevT.cw2;

import java.io.FileInputStream;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Callable;

public class Deserializer<T> implements Callable<T> {
    private final Class<T> classInfo;
    private final String fileName;

    public Deserializer(Class<T> classInfo, String fileName) {
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
    public T call() throws Exception {
        T t = classInfo.getConstructor().newInstance();
        FileInputStream outputStream = new FileInputStream(fileName + ".properties");
        Properties properties = new Properties();
        properties.load(outputStream);

        for (Map.Entry<Object, Object> objectObjectEntry : properties.entrySet()) {
            String propertyName = objectObjectEntry.getKey().toString();
            String propertyValue = objectObjectEntry.getValue().toString();
            trySetPropertyValue(t, propertyName, propertyValue);
        }
        return t;
    }

    private void trySetPropertyValue(T object, String propertyName, String propertyValue) throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
        for (Method setter : classInfo.getMethods()) {
            if (setter.getName().equals("set" + propertyName)
                    && setter.getParameterTypes().length == 1
                    && (setter.getParameterTypes()[0].isPrimitive()
                    || setter.getParameterTypes()[0].equals(String.class))) {
                Class<?> parameterType = setter.getParameterTypes()[0];
                if (parameterType.getName().equals("java.lang.String")) {
                    setter.invoke(object, propertyValue);
                } else {
                    Class<?> parameterTypeBoxed = Array.get(Array.newInstance(parameterType, 1), 0).getClass();
                    String parseMethodName = "parse"
                            + parameterType.getName().substring(0, 1).toUpperCase()
                            + parameterType.getName().substring(1).toLowerCase();
                    Method parseMethod = parameterTypeBoxed.getMethod(parseMethodName, String.class);
                    Object parsedPropertyValue = parseMethod.invoke(null, propertyValue);
                    setter.invoke(object, parsedPropertyValue);
                }
                break;
            }
        }
    }
}
