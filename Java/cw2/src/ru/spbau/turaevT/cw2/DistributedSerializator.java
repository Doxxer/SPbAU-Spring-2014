package ru.spbau.turaevT.cw2;

import java.io.*;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.Properties;

public class DistributedSerializator<T> {
    private final Class<T> classInfo;

    public DistributedSerializator(Class<T> classInfo) {
        this.classInfo = classInfo;
    }

    void serialize(T o, String fileName) throws InterruptedException {
        try {
            serialize_worker(o, fileName);
        } catch (NoSuchMethodException e) {
            throw new InterruptedException();
        } catch (IllegalAccessException e) {
            throw new InterruptedException();
        } catch (InvocationTargetException e) {
            throw new InterruptedException();
        } catch (IOException e) {
            throw new InterruptedException();
        }
    }

    private void serialize_worker(T o, String fileName) throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, IOException {
        // check constructor
        classInfo.getConstructor();
        OutputStream outputStream = new FileOutputStream(fileName + ".properties");
        Properties properties = new Properties();

        for (Method property : classInfo.getMethods()) {
            if (property.getName().startsWith("get") && !property.getName().equals("getClass")
                    && (property.getReturnType().isPrimitive() || property.getReturnType().getName().equals("java.lang.String"))
                    && property.getParameterTypes().length == 0) {
                properties.setProperty(property.getName().substring(3), property.invoke(o).toString());
            }
        }
        properties.store(outputStream, "");
    }

    T deserialize(String fileName) throws InterruptedException {
        try {
            return deserialize_worker(fileName);
        } catch (InstantiationException e) {
            throw new InterruptedException();
        } catch (IllegalAccessException e) {
            throw new InterruptedException();
        } catch (InvocationTargetException e) {
            throw new InterruptedException();
        } catch (NoSuchMethodException e) {
            throw new InterruptedException();
        } catch (FileNotFoundException e) {
            throw new InterruptedException();
        } catch (IOException e) {
            throw new InterruptedException();
        }
    }

    private T deserialize_worker(String fileName) throws InstantiationException, IllegalAccessException, InvocationTargetException, NoSuchMethodException, IOException {
        T t = classInfo.getConstructor().newInstance();
        FileInputStream outputStream = new FileInputStream(fileName + ".properties");
        Properties properties = new Properties();
        properties.load(outputStream);

        for (Map.Entry<Object, Object> objectObjectEntry : properties.entrySet()) {
            String propertyName = objectObjectEntry.getKey().toString();
            String propertyValue = objectObjectEntry.getValue().toString();
            for (Method property : classInfo.getMethods()) {
                if (property.getName().equals("set" + propertyName)
                        && property.getParameterTypes().length == 1
                        && (property.getParameterTypes()[0].isPrimitive()
                        || property.getParameterTypes()[0].getName().equals("java.lang.String"))) {
                    Class<?> parameterType = property.getParameterTypes()[0];
                    if (parameterType.getName().equals("java.lang.String")) {
                        property.invoke(t, propertyValue);
                    } else {
                        Class<?> parameterTypeBox = Array.get(Array.newInstance(parameterType, 1), 0).getClass();
                        for (Method setter : parameterTypeBox.getMethods()) {
                            if (setter.getName().equals("valueOf") && setter.getParameterTypes().length == 2) {

                                // it doesn't work. Bug
                                System.out.println(setter.getTypeParameters());
                                System.out.println(setter.getName());
                                setter.invoke(t, propertyValue);
                            }
                        }
                    }
                    break;
                }
            }
        }
        return t;
    }
}
