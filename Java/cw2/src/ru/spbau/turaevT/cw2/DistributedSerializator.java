package ru.spbau.turaevT.cw2;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class DistributedSerializator<T> implements AutoCloseable {
    private final Class<T> classInfo;
    private final ExecutorService executorService;

    public DistributedSerializator(Class<T> classInfo) {
        this.classInfo = classInfo;
        this.executorService = Executors.newFixedThreadPool(5);
    }

    void serialize(T object, String fileName) throws InterruptedException {
        Future<Integer> submit = executorService.submit(new Serializer<>(object, classInfo, fileName));
        try {
            submit.get();
        } catch (ExecutionException e) {
            throw new InterruptedException(e.getMessage());
        }
    }

    T deserialize(String fileName) throws InterruptedException {
        Future<T> submit = executorService.submit(new Deserializer<>(classInfo, fileName));
        try {
            return submit.get();
        } catch (ExecutionException e) {
            throw new InterruptedException(e.getMessage());
        }
    }

    @Override
    public void close() throws IOException {
        executorService.shutdown();
    }
}
