package ru.spbau.turaevT.cw3;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class DistributedChecker implements AutoCloseable {
    private final ExecutorService executorService;

    public DistributedChecker() {
        this.executorService = Executors.newFixedThreadPool(5);
    }

    List<String> check(Class<?> classInfo) throws InterruptedException {
        Future<List<String>> submit = executorService.submit(new Checker(classInfo));
        try {
            return submit.get();
        } catch (ExecutionException e) {
            throw new InterruptedException(e.getMessage());
        }
    }

    @Override
    public void close() throws Exception {
        executorService.shutdown();
    }
}
