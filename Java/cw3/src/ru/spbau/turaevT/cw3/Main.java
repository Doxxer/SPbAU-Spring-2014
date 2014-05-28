package ru.spbau.turaevT.cw3;

import java.text.MessageFormat;
import java.util.List;

public class Main {

    public static void main(String[] args) {
        try (DistributedChecker distributedChecker = new DistributedChecker()) {
            List<String> result = distributedChecker.check(User.class);

            System.out.print(MessageFormat.format("Compile result for {0}: ", User.class.getSimpleName()));
            if (result.isEmpty())
                System.out.println("ok");
            else {
                for (String error : result) {
                    System.out.println(error);
                }
            }
        } catch (Exception e) {
            System.err.println("Error occurred: " + e.getMessage());
        }
    }
}
