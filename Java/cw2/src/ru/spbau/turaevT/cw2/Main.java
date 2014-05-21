package ru.spbau.turaevT.cw2;

import java.io.IOException;

public class Main {

    public static void main(String[] args) {
        try (DistributedSerializator<User> s = new DistributedSerializator<>(User.class)) {

            User user1 = new User();
            user1.setName("name_user1");
            user1.setW(23);
            user1.setAge(111);

            try {
                s.serialize(user1, "user1");
            } catch (InterruptedException e) {
                System.err.println("Error occurred while serializing: " + e.getMessage());
            }

            try {
                User user11 = s.deserialize("user1");
                user11.info();
            } catch (InterruptedException e) {
                System.err.println("Error occurred while deserializing: " + e.getMessage());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
