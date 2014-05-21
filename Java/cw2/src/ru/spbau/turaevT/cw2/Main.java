package ru.spbau.turaevT.cw2;

public class Main {

    public static void main(String[] args) throws InterruptedException {
        DistributedSerializator<User> s = new DistributedSerializator<User>(User.class);

        User user1 = new User();
        user1.setName("name_user1");
        user1.setW(23);

        s.serialize(user1, "user1");

        User user11 = s.deserialize("user1");
    }
}
