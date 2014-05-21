package ru.spbau.turaevT.cw2;

import java.text.MessageFormat;

public class User {
    private String name;
    private int age = 80;

    public User() {
    }

    public void info() {
        System.out.println(MessageFormat.format("Name = {0}", getName()));
        System.out.println(MessageFormat.format("Age = {0}", getAge()));
        System.out.println(MessageFormat.format("ID = {0}", getID()));
        System.out.println(MessageFormat.format("W = {0}", getW()));
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public int getW() {
        return 120;
    }

    public void setW(int w) {

    }

    public int getID() {
        return 42;
    }

    public int method() {
        return 0;
    }
}
