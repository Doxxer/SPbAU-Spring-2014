package ru.spbau.turaevT.cw3;

import java.text.MessageFormat;

public abstract class User {
    private String name;
    private int age = 80;
    public final Integer adasda = 23;

    public User() {
    }

    protected User(String name) {
        this.name = name;
    }

    public void info() {
        System.out.println(MessageFormat.format("Name = {0}", getName()));
        System.out.println(MessageFormat.format("Age = {0}", getAge()));
        System.out.println(MessageFormat.format("ID = {0}", getID()));
    }

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public static Integer getW(final int a)
    {
        return 120;
    }

    public void setW(int w) {

    }

    public int getID() {
        return 42;
    }

    public Boolean method() {
        return false;
    }
}
