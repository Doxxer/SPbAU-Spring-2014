package com.aptu.sd.coffeemachine.shell;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/23/12, 12:28 AM
 */
public class CommandParseException extends Exception {
    public CommandParseException() {
    }

    public CommandParseException(String s) {
        super(s);
    }

    public CommandParseException(String s, Throwable throwable) {
        super(s, throwable);
    }

    public CommandParseException(Throwable throwable) {
        super(throwable);
    }
}
