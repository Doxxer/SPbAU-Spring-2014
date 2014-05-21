package com.aptu.sd.coffeemachine.shell;

/**
 * Created by IntelliJ IDEA.
 * User: andrey
 * Date: 5/23/12, 12:47 AM
 */
public class CommandUtil {
    public static void assertArgsLength(String[] args, int length) throws CommandParseException {
        if (args.length != length) {
            throw new CommandParseException("Invalid arguments");
        }
    }
}
