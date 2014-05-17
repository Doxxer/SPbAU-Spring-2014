package Parsing;

public class Token {
    public Type type;
    public String value;
    public int begin;
    public int end;

    public Token(Type type, String value) {
        this.type = type;
        this.value = value;
    }

    public enum Type {
        OPENING_BRACKET,
        CLOSING_BRACKET,
        VAR,
        NUMBER,
        OPERATION_PLUS,  // +
        OPERATION_MINUS, // -
        OPERATION_MULT,  // *
        OPERATION_DIV,   // /
        OPERATION_EQ,    // =
        //EOF,             // EOF
        UNKNOWN
    }
}
