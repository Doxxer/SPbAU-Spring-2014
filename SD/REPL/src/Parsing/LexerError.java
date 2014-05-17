package Parsing;

public class LexerError extends Exception {

    private final int position;

    public LexerError(String message, int position) {
        super(message);
        this.position = position;
    }

    public int getPosition() {
        return position;
    }
}
