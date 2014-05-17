package Parsing;

import java.util.ArrayList;

public class Lexer implements Selector<Token> {
    private final String line;
    private int currentToken = 0;
    private ArrayList<Token> tokens;

    public Lexer(String line) {
        this.line = line;
        tokens = new ArrayList<>();
    }

    public void debug() throws LexerError {
        process();
        for (Token token : tokens) {
            System.out.println(token.type + " " + token.value);
        }
        System.out.println();
    }

    void process() throws LexerError {
        for (int pos = 0; pos < line.length(); pos++) {
            Character c = line.charAt(pos);

            if (Character.isWhitespace(c)) {
                continue;
            }

            Token currentToken = new Token(Token.Type.UNKNOWN, "");
            if (Character.isDigit(c))
                pos = getNumber(pos, currentToken);
            else if (Character.isAlphabetic(c))
                pos = getVariable(pos, currentToken);
            else {
                pos = getOperation(pos, currentToken);
            }

            if (currentToken.type != Token.Type.UNKNOWN) {
                tokens.add(currentToken);
            } else {
                throw new LexerError("unknown token", pos);
            }
        }
        //tokens.add(new Token(Token.Type.EOF, "eof"));
    }

    private int getOperation(int pos, Token token) {
        char c = line.charAt(pos);
        token.value = Character.toString(c);
        switch (c) {
            case '+':
                token.type = Token.Type.OPERATION_PLUS;
                break;
            case '-':
                token.type = Token.Type.OPERATION_MINUS;
                break;
            case '*':
                token.type = Token.Type.OPERATION_MULT;
                break;
            case '/':
                token.type = Token.Type.OPERATION_DIV;
                break;
            case '=':
                token.type = Token.Type.OPERATION_EQ;
                break;
            case ')':
                token.type = Token.Type.CLOSING_BRACKET;
                break;
            case '(':
                token.type = Token.Type.OPENING_BRACKET;
                break;
        }
        return pos;
    }

    private int getNumber(int pos, Token token) throws LexerError {
        String value = "";
        while (pos < line.length() && (Character.isDigit(line.charAt(pos)) || line.charAt(pos) == '.')) {
            value += line.charAt(pos);
            pos++;
        }

        if (pos < line.length() && (Character.isDigit(line.charAt(pos)) || line.charAt(pos) == '_'))
            throw new LexerError("not a number", pos);

        token.type = Token.Type.NUMBER;
        token.value = value;
        return pos - 1;
    }

    private int getVariable(int pos, Token token) {
        String value = "";
        while (pos < line.length() && (
                Character.isAlphabetic(line.charAt(pos)) || line.charAt(pos) == '_' || Character.isDigit(line.charAt(pos)))) {
            value += line.charAt(pos);
            pos++;
        }
        token.type = Token.Type.VAR;
        token.value = value;
        return pos - 1;
    }

    @Override
    public boolean hasNext() {
        return currentToken < tokens.size() - 1;
    }

    @Override
    public Token next() {
        if (hasNext()) {
            currentToken++;
            return current();
        } else return null;
    }

    @Override
    public Token current() {
        return tokens.get(currentToken);
    }
}
