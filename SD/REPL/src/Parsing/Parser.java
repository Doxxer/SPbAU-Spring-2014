package Parsing;

import Expression.*;

import java.text.ParseException;

public class Parser {
    private final Lexer lexer;

    public Parser(String line) {
        lexer = new Lexer(line);
    }

    public Exp process() throws ParseException {
        try {
            lexer.process();

            Exp exp = parseExpression();
            if (exp == null || lexer.hasNext()) {
                throw new ParseException("Cannot parse that string", 0);
            }
            return exp;
        } catch (LexerError lexerError) {
            throw new ParseException(lexerError.getMessage(), lexerError.getPosition());
        }
    }

    private Exp parseVariable() {
        if (lexer.current().type != Token.Type.VAR) {
            return null;
        }

        String value = lexer.current().value;
        int varBegin = lexer.current().begin;
        int varEnd = lexer.current().end;

        if (!lexer.hasNext()) {
            return new Var(value, varBegin, varEnd);
        }
        lexer.next();
        if (lexer.current().type == Token.Type.OPERATION_EQ) {
            if (lexer.hasNext()) {
                lexer.next(); // =
                Exp exp = parseExpression();
                if (exp == null)
                    return null;
                return new Assignment(new Var(value, varBegin, varEnd), exp, lexer.current().begin, lexer.current().end);
            }
            else {
                return null;
            }
        }
        return new Var(value, varBegin, varEnd);
    }

    private Exp parseExpression() {
        Exp lhs = parsePrimary();
        if (lhs == null)
            return null;

        if (lexer.current().type != Token.Type.OPERATION_MINUS && lexer.current().type != Token.Type.OPERATION_PLUS)
            return lhs;

        Token.Type type = lexer.current().type;
        if (lexer.hasNext())
            lexer.next();

        Exp rhs = parseExpression();
        if (rhs == null)
            return null;

        if (lhs instanceof Assignment || rhs instanceof Assignment)
            return null;

        switch (type) {
            case OPERATION_PLUS:
                return new Sum(lhs, rhs, lhs.getBegin(), rhs.getEnd());
            case OPERATION_MINUS:
                return new Sub(lhs, rhs, lhs.getBegin(), rhs.getEnd());
            default:
                return null;
        }
    }

    private Exp parsePrimary() {
        Exp lhs = parseValue();
        if (lhs == null)
            return null;

        if (lexer.current().type != Token.Type.OPERATION_DIV && lexer.current().type != Token.Type.OPERATION_MULT)
            return lhs;

        Token.Type type = lexer.current().type;
        if (lexer.hasNext())
            lexer.next();

        Exp rhs = parsePrimary();
        if (rhs == null)
            return null;

        switch (type) {
            case OPERATION_DIV:
                return new Div(lhs, rhs, lhs.getBegin(), rhs.getEnd());
            case OPERATION_MULT:
                return new Mul(lhs, rhs, lhs.getBegin(), rhs.getEnd());
            default:
                return null;
        }
    }

    private Exp parseValue() {
        // parse unary minus
        if (lexer.current().type == Token.Type.OPERATION_MINUS) {
            if (!lexer.hasNext())
                return null;
            lexer.next();
            Exp value = parseValue();
            if (value == null)
                return null;

            return new Sub(new Num(0, lexer.current().begin, lexer.current().begin), value, lexer.current().begin, value.getEnd());
        }

        // not? parse (...)
        if (lexer.current().type == Token.Type.OPENING_BRACKET) {
            if (!lexer.hasNext())
                return null;
            lexer.next();
            Exp exp = parseExpression();
            if (exp == null || lexer.current().type != Token.Type.CLOSING_BRACKET)
                return null;
            if (lexer.hasNext())
                lexer.next();
            return exp;
        }

        Exp value = parseNumber();
        if (value == null)
            value = parseVariable();
        return value;
    }

    private Exp parseNumber() {
        if (lexer.current().type != Token.Type.NUMBER)
            return null;

        Num num = new Num(Double.parseDouble(lexer.current().value), lexer.current().begin, lexer.current().end);
        if (lexer.hasNext())
            lexer.next();
        return num;
    }
}
