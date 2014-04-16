package ru.spbau.turaevT.Lamps;

public class TurnOnFailedException extends RuntimeException {
    TurnOnFailedException(String message) {
        super(message);
    }
}
